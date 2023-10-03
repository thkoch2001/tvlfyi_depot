package pathinfosvc

import (
	"bufio"
	"bytes"
	"context"
	"encoding/base64"
	"fmt"
	"io"
	"net/http"
	"net/url"

	castorev1pb "code.tvl.fyi/tvix/castore/protos"
	"code.tvl.fyi/tvix/nar-bridge/pkg/importer"
	storev1pb "code.tvl.fyi/tvix/store/protos"
	mh "github.com/multiformats/go-multihash/core"
	"github.com/nix-community/go-nix/pkg/narinfo"
	"github.com/nix-community/go-nix/pkg/nixbase32"
	"github.com/nix-community/go-nix/pkg/nixpath"
	"github.com/sirupsen/logrus"
	"github.com/ulikunitz/xz"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
)

var _ storev1pb.PathInfoServiceServer = &PathInfoServiceServer{}

// PathInfoServiceServer exposes a Nix HTTP Binary Cache as a storev1pb.PathInfoServiceServer.
type PathInfoServiceServer struct {
	storev1pb.UnimplementedPathInfoServiceServer
	httpEndpoint *url.URL
	httpClient   *http.Client
	// TODO: signatures

	directoryServiceClient castorev1pb.DirectoryServiceClient
	blobServiceClient      castorev1pb.BlobServiceClient
}

func New(httpEndpoint *url.URL, httpClient *http.Client, directoryServiceClient castorev1pb.DirectoryServiceClient, blobServiceClient castorev1pb.BlobServiceClient) *PathInfoServiceServer {
	return &PathInfoServiceServer{
		httpEndpoint:           httpEndpoint,
		httpClient:             httpClient,
		directoryServiceClient: directoryServiceClient,
		blobServiceClient:      blobServiceClient,
	}
}

// CalculateNAR implements storev1.PathInfoServiceServer.
// It returns PermissionDenied, as clients are supposed to calculate NAR hashes themselves.
func (*PathInfoServiceServer) CalculateNAR(context.Context, *castorev1pb.Node) (*storev1pb.CalculateNARResponse, error) {
	return nil, status.Error(codes.PermissionDenied, "do it yourself please")
}

// Get implements storev1.PathInfoServiceServer.
// It only supports lookup my outhash, translates them to a corresponding GET $outhash.narinfo request,
// ingests the NAR file, while populating blob and directory service, then returns the PathInfo node.
// Subsequent requests will traverse the NAR file again, so make sure to compose this with another
// PathInfoService as caching layer.
func (p *PathInfoServiceServer) Get(ctx context.Context, getPathInfoRequest *storev1pb.GetPathInfoRequest) (*storev1pb.PathInfo, error) {
	outputHash := getPathInfoRequest.GetByOutputHash()
	if outputHash == nil {
		return nil, status.Error(codes.Unimplemented, "only by output hash supported")
	}

	// construct NARInfo URL
	narinfoURL := p.httpEndpoint.JoinPath(fmt.Sprintf("%v.narinfo", nixbase32.EncodeToString(outputHash)))
	// used in error handling only
	outputHashB64 := base64.StdEncoding.EncodeToString(outputHash)

	log := logrus.WithField("output_hash", outputHashB64)

	// Do a HEAD first, if it doesn't exist we can early exit
	{
		rq, err := http.NewRequestWithContext(ctx, "HEAD", narinfoURL.String(), nil)
		if err != nil {
			log.WithError(err).Error("Unable to construct HEAD NARInfo request")
			return nil, status.Errorf(codes.Internal, "unable to construct HEAD NARInfo request")
		}

		resp, err := p.httpClient.Do(rq)
		if err != nil {
			log.WithError(err).Error("Unable to do HEAD NARInfo request")
			return nil, status.Errorf(codes.Internal, "unable to do HEAD NARInfo request")
		}

		// In the case of a 404, return a NotFound.
		// We also return a NotFound in case of a 403 - this is to match the behaviour as Nix,
		// when querying nix-cache.s3.amazonaws.com directly, rather than cache.nixos.org.
		if resp.StatusCode == http.StatusNotFound || resp.StatusCode == http.StatusForbidden {
			return nil, status.Errorf(codes.NotFound, "outhash %s does not exist", outputHashB64)
		}
	}

	// Now construct a GET request.
	niRq, err := http.NewRequestWithContext(ctx, "GET", narinfoURL.String(), nil)
	if err != nil {
		log.WithError(err).Error("unable to construct GET NARInfo request")
		return nil, status.Errorf(codes.Internal, "unable to construct GET NARInfo request")
	}

	// Do the actual request; this follows redirects.
	niResp, err := p.httpClient.Do(niRq)
	if err != nil {
		log.WithError(err).Error("unable to do GET NARInfo request")
		return nil, status.Errorf(codes.Internal, "unable to do GET NARInfo request")
	}
	defer niResp.Body.Close()

	if niResp.StatusCode == http.StatusNotFound || niResp.StatusCode == http.StatusForbidden {
		// Odd to see if the HEAD just succeeded though.
		log.Warn("Got 404 trying to GET NARInfo, even though HEAD succeeded before")
		return nil, status.Errorf(codes.NotFound, "outhash %s does not exist", outputHashB64)
	}

	if niResp.StatusCode < 200 || niResp.StatusCode >= 300 {
		log.WithField("status_code", niResp.StatusCode).Warn("Got non-success when trying to GET NARInfo")
		return nil, status.Errorf(codes.Internal, "got status code %v trying to access NARInfo", niResp.StatusCode)
	}

	// parse the NARInfo file.
	narInfo, err := narinfo.Parse(niResp.Body)
	if err != nil {
		log.WithError(err).Warn("Unable to parse NARInfo")
		return nil, status.Errorf(codes.Internal, "unable to parse NARInfo for %s", outputHash)
	}

	// validate the NAR file. This ensures strings we need to parse actually parse,
	// so we can just plain panic further down.
	if err := narInfo.Check(); err != nil {
		log.WithError(err).Warn("Unable to validate NARInfo")
		return nil, status.Errorf(codes.Internal, "unable to validate NARInfo: %s", err)
	}

	// only allow sha256 here. Is anything else even supported by Nix?
	if narInfo.NarHash.HashType != mh.SHA2_256 {
		log.Error("Unsupported hash type")
		return nil, status.Errorf(codes.Internal, "unsuported hash type in NarHash: %s", narInfo.NarHash.SRIString())
	}

	// TODO: calculate fingerprint, check with trusted pubkeys, decide what to do on mismatch

	log = log.WithField("narinfo_narhash", narInfo.NarHash.SRIString())
	log = log.WithField("narurl", narInfo.URL)

	// prepare the GET request for the NAR file.
	narRq, err := http.NewRequestWithContext(ctx, "GET", p.httpEndpoint.JoinPath(narInfo.URL).String(), nil)
	if err != nil {
		log.WithError(err).Error("unable to construct GET NAR request")
		return nil, status.Errorf(codes.Internal, "unable to construct GET nar request")
	}

	log.Info("requesting NAR")
	narResp, err := p.httpClient.Do(narRq)
	if err != nil {
		log.WithError(err).Error("error during GET NAR request")
		return nil, status.Errorf(codes.Internal, "error during GET NAR request")
	}
	defer narResp.Body.Close()

	// If we can't access the NAR file that the NARInfo is referring to, this is a store inconsistency.
	// Propagate a more serious Internal error, rather than just a NotFound.
	if narResp.StatusCode == http.StatusNotFound || narResp.StatusCode == http.StatusForbidden {
		log.Error("Unable to find NAR")
		return nil, status.Errorf(codes.Internal, "nar at path %s does not exist", narInfo.URL)
	}

	// wrap narResp.Body with some buffer.
	// We already defer closing the http body, so it's ok to loose io.Close here.
	var narBody io.Reader
	narBody = bufio.NewReaderSize(narResp.Body, 10*1024*1024)

	if narInfo.Compression == "none" {
		// Nothing to do
	} else if narInfo.Compression == "xz" {
		narBody, err = xz.NewReader(narBody)
		if err != nil {
			log.WithError(err).Error("failed to open xz")
			return nil, status.Errorf(codes.Internal, "failed to open xz")
		}
	} else {
		log.WithField("nar_compression", narInfo.Compression).Error("unsupported compression")
		return nil, fmt.Errorf("unsupported NAR compression: %s", narInfo.Compression)
	}

	directoriesUploader := importer.NewDirectoriesUploader(ctx, p.directoryServiceClient)
	defer directoriesUploader.Done() //nolint:errcheck

	blobUploaderCb := importer.GenBlobUploaderCb(ctx, p.blobServiceClient)

	importedPathInfo, err := importer.Import(
		ctx,
		narBody,
		func(blobReader io.Reader) ([]byte, error) {
			blobDigest, err := blobUploaderCb(blobReader)
			if err != nil {
				return nil, err
			}
			log.WithField("blob_digest", base64.StdEncoding.EncodeToString(blobDigest)).Debug("upload blob")
			return blobDigest, nil
		},
		func(directory *castorev1pb.Directory) ([]byte, error) {
			directoryDigest, err := directoriesUploader.Put(directory)
			if err != nil {
				return nil, err
			}
			log.WithField("directory_digest", base64.StdEncoding.EncodeToString(directoryDigest)).Debug("upload directory")
			return directoryDigest, nil
		},
	)

	if err != nil {
		log.WithError(err).Error("error during NAR import")
		return nil, status.Error(codes.Internal, "error during NAR import")
	}

	// Close the directories uploader
	directoriesPutResponse, err := directoriesUploader.Done()
	if err != nil {
		log.WithError(err).Error("error during directory upload")

		return nil, status.Error(codes.Internal, "error during directory upload")
	}

	// If we uploaded directories (so directoriesPutResponse doesn't return null),
	// the RootDigest field in directoriesPutResponse should match the digest
	// returned in importedPathInfo.
	// This check ensures the directory service came up with the same root hash as we did.
	if directoriesPutResponse != nil {
		rootDigestPathInfo := importedPathInfo.GetNode().GetDirectory().GetDigest()
		rootDigestDirectoriesPutResponse := directoriesPutResponse.GetRootDigest()

		log := log.WithFields(logrus.Fields{
			"root_digest_pathinfo":             rootDigestPathInfo,
			"root_digest_directories_put_resp": rootDigestDirectoriesPutResponse,
		})
		if !bytes.Equal(rootDigestPathInfo, rootDigestDirectoriesPutResponse) {
			log.Errorf("returned root digest doesn't match what's calculated")
			return nil, status.Error(codes.Internal, "error in root digest calculation")
		}
	}

	// Compare nar hash in the NARInfo with the one we calculated while reading the NAR
	// We already checked above that the digest is in sha256.
	importedNarSha256 := importedPathInfo.GetNarinfo().GetNarSha256()
	if !bytes.Equal(narInfo.NarHash.Digest(), importedNarSha256) {
		log := log.WithField("imported_nar_sha256", base64.StdEncoding.EncodeToString(importedNarSha256))
		log.Error("imported digest doesn't match NARInfo digest")

		return nil, fmt.Errorf("imported digest doesn't match NARInfo digest")
	}

	// annotate importedPathInfo with the rest of the metadata from NARINfo.

	// extract the output hashes from narInfo.References into importedPathInfo.References.
	{
		// Length of the hash portion of the store path in base32.
		encodedPathHashSize := nixbase32.EncodedLen(20)
		for _, referenceStr := range narInfo.References {
			if len(referenceStr) < encodedPathHashSize {
				return nil, fmt.Errorf("reference string '%s' is too small", referenceStr)
			}

			decodedReferenceHash, err := nixbase32.DecodeString(referenceStr[0:encodedPathHashSize])
			if err != nil {
				return nil, fmt.Errorf("unable to decode reference string '%s': %w", referenceStr, err)

			}
			importedPathInfo.References = append(importedPathInfo.References, decodedReferenceHash)
		}
	}
	importedPathInfo.Narinfo.ReferenceNames = narInfo.References

	for _, signature := range narInfo.Signatures {
		importedPathInfo.Narinfo.Signatures = append(importedPathInfo.Narinfo.Signatures, &storev1pb.NARInfo_Signature{
			Name: signature.Name,
			Data: signature.Data,
		})
	}

	// set the root node name to the basename of the output path in the narInfo.
	// currently the root node has no name yet.
	outPath, err := nixpath.FromString(narInfo.StorePath)
	if err != nil {
		// unreachable due to nixpath.Check()
		panic(err)
	}
	newName := []byte(nixbase32.EncodeToString(outPath.Digest) + "-" + string(outPath.Name))

	// set the root name in all three cases.
	if node := importedPathInfo.Node.GetDirectory(); node != nil {
		node.Name = newName
	} else if node := importedPathInfo.Node.GetFile(); node != nil {
		node.Name = newName
	} else if node := importedPathInfo.Node.GetSymlink(); node != nil {
		node.Name = newName
	} else {
		panic("node may not be nil")
	}

	return importedPathInfo, nil

	// TODO: Deriver, System, CA
}

// List implements storev1.PathInfoServiceServer.
// It returns a permission denied, because normally you can't get a listing
func (*PathInfoServiceServer) List(*storev1pb.ListPathInfoRequest, storev1pb.PathInfoService_ListServer) error {
	return status.Error(codes.Unimplemented, "unimplemented")
}

// Put implements storev1.PathInfoServiceServer.
func (*PathInfoServiceServer) Put(context.Context, *storev1pb.PathInfo) (*storev1pb.PathInfo, error) {
	return nil, status.Error(codes.Unimplemented, "unimplemented")
}
