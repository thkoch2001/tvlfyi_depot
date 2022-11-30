// SPDX-License-Identifier: MIT
// Copyright © 2022 The Tvix Authors

// Code generated by protoc-gen-go. DO NOT EDIT.
// versions:
// 	protoc-gen-go v1.28.1
// 	protoc        (unknown)
// source: tvix/store/protos/rpc_blobstore.proto

package storev1

import (
	protoreflect "google.golang.org/protobuf/reflect/protoreflect"
	protoimpl "google.golang.org/protobuf/runtime/protoimpl"
	reflect "reflect"
	sync "sync"
)

const (
	// Verify that this generated code is sufficiently up-to-date.
	_ = protoimpl.EnforceVersion(20 - protoimpl.MinVersion)
	// Verify that runtime/protoimpl is sufficiently up-to-date.
	_ = protoimpl.EnforceVersion(protoimpl.MaxVersion - 20)
)

type GetBlobRequest struct {
	state         protoimpl.MessageState
	sizeCache     protoimpl.SizeCache
	unknownFields protoimpl.UnknownFields

	// The blake3 digest of the blob requested
	Digest []byte `protobuf:"bytes,1,opt,name=digest,proto3" json:"digest,omitempty"`
}

func (x *GetBlobRequest) Reset() {
	*x = GetBlobRequest{}
	if protoimpl.UnsafeEnabled {
		mi := &file_tvix_store_protos_rpc_blobstore_proto_msgTypes[0]
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		ms.StoreMessageInfo(mi)
	}
}

func (x *GetBlobRequest) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*GetBlobRequest) ProtoMessage() {}

func (x *GetBlobRequest) ProtoReflect() protoreflect.Message {
	mi := &file_tvix_store_protos_rpc_blobstore_proto_msgTypes[0]
	if protoimpl.UnsafeEnabled && x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use GetBlobRequest.ProtoReflect.Descriptor instead.
func (*GetBlobRequest) Descriptor() ([]byte, []int) {
	return file_tvix_store_protos_rpc_blobstore_proto_rawDescGZIP(), []int{0}
}

func (x *GetBlobRequest) GetDigest() []byte {
	if x != nil {
		return x.Digest
	}
	return nil
}

type PutBlobResponse struct {
	state         protoimpl.MessageState
	sizeCache     protoimpl.SizeCache
	unknownFields protoimpl.UnknownFields

	// The blake3 digest of the data that was sent.
	Digest []byte `protobuf:"bytes,1,opt,name=digest,proto3" json:"digest,omitempty"`
}

func (x *PutBlobResponse) Reset() {
	*x = PutBlobResponse{}
	if protoimpl.UnsafeEnabled {
		mi := &file_tvix_store_protos_rpc_blobstore_proto_msgTypes[1]
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		ms.StoreMessageInfo(mi)
	}
}

func (x *PutBlobResponse) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*PutBlobResponse) ProtoMessage() {}

func (x *PutBlobResponse) ProtoReflect() protoreflect.Message {
	mi := &file_tvix_store_protos_rpc_blobstore_proto_msgTypes[1]
	if protoimpl.UnsafeEnabled && x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use PutBlobResponse.ProtoReflect.Descriptor instead.
func (*PutBlobResponse) Descriptor() ([]byte, []int) {
	return file_tvix_store_protos_rpc_blobstore_proto_rawDescGZIP(), []int{1}
}

func (x *PutBlobResponse) GetDigest() []byte {
	if x != nil {
		return x.Digest
	}
	return nil
}

// This represents a part of a chunk.
// Blobs are sent in smaller chunks to keep message sizes manageable.
type BlobChunk struct {
	state         protoimpl.MessageState
	sizeCache     protoimpl.SizeCache
	unknownFields protoimpl.UnknownFields

	Data []byte `protobuf:"bytes,1,opt,name=data,proto3" json:"data,omitempty"`
}

func (x *BlobChunk) Reset() {
	*x = BlobChunk{}
	if protoimpl.UnsafeEnabled {
		mi := &file_tvix_store_protos_rpc_blobstore_proto_msgTypes[2]
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		ms.StoreMessageInfo(mi)
	}
}

func (x *BlobChunk) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*BlobChunk) ProtoMessage() {}

func (x *BlobChunk) ProtoReflect() protoreflect.Message {
	mi := &file_tvix_store_protos_rpc_blobstore_proto_msgTypes[2]
	if protoimpl.UnsafeEnabled && x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use BlobChunk.ProtoReflect.Descriptor instead.
func (*BlobChunk) Descriptor() ([]byte, []int) {
	return file_tvix_store_protos_rpc_blobstore_proto_rawDescGZIP(), []int{2}
}

func (x *BlobChunk) GetData() []byte {
	if x != nil {
		return x.Data
	}
	return nil
}

var File_tvix_store_protos_rpc_blobstore_proto protoreflect.FileDescriptor

var file_tvix_store_protos_rpc_blobstore_proto_rawDesc = []byte{
	0x0a, 0x25, 0x74, 0x76, 0x69, 0x78, 0x2f, 0x73, 0x74, 0x6f, 0x72, 0x65, 0x2f, 0x70, 0x72, 0x6f,
	0x74, 0x6f, 0x73, 0x2f, 0x72, 0x70, 0x63, 0x5f, 0x62, 0x6c, 0x6f, 0x62, 0x73, 0x74, 0x6f, 0x72,
	0x65, 0x2e, 0x70, 0x72, 0x6f, 0x74, 0x6f, 0x12, 0x0d, 0x74, 0x76, 0x69, 0x78, 0x2e, 0x73, 0x74,
	0x6f, 0x72, 0x65, 0x2e, 0x76, 0x31, 0x22, 0x28, 0x0a, 0x0e, 0x47, 0x65, 0x74, 0x42, 0x6c, 0x6f,
	0x62, 0x52, 0x65, 0x71, 0x75, 0x65, 0x73, 0x74, 0x12, 0x16, 0x0a, 0x06, 0x64, 0x69, 0x67, 0x65,
	0x73, 0x74, 0x18, 0x01, 0x20, 0x01, 0x28, 0x0c, 0x52, 0x06, 0x64, 0x69, 0x67, 0x65, 0x73, 0x74,
	0x22, 0x29, 0x0a, 0x0f, 0x50, 0x75, 0x74, 0x42, 0x6c, 0x6f, 0x62, 0x52, 0x65, 0x73, 0x70, 0x6f,
	0x6e, 0x73, 0x65, 0x12, 0x16, 0x0a, 0x06, 0x64, 0x69, 0x67, 0x65, 0x73, 0x74, 0x18, 0x01, 0x20,
	0x01, 0x28, 0x0c, 0x52, 0x06, 0x64, 0x69, 0x67, 0x65, 0x73, 0x74, 0x22, 0x1f, 0x0a, 0x09, 0x42,
	0x6c, 0x6f, 0x62, 0x43, 0x68, 0x75, 0x6e, 0x6b, 0x12, 0x12, 0x0a, 0x04, 0x64, 0x61, 0x74, 0x61,
	0x18, 0x01, 0x20, 0x01, 0x28, 0x0c, 0x52, 0x04, 0x64, 0x61, 0x74, 0x61, 0x32, 0x92, 0x01, 0x0a,
	0x0b, 0x42, 0x6c, 0x6f, 0x62, 0x53, 0x65, 0x72, 0x76, 0x69, 0x63, 0x65, 0x12, 0x40, 0x0a, 0x03,
	0x47, 0x65, 0x74, 0x12, 0x1d, 0x2e, 0x74, 0x76, 0x69, 0x78, 0x2e, 0x73, 0x74, 0x6f, 0x72, 0x65,
	0x2e, 0x76, 0x31, 0x2e, 0x47, 0x65, 0x74, 0x42, 0x6c, 0x6f, 0x62, 0x52, 0x65, 0x71, 0x75, 0x65,
	0x73, 0x74, 0x1a, 0x18, 0x2e, 0x74, 0x76, 0x69, 0x78, 0x2e, 0x73, 0x74, 0x6f, 0x72, 0x65, 0x2e,
	0x76, 0x31, 0x2e, 0x42, 0x6c, 0x6f, 0x62, 0x43, 0x68, 0x75, 0x6e, 0x6b, 0x30, 0x01, 0x12, 0x41,
	0x0a, 0x03, 0x50, 0x75, 0x74, 0x12, 0x18, 0x2e, 0x74, 0x76, 0x69, 0x78, 0x2e, 0x73, 0x74, 0x6f,
	0x72, 0x65, 0x2e, 0x76, 0x31, 0x2e, 0x42, 0x6c, 0x6f, 0x62, 0x43, 0x68, 0x75, 0x6e, 0x6b, 0x1a,
	0x1e, 0x2e, 0x74, 0x76, 0x69, 0x78, 0x2e, 0x73, 0x74, 0x6f, 0x72, 0x65, 0x2e, 0x76, 0x31, 0x2e,
	0x50, 0x75, 0x74, 0x42, 0x6c, 0x6f, 0x62, 0x52, 0x65, 0x73, 0x70, 0x6f, 0x6e, 0x73, 0x65, 0x28,
	0x01, 0x42, 0x28, 0x5a, 0x26, 0x63, 0x6f, 0x64, 0x65, 0x2e, 0x74, 0x76, 0x6c, 0x2e, 0x66, 0x79,
	0x69, 0x2f, 0x74, 0x76, 0x69, 0x78, 0x2f, 0x73, 0x74, 0x6f, 0x72, 0x65, 0x2f, 0x70, 0x72, 0x6f,
	0x74, 0x6f, 0x73, 0x3b, 0x73, 0x74, 0x6f, 0x72, 0x65, 0x76, 0x31, 0x62, 0x06, 0x70, 0x72, 0x6f,
	0x74, 0x6f, 0x33,
}

var (
	file_tvix_store_protos_rpc_blobstore_proto_rawDescOnce sync.Once
	file_tvix_store_protos_rpc_blobstore_proto_rawDescData = file_tvix_store_protos_rpc_blobstore_proto_rawDesc
)

func file_tvix_store_protos_rpc_blobstore_proto_rawDescGZIP() []byte {
	file_tvix_store_protos_rpc_blobstore_proto_rawDescOnce.Do(func() {
		file_tvix_store_protos_rpc_blobstore_proto_rawDescData = protoimpl.X.CompressGZIP(file_tvix_store_protos_rpc_blobstore_proto_rawDescData)
	})
	return file_tvix_store_protos_rpc_blobstore_proto_rawDescData
}

var file_tvix_store_protos_rpc_blobstore_proto_msgTypes = make([]protoimpl.MessageInfo, 3)
var file_tvix_store_protos_rpc_blobstore_proto_goTypes = []interface{}{
	(*GetBlobRequest)(nil),  // 0: tvix.store.v1.GetBlobRequest
	(*PutBlobResponse)(nil), // 1: tvix.store.v1.PutBlobResponse
	(*BlobChunk)(nil),       // 2: tvix.store.v1.BlobChunk
}
var file_tvix_store_protos_rpc_blobstore_proto_depIdxs = []int32{
	0, // 0: tvix.store.v1.BlobService.Get:input_type -> tvix.store.v1.GetBlobRequest
	2, // 1: tvix.store.v1.BlobService.Put:input_type -> tvix.store.v1.BlobChunk
	2, // 2: tvix.store.v1.BlobService.Get:output_type -> tvix.store.v1.BlobChunk
	1, // 3: tvix.store.v1.BlobService.Put:output_type -> tvix.store.v1.PutBlobResponse
	2, // [2:4] is the sub-list for method output_type
	0, // [0:2] is the sub-list for method input_type
	0, // [0:0] is the sub-list for extension type_name
	0, // [0:0] is the sub-list for extension extendee
	0, // [0:0] is the sub-list for field type_name
}

func init() { file_tvix_store_protos_rpc_blobstore_proto_init() }
func file_tvix_store_protos_rpc_blobstore_proto_init() {
	if File_tvix_store_protos_rpc_blobstore_proto != nil {
		return
	}
	if !protoimpl.UnsafeEnabled {
		file_tvix_store_protos_rpc_blobstore_proto_msgTypes[0].Exporter = func(v interface{}, i int) interface{} {
			switch v := v.(*GetBlobRequest); i {
			case 0:
				return &v.state
			case 1:
				return &v.sizeCache
			case 2:
				return &v.unknownFields
			default:
				return nil
			}
		}
		file_tvix_store_protos_rpc_blobstore_proto_msgTypes[1].Exporter = func(v interface{}, i int) interface{} {
			switch v := v.(*PutBlobResponse); i {
			case 0:
				return &v.state
			case 1:
				return &v.sizeCache
			case 2:
				return &v.unknownFields
			default:
				return nil
			}
		}
		file_tvix_store_protos_rpc_blobstore_proto_msgTypes[2].Exporter = func(v interface{}, i int) interface{} {
			switch v := v.(*BlobChunk); i {
			case 0:
				return &v.state
			case 1:
				return &v.sizeCache
			case 2:
				return &v.unknownFields
			default:
				return nil
			}
		}
	}
	type x struct{}
	out := protoimpl.TypeBuilder{
		File: protoimpl.DescBuilder{
			GoPackagePath: reflect.TypeOf(x{}).PkgPath(),
			RawDescriptor: file_tvix_store_protos_rpc_blobstore_proto_rawDesc,
			NumEnums:      0,
			NumMessages:   3,
			NumExtensions: 0,
			NumServices:   1,
		},
		GoTypes:           file_tvix_store_protos_rpc_blobstore_proto_goTypes,
		DependencyIndexes: file_tvix_store_protos_rpc_blobstore_proto_depIdxs,
		MessageInfos:      file_tvix_store_protos_rpc_blobstore_proto_msgTypes,
	}.Build()
	File_tvix_store_protos_rpc_blobstore_proto = out.File
	file_tvix_store_protos_rpc_blobstore_proto_rawDesc = nil
	file_tvix_store_protos_rpc_blobstore_proto_goTypes = nil
	file_tvix_store_protos_rpc_blobstore_proto_depIdxs = nil
}
