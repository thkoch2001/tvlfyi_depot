// SPDX-License-Identifier: MIT
// Copyright © 2022 The Tvix Authors

// Code generated by protoc-gen-go-grpc. DO NOT EDIT.
// versions:
// - protoc-gen-go-grpc v1.3.0
// - protoc             (unknown)
// source: tvix/store/protos/rpc_pathinfo.proto

package storev1

import (
	castore_go "code.tvl.fyi/tvix/castore-go"
	context "context"
	grpc "google.golang.org/grpc"
	codes "google.golang.org/grpc/codes"
	status "google.golang.org/grpc/status"
)

// This is a compile-time assertion to ensure that this generated file
// is compatible with the grpc package it is being compiled against.
// Requires gRPC-Go v1.32.0 or later.
const _ = grpc.SupportPackageIsVersion7

const (
	PathInfoService_Get_FullMethodName          = "/tvix.store.v1.PathInfoService/Get"
	PathInfoService_Put_FullMethodName          = "/tvix.store.v1.PathInfoService/Put"
	PathInfoService_CalculateNAR_FullMethodName = "/tvix.store.v1.PathInfoService/CalculateNAR"
	PathInfoService_List_FullMethodName         = "/tvix.store.v1.PathInfoService/List"
)

// PathInfoServiceClient is the client API for PathInfoService service.
//
// For semantics around ctx use and closing/ending streaming RPCs, please refer to https://pkg.go.dev/google.golang.org/grpc/?tab=doc#ClientConn.NewStream.
type PathInfoServiceClient interface {
	// Return a PathInfo message matching the criteria specified in the
	// GetPathInfoRequest message.
	Get(ctx context.Context, in *GetPathInfoRequest, opts ...grpc.CallOption) (*PathInfo, error)
	// Upload a PathInfo object to the remote end. It MUST not return until the
	// PathInfo object has been written on the the remote end.
	//
	// The remote end MAY check if a potential DirectoryNode has already been
	// uploaded.
	//
	// Uploading clients SHOULD obviously not steer other machines to try to
	// substitute before from the remote end before having finished uploading
	// PathInfo, Directories and Blobs.
	// The returned PathInfo object MAY contain additional narinfo signatures,
	// but is otherwise left untouched.
	Put(ctx context.Context, in *PathInfo, opts ...grpc.CallOption) (*PathInfo, error)
	// Calculate the NAR representation of the contents specified by the
	// root_node. The calculation SHOULD be cached server-side for subsequent
	// requests.
	//
	// All references (to blobs or Directory messages) MUST already exist in
	// the store.
	//
	// The method can be used to produce a Nix fixed-output path, which
	// contains the (compressed) sha256 of the NAR content representation in
	// the root_node name (suffixed with the name).
	//
	// It can also be used to calculate arbitrary NAR hashes of output paths,
	// in case a legacy Nix Binary Cache frontend is provided.
	CalculateNAR(ctx context.Context, in *castore_go.Node, opts ...grpc.CallOption) (*CalculateNARResponse, error)
	// Return a stream of PathInfo messages matching the criteria specified in
	// ListPathInfoRequest.
	List(ctx context.Context, in *ListPathInfoRequest, opts ...grpc.CallOption) (PathInfoService_ListClient, error)
}

type pathInfoServiceClient struct {
	cc grpc.ClientConnInterface
}

func NewPathInfoServiceClient(cc grpc.ClientConnInterface) PathInfoServiceClient {
	return &pathInfoServiceClient{cc}
}

func (c *pathInfoServiceClient) Get(ctx context.Context, in *GetPathInfoRequest, opts ...grpc.CallOption) (*PathInfo, error) {
	out := new(PathInfo)
	err := c.cc.Invoke(ctx, PathInfoService_Get_FullMethodName, in, out, opts...)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (c *pathInfoServiceClient) Put(ctx context.Context, in *PathInfo, opts ...grpc.CallOption) (*PathInfo, error) {
	out := new(PathInfo)
	err := c.cc.Invoke(ctx, PathInfoService_Put_FullMethodName, in, out, opts...)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (c *pathInfoServiceClient) CalculateNAR(ctx context.Context, in *castore_go.Node, opts ...grpc.CallOption) (*CalculateNARResponse, error) {
	out := new(CalculateNARResponse)
	err := c.cc.Invoke(ctx, PathInfoService_CalculateNAR_FullMethodName, in, out, opts...)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (c *pathInfoServiceClient) List(ctx context.Context, in *ListPathInfoRequest, opts ...grpc.CallOption) (PathInfoService_ListClient, error) {
	stream, err := c.cc.NewStream(ctx, &PathInfoService_ServiceDesc.Streams[0], PathInfoService_List_FullMethodName, opts...)
	if err != nil {
		return nil, err
	}
	x := &pathInfoServiceListClient{stream}
	if err := x.ClientStream.SendMsg(in); err != nil {
		return nil, err
	}
	if err := x.ClientStream.CloseSend(); err != nil {
		return nil, err
	}
	return x, nil
}

type PathInfoService_ListClient interface {
	Recv() (*PathInfo, error)
	grpc.ClientStream
}

type pathInfoServiceListClient struct {
	grpc.ClientStream
}

func (x *pathInfoServiceListClient) Recv() (*PathInfo, error) {
	m := new(PathInfo)
	if err := x.ClientStream.RecvMsg(m); err != nil {
		return nil, err
	}
	return m, nil
}

// PathInfoServiceServer is the server API for PathInfoService service.
// All implementations must embed UnimplementedPathInfoServiceServer
// for forward compatibility
type PathInfoServiceServer interface {
	// Return a PathInfo message matching the criteria specified in the
	// GetPathInfoRequest message.
	Get(context.Context, *GetPathInfoRequest) (*PathInfo, error)
	// Upload a PathInfo object to the remote end. It MUST not return until the
	// PathInfo object has been written on the the remote end.
	//
	// The remote end MAY check if a potential DirectoryNode has already been
	// uploaded.
	//
	// Uploading clients SHOULD obviously not steer other machines to try to
	// substitute before from the remote end before having finished uploading
	// PathInfo, Directories and Blobs.
	// The returned PathInfo object MAY contain additional narinfo signatures,
	// but is otherwise left untouched.
	Put(context.Context, *PathInfo) (*PathInfo, error)
	// Calculate the NAR representation of the contents specified by the
	// root_node. The calculation SHOULD be cached server-side for subsequent
	// requests.
	//
	// All references (to blobs or Directory messages) MUST already exist in
	// the store.
	//
	// The method can be used to produce a Nix fixed-output path, which
	// contains the (compressed) sha256 of the NAR content representation in
	// the root_node name (suffixed with the name).
	//
	// It can also be used to calculate arbitrary NAR hashes of output paths,
	// in case a legacy Nix Binary Cache frontend is provided.
	CalculateNAR(context.Context, *castore_go.Node) (*CalculateNARResponse, error)
	// Return a stream of PathInfo messages matching the criteria specified in
	// ListPathInfoRequest.
	List(*ListPathInfoRequest, PathInfoService_ListServer) error
	mustEmbedUnimplementedPathInfoServiceServer()
}

// UnimplementedPathInfoServiceServer must be embedded to have forward compatible implementations.
type UnimplementedPathInfoServiceServer struct {
}

func (UnimplementedPathInfoServiceServer) Get(context.Context, *GetPathInfoRequest) (*PathInfo, error) {
	return nil, status.Errorf(codes.Unimplemented, "method Get not implemented")
}
func (UnimplementedPathInfoServiceServer) Put(context.Context, *PathInfo) (*PathInfo, error) {
	return nil, status.Errorf(codes.Unimplemented, "method Put not implemented")
}
func (UnimplementedPathInfoServiceServer) CalculateNAR(context.Context, *castore_go.Node) (*CalculateNARResponse, error) {
	return nil, status.Errorf(codes.Unimplemented, "method CalculateNAR not implemented")
}
func (UnimplementedPathInfoServiceServer) List(*ListPathInfoRequest, PathInfoService_ListServer) error {
	return status.Errorf(codes.Unimplemented, "method List not implemented")
}
func (UnimplementedPathInfoServiceServer) mustEmbedUnimplementedPathInfoServiceServer() {}

// UnsafePathInfoServiceServer may be embedded to opt out of forward compatibility for this service.
// Use of this interface is not recommended, as added methods to PathInfoServiceServer will
// result in compilation errors.
type UnsafePathInfoServiceServer interface {
	mustEmbedUnimplementedPathInfoServiceServer()
}

func RegisterPathInfoServiceServer(s grpc.ServiceRegistrar, srv PathInfoServiceServer) {
	s.RegisterService(&PathInfoService_ServiceDesc, srv)
}

func _PathInfoService_Get_Handler(srv interface{}, ctx context.Context, dec func(interface{}) error, interceptor grpc.UnaryServerInterceptor) (interface{}, error) {
	in := new(GetPathInfoRequest)
	if err := dec(in); err != nil {
		return nil, err
	}
	if interceptor == nil {
		return srv.(PathInfoServiceServer).Get(ctx, in)
	}
	info := &grpc.UnaryServerInfo{
		Server:     srv,
		FullMethod: PathInfoService_Get_FullMethodName,
	}
	handler := func(ctx context.Context, req interface{}) (interface{}, error) {
		return srv.(PathInfoServiceServer).Get(ctx, req.(*GetPathInfoRequest))
	}
	return interceptor(ctx, in, info, handler)
}

func _PathInfoService_Put_Handler(srv interface{}, ctx context.Context, dec func(interface{}) error, interceptor grpc.UnaryServerInterceptor) (interface{}, error) {
	in := new(PathInfo)
	if err := dec(in); err != nil {
		return nil, err
	}
	if interceptor == nil {
		return srv.(PathInfoServiceServer).Put(ctx, in)
	}
	info := &grpc.UnaryServerInfo{
		Server:     srv,
		FullMethod: PathInfoService_Put_FullMethodName,
	}
	handler := func(ctx context.Context, req interface{}) (interface{}, error) {
		return srv.(PathInfoServiceServer).Put(ctx, req.(*PathInfo))
	}
	return interceptor(ctx, in, info, handler)
}

func _PathInfoService_CalculateNAR_Handler(srv interface{}, ctx context.Context, dec func(interface{}) error, interceptor grpc.UnaryServerInterceptor) (interface{}, error) {
	in := new(castore_go.Node)
	if err := dec(in); err != nil {
		return nil, err
	}
	if interceptor == nil {
		return srv.(PathInfoServiceServer).CalculateNAR(ctx, in)
	}
	info := &grpc.UnaryServerInfo{
		Server:     srv,
		FullMethod: PathInfoService_CalculateNAR_FullMethodName,
	}
	handler := func(ctx context.Context, req interface{}) (interface{}, error) {
		return srv.(PathInfoServiceServer).CalculateNAR(ctx, req.(*castore_go.Node))
	}
	return interceptor(ctx, in, info, handler)
}

func _PathInfoService_List_Handler(srv interface{}, stream grpc.ServerStream) error {
	m := new(ListPathInfoRequest)
	if err := stream.RecvMsg(m); err != nil {
		return err
	}
	return srv.(PathInfoServiceServer).List(m, &pathInfoServiceListServer{stream})
}

type PathInfoService_ListServer interface {
	Send(*PathInfo) error
	grpc.ServerStream
}

type pathInfoServiceListServer struct {
	grpc.ServerStream
}

func (x *pathInfoServiceListServer) Send(m *PathInfo) error {
	return x.ServerStream.SendMsg(m)
}

// PathInfoService_ServiceDesc is the grpc.ServiceDesc for PathInfoService service.
// It's only intended for direct use with grpc.RegisterService,
// and not to be introspected or modified (even as a copy)
var PathInfoService_ServiceDesc = grpc.ServiceDesc{
	ServiceName: "tvix.store.v1.PathInfoService",
	HandlerType: (*PathInfoServiceServer)(nil),
	Methods: []grpc.MethodDesc{
		{
			MethodName: "Get",
			Handler:    _PathInfoService_Get_Handler,
		},
		{
			MethodName: "Put",
			Handler:    _PathInfoService_Put_Handler,
		},
		{
			MethodName: "CalculateNAR",
			Handler:    _PathInfoService_CalculateNAR_Handler,
		},
	},
	Streams: []grpc.StreamDesc{
		{
			StreamName:    "List",
			Handler:       _PathInfoService_List_Handler,
			ServerStreams: true,
		},
	},
	Metadata: "tvix/store/protos/rpc_pathinfo.proto",
}
