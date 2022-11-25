// SPDX-License-Identifier: MIT
// Copyright © 2022 The Tvix Authors

// Code generated by protoc-gen-go. DO NOT EDIT.
// versions:
// 	protoc-gen-go v1.28.1
// 	protoc        (unknown)
// source: tvix/store/protos/rpc_pathinfo.proto

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

// GetPathInfoRequest describes the lookup parameters that can be used to
// lookup a PathInfo objects.
// Currently, only a lookup by output hash is supported.
type GetPathInfoRequest struct {
	state         protoimpl.MessageState
	sizeCache     protoimpl.SizeCache
	unknownFields protoimpl.UnknownFields

	// Types that are assignable to ByWhat:
	//
	//	*GetPathInfoRequest_ByOutputHash
	ByWhat isGetPathInfoRequest_ByWhat `protobuf_oneof:"by_what"`
}

func (x *GetPathInfoRequest) Reset() {
	*x = GetPathInfoRequest{}
	if protoimpl.UnsafeEnabled {
		mi := &file_tvix_store_protos_rpc_pathinfo_proto_msgTypes[0]
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		ms.StoreMessageInfo(mi)
	}
}

func (x *GetPathInfoRequest) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*GetPathInfoRequest) ProtoMessage() {}

func (x *GetPathInfoRequest) ProtoReflect() protoreflect.Message {
	mi := &file_tvix_store_protos_rpc_pathinfo_proto_msgTypes[0]
	if protoimpl.UnsafeEnabled && x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use GetPathInfoRequest.ProtoReflect.Descriptor instead.
func (*GetPathInfoRequest) Descriptor() ([]byte, []int) {
	return file_tvix_store_protos_rpc_pathinfo_proto_rawDescGZIP(), []int{0}
}

func (m *GetPathInfoRequest) GetByWhat() isGetPathInfoRequest_ByWhat {
	if m != nil {
		return m.ByWhat
	}
	return nil
}

func (x *GetPathInfoRequest) GetByOutputHash() []byte {
	if x, ok := x.GetByWhat().(*GetPathInfoRequest_ByOutputHash); ok {
		return x.ByOutputHash
	}
	return nil
}

type isGetPathInfoRequest_ByWhat interface {
	isGetPathInfoRequest_ByWhat()
}

type GetPathInfoRequest_ByOutputHash struct {
	// The output hash of a nix path (20 bytes)
	ByOutputHash []byte `protobuf:"bytes,1,opt,name=by_output_hash,json=byOutputHash,proto3,oneof"`
}

func (*GetPathInfoRequest_ByOutputHash) isGetPathInfoRequest_ByWhat() {}

// PutPathInfoRequest wraps the PathInfo message that we sent and annotates it with metadata
type PutPathInfoRequest struct {
	state         protoimpl.MessageState
	sizeCache     protoimpl.SizeCache
	unknownFields protoimpl.UnknownFields

	PathInfo   *PathInfo `protobuf:"bytes,1,opt,name=path_info,json=pathInfo,proto3" json:"path_info,omitempty"`
	OutputHash []byte    `protobuf:"bytes,2,opt,name=output_hash,json=outputHash,proto3" json:"output_hash,omitempty"`
}

func (x *PutPathInfoRequest) Reset() {
	*x = PutPathInfoRequest{}
	if protoimpl.UnsafeEnabled {
		mi := &file_tvix_store_protos_rpc_pathinfo_proto_msgTypes[1]
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		ms.StoreMessageInfo(mi)
	}
}

func (x *PutPathInfoRequest) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*PutPathInfoRequest) ProtoMessage() {}

func (x *PutPathInfoRequest) ProtoReflect() protoreflect.Message {
	mi := &file_tvix_store_protos_rpc_pathinfo_proto_msgTypes[1]
	if protoimpl.UnsafeEnabled && x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use PutPathInfoRequest.ProtoReflect.Descriptor instead.
func (*PutPathInfoRequest) Descriptor() ([]byte, []int) {
	return file_tvix_store_protos_rpc_pathinfo_proto_rawDescGZIP(), []int{1}
}

func (x *PutPathInfoRequest) GetPathInfo() *PathInfo {
	if x != nil {
		return x.PathInfo
	}
	return nil
}

func (x *PutPathInfoRequest) GetOutputHash() []byte {
	if x != nil {
		return x.OutputHash
	}
	return nil
}

var File_tvix_store_protos_rpc_pathinfo_proto protoreflect.FileDescriptor

var file_tvix_store_protos_rpc_pathinfo_proto_rawDesc = []byte{
	0x0a, 0x24, 0x74, 0x76, 0x69, 0x78, 0x2f, 0x73, 0x74, 0x6f, 0x72, 0x65, 0x2f, 0x70, 0x72, 0x6f,
	0x74, 0x6f, 0x73, 0x2f, 0x72, 0x70, 0x63, 0x5f, 0x70, 0x61, 0x74, 0x68, 0x69, 0x6e, 0x66, 0x6f,
	0x2e, 0x70, 0x72, 0x6f, 0x74, 0x6f, 0x12, 0x0d, 0x74, 0x76, 0x69, 0x78, 0x2e, 0x73, 0x74, 0x6f,
	0x72, 0x65, 0x2e, 0x76, 0x31, 0x1a, 0x20, 0x74, 0x76, 0x69, 0x78, 0x2f, 0x73, 0x74, 0x6f, 0x72,
	0x65, 0x2f, 0x70, 0x72, 0x6f, 0x74, 0x6f, 0x73, 0x2f, 0x70, 0x61, 0x74, 0x68, 0x69, 0x6e, 0x66,
	0x6f, 0x2e, 0x70, 0x72, 0x6f, 0x74, 0x6f, 0x22, 0x47, 0x0a, 0x12, 0x47, 0x65, 0x74, 0x50, 0x61,
	0x74, 0x68, 0x49, 0x6e, 0x66, 0x6f, 0x52, 0x65, 0x71, 0x75, 0x65, 0x73, 0x74, 0x12, 0x26, 0x0a,
	0x0e, 0x62, 0x79, 0x5f, 0x6f, 0x75, 0x74, 0x70, 0x75, 0x74, 0x5f, 0x68, 0x61, 0x73, 0x68, 0x18,
	0x01, 0x20, 0x01, 0x28, 0x0c, 0x48, 0x00, 0x52, 0x0c, 0x62, 0x79, 0x4f, 0x75, 0x74, 0x70, 0x75,
	0x74, 0x48, 0x61, 0x73, 0x68, 0x42, 0x09, 0x0a, 0x07, 0x62, 0x79, 0x5f, 0x77, 0x68, 0x61, 0x74,
	0x22, 0x6b, 0x0a, 0x12, 0x50, 0x75, 0x74, 0x50, 0x61, 0x74, 0x68, 0x49, 0x6e, 0x66, 0x6f, 0x52,
	0x65, 0x71, 0x75, 0x65, 0x73, 0x74, 0x12, 0x34, 0x0a, 0x09, 0x70, 0x61, 0x74, 0x68, 0x5f, 0x69,
	0x6e, 0x66, 0x6f, 0x18, 0x01, 0x20, 0x01, 0x28, 0x0b, 0x32, 0x17, 0x2e, 0x74, 0x76, 0x69, 0x78,
	0x2e, 0x73, 0x74, 0x6f, 0x72, 0x65, 0x2e, 0x76, 0x31, 0x2e, 0x50, 0x61, 0x74, 0x68, 0x49, 0x6e,
	0x66, 0x6f, 0x52, 0x08, 0x70, 0x61, 0x74, 0x68, 0x49, 0x6e, 0x66, 0x6f, 0x12, 0x1f, 0x0a, 0x0b,
	0x6f, 0x75, 0x74, 0x70, 0x75, 0x74, 0x5f, 0x68, 0x61, 0x73, 0x68, 0x18, 0x02, 0x20, 0x01, 0x28,
	0x0c, 0x52, 0x0a, 0x6f, 0x75, 0x74, 0x70, 0x75, 0x74, 0x48, 0x61, 0x73, 0x68, 0x32, 0x97, 0x01,
	0x0a, 0x0f, 0x50, 0x61, 0x74, 0x68, 0x49, 0x6e, 0x66, 0x6f, 0x53, 0x65, 0x72, 0x76, 0x69, 0x63,
	0x65, 0x12, 0x41, 0x0a, 0x03, 0x47, 0x65, 0x74, 0x12, 0x21, 0x2e, 0x74, 0x76, 0x69, 0x78, 0x2e,
	0x73, 0x74, 0x6f, 0x72, 0x65, 0x2e, 0x76, 0x31, 0x2e, 0x47, 0x65, 0x74, 0x50, 0x61, 0x74, 0x68,
	0x49, 0x6e, 0x66, 0x6f, 0x52, 0x65, 0x71, 0x75, 0x65, 0x73, 0x74, 0x1a, 0x17, 0x2e, 0x74, 0x76,
	0x69, 0x78, 0x2e, 0x73, 0x74, 0x6f, 0x72, 0x65, 0x2e, 0x76, 0x31, 0x2e, 0x50, 0x61, 0x74, 0x68,
	0x49, 0x6e, 0x66, 0x6f, 0x12, 0x41, 0x0a, 0x03, 0x50, 0x75, 0x74, 0x12, 0x21, 0x2e, 0x74, 0x76,
	0x69, 0x78, 0x2e, 0x73, 0x74, 0x6f, 0x72, 0x65, 0x2e, 0x76, 0x31, 0x2e, 0x50, 0x75, 0x74, 0x50,
	0x61, 0x74, 0x68, 0x49, 0x6e, 0x66, 0x6f, 0x52, 0x65, 0x71, 0x75, 0x65, 0x73, 0x74, 0x1a, 0x17,
	0x2e, 0x74, 0x76, 0x69, 0x78, 0x2e, 0x73, 0x74, 0x6f, 0x72, 0x65, 0x2e, 0x76, 0x31, 0x2e, 0x50,
	0x61, 0x74, 0x68, 0x49, 0x6e, 0x66, 0x6f, 0x42, 0x28, 0x5a, 0x26, 0x63, 0x6f, 0x64, 0x65, 0x2e,
	0x74, 0x76, 0x6c, 0x2e, 0x66, 0x79, 0x69, 0x2f, 0x74, 0x76, 0x69, 0x78, 0x2f, 0x73, 0x74, 0x6f,
	0x72, 0x65, 0x2f, 0x70, 0x72, 0x6f, 0x74, 0x6f, 0x73, 0x3b, 0x73, 0x74, 0x6f, 0x72, 0x65, 0x76,
	0x31, 0x62, 0x06, 0x70, 0x72, 0x6f, 0x74, 0x6f, 0x33,
}

var (
	file_tvix_store_protos_rpc_pathinfo_proto_rawDescOnce sync.Once
	file_tvix_store_protos_rpc_pathinfo_proto_rawDescData = file_tvix_store_protos_rpc_pathinfo_proto_rawDesc
)

func file_tvix_store_protos_rpc_pathinfo_proto_rawDescGZIP() []byte {
	file_tvix_store_protos_rpc_pathinfo_proto_rawDescOnce.Do(func() {
		file_tvix_store_protos_rpc_pathinfo_proto_rawDescData = protoimpl.X.CompressGZIP(file_tvix_store_protos_rpc_pathinfo_proto_rawDescData)
	})
	return file_tvix_store_protos_rpc_pathinfo_proto_rawDescData
}

var file_tvix_store_protos_rpc_pathinfo_proto_msgTypes = make([]protoimpl.MessageInfo, 2)
var file_tvix_store_protos_rpc_pathinfo_proto_goTypes = []interface{}{
	(*GetPathInfoRequest)(nil), // 0: tvix.store.v1.GetPathInfoRequest
	(*PutPathInfoRequest)(nil), // 1: tvix.store.v1.PutPathInfoRequest
	(*PathInfo)(nil),           // 2: tvix.store.v1.PathInfo
}
var file_tvix_store_protos_rpc_pathinfo_proto_depIdxs = []int32{
	2, // 0: tvix.store.v1.PutPathInfoRequest.path_info:type_name -> tvix.store.v1.PathInfo
	0, // 1: tvix.store.v1.PathInfoService.Get:input_type -> tvix.store.v1.GetPathInfoRequest
	1, // 2: tvix.store.v1.PathInfoService.Put:input_type -> tvix.store.v1.PutPathInfoRequest
	2, // 3: tvix.store.v1.PathInfoService.Get:output_type -> tvix.store.v1.PathInfo
	2, // 4: tvix.store.v1.PathInfoService.Put:output_type -> tvix.store.v1.PathInfo
	3, // [3:5] is the sub-list for method output_type
	1, // [1:3] is the sub-list for method input_type
	1, // [1:1] is the sub-list for extension type_name
	1, // [1:1] is the sub-list for extension extendee
	0, // [0:1] is the sub-list for field type_name
}

func init() { file_tvix_store_protos_rpc_pathinfo_proto_init() }
func file_tvix_store_protos_rpc_pathinfo_proto_init() {
	if File_tvix_store_protos_rpc_pathinfo_proto != nil {
		return
	}
	file_tvix_store_protos_pathinfo_proto_init()
	if !protoimpl.UnsafeEnabled {
		file_tvix_store_protos_rpc_pathinfo_proto_msgTypes[0].Exporter = func(v interface{}, i int) interface{} {
			switch v := v.(*GetPathInfoRequest); i {
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
		file_tvix_store_protos_rpc_pathinfo_proto_msgTypes[1].Exporter = func(v interface{}, i int) interface{} {
			switch v := v.(*PutPathInfoRequest); i {
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
	file_tvix_store_protos_rpc_pathinfo_proto_msgTypes[0].OneofWrappers = []interface{}{
		(*GetPathInfoRequest_ByOutputHash)(nil),
	}
	type x struct{}
	out := protoimpl.TypeBuilder{
		File: protoimpl.DescBuilder{
			GoPackagePath: reflect.TypeOf(x{}).PkgPath(),
			RawDescriptor: file_tvix_store_protos_rpc_pathinfo_proto_rawDesc,
			NumEnums:      0,
			NumMessages:   2,
			NumExtensions: 0,
			NumServices:   1,
		},
		GoTypes:           file_tvix_store_protos_rpc_pathinfo_proto_goTypes,
		DependencyIndexes: file_tvix_store_protos_rpc_pathinfo_proto_depIdxs,
		MessageInfos:      file_tvix_store_protos_rpc_pathinfo_proto_msgTypes,
	}.Build()
	File_tvix_store_protos_rpc_pathinfo_proto = out.File
	file_tvix_store_protos_rpc_pathinfo_proto_rawDesc = nil
	file_tvix_store_protos_rpc_pathinfo_proto_goTypes = nil
	file_tvix_store_protos_rpc_pathinfo_proto_depIdxs = nil
}
