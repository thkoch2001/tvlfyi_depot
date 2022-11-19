// SPDX-License-Identifier: MIT
// Copyright © 2022 The Tvix Authors

// Code generated by protoc-gen-go. DO NOT EDIT.
// versions:
// 	protoc-gen-go v1.28.1
// 	protoc        (unknown)
// source: tvix/store/protos/rpc_directory.proto

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

type GetDirectoryRequest struct {
	state         protoimpl.MessageState
	sizeCache     protoimpl.SizeCache
	unknownFields protoimpl.UnknownFields

	// Types that are assignable to ByWhat:
	//
	//	*GetDirectoryRequest_Digest
	ByWhat isGetDirectoryRequest_ByWhat `protobuf_oneof:"by_what"`
	// If set to true, recursively resolve all child Directory messages.
	// Directory messages SHOULD be streamed in a recursive breadth-first walk,
	// but other orders are also fine, as long as Directory messages are only
	// sent after they are referred to from previously sent Directory messages.
	Recursive bool `protobuf:"varint,2,opt,name=recursive,proto3" json:"recursive,omitempty"`
}

func (x *GetDirectoryRequest) Reset() {
	*x = GetDirectoryRequest{}
	if protoimpl.UnsafeEnabled {
		mi := &file_tvix_store_protos_rpc_directory_proto_msgTypes[0]
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		ms.StoreMessageInfo(mi)
	}
}

func (x *GetDirectoryRequest) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*GetDirectoryRequest) ProtoMessage() {}

func (x *GetDirectoryRequest) ProtoReflect() protoreflect.Message {
	mi := &file_tvix_store_protos_rpc_directory_proto_msgTypes[0]
	if protoimpl.UnsafeEnabled && x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use GetDirectoryRequest.ProtoReflect.Descriptor instead.
func (*GetDirectoryRequest) Descriptor() ([]byte, []int) {
	return file_tvix_store_protos_rpc_directory_proto_rawDescGZIP(), []int{0}
}

func (m *GetDirectoryRequest) GetByWhat() isGetDirectoryRequest_ByWhat {
	if m != nil {
		return m.ByWhat
	}
	return nil
}

func (x *GetDirectoryRequest) GetDigest() []byte {
	if x, ok := x.GetByWhat().(*GetDirectoryRequest_Digest); ok {
		return x.Digest
	}
	return nil
}

func (x *GetDirectoryRequest) GetRecursive() bool {
	if x != nil {
		return x.Recursive
	}
	return false
}

type isGetDirectoryRequest_ByWhat interface {
	isGetDirectoryRequest_ByWhat()
}

type GetDirectoryRequest_Digest struct {
	// The blake3 hash of the (root) Directory message, serialized in
	// protobuf canonical form.
	// Keep in mind this can be a subtree of another root.
	Digest []byte `protobuf:"bytes,1,opt,name=digest,proto3,oneof"`
}

func (*GetDirectoryRequest_Digest) isGetDirectoryRequest_ByWhat() {}

type PutDirectoryResponse struct {
	state         protoimpl.MessageState
	sizeCache     protoimpl.SizeCache
	unknownFields protoimpl.UnknownFields

	RootDigest []byte `protobuf:"bytes,1,opt,name=root_digest,json=rootDigest,proto3" json:"root_digest,omitempty"`
}

func (x *PutDirectoryResponse) Reset() {
	*x = PutDirectoryResponse{}
	if protoimpl.UnsafeEnabled {
		mi := &file_tvix_store_protos_rpc_directory_proto_msgTypes[1]
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		ms.StoreMessageInfo(mi)
	}
}

func (x *PutDirectoryResponse) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*PutDirectoryResponse) ProtoMessage() {}

func (x *PutDirectoryResponse) ProtoReflect() protoreflect.Message {
	mi := &file_tvix_store_protos_rpc_directory_proto_msgTypes[1]
	if protoimpl.UnsafeEnabled && x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use PutDirectoryResponse.ProtoReflect.Descriptor instead.
func (*PutDirectoryResponse) Descriptor() ([]byte, []int) {
	return file_tvix_store_protos_rpc_directory_proto_rawDescGZIP(), []int{1}
}

func (x *PutDirectoryResponse) GetRootDigest() []byte {
	if x != nil {
		return x.RootDigest
	}
	return nil
}

var File_tvix_store_protos_rpc_directory_proto protoreflect.FileDescriptor

var file_tvix_store_protos_rpc_directory_proto_rawDesc = []byte{
	0x0a, 0x25, 0x74, 0x76, 0x69, 0x78, 0x2f, 0x73, 0x74, 0x6f, 0x72, 0x65, 0x2f, 0x70, 0x72, 0x6f,
	0x74, 0x6f, 0x73, 0x2f, 0x72, 0x70, 0x63, 0x5f, 0x64, 0x69, 0x72, 0x65, 0x63, 0x74, 0x6f, 0x72,
	0x79, 0x2e, 0x70, 0x72, 0x6f, 0x74, 0x6f, 0x12, 0x0d, 0x74, 0x76, 0x69, 0x78, 0x2e, 0x73, 0x74,
	0x6f, 0x72, 0x65, 0x2e, 0x76, 0x31, 0x1a, 0x1f, 0x74, 0x76, 0x69, 0x78, 0x2f, 0x73, 0x74, 0x6f,
	0x72, 0x65, 0x2f, 0x70, 0x72, 0x6f, 0x74, 0x6f, 0x73, 0x2f, 0x63, 0x61, 0x73, 0x74, 0x6f, 0x72,
	0x65, 0x2e, 0x70, 0x72, 0x6f, 0x74, 0x6f, 0x22, 0x58, 0x0a, 0x13, 0x47, 0x65, 0x74, 0x44, 0x69,
	0x72, 0x65, 0x63, 0x74, 0x6f, 0x72, 0x79, 0x52, 0x65, 0x71, 0x75, 0x65, 0x73, 0x74, 0x12, 0x18,
	0x0a, 0x06, 0x64, 0x69, 0x67, 0x65, 0x73, 0x74, 0x18, 0x01, 0x20, 0x01, 0x28, 0x0c, 0x48, 0x00,
	0x52, 0x06, 0x64, 0x69, 0x67, 0x65, 0x73, 0x74, 0x12, 0x1c, 0x0a, 0x09, 0x72, 0x65, 0x63, 0x75,
	0x72, 0x73, 0x69, 0x76, 0x65, 0x18, 0x02, 0x20, 0x01, 0x28, 0x08, 0x52, 0x09, 0x72, 0x65, 0x63,
	0x75, 0x72, 0x73, 0x69, 0x76, 0x65, 0x42, 0x09, 0x0a, 0x07, 0x62, 0x79, 0x5f, 0x77, 0x68, 0x61,
	0x74, 0x22, 0x37, 0x0a, 0x14, 0x50, 0x75, 0x74, 0x44, 0x69, 0x72, 0x65, 0x63, 0x74, 0x6f, 0x72,
	0x79, 0x52, 0x65, 0x73, 0x70, 0x6f, 0x6e, 0x73, 0x65, 0x12, 0x1f, 0x0a, 0x0b, 0x72, 0x6f, 0x6f,
	0x74, 0x5f, 0x64, 0x69, 0x67, 0x65, 0x73, 0x74, 0x18, 0x01, 0x20, 0x01, 0x28, 0x0c, 0x52, 0x0a,
	0x72, 0x6f, 0x6f, 0x74, 0x44, 0x69, 0x67, 0x65, 0x73, 0x74, 0x32, 0xa1, 0x01, 0x0a, 0x10, 0x44,
	0x69, 0x72, 0x65, 0x63, 0x74, 0x6f, 0x72, 0x79, 0x53, 0x65, 0x72, 0x76, 0x69, 0x63, 0x65, 0x12,
	0x45, 0x0a, 0x03, 0x47, 0x65, 0x74, 0x12, 0x22, 0x2e, 0x74, 0x76, 0x69, 0x78, 0x2e, 0x73, 0x74,
	0x6f, 0x72, 0x65, 0x2e, 0x76, 0x31, 0x2e, 0x47, 0x65, 0x74, 0x44, 0x69, 0x72, 0x65, 0x63, 0x74,
	0x6f, 0x72, 0x79, 0x52, 0x65, 0x71, 0x75, 0x65, 0x73, 0x74, 0x1a, 0x18, 0x2e, 0x74, 0x76, 0x69,
	0x78, 0x2e, 0x73, 0x74, 0x6f, 0x72, 0x65, 0x2e, 0x76, 0x31, 0x2e, 0x44, 0x69, 0x72, 0x65, 0x63,
	0x74, 0x6f, 0x72, 0x79, 0x30, 0x01, 0x12, 0x46, 0x0a, 0x03, 0x50, 0x75, 0x74, 0x12, 0x18, 0x2e,
	0x74, 0x76, 0x69, 0x78, 0x2e, 0x73, 0x74, 0x6f, 0x72, 0x65, 0x2e, 0x76, 0x31, 0x2e, 0x44, 0x69,
	0x72, 0x65, 0x63, 0x74, 0x6f, 0x72, 0x79, 0x1a, 0x23, 0x2e, 0x74, 0x76, 0x69, 0x78, 0x2e, 0x73,
	0x74, 0x6f, 0x72, 0x65, 0x2e, 0x76, 0x31, 0x2e, 0x50, 0x75, 0x74, 0x44, 0x69, 0x72, 0x65, 0x63,
	0x74, 0x6f, 0x72, 0x79, 0x52, 0x65, 0x73, 0x70, 0x6f, 0x6e, 0x73, 0x65, 0x28, 0x01, 0x42, 0x28,
	0x5a, 0x26, 0x63, 0x6f, 0x64, 0x65, 0x2e, 0x74, 0x76, 0x6c, 0x2e, 0x66, 0x79, 0x69, 0x2f, 0x74,
	0x76, 0x69, 0x78, 0x2f, 0x73, 0x74, 0x6f, 0x72, 0x65, 0x2f, 0x70, 0x72, 0x6f, 0x74, 0x6f, 0x73,
	0x3b, 0x73, 0x74, 0x6f, 0x72, 0x65, 0x76, 0x31, 0x62, 0x06, 0x70, 0x72, 0x6f, 0x74, 0x6f, 0x33,
}

var (
	file_tvix_store_protos_rpc_directory_proto_rawDescOnce sync.Once
	file_tvix_store_protos_rpc_directory_proto_rawDescData = file_tvix_store_protos_rpc_directory_proto_rawDesc
)

func file_tvix_store_protos_rpc_directory_proto_rawDescGZIP() []byte {
	file_tvix_store_protos_rpc_directory_proto_rawDescOnce.Do(func() {
		file_tvix_store_protos_rpc_directory_proto_rawDescData = protoimpl.X.CompressGZIP(file_tvix_store_protos_rpc_directory_proto_rawDescData)
	})
	return file_tvix_store_protos_rpc_directory_proto_rawDescData
}

var file_tvix_store_protos_rpc_directory_proto_msgTypes = make([]protoimpl.MessageInfo, 2)
var file_tvix_store_protos_rpc_directory_proto_goTypes = []interface{}{
	(*GetDirectoryRequest)(nil),  // 0: tvix.store.v1.GetDirectoryRequest
	(*PutDirectoryResponse)(nil), // 1: tvix.store.v1.PutDirectoryResponse
	(*Directory)(nil),            // 2: tvix.store.v1.Directory
}
var file_tvix_store_protos_rpc_directory_proto_depIdxs = []int32{
	0, // 0: tvix.store.v1.DirectoryService.Get:input_type -> tvix.store.v1.GetDirectoryRequest
	2, // 1: tvix.store.v1.DirectoryService.Put:input_type -> tvix.store.v1.Directory
	2, // 2: tvix.store.v1.DirectoryService.Get:output_type -> tvix.store.v1.Directory
	1, // 3: tvix.store.v1.DirectoryService.Put:output_type -> tvix.store.v1.PutDirectoryResponse
	2, // [2:4] is the sub-list for method output_type
	0, // [0:2] is the sub-list for method input_type
	0, // [0:0] is the sub-list for extension type_name
	0, // [0:0] is the sub-list for extension extendee
	0, // [0:0] is the sub-list for field type_name
}

func init() { file_tvix_store_protos_rpc_directory_proto_init() }
func file_tvix_store_protos_rpc_directory_proto_init() {
	if File_tvix_store_protos_rpc_directory_proto != nil {
		return
	}
	file_tvix_store_protos_castore_proto_init()
	if !protoimpl.UnsafeEnabled {
		file_tvix_store_protos_rpc_directory_proto_msgTypes[0].Exporter = func(v interface{}, i int) interface{} {
			switch v := v.(*GetDirectoryRequest); i {
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
		file_tvix_store_protos_rpc_directory_proto_msgTypes[1].Exporter = func(v interface{}, i int) interface{} {
			switch v := v.(*PutDirectoryResponse); i {
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
	file_tvix_store_protos_rpc_directory_proto_msgTypes[0].OneofWrappers = []interface{}{
		(*GetDirectoryRequest_Digest)(nil),
	}
	type x struct{}
	out := protoimpl.TypeBuilder{
		File: protoimpl.DescBuilder{
			GoPackagePath: reflect.TypeOf(x{}).PkgPath(),
			RawDescriptor: file_tvix_store_protos_rpc_directory_proto_rawDesc,
			NumEnums:      0,
			NumMessages:   2,
			NumExtensions: 0,
			NumServices:   1,
		},
		GoTypes:           file_tvix_store_protos_rpc_directory_proto_goTypes,
		DependencyIndexes: file_tvix_store_protos_rpc_directory_proto_depIdxs,
		MessageInfos:      file_tvix_store_protos_rpc_directory_proto_msgTypes,
	}.Build()
	File_tvix_store_protos_rpc_directory_proto = out.File
	file_tvix_store_protos_rpc_directory_proto_rawDesc = nil
	file_tvix_store_protos_rpc_directory_proto_goTypes = nil
	file_tvix_store_protos_rpc_directory_proto_depIdxs = nil
}
