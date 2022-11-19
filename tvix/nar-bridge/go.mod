module code.tvl.fyi/tvix/nar-bridge

require (
	code.tvl.fyi/tvix/store/protos v0.0.0
	github.com/alecthomas/kong v0.7.1
	github.com/go-chi/chi v1.5.4
	github.com/go-chi/chi/v5 v5.0.7
	github.com/sirupsen/logrus v1.9.0
	google.golang.org/grpc v1.51.0
)

require (
	github.com/golang/protobuf v1.5.2 // indirect
	github.com/klauspost/cpuid/v2 v2.0.9 // indirect
	golang.org/x/net v0.0.0-20220722155237-a158d28d115b // indirect
	golang.org/x/sys v0.0.0-20220722155257-8c9f86f7a55f // indirect
	golang.org/x/text v0.4.0 // indirect
	google.golang.org/genproto v0.0.0-20200526211855-cb27e3aa2013 // indirect
	google.golang.org/protobuf v1.28.1 // indirect
	lukechampine.com/blake3 v1.1.7 // indirect
)

replace code.tvl.fyi/tvix/store/protos v0.0.0 => ../store/protos

go 1.19
