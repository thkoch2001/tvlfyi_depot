package main

// #cgo pkg-config: nix-expr
// #cgo CXXFLAGS: -std=c++17
// #include "nixexpr.hh"
// #include <stdlib.h>
import "C"
import (
	"fmt"
	"unsafe"
)

func main() {
	store := C.OpenStore()
	defer C.CloseStore(store)
	es := C.CreateEvalState(store)
	defer C.DeleteEvalState(es)

	expr := `let depot = import ../.. {}; in depot.depotPath`
	exprStr := C.CString(expr)
	defer C.free(unsafe.Pointer(exprStr))
	v := C.ParseExprFromString(es, exprStr)
	vs := C.ExprToString(es, v)
	defer C.free(unsafe.Pointer(vs))
	fmt.Println(C.GoString(vs))
}
