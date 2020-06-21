#include "nixexpr.hh"
#include "libstore/store-api.hh"
#include "libexpr/eval.hh"
#include <iostream>

extern "C" {

struct StoreWrap {
	StoreWrap(const nix::ref<nix::Store> &s) : store(s) {}

	nix::ref<nix::Store> store;
};

Store OpenStore() {
	auto storePtr = nix::openStore();
	StoreWrap* sw = new StoreWrap(storePtr);
	return sw;
}

void CloseStore(Store store) {
	StoreWrap* sw = (StoreWrap*)store;
	delete sw;
}

EvalState CreateEvalState(Store store) {
	StoreWrap* sw = (StoreWrap*)store;
	nix::Strings strings;
	return new nix::EvalState(strings, sw->store);
}

void DeleteEvalState(EvalState esp) {
	nix::EvalState* es = (nix::EvalState*)esp;
	delete es;
}

Value ParseExprFromString(EvalState esp, char* expr) {
	nix::EvalState* es = (nix::EvalState*)esp;
	nix::Value* v = es->allocValue();
	es->eval(es->parseExprFromString(std::string(expr), nix::absPath(".")), *v);
	return v;
}

char* ExprToString(EvalState esp, Value v) {
	nix::EvalState* es = (nix::EvalState*)esp;
	nix::Pos noPos;
	std::ostringstream s;
	nix::PathSet context;
	s << es->coerceToString(noPos, *(nix::Value*)v, context);
	return strdup(s.str().c_str());
}

}
