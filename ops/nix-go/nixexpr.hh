#pragma once

#ifdef __cplusplus
extern "C" {
#endif

typedef void* Store;
Store OpenStore();
void CloseStore(Store);

typedef void* EvalState;
EvalState CreateEvalState(Store);
void DeleteEvalState(EvalState);

typedef void* Value;
Value ParseExprFromString(EvalState, char*);
char* ExprToString(EvalState, Value);

#ifdef __cplusplus
}
#endif
