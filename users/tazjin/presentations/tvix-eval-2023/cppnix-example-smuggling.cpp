struct Env {
  Value * values[0];
  // ... some more fields
};

// ....

if (env->type == Env::HasWithExpr) {
  Value * v = allocValue();
  evalAttrs(*env->up, (Expr *) env->values[0], *v, noPos, "<borked>");
  // ...
}
