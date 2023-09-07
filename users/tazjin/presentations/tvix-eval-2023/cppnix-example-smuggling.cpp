struct Env {
  // ... some struct fields ...
  Value* values[0];
};

// ....

if (env->type == Env::HasWithExpr) {
  // ...
  evalAttrs(*env->up, (Expr *) env->values[0], *v, noPos, "<borked>");
  //                  ^^^^^^^^^^^^^^^^^^^^^^^
}
