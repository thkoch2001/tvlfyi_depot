//! This module implements compiler logic related to the construction
//! of bindings (attribute sets and `let` expressions).

use super::*;

// Data structures to track the bindings observed in the
// second path, and forward the information needed to compile
// their value.
enum BindingKind {
    InheritFrom {
        namespace: ast::Expr,
        ident: ast::Ident,
    },

    Plain {
        expr: ast::Expr,
    },
}

struct KeySlot {
    slot: LocalIdx,
    name: SmolStr,
}

struct TrackedBinding {
    key_slot: Option<KeySlot>,
    value_slot: LocalIdx,
    kind: BindingKind,
}

#[derive(Clone, Copy, PartialEq)]
enum BindingScope {
    LetIn,
    Attrs,
    RecAttrs,
}

impl BindingScope {
    fn is_attrs(&self) -> bool {
        matches!(self, BindingScope::Attrs | BindingScope::RecAttrs)
    }
}

impl Compiler<'_> {
    pub(super) fn compile_attr(&mut self, slot: LocalIdx, node: ast::Attr) {
        match node {
            ast::Attr::Dynamic(dynamic) => {
                self.compile(slot, dynamic.expr().unwrap());
                self.emit_force(&dynamic.expr().unwrap());
            }

            ast::Attr::Str(s) => {
                self.compile_str(slot, s.clone());
                self.emit_force(&s);
            }

            ast::Attr::Ident(ident) => self.emit_literal_ident(&ident),
        }
    }

    fn compile_inherit(
        &mut self,
        bindings: &mut Vec<TrackedBinding>,
        scope_kind: BindingScope,
        node: ast::Inherit,
    ) {
        // First pass to find all plain inherits (if they are not useless).
        // Since they always resolve to a higher scope, we can just compile and
        // declare them immediately. This needs to happen *before* we declare
        // any other locals in the scope or the stack order gets messed up.
        // While we are looping through the inherits, already note all inherit
        // (from) expressions, that may very well resolve recursively and need
        // to be compiled like normal let in bindings.
        let mut inherit_froms: Vec<(ast::Expr, ast::Ident)> = vec![];

        match node.from() {
            // Within a `let` binding, inheriting from the outer scope
            // is a no-op iff the identifier can be statically
            // resolved.
            None if scope_kind == BindingScope::LetIn && !self.scope().has_with() => {
                self.emit_warning(&node, WarningKind::UselessInherit);
                return;
            }

            None => {
                for ident in node.idents() {
                    // If the identifier resolves statically in a
                    // `let`, it has precedence over dynamic bindings,
                    // and the inherit is useless.
                    if scope_kind == BindingScope::LetIn
                        && matches!(
                            self.scope_mut()
                                .resolve_local(ident.ident_token().unwrap().text()),
                            LocalPosition::Known(_)
                        )
                    {
                        self.emit_warning(&ident, WarningKind::UselessInherit);
                        continue;
                    }

                    // When compiling attribute sets, the key must be
                    // placed on the stack.
                    if scope_kind.is_attrs() {
                        self.emit_literal_ident(&ident);
                        let span = self.span_for(&ident);
                        self.scope_mut().declare_phantom(span, true);
                    }

                    // When compiling non-recursive attribute sets,
                    // the local should be declared as a phantom (i.e.
                    // it should not resolve in the same scope).
                    let idx = if scope_kind == BindingScope::Attrs {
                        let span = self.span_for(&ident);
                        self.scope_mut().declare_phantom(span, false)
                    } else {
                        self.declare_local(&ident, ident.ident_token().unwrap().text())
                    };

                    // Place the value on the stack by compiling the
                    // identifier access as normal.
                    self.compile_ident(idx, ident.clone());
                    self.scope_mut().mark_initialised(idx);
                }
            }

            Some(from) => {
                for ident in node.idents() {
                    inherit_froms.push((from.expr().unwrap(), ident));
                }
            }
        }

        // Second pass over the inherits that have a namespace, to
        // declare them in the tracked bindings.
        //
        // Note that this will not actually compile the access and
        // leave their values on the stack. This is the caller's
        // responsibility.
        for (from, ident) in inherit_froms {
            let key_slot = match scope_kind {
                // In a `let` expression, the keys are not placed on
                // the stack at all.
                BindingScope::LetIn => None,

                // In an attribute set, the keys themselves are placed
                // on the stack but their stack slot is inaccessible
                // (it is only consumed by `OpAttrs`).
                BindingScope::Attrs | BindingScope::RecAttrs => {
                    let span = self.span_for(&ident);
                    Some(KeySlot {
                        slot: self.scope_mut().declare_phantom(span, false),
                        name: SmolStr::new(ident.ident_token().unwrap().text()),
                    })
                }
            };

            let value_slot = match scope_kind {
                // In recursive scopes, the value needs to be
                // accessible on the stack.
                BindingScope::LetIn | BindingScope::RecAttrs => {
                    self.declare_local(&ident, ident.ident_token().unwrap().text())
                }

                // In non-recursive attribute sets, the value is
                // inaccessible (only consumed by `OpAttrs`).
                BindingScope::Attrs => {
                    let ident_span = self.span_for(&ident);
                    self.scope_mut().declare_phantom(ident_span, false)
                }
            };

            bindings.push(TrackedBinding {
                key_slot,
                value_slot,
                kind: BindingKind::InheritFrom {
                    ident,
                    namespace: from,
                },
            });
        }
    }

    fn compile_recursive_scope<N>(&mut self, scope_kind: BindingScope, node: &N)
    where
        N: ToSpan + ast::HasEntry,
    {
        self.scope_mut().begin_scope();

        // Vector to track all observed bindings.
        let mut bindings: Vec<TrackedBinding> = vec![];

        // Pass over all inherits, which will compile & initialise
        // inherits without namespaces, and leave inherits with
        // namespaces (which can potentially resolve recursively and
        // can not be initialised until later) on the `bindings`
        // vector.
        for inherit in node.inherits() {
            self.compile_inherit(&mut bindings, scope_kind, inherit);
        }

        // Declare all regular bindings
        for entry in node.attrpath_values() {
            let mut path = match self.normalise_ident_path(entry.attrpath().unwrap().attrs()) {
                Ok(p) => p,
                Err(err) => {
                    self.errors.push(err);
                    continue;
                }
            };

            if path.len() != 1 {
                self.emit_error(
                    &entry,
                    ErrorKind::NotImplemented("nested bindings in recursive scope :("),
                );
                continue;
            }

            let key_slot = if scope_kind.is_attrs() {
                // TODO: distinguish rec/non-rec
                let span = self.span_for(&entry.attrpath().unwrap());
                Some(KeySlot {
                    slot: self.scope_mut().declare_phantom(span, false),
                    name: SmolStr::new(&path[0]),
                })
            } else {
                None
            };

            let value_slot = self.declare_local(&entry.attrpath().unwrap(), path.pop().unwrap());

            bindings.push(TrackedBinding {
                key_slot,
                value_slot,
                kind: BindingKind::Plain {
                    expr: entry.value().unwrap(),
                },
            });
        }

        // Third pass to place the values in the correct stack slots.
        let mut value_indices: Vec<LocalIdx> = vec![];
        for binding in bindings.into_iter() {
            value_indices.push(binding.value_slot);

            if let Some(key_slot) = binding.key_slot {
                // TODO: emit_constant should be able to take a span directly
                let span = self.scope()[key_slot.slot].span;
                let idx = self
                    .chunk()
                    .push_constant(Value::String(key_slot.name.into()));

                self.chunk().push_op(OpCode::OpConstant(idx), span);
                self.scope_mut().mark_initialised(key_slot.slot);
            }

            match binding.kind {
                // This entry is an inherit (from) expr. The value is
                // placed on the stack by selecting an attribute.
                BindingKind::InheritFrom { namespace, ident } => {
                    // Create a thunk wrapping value (which may be one as well) to
                    // avoid forcing the from expr too early.
                    self.thunk(binding.value_slot, &namespace, move |c, n, s| {
                        c.compile(s, n.clone());
                        c.emit_force(n);

                        c.emit_literal_ident(&ident);
                        c.push_op(OpCode::OpAttrsSelect, &ident);
                    })
                }

                // Binding is "just" a plain expression that needs to
                // be compiled.
                BindingKind::Plain { expr } => self.compile(binding.value_slot, expr),
            }

            // Any code after this point will observe the value in the
            // right stack slot, so mark it as initialised.
            self.scope_mut().mark_initialised(binding.value_slot);
        }

        // Fourth pass to emit finaliser instructions if necessary.
        for idx in value_indices {
            if self.scope()[idx].needs_finaliser {
                let stack_idx = self.scope().stack_index(idx);
                self.push_op(OpCode::OpFinalise(stack_idx), node);
            }
        }
    }

    /// Compile a standard `let ...; in ...` expression.
    ///
    /// Unless in a non-standard scope, the encountered values are
    /// simply pushed on the stack and their indices noted in the
    /// entries vector.
    pub(super) fn compile_let_in(&mut self, slot: LocalIdx, node: ast::LetIn) {
        self.compile_recursive_scope(BindingScope::LetIn, &node);

        // Deal with the body, then clean up the locals afterwards.
        self.compile(slot, node.body().unwrap());
        self.cleanup_scope(&node);
    }

    pub(super) fn compile_legacy_let(&mut self, _slot: LocalIdx, node: ast::LegacyLet) {
        self.emit_warning(&node, WarningKind::DeprecatedLegacyLet);
        self.scope_mut().begin_scope();
        self.compile_recursive_scope(BindingScope::RecAttrs, &node);
        self.push_op(OpCode::OpAttrs(Count(node.entries().count())), &node);
        self.emit_constant(Value::String(SmolStr::new_inline("body").into()), &node);
        self.push_op(OpCode::OpAttrsSelect, &node);
    }

    /// Compiles inherited values in an attribute set. Inherited
    /// values are *always* inherited from the outer scope, even if
    /// there is a matching name within a recursive attribute set.
    fn compile_inherit_attrs(
        &mut self,
        slot: LocalIdx,
        inherits: AstChildren<ast::Inherit>,
    ) -> usize {
        // Count the number of inherited values, so that the outer
        // constructor can emit the correct number of pairs when
        // constructing attribute sets.
        let mut count = 0;

        for inherit in inherits {
            match inherit.from() {
                Some(from) => {
                    for ident in inherit.idents() {
                        count += 1;

                        // First emit the identifier itself (this
                        // becomes the new key).
                        self.emit_literal_ident(&ident);
                        let ident_span = self.span_for(&ident);
                        self.scope_mut().declare_phantom(ident_span, true);

                        // Then emit the node that we're inheriting
                        // from.
                        //
                        // TODO: Likely significant optimisation
                        // potential in having a multi-select
                        // instruction followed by a merge, rather
                        // than pushing/popping the same attrs
                        // potentially a lot of times.
                        let val_idx = self.scope_mut().declare_phantom(ident_span, false);
                        self.compile(val_idx, from.expr().unwrap());
                        self.emit_force(&from.expr().unwrap());
                        self.emit_literal_ident(&ident);
                        self.push_op(OpCode::OpAttrsSelect, &ident);
                        self.scope_mut().mark_initialised(val_idx);
                    }
                }

                None => {
                    for ident in inherit.idents() {
                        let ident_span = self.span_for(&ident);
                        count += 1;

                        // Emit the key to use for OpAttrs
                        self.emit_literal_ident(&ident);
                        self.scope_mut().declare_phantom(ident_span, true);

                        // Emit the value.
                        self.compile_ident(slot, ident);
                        self.scope_mut().declare_phantom(ident_span, true);
                    }
                }
            }
        }

        count
    }

    /// Compile the statically known entries of an attribute set. Which
    /// keys are which is not known from the iterator, so discovered
    /// dynamic keys are returned from here.
    fn compile_static_attr_entries(
        &mut self,
        count: &mut usize,
        entries: AstChildren<ast::AttrpathValue>,
    ) -> Vec<ast::AttrpathValue> {
        let mut dynamic_attrs: Vec<ast::AttrpathValue> = vec![];

        'entries: for kv in entries {
            // Attempt to turn the attrpath into a list of static
            // strings, but abort this process if any dynamic
            // fragments are encountered.
            let static_attrpath: Option<Vec<String>> = kv
                .attrpath()
                .unwrap()
                .attrs()
                .map(|a| self.expr_static_attr_str(&a))
                .collect();

            let fragments = match static_attrpath {
                Some(fragments) => fragments,
                None => {
                    dynamic_attrs.push(kv);
                    continue 'entries;
                }
            };

            // At this point we can increase the counter because we
            // know that this particular attribute is static and can
            // thus be processed here.
            *count += 1;

            let key_count = fragments.len();
            for fragment in fragments.into_iter() {
                self.emit_constant(Value::String(fragment.into()), &kv.attrpath().unwrap());
            }

            // We're done with the key if there was only one fragment,
            // otherwise we need to emit an instruction to construct
            // the attribute path.
            if key_count > 1 {
                self.push_op(
                    OpCode::OpAttrPath(Count(key_count)),
                    &kv.attrpath().unwrap(),
                );
            }

            // The value is just compiled as normal so that its
            // resulting value is on the stack when the attribute set
            // is constructed at runtime.
            let value_span = self.span_for(&kv.value().unwrap());
            let value_slot = self.scope_mut().declare_phantom(value_span, false);
            self.compile(value_slot, kv.value().unwrap());
            self.scope_mut().mark_initialised(value_slot);
        }

        dynamic_attrs
    }

    /// Compile the dynamic entries of an attribute set, where keys
    /// are only known at runtime.
    fn compile_dynamic_attr_entries(
        &mut self,
        count: &mut usize,
        entries: Vec<ast::AttrpathValue>,
    ) {
        for entry in entries.into_iter() {
            *count += 1;

            let mut key_count = 0;
            let key_span = self.span_for(&entry.attrpath().unwrap());
            let key_idx = self.scope_mut().declare_phantom(key_span, false);

            for fragment in entry.attrpath().unwrap().attrs() {
                // Key fragments can contain dynamic expressions,
                // which makes accounting for their stack slots very
                // tricky.
                //
                // In order to ensure the locals are correctly cleaned
                // up, we essentially treat any key fragment after the
                // first one (which has a locals index that will
                // become that of the final key itself) as being part
                // of a separate scope, which results in the somewhat
                // annoying setup logic below.
                let fragment_slot = match key_count {
                    0 => key_idx,
                    1 => {
                        self.scope_mut().begin_scope();
                        self.scope_mut().declare_phantom(key_span, false)
                    }
                    _ => self.scope_mut().declare_phantom(key_span, false),
                };

                key_count += 1;
                self.compile_attr(fragment_slot, fragment);
                self.scope_mut().mark_initialised(fragment_slot);
            }

            // We're done with the key if there was only one fragment,
            // otherwise we need to emit an instruction to construct
            // the attribute path.
            if key_count > 1 {
                self.push_op(
                    OpCode::OpAttrPath(Count(key_count)),
                    &entry.attrpath().unwrap(),
                );

                // Close the temporary scope that was set up for the
                // key fragments.
                self.scope_mut().end_scope();
            }

            // The value is just compiled as normal so that its
            // resulting value is on the stack when the attribute set
            // is constructed at runtime.
            let value_span = self.span_for(&entry.value().unwrap());
            let value_slot = self.scope_mut().declare_phantom(value_span, false);
            self.compile(value_slot, entry.value().unwrap());
            self.scope_mut().mark_initialised(value_slot);
        }
    }

    /// Compile attribute set literals into equivalent bytecode.
    ///
    /// This is complicated by a number of features specific to Nix
    /// attribute sets, most importantly:
    ///
    /// 1. Keys can be dynamically constructed through interpolation.
    /// 2. Keys can refer to nested attribute sets.
    /// 3. Attribute sets can (optionally) be recursive.
    pub(super) fn compile_attr_set(&mut self, slot: LocalIdx, node: ast::AttrSet) {
        // Open a scope to track the positions of the temporaries used
        // by the `OpAttrs` instruction.
        self.scope_mut().begin_scope();

        if node.rec_token().is_some() {
            self.compile_recursive_scope(BindingScope::RecAttrs, &node);
            self.push_op(OpCode::OpAttrs(Count(node.entries().count())), &node);
        } else {
            let mut count = self.compile_inherit_attrs(slot, node.inherits());

            let dynamic_entries =
                self.compile_static_attr_entries(&mut count, node.attrpath_values());

            self.compile_dynamic_attr_entries(&mut count, dynamic_entries);

            self.push_op(OpCode::OpAttrs(Count(count)), &node);
        }

        // Remove the temporary scope, but do not emit any additional
        // cleanup (OpAttrs consumes all of these locals).
        self.scope_mut().end_scope();
    }
}
