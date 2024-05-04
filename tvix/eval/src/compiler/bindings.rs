//! This module implements compiler logic related to name/value binding
//! definitions (that is, attribute sets and let-expressions).
//!
//! In the case of recursive scopes these cases share almost all of their
//! (fairly complex) logic.

use super::syn::{
    Attr, AttrPath, AttrPathValue, AttrSet, Expr, HasEntries, Ident, Inherit, LetIn,
    StaticOrDynamic,
};

use super::*;

/// What kind of bindings scope is being compiled?
#[derive(Clone, Copy, PartialEq)]
enum BindingsKind {
    /// Standard `let ... in ...`-expression.
    LetIn,

    /// Non-recursive attribute set.
    Attrs,

    /// Recursive attribute set.
    RecAttrs,
}

impl BindingsKind {
    fn is_attrs(&self) -> bool {
        matches!(self, BindingsKind::Attrs | BindingsKind::RecAttrs)
    }
}

/// A single binding (excluding the key being bound, except for `Syntax::Inherit`).
enum Binding<Syntax>
where
    Syntax: syn::Syntax,
{
    /// `inherit (namespace) name`
    Inherit(Syntax::Inherit),

    /// `key = /*expr*/;` -- note: this representation is redundant with Set
    Plain { expr: Syntax::Expr },

    /// `key = { ... };`
    Set {
        /// Original span at which this set was first encountered.
        span: Span,

        /// Tracks the kind of set (rec or not).
        kind: BindingsKind,

        /// All inherited entries
        inherits: Vec<Syntax::Inherit>,

        /// All "path=value" entries
        entries: Vec<Syntax::AttrPathValue>,
    },
}

impl<Syntax> ToSpan for Binding<Syntax>
where
    Syntax: syn::Syntax,
{
    fn span_for(&self, f: &codemap::File) -> Span {
        use Binding::*;
        match self {
            Inherit(inherit_from) => inherit_from.span_for(f),
            Plain { expr, .. } => expr.span_for(f),
            Set { span, .. } => *span,
        }
    }
}

impl<Syntax> HasEntries<Syntax> for Binding<Syntax>
where
    Syntax: syn::Syntax,
{
    fn inherits(&self, _c: &Compiler) -> Vec<Syntax::Inherit> {
        match self {
            Binding::Set { inherits, .. } => inherits.clone(),
            _ => vec![],
        }
    }

    fn attributes<'a>(&self, _c: &Compiler) -> Vec<Syntax::AttrPathValue> {
        match self {
            Binding::Set { entries, .. } => entries.to_vec(),
            _ => vec![],
        }
    }
}

impl<Syntax> Binding<Syntax>
where
    Syntax: syn::Syntax,
{
    /// Merge value into self, or emit an error if this turns out to be
    /// impossible.
    fn merge(
        &mut self,
        c: &mut Compiler,
        span: Span,
        mut remaining_path: Syntax::AttrPath,
        value: Syntax::Expr,
    ) {
        match self {
            Binding::Inherit(inherit_from) => c.emit_error(
                &inherit_from.span_for(c.file),
                ErrorKind::UnmergeableInherit {
                    name: "fixme".into(),
                },
            ),

            // If the value is not yet a nested binding, flip the representation
            // and recurse.
            Binding::Plain { expr } => {
                if let Some(existing) = expr.clone().as_attrset(c) {
                    *self = Binding::from_attrset(c, existing);
                    self.merge(c, span, remaining_path, value);
                } else {
                    c.emit_error(&value, ErrorKind::UnmergeableValue)
                }
            }

            // If the value is nested further, it is simply inserted into the
            // bindings with its full path and resolved recursively further
            // down.
            Binding::Set { entries, .. } if remaining_path.first().is_some() => {
                entries.push(Syntax::AttrPathValue::new(span, remaining_path, value))
            }

            Binding::Set {
                inherits, entries, ..
            } => {
                if let Some(attrset) = value.as_attrset(c) {
                    inherits.extend(attrset.inherits(c));
                    entries.extend(attrset.attributes(c));
                } else {
                    // This branch is unreachable because in cases where the
                    // path is empty (i.e. there is no further nesting), the
                    // previous try_merge function already verified that the
                    // expression is an attribute set.

                    // TODO(tazjin): Consider making this branch live by
                    // shuffling that check around and emitting a static error
                    // here instead of a runtime error.
                    unreachable!()
                }
            }
        }
    }
    fn from_attrset(c: &Compiler, attrset: Syntax::AttrSet) -> Self {
        Binding::Set {
            span: c.span_for(&attrset),

            // Kind of the attrs depends on the first time it is
            // encountered. We actually believe this to be a Nix
            // bug: https://github.com/NixOS/nix/issues/7111
            kind: if attrset.is_rec() {
                BindingsKind::RecAttrs
            } else {
                BindingsKind::Attrs
            },

            inherits: attrset.inherits(c),
            entries: attrset.attributes(c),
        }
    }
}

enum KeySlot<Syntax>
where
    Syntax: syn::Syntax,
{
    /// There is no key slot (`let`-expressions do not emit their key).
    None {
        name: SmolStr,
    },

    Attr {
        slot: LocalIdx,
        attr: Syntax::Attr,
    },
}

struct TrackedBinding<Syntax>
where
    Syntax: syn::Syntax,
{
    key_slot: KeySlot<Syntax>,
    value_slot: LocalIdx,
    binding: Binding<Syntax>,
}

impl<Syntax> TrackedBinding<Syntax>
where
    Syntax: syn::Syntax,
{
    /// Does this binding match the given key?
    ///
    /// Used to determine which binding to merge another one into.
    fn matches(&self, key: &str) -> bool {
        match &self.key_slot {
            KeySlot::None { name } => name == key,
            KeySlot::Attr { slot: _, attr } => match attr.attr() {
                StaticOrDynamic::Static(smol_str) => smol_str == key,
                _ => false,
            },
        }
    }
}

struct TrackedBindings<Syntax>
where
    Syntax: syn::Syntax,
{
    bindings: Vec<TrackedBinding<Syntax>>,
}

impl<Syntax> TrackedBindings<Syntax>
where
    Syntax: syn::Syntax,
{
    fn new() -> Self {
        TrackedBindings { bindings: vec![] }
    }

    /// Attempt to merge an entry into an existing matching binding, assuming
    /// that the provided binding is mergable (i.e. either a nested key or an
    /// attribute set literal).
    ///
    /// Returns true if the binding was merged, false if it needs to be compiled
    /// separately as a new binding.
    fn try_merge(
        &mut self,
        c: &mut Compiler,
        span: Span,
        attr: &Syntax::Attr,
        mut remaining_path: Syntax::AttrPath,
        value: Syntax::Expr,
    ) -> bool {
        // If the path has no more entries, and if the entry is not an
        // attribute set literal, the entry can not be merged.
        if remaining_path.first().is_none() && !value.is_attrset() {
            return false;
        }

        // If the first element of the path is not statically known, the entry
        // can not be merged.
        let name = match attr.attr() {
            StaticOrDynamic::Static(name) => name,
            _ => return false,
        };

        // If there is no existing binding with this key, the entry can not be
        // merged.
        // TODO: benchmark whether using a map or something is useful over the
        // `find` here
        let binding = match self.bindings.iter_mut().find(|b| b.matches(name.as_str())) {
            Some(b) => b,
            None => return false,
        };

        // No more excuses ... the binding can be merged!
        binding.binding.merge(c, span, remaining_path, value);

        true
    }

    /// Add a completely new binding to the tracked bindings.
    fn track_new(
        &mut self,
        key_slot: KeySlot<Syntax>,
        value_slot: LocalIdx,
        binding: Binding<Syntax>,
    ) {
        self.bindings.push(TrackedBinding {
            key_slot,
            value_slot,
            binding,
        });
    }
}

/// AST-traversing functions related to bindings.
impl Compiler<'_, '_> {
    /// Compile all inherits of a node with entries that do *not* have a
    /// namespace to inherit from, and return the remaining ones that do.
    fn compile_plain_inherits<N, Syntax>(
        &mut self,
        slot: LocalIdx,
        kind: BindingsKind,
        count: &mut usize,
        node: &N,
    ) -> Vec<Syntax::Inherit>
    where
        Syntax: syn::Syntax,
        N: ToSpan + HasEntries<Syntax>,
    {
        // Pass over all inherits, resolving only those without namespaces.
        // Since they always resolve in a higher scope, we can just compile and
        // declare them immediately.
        //
        // Inherits with namespaces are returned to the caller.
        let mut inherit_froms: Vec<Syntax::Inherit> = vec![];

        for inherit in node.inherits(&self) {
            if inherit.attrs().first().is_none() {
                self.emit_warning(&inherit, WarningKind::EmptyInherit);
                continue;
            }

            match inherit.namespace() {
                // Within a `let` binding, inheriting from the outer scope is a
                // no-op *if* there are no dynamic bindings.
                None if !kind.is_attrs() && !self.has_dynamic_ancestor() => {
                    self.emit_warning(&inherit, WarningKind::UselessInherit);
                    continue;
                }

                None => {
                    for attr in inherit.attrs().iter() {
                        let name = match attr.attr() {
                            StaticOrDynamic::Static(name) => name,
                            _ => {
                                self.emit_error(attr, ErrorKind::DynamicKeyInScope("inherit"));
                                continue;
                            }
                        };

                        // If the identifier resolves statically in a `let`, it
                        // has precedence over dynamic bindings, and the inherit
                        // is useless.
                        if kind == BindingsKind::LetIn
                            && matches!(
                                self.scope_mut().resolve_local(&name),
                                LocalPosition::Known(_)
                            )
                        {
                            self.emit_warning(&inherit, WarningKind::UselessInherit);
                            continue;
                        }

                        *count += 1;

                        // Place key on the stack when compiling attribute sets.
                        if kind.is_attrs() {
                            self.emit_constant(name.as_str().into(), attr);
                            let span = self.span_for(&inherit);
                            self.scope_mut().declare_phantom(span, true);
                        }

                        // Place the value on the stack. Note that because plain
                        // inherits are always in the outer scope, the slot of
                        // *this* scope itself is used.
                        self.compile_identifier_access(slot, &name, attr);

                        // In non-recursive attribute sets, the key slot must be
                        // a phantom (i.e. the identifier can not be resolved in
                        // this scope).
                        let idx = if kind == BindingsKind::Attrs {
                            let span = self.span_for(attr);
                            self.scope_mut().declare_phantom(span, false)
                        } else {
                            self.declare_local(attr, name.clone())
                        };

                        self.scope_mut().mark_initialised(idx);
                    }
                }

                Some(from) => {
                    for attr in inherit.attrs() {
                        let name = match attr.attr() {
                            StaticOrDynamic::Static(name) => name,
                            _ => {
                                self.emit_error(&attr, ErrorKind::DynamicKeyInScope("inherit"));
                                continue;
                            }
                        };

                        *count += 1;
                        inherit_froms.push(Syntax::Inherit::new(
                            Some(from.clone()),
                            vec![Syntax::Attr::from_static(
                                name.clone(),
                                self.span_for(&attr),
                            )],
                            self.span_for(&attr),
                        ));
                    }
                }
            }
        }

        inherit_froms
    }

    /// Declare all namespaced inherits, that is inherits which are inheriting
    /// values from an attribute set.
    ///
    /// This only ensures that the locals stack is aware of the inherits, it
    /// does not yet emit bytecode that places them on the stack. This is up to
    /// the owner of the `bindings` vector, which this function will populate.
    fn declare_namespaced_inherits<Syntax>(
        &mut self,
        kind: BindingsKind,
        inherit_froms: Vec<Syntax::Inherit>,
        bindings: &mut TrackedBindings<Syntax>,
    ) where
        Syntax: syn::Syntax,
    {
        for inherit in inherit_froms {
            let namespace = inherit.namespace();
            for attr in inherit.attrs() {
                let span = inherit.span_for(self.file);
                let key_slot = if kind.is_attrs() {
                    // In an attribute set, the keys themselves are placed on the
                    // stack but their stack slot is inaccessible (it is only
                    // consumed by `OpAttrs`).
                    KeySlot::Attr {
                        slot: self.scope_mut().declare_phantom(span, false),
                        attr: attr.clone(),
                    }
                } else {
                    KeySlot::None {
                        name: attr.as_static().unwrap(),
                    }
                };

                let value_slot = match kind {
                    // In recursive scopes, the value needs to be accessible on the
                    // stack.
                    BindingsKind::LetIn | BindingsKind::RecAttrs => {
                        self.declare_local(&span, attr.as_static().unwrap())
                    }

                    // In non-recursive attribute sets, the value is inaccessible
                    // (only consumed by `OpAttrs`).
                    BindingsKind::Attrs => self.scope_mut().declare_phantom(span, false),
                };

                bindings.track_new(
                    key_slot,
                    value_slot,
                    Binding::Inherit(Syntax::Inherit::new(namespace.clone(), vec![attr], span)),
                );
            }
        }
    }

    /// Declare all regular bindings (i.e. `key = value;`) in a bindings scope,
    /// but do not yet compile their values.
    fn declare_bindings<N, Syntax>(
        &mut self,
        kind: BindingsKind,
        count: &mut usize,
        bindings: &mut TrackedBindings<Syntax>,
        node: &N,
    ) where
        Syntax: syn::Syntax,
        N: ToSpan + HasEntries<Syntax>,
    {
        for entry in node.attributes(self) {
            let mut remaining_path = entry.path();
            let value = entry.value();
            let key = remaining_path.take_first().unwrap();

            if bindings.try_merge(
                self,
                entry.span_for(self.file),
                &key,
                remaining_path.clone(),
                value.clone(),
            ) {
                // Binding is nested, or already exists and was merged, move on.
                continue;
            }

            *count += 1;

            let key_span = self.span_for(&key);
            let key_slot = match key.attr() {
                StaticOrDynamic::Static(_name) if kind.is_attrs() => KeySlot::Attr {
                    attr: key,
                    slot: self.scope_mut().declare_phantom(key_span, false),
                },

                StaticOrDynamic::Static(name) => KeySlot::None { name: name.clone() },

                StaticOrDynamic::Dynamic(_dynamic) if kind.is_attrs() => KeySlot::Attr {
                    attr: key,
                    slot: self.scope_mut().declare_phantom(key_span, false),
                },

                _ => {
                    self.emit_error(&key, ErrorKind::DynamicKeyInScope("let-expression"));
                    continue;
                }
            };

            let value_slot = match kind {
                BindingsKind::LetIn | BindingsKind::RecAttrs => match &key_slot {
                    // In recursive scopes, the value needs to be accessible on the
                    // stack if it is statically known
                    KeySlot::None { name } => self.declare_local(&key_span, name.as_str()),

                    // Dynamic values are never resolvable (as their names are
                    // of course only known at runtime).
                    //
                    // Note: This branch is unreachable in `let`-expressions.
                    KeySlot::Attr { slot: _, attr } => {
                        let attr: &Syntax::Attr = attr;
                        if let Some(name) = attr.as_static() {
                            self.declare_local(&key_span, name.as_str())
                        } else {
                            self.scope_mut().declare_phantom(key_span, false)
                        }
                    }
                },

                // In non-recursive attribute sets, the value is inaccessible
                // (only consumed by `OpAttrs`).
                BindingsKind::Attrs => self.scope_mut().declare_phantom(key_span, false),
            };

            let binding = if remaining_path.first().is_some() {
                Binding::Set {
                    span: entry.span_for(self.file),
                    kind: BindingsKind::Attrs,
                    inherits: vec![],
                    entries: vec![Syntax::AttrPathValue::new(
                        entry.span_for(self.file),
                        remaining_path,
                        value,
                    )],
                }
            } else {
                Binding::Plain { expr: value }
            };

            bindings.track_new(key_slot, value_slot, binding);
        }
    }

    /// Compile attribute set literals into equivalent bytecode.
    ///
    /// This is complicated by a number of features specific to Nix attribute
    /// sets, most importantly:
    ///
    /// 1. Keys can be dynamically constructed through interpolation.
    /// 2. Keys can refer to nested attribute sets.
    /// 3. Attribute sets can (optionally) be recursive.
    pub(super) fn compile_attr_set<Syntax>(&mut self, slot: LocalIdx, node: &Syntax::AttrSet)
    where
        Syntax: syn::Syntax,
        Syntax::AttrSet: HasEntries<Syntax>,
    {
        // Open a scope to track the positions of the temporaries used by the
        // `OpAttrs` instruction.
        self.scope_mut().begin_scope();

        let kind = if node.is_rec() {
            BindingsKind::RecAttrs
        } else {
            BindingsKind::Attrs
        };

        self.compile_bindings::<_, Syntax>(slot, kind, node);

        // Remove the temporary scope, but do not emit any additional cleanup
        // (OpAttrs consumes all of these locals).
        self.scope_mut().end_scope();
    }

    /// Actually binds all tracked bindings by emitting the bytecode that places
    /// them in their stack slots.
    fn bind_values<Syntax>(&mut self, bindings: TrackedBindings<Syntax>)
    where
        Syntax: syn::Syntax,
    {
        let mut value_indices: Vec<LocalIdx> = vec![];

        for binding in bindings.bindings.into_iter() {
            value_indices.push(binding.value_slot);

            match binding.key_slot {
                KeySlot::None { .. } => {} // nothing to do here
                KeySlot::Attr { slot, attr } => match attr.attr() {
                    StaticOrDynamic::Static(name) => {
                        let span = self.scope()[slot].span;
                        self.emit_constant(name.as_str().into(), &span);
                        self.scope_mut().mark_initialised(slot);
                    }
                    StaticOrDynamic::Dynamic(expr) => {
                        let expr = expr.clone().to_cst();
                        self.compile(slot, expr.clone());
                        self.emit_force(&expr);
                        self.scope_mut().mark_initialised(slot);
                    }
                },
            }

            match binding.binding {
                // This entry is an inherit (from) expr. The value is placed on
                // the stack by selecting an attribute.
                Binding::Inherit(inherit_from) => {
                    if let Some(namespace) = inherit_from.namespace() {
                        // Create a thunk wrapping value (which may be one as well)
                        // to avoid forcing the from expr too early.
                        self.thunk(binding.value_slot, &namespace, |c, s| {
                            let attrs = inherit_from.attrs();
                            assert!(attrs.len() == 1);
                            let name = attrs
                                .first()
                                .unwrap()
                                .clone()
                                .as_static()
                                .unwrap()
                                .as_str()
                                .into();
                            c.compile(s, namespace.clone().to_cst());
                            c.emit_force(&namespace);
                            c.emit_constant(name, &inherit_from.span_for(c.file));
                            c.push_op(OpCode::OpAttrsSelect, &inherit_from.span_for(c.file));
                        })
                    } else {
                        unimplemented!()
                    }
                }

                // Binding is "just" a plain expression that needs to be
                // compiled.
                Binding::Plain { expr } => self.compile(binding.value_slot, expr.to_cst()),

                // Binding is a merged or nested attribute set, and needs to be
                // recursively compiled as another binding.
                ref set @ Binding::Set { kind, .. } => {
                    self.thunk(binding.value_slot, set, |c, _| {
                        c.scope_mut().begin_scope();
                        c.compile_bindings::<_, Syntax>(binding.value_slot, kind, set);
                        c.scope_mut().end_scope();
                    })
                }
            }

            // Any code after this point will observe the value in the right
            // stack slot, so mark it as initialised.
            self.scope_mut().mark_initialised(binding.value_slot);
        }

        // Final pass to emit finaliser instructions if necessary.
        for idx in value_indices {
            if self.scope()[idx].needs_finaliser {
                let stack_idx = self.scope().stack_index(idx);
                let span = self.scope()[idx].span;
                self.push_op(OpCode::OpFinalise(stack_idx), &span);
            }
        }
    }

    fn compile_bindings<N, Syntax>(&mut self, slot: LocalIdx, kind: BindingsKind, node: &N)
    where
        Syntax: syn::Syntax,
        N: ToSpan + HasEntries<Syntax>,
    {
        let mut count = 0;
        self.scope_mut().begin_scope();

        // Vector to track all observed bindings.
        let mut bindings: TrackedBindings<Syntax> = TrackedBindings::new();

        let inherit_froms = self.compile_plain_inherits::<_, Syntax>(slot, kind, &mut count, node);
        self.declare_namespaced_inherits(kind, inherit_froms, &mut bindings);

        self.declare_bindings(kind, &mut count, &mut bindings, node);

        // Check if we can bail out on empty bindings
        if count == 0 {
            // still need an attrset to exist, but it is empty.
            if kind.is_attrs() {
                self.emit_constant(Value::Attrs(Box::new(NixAttrs::empty())), node);
                return;
            }

            self.emit_warning(node, WarningKind::EmptyLet);
            return;
        }

        // Actually bind values and ensure they are on the stack.
        self.bind_values(bindings);

        if kind.is_attrs() {
            self.push_op(OpCode::OpAttrs(Count(count)), node);
        }

        if count == 0 {
            self.unthunk();
        }
    }

    /// Compile a standard `let ...; in ...` expression.
    ///
    /// Unless in a non-standard scope, the encountered values are simply pushed
    /// on the stack and their indices noted in the entries vector.
    pub(super) fn compile_let_in<Syntax>(&mut self, slot: LocalIdx, node: &Syntax::LetIn)
    where
        Syntax: syn::Syntax,
    {
        self.compile_bindings::<_, Syntax>(slot, BindingsKind::LetIn, node);

        // Deal with the body, then clean up the locals afterwards.
        self.compile(slot, node.clone().body().clone().to_cst());
        self.cleanup_scope(node);
    }

    pub(super) fn compile_legacy_let<Syntax>(&mut self, slot: LocalIdx, node: &Syntax::LegacyLet)
    where
        Syntax: syn::Syntax,
    {
        self.emit_warning(node, WarningKind::DeprecatedLegacyLet);
        self.scope_mut().begin_scope();
        self.compile_bindings::<_, Syntax>(slot, BindingsKind::RecAttrs, node);

        // Remove the temporary scope, but do not emit any additional cleanup
        // (OpAttrs consumes all of these locals).
        self.scope_mut().end_scope();

        self.emit_constant("body".into(), node);
        self.push_op(OpCode::OpAttrsSelect, node);
    }

    /// Is the given identifier defined *by the user* in any current scope?
    pub(super) fn is_user_defined(&mut self, ident: &str) -> bool {
        matches!(
            self.scope_mut().resolve_local(ident),
            LocalPosition::Known(_) | LocalPosition::Recursive(_)
        )
    }

    /// Resolve and compile access to an identifier in the scope.
    fn compile_identifier_access<N: ToSpan + Clone>(
        &mut self,
        slot: LocalIdx,
        ident: &str,
        node: &N,
    ) {
        match self.scope_mut().resolve_local(ident) {
            LocalPosition::Unknown => {
                // Are we possibly dealing with an upvalue?
                if let Some(idx) = self.resolve_upvalue(self.contexts.len() - 1, ident, node) {
                    self.push_op(OpCode::OpGetUpvalue(idx), node);
                    return;
                }

                // Globals are the "upmost upvalues": they behave
                // exactly like a `let ... in` prepended to the
                // program's text, and the global scope is nothing
                // more than the parent scope of the root scope.
                if let Some(global) = self.globals.get(ident) {
                    self.emit_constant(global.clone(), &self.span_for(node));
                    return;
                }

                // If there is a non-empty `with`-stack (or a parent context
                // with one), emit a runtime dynamic resolution instruction.
                //
                // Since it is possible for users to e.g. assign a variable to a
                // dynamic resolution without actually using it, this operation
                // is wrapped in an extra thunk.
                if self.has_dynamic_ancestor() {
                    self.thunk(slot, node, |c, _| {
                        c.context_mut().captures_with_stack = true;
                        c.emit_constant(ident.into(), node);
                        c.push_op(OpCode::OpResolveWith, node);
                    });
                    return;
                }

                // Otherwise, this variable is missing.
                self.emit_error(node, ErrorKind::UnknownStaticVariable);
            }

            LocalPosition::Known(idx) => {
                let stack_idx = self.scope().stack_index(idx);
                self.push_op(OpCode::OpGetLocal(stack_idx), node);
            }

            // This identifier is referring to a value from the same scope which
            // is not yet defined. This identifier access must be thunked.
            LocalPosition::Recursive(idx) => self.thunk(slot, node, move |compiler, _| {
                let upvalue_idx = compiler.add_upvalue(
                    compiler.contexts.len() - 1,
                    node,
                    UpvalueKind::Local(idx),
                );
                compiler.push_op(OpCode::OpGetUpvalue(upvalue_idx), node);
            }),
        };
    }

    pub(super) fn compile_ident<Syntax>(&mut self, slot: LocalIdx, node: &Syntax::Ident)
    where
        Syntax: syn::Syntax,
    {
        self.compile_identifier_access(slot, node.ident(), node);
    }
}

/// Private compiler helpers related to bindings.
impl Compiler<'_, '_> {
    fn resolve_upvalue<N: ToSpan>(
        &mut self,
        ctx_idx: usize,
        name: &str,
        node: &N,
    ) -> Option<UpvalueIdx> {
        if ctx_idx == 0 {
            // There can not be any upvalue at the outermost context.
            return None;
        }

        // Determine whether the upvalue is a local in the enclosing context.
        match self.contexts[ctx_idx - 1].scope.resolve_local(name) {
            // recursive upvalues are dealt with the same way as standard known
            // ones, as thunks and closures are guaranteed to be placed on the
            // stack (i.e. in the right position) *during* their runtime
            // construction
            LocalPosition::Known(idx) | LocalPosition::Recursive(idx) => {
                return Some(self.add_upvalue(ctx_idx, node, UpvalueKind::Local(idx)))
            }

            LocalPosition::Unknown => { /* continue below */ }
        };

        // If the upvalue comes from even further up, we need to recurse to make
        // sure that the upvalues are created at each level.
        if let Some(idx) = self.resolve_upvalue(ctx_idx - 1, name, node) {
            return Some(self.add_upvalue(ctx_idx, node, UpvalueKind::Upvalue(idx)));
        }

        None
    }

    fn add_upvalue<N: ToSpan>(
        &mut self,
        ctx_idx: usize,
        node: &N,
        kind: UpvalueKind,
    ) -> UpvalueIdx {
        // If there is already an upvalue closing over the specified index,
        // retrieve that instead.
        for (idx, existing) in self.contexts[ctx_idx].scope.upvalues.iter().enumerate() {
            if existing.kind == kind {
                return UpvalueIdx(idx);
            }
        }

        let span = self.span_for(node);
        self.contexts[ctx_idx]
            .scope
            .upvalues
            .push(Upvalue { kind, span });

        let idx = UpvalueIdx(self.contexts[ctx_idx].lambda.upvalue_count);
        self.contexts[ctx_idx].lambda.upvalue_count += 1;
        idx
    }
}
