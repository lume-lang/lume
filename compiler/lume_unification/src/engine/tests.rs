use std::collections::{HashMap, HashSet};

use crate::engine::{Type as _, *};

#[derive(Default, Hash, Debug, Clone, Copy, PartialEq, Eq)]
struct ID(u64);

impl ID {
    #[inline]
    pub fn from_name<N: Hash + ?Sized>(name: &N) -> Self {
        Self(lume_hash::portable_hash(name) as u64)
    }
}

#[derive(Default)]
struct DummyContext {
    next_id: u64,

    /// Declares all the subtypes of each type within the context.
    subtypes: HashMap<ID, HashSet<ID>>,
}

impl DummyContext {
    fn next_id(&mut self) -> ID {
        let id = ID(self.next_id + 1);
        self.next_id += 1;

        id
    }

    #[inline]
    pub fn var(&mut self) -> TypeVar<Self> {
        self.fresh_var(ID::default(), ID::default(), Location::empty())
    }
}

impl Context for DummyContext {
    type ID = ID;
    type Ty = Type;

    /// Only used for tracing
    fn name_of(&self, _id: Self::ID) -> Result<String> {
        Ok(String::new())
    }

    /// Only used for tracing
    fn name_of_type(&self, _ty: &Self::Ty) -> Result<String> {
        Ok(String::new())
    }

    /// Only used for tracing
    fn span_of(&self, _id: Self::ID) -> Location {
        Location::empty()
    }

    fn fresh_var(&mut self, _owner: Self::ID, _binding: Self::ID, _location: Location) -> TypeVar<Self> {
        TypeVar(self.next_id())
    }

    fn as_type(&self, type_var: TypeVar<Self>) -> Self::Ty {
        Type::Var(type_var.0)
    }

    fn kind_of_type(&self, ty: &Self::Ty) -> crate::engine::TypeKind<Self> {
        match ty {
            Type::Con { id, .. } => crate::engine::TypeKind::Concrete(*id),
            Type::Var(id) => crate::engine::TypeKind::Variable(TypeVar(*id)),
            Type::Param(id) => crate::engine::TypeKind::Parameter(*id),
        }
    }

    fn as_type_variable(&self, ty: &Self::Ty) -> Option<TypeVar<Self>> {
        match ty {
            Type::Con { .. } | Type::Param(_) => None,
            Type::Var(id) => Some(TypeVar(*id)),
        }
    }

    fn implements_subtype(&self, ty: &Self::Ty, subtype: &Self::Ty) -> bool {
        self.subtypes
            .get(&ty.id())
            .is_some_and(|set| set.contains(&subtype.id()))
    }
}

#[derive(derive_more::Debug, derive_more::PartialEq, derive_more::Eq, Hash, Clone)]
enum Type {
    /// Concrete type with an ID and an optional set of bound types.
    Con {
        id: ID,
        kind: TypeKind,
        bindings: Vec<Self>,
    },

    /// A potentially unsolved type variable.
    Var(ID),

    /// A rigid type parameter
    Param(ID),
}

impl Type {
    #[inline]
    pub fn con0<N: Hash + ?Sized>(name: &N) -> Self {
        Self::con(name, Vec::new())
    }

    #[inline]
    pub fn con<N: Hash + ?Sized>(name: &N, bindings: Vec<Self>) -> Self {
        Self::Con {
            id: ID::from_name(name),
            kind: TypeKind::Struct,
            bindings,
        }
    }

    #[inline]
    pub fn sub0<N: Hash + ?Sized>(name: &N) -> Self {
        Self::sub(name, Vec::new())
    }

    #[inline]
    pub fn sub<N: Hash + ?Sized>(name: &N, bindings: Vec<Self>) -> Self {
        Self::Con {
            id: ID::from_name(name),
            kind: TypeKind::Trait,
            bindings,
        }
    }

    #[inline]
    pub fn param<N: Hash + ?Sized>(name: &N) -> Self {
        Self::Param(ID::from_name(name))
    }
}

#[derive(Hash, Debug, Clone, Copy, PartialEq, Eq)]
enum TypeKind {
    Struct,
    Trait,
}

impl crate::engine::Type<DummyContext> for Type {
    fn id(&self) -> <DummyContext as Context>::ID {
        match self {
            Self::Con { id, .. } | Self::Var(id) | Self::Param(id) => *id,
        }
    }

    fn bound_types(&self) -> &[Self] {
        match self {
            Self::Con { bindings, .. } => bindings,
            Self::Var { .. } | Self::Param(_) => &[],
        }
    }

    fn bound_types_mut(&mut self) -> &mut [Self] {
        match self {
            Self::Con { bindings, .. } => bindings,
            Self::Var { .. } | Self::Param(_) => &mut [],
        }
    }
}

#[test]
fn single_equality() {
    let mut ctx = DummyContext::default();

    let t1 = Type::con0("i32");
    let v1 = ctx.var();

    let engine = Engine::new(&mut ctx);

    engine.eq(v1, t1.clone());
    engine.substitute_all().unwrap();

    assert_eq!(engine.resolve(v1).unwrap(), t1);
}

#[test]
fn variable_chaining() {
    let mut ctx = DummyContext::default();

    let v1 = ctx.var();
    let v2 = ctx.var();

    let t1 = Type::con0("i32");

    let engine = Engine::new(&mut ctx);

    engine.eq(v1, engine.ctx.as_type(v2));
    engine.eq(v2, t1.clone());
    engine.substitute_all().unwrap();

    assert_eq!(engine.resolve(v1).unwrap(), t1);
    assert_eq!(engine.resolve(v2).unwrap(), t1);
}

#[test]
fn mismatched_instances() {
    let mut ctx = DummyContext::default();

    let var = ctx.var();
    let con1 = Type::con0("i32");
    let con2 = Type::con0("u32");

    let engine = Engine::new(&mut ctx);
    engine.eq(var, con1);
    engine.eq(var, con2);

    let err = engine.solve(var).unwrap_err();
    assert!(matches!(err, Error::Mismatch { .. }));
}

#[test]
fn mismatched_bound_types() {
    let mut ctx = DummyContext::default();

    let var = ctx.var();
    let con1 = Type::con("Option", vec![Type::con0("i32")]);
    let con2 = Type::con("Option", vec![Type::con0("u32")]);

    let engine = Engine::new(&mut ctx);
    engine.eq(var, con1);
    engine.eq(var, con2);

    let err = engine.solve(var).unwrap_err();
    assert!(matches!(err, Error::Mismatch { .. }));
}

#[test]
fn mismatched_bound_types_len() {
    let mut ctx = DummyContext::default();

    let var = ctx.var();
    let con1 = Type::con("Option", vec![Type::con0("i32")]);
    let con2 = Type::con("Option", vec![Type::con0("i32"), Type::con0("u32")]);

    let engine = Engine::new(&mut ctx);
    engine.eq(var, con1);
    engine.eq(var, con2);

    let err = engine.solve(var).unwrap_err();
    assert!(matches!(err, Error::Mismatch { .. }));
}

#[test]
fn bound_unsatisfied() {
    let mut ctx = DummyContext::default();

    let var = ctx.var();
    let con2 = Type::con0("Foo");
    let obl2 = Type::sub0("Display");

    let engine = Engine::new(&mut ctx);
    engine.eq(var, con2);
    engine.sub(var, obl2, ID::from_name("T"));

    let errors = engine.substitute_all().unwrap_err();
    let error = errors.into_values().next().unwrap();

    assert!(matches!(error, Error::BoundUnsatisfied { .. }));
}

#[test]
fn infinite_type() {
    let mut ctx = DummyContext::default();

    let var = ctx.var();
    let con1 = Type::con("Option", vec![ctx.as_type(var)]);

    let engine = Engine::new(&mut ctx);
    engine.eq(var, con1);

    let err = engine.solve(var).unwrap_err();
    assert!(matches!(err, Error::InfiniteType { .. }));
}

#[test]
fn type_parameter_substitution() {
    // ```lm
    // fn array_of<TItem>(_: TItem) -> Array<TItem>;
    //
    // fn foo<T>(val: T) {
    //     // `value` = `T?X`
    //     // `arr` = `T?Y`
    //
    //     let arr = array_of(val);
    //     // `arr` should resolve to `Array<T>` instead of `Array<T?X>`
    // }
    // ```

    let mut ctx = DummyContext::default();

    let param1 = Type::param("T");

    let var1 = ctx.var(); // Type variable `X`
    let var2 = ctx.var(); // Type variable `Y`

    let arr = Type::con("Array", vec![ctx.as_type(var1)]);

    let engine = Engine::new(&mut ctx);
    engine.eq(var1, param1.clone());
    engine.eq(var2, arr.clone());

    engine.substitute_all().unwrap();

    assert_eq!(engine.resolve(var1).unwrap(), param1);
    assert_eq!(engine.resolve(var2).unwrap(), Type::con("Array", vec![param1]));
}
