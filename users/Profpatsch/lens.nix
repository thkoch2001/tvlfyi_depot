{ ... }:
let
  id = x: x;

  const = x: y: x;

  comp = f: g: x: f (g x);

  _ = v: f: f v;

  # Profunctor (p :: Type -> Type -> Type)
  Profunctor = rec {
    # dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
    dimap = f: g: x: lmap f (rmap g x);
    # lmap :: (a -> b) -> p b c -> p a c
    lmap = f: dimap f id;
    # rmap :: (c -> d) -> p b c -> p b d
    rmap = g: dimap id g;
  };

  # Profunctor (->)
  profunctorFun = Profunctor // {
    # dimap :: (a -> b) -> (c -> d) -> (b -> c) -> a -> d
    dimap = ab: cd: bc: a: cd (bc (ab a));
    # lmap :: (a -> b) -> (b -> c) -> (a -> c)
    lmap = ab: bc: a: bc (ab a);
    # rmap :: (c -> d) -> (b -> c) -> (b -> d)
    rmap = cd: bc: b: cd (bc b);
  };

  tuple = fst: snd: {
    inherit fst snd;
  };

  swap = { fst, snd }: {
    fst = snd;
    snd = fst;
  };

  # Profunctor p => Strong (p :: Type -> Type -> Type)
  Strong = pro: pro // rec {
    # firstP :: p a b -> p (a, c) (b, c)
    firstP = pab: pro.dimap swap swap (pro.secondP pab);
    # secondP :: p a b -> p (c, a) (c, b)
    secondP = pab: pro.dimap swap swap (pro.firstP pab);
  };

  # Strong (->)
  strongFun = Strong profunctorFun // {
    # firstP :: (a -> b) -> (a, c) -> (b, c)
    firstP = f: { fst, snd }: { fst = f fst; inherit snd; };
    # secondP :: (a -> b) -> (c, a) -> (c, b)
    secondP = f: { snd, fst }: { snd = f snd; inherit fst; };
  };

  # Iso s t a b :: forall p. Profunctor p -> p a b -> p s t

  # iso :: (s -> a) -> (b -> t) -> Iso s t a b
  iso = pro: pro.dimap;

  # Lens s t a b :: forall p. Strong p -> p a b -> p s t

  # lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
  lens = strong: get: set: pab:
    lensP
      strong
      (s: tuple (get s) (b: set s b))
      pab;

  # lensP :: (s -> (a, b -> t)) -> Lens s t a b
  lensP = strong: to: pab:
    strong.dimap
      to
      ({ fst, snd }: snd fst)
      (strong.firstP pab);

  # first element of a tuple
  # _1 :: Lens (a, c) (b, c) a b
  _1 = strong: strong.firstP;

  # second element of a tuple
  # _2 :: Lens (c, a) (c, b) a b
  _2 = strong: strong.secondP;

  # a the given field in the record
  # field :: (f :: String) -> Lens { f :: a; ... } { f :: b; ... } a b
  field = name: strong:
    lens
      strong
      (attrs: attrs.${name})
      (attrs: a: attrs // { ${name} = a; });

  # Setter :: (->) a b -> (->) s t
  # Setter :: (a -> b) -> (s -> t)


  # Subclasses of profunctor for (->).
  # We only have Strong for now, but when we implement Choice we need to add it here.
  profunctorSubclassesFun = strongFun;

  # over :: Setter s t a b -> (a -> b) -> s -> t
  over = setter:
    # A setter needs to be instanced to the profunctor-subclass instances of (->).
    (setter profunctorSubclassesFun);

  # set :: Setter s t a b -> b -> s -> t
  set = setter: b: over setter (const b);

  # combine a bunch of optics, for the subclass instance of profunctor you give it.
  optic = accessors: profunctorSubclass:
    builtins.foldl' comp id
      (map (accessor: accessor profunctorSubclass) accessors);


in
{
  inherit
    id
    _
    const
    comp
    Profunctor
    profunctorFun
    Strong
    strongFun
    iso
    lens
    optic
    _1
    _2
    field
    tuple
    swap
    over
    set
    ;
}
