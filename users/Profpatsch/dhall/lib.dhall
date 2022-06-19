let List/map
    : ∀(a : Type) → ∀(b : Type) → (a → b) → List a → List b
    = λ(a : Type) →
      λ(b : Type) →
      λ(f : a → b) →
      λ(xs : List a) →
        List/build
          b
          ( λ(list : Type) →
            λ(cons : b → list → list) →
              List/fold a xs list (λ(x : a) → cons (f x))
          )

let

    --| Concatenate a `List` of `List`s into a single `List`
    List/concat
    : ∀(a : Type) → List (List a) → List a
    = λ(a : Type) →
      λ(xss : List (List a)) →
        List/build
          a
          ( λ(list : Type) →
            λ(cons : a → list → list) →
            λ(nil : list) →
              List/fold
                (List a)
                xss
                list
                (λ(xs : List a) → λ(ys : list) → List/fold a xs list cons ys)
                nil
          )

let


    -- Transform a list by applying a function to each element and flattening the results
    List/concatMap
    : ∀(a : Type) → ∀(b : Type) → (a → List b) → List a → List b
    = λ(a : Type) →
      λ(b : Type) →
      λ(f : a → List b) →
      λ(xs : List a) →
        List/build
          b
          ( λ(list : Type) →
            λ(cons : b → list → list) →
              List/fold a xs list (λ(x : a) → List/fold b (f x) list cons)
          )

let Status = < Empty | NonEmpty : Text >

let

    {-|
    Transform each value in a `List` to `Text` and then concatenate them with a
    separator in between each value
    -}
    Text/concatMapSep
    : ∀(separator : Text) → ∀(a : Type) → (a → Text) → List a → Text
    = λ(separator : Text) →
      λ(a : Type) →
      λ(f : a → Text) →
      λ(elements : List a) →
        let status =
              List/fold
                a
                elements
                Status
                ( λ(x : a) →
                  λ(status : Status) →
                    merge
                      { Empty = Status.NonEmpty (f x)
                      , NonEmpty =
                          λ(result : Text) →
                            Status.NonEmpty (f x ++ separator ++ result)
                      }
                      status
                )
                Status.Empty

        in  merge { Empty = "", NonEmpty = λ(result : Text) → result } status

in  { List/map, List/concat, List/concatMap, Text/concatMapSep }
