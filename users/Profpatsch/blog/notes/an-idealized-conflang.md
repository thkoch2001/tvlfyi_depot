tags: netencode, json
date: 2022-03-31
certainty: likely
status: initial
title: An idealized Configuration Language

# An Idealized Configuration Language

JSON brought us one step closer to what an idealized configuration language is,
which I define as “data, stripped of all externalities of the system it is working in”.

Specifically, JSON is very close to what I consider the minimal properties to represent structured data.

## A short history, according to me

In the beginning, Lisp defined s-expressions as a stand-in for an actual syntax.
Then, people figured out that it’s also a way to represent structured data.
It has scalars, which can be nested into lists, recursively.

```
(this is (a (list) (of lists)))
```

This provides the first three rules of our idealized language:

1. A **scalar** is a primitive value that is domain-specific.
   We can assume a bunch of bytes here, or a text or an integer.
   
2. A **list** gives an ordering to `0..n` (or `1..n`) values
   
3. Both a scalar and a list are the *same kind* of “thing” (from here on called **value**),
   lists can be created from arbitrary values *recursively*
   (for example scalars, or lists of scalars and other lists)


Later, ASN.1 came and had the important insight that the same idealized data structure
can be represented in different fashions,
for example as a binary-efficient version and a human-readable format.

Then, XML “graced” the world for a decade or two, and the main lesson from it was
that you don’t want to mix markup languages and configuration languages,
and that you don’t want a committee to design these things.

---

In the meantime, Brendan Eich designed Javascript. Its prototype-based object system
arguably stripped down the rituals of existing OO-systems.
Douglas Crockford later extracted the object format (minus functions) into a syntax, and we got JSON.

```
{
  "foo": [
    { "nested": "attrs" },
    "some text"
  ],
  "bar": 42
}
```

JSON adds another fundamental idea into the mix:

4. **Records** are unordered collections of `name`/`value` pairs.
   A `name` is defined to be a unicode string, so a semantic descriptor of the nested `value`.

Unfortunately, the JSON syntax does not actually specify any semantics of records (`objects` in JSON lingo),
in particular it does not mention what the meaning is if a `name` appears twice in one record.

If records can have multiple entries with the same `name`, suddenly ordering becomes important!
But wait, remember earlier we defined *lists* to impose ordering on two values.
So in order to rectify that problem, we say that

5. A `name` can only appear in a record *once*, names must be unique.

This is the current state of the programming community at large,
where most “modern” configuration languages basically use a version of the JSON model
as their underlying data structure. (However not all of them use the same version.)

## Improving JSON’s data model

We are not yet at the final “idealized” configuration language, though.

Modern languages like Standard ML define their data types as a mixture of 

* *records* (“structs” in the C lingo)
* and *sums* (which you can think about as enums that can hold more `value`s inside them)

This allows to express the common pattern where some fields in a record are only meaningful
if another field—the so-called `tag`-field—is set to a specific value.

An easy example: if a request can fail with an error message or succeed with a result.

You could model that as 

```
{
  "was_error": true,
  "error_message": "there was an error"
}
```

or

```
{
  "was_error": false,
  "result": 42
}
```

in your JSON representation.

But in a ML-like language (like, for example, Rust), you would instead model it as

```
type RequestResult 
  = Error { error_message: String }
  | Success { result: i64 }
```

where the distinction in `Error` or `Success` makes it clear that `error_message` and `result`
only exist in one of these cases, not the other.

We *can* encode exactly that idea into JSON in multiple ways, but not a “blessed” way.

For example, another way to encode the above would be

```
{ 
  "Error": { 
    "error_message": "there was an error"
  }
}
```

and

```
{ 
  "Success": { 
    "result": 42
  }
}
```

Particularly notice the difference between the language representation, where the type is “closed”only `Success` or `Error` can happen—
and the data representation where the type is “open”, more cases could potentially exist.

This is an important differentiation from a type system:
Our idealized configuration language just gives more structure to a bag of data,
it does not restrict which value can be where.
Think of a value in an unityped language, like Python.


So far we have the notion of 

1. a scalar (a primitive)
2. a list (ordering on values)
3. a record (unordered collection of named values)

and in order to get the “open” `tag`ged enumeration values, we introduce

4. a `tag`, which gives a name to a value

We can then redefine `record` to mean “an unordered collection of `tag`ged values”,
which further reduces the amount of concepts needed.

And that’s it, this is the full idealized configuration language.


## Some examples of data modelling with tags

This is all well and good, but what does it look like in practice?

For these examples I will be using JSON with a new `< "tag": value >` syntax
to represent `tag`s.

From a compatibility standpoint, `tag`s (or sum types) have dual properties to record types.

With a record, when you have a producer that *adds* a field to it, the consumer will still be able to handle the record (provided the semantics of the existing fields is not changed by the new field).

With a tag, *removing* a tag from the producer will mean that the consumer will still be able to handle the tag. It might do one “dead” check on the removed `tag`, but can still handle the remaining ones just fine.

<!-- TODO: some illustration here -->
    
An example of how that is applied in practice is that in `protobuf3`, fields of a record are *always* optional fields.

We can model optional fields by wrapping them in `< "Some": value >` or `< "None": {} >` (where the actual value of the `None` is ignored or always an empty record).

So a protobuf with the fields `foo: int` and `bar: string` has to be parsed by the receiver als containing *four* possibilities:

№|foo|bar|
|--:|---|---|
|1|`<"None":{}>`|`<"None":{}>`|
|2|`<"Some":42>`|`<"None":{}>`|
|3|`<"None":{}>`|`<"Some":"x">`|
|4|`<"Some":42>`|`<"Some":"x">`|

Now, iff the receiver actually handles all four possibilities
(and doesn’t just crash if a field is not set, as customary in million-dollar-mistake languages),
it’s easy to see how removing a field from the producer is semantically equal to always setting it to `<"None":{}>`.
Since all receivers should be ready to receive `None` for every field, this provides a simple forward-compatibility scheme.

We can abstract this to any kind of tag value:
If you start with “more” tags, you give yourself space to remove them later without breaking compatibility, typically called “forward compatibility”.


## To empty list/record or not to

Something to think about is whether records and fields should be defined
to always contain at least one element.

As it stands, JSON has multiple ways of expressing the “empty value”:

* `null`
* `[]`
* `{}`
* `""`
* *leave out the field*

and two of those come from the possibility of having empty structured values.

## Representations of this language

This line of thought originally fell out of me designing [`netencode`](https://code.tvl.fyi/tree/users/Profpatsch/netencode/README.md)
as a small human-debuggable format for pipeline serialization.

In addition to the concepts mentioned here (especially tags),
it provides a better set of scalars than JSON (specifically arbitrary bytestrings),
but it cannot practically be written or modified by hand,
which might be a good thing depending on how you look at it.

---

The way that is compatible with the rest of the ecosystem is probably to use a subset of json
to represent our idealized language.

There is multiple ways of encoding tags in json, which each have their pros and cons.

The most common is probably the “tag field” variant, where the tag is pulled into the nested record:

```
{
  "_tag": "Success",
  "result": 42
}
```

Which has the advantage that people know how to deal with it and that it’s easy to “just add another field”,
plus it is backward-compatible when you had a record in the first place.

It has multiple disadvantages however:

* If your value wasn’t a record (e.g. an int) before, you have to put it in a record and assign an arbitrary name to its field
* People are not forced to “unwrap” the tag first, so they are going to forget to check it
* The magic “_tag” name cannot be used by any of the record’s fields


An in-between version of this with less downsides is to always push a json record onto the stack:

```
{
  "tag": "Success",
  "value": {
    "result": 42
  }
}
```

This makes it harder for people to miss checking the `tag`, but still possible of course.
It also makes it easily possible to inspect the contents of `value` without knowing the
exhaustive list of `tag`s, which can be useful in practice (though often not sound!).
It also gets rid of the “_tag” field name clash problem.

Disadvantages:

* Breaks the backwards-compatibility with an existing record-based approach if you want to introduce `tag`s
* Verbosity of representation
* hard to distinguish a record with the `tag` and `value` fields from a `tag`ed value (though you know the type layout of your data on a higher level, don’t you? ;) )


The final, “most pure” representation is the one I gave in the original introduction:

```
{
  "Success": {
    "result": 42
  }
}
```

Now you *have* to match on the `tag` name first, before you can actually access your data,
and it’s less verbose than the above representation.

Disavantages:

* You also have to *know* what `tag`s to expect, it’s harder to query cause you need to extract the keys and values from the dict and then take the first one.
* Doing a “tag backwards compat” check is harder,
  because you can’t just check whether `_tag` or `tag`/`value` are the keys in the dict.
