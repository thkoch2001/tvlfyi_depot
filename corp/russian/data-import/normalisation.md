Corpora data normalisation
==========================

The point of this project is not just to import the import files into a SQLite
database, but also to normalise the data into a structure that is actually
pleasant to work with.

This document describes which data is available, how it is likely to be queried,
and how we can get from the raw data to a useful layout.

## Term definitions

We are dealing with different types of data, which are referred to by different
names between the different corpora:

* lemmata: a lemma is the base / "dictionary" form of a word
* forms: morphological variations of a word (e.g. for a lemma "видеть" a form
  might be "вижу")
* grammeme: a grammatical "feature", e.g. a case, conjugation, gender etc.

## Dataset issues

The biggest problem for our normalisation is that the datasets have slightly
different definitions of what is what, for example something that is considered
a lemma in OpenCorpora might not be a lemma in OpenRussian.

For example, as of 2023-01-25 we have these discrepancies:

```sql
-- How many lemmata from OpenCorpora are not lemmata in OpenRussian?
SELECT count(*) FROM oc_lemmas WHERE lemma NOT IN (SELECT bare FROM or_words);
-- => 302739

-- And vice-versa?
SELECT count(*) FROM or_words WHERE bare NOT IN (SELECT lemma FROM oc_lemmas)
-- => 4627
```

Regardless of the definition of a lemma, the dataset of OpenCorpora is *much*
larger than that of OpenRussian, and when considering all word forms the
differences look like this:

```sql
-- How many word forms from OpenCorpora are not known in OpenRussian?
SELECT count(*) FROM oc_words WHERE word NOT IN (SELECT form_bare FROM or_words_forms)
-- => 3420344

-- And vice-versa?
SELECT count(*) FROM or_words_forms WHERE form_bare NOT IN (SELECT word FROM oc_words)
-- => 34335
```

This means that the coverage of the forms known in OpenRussian is *almost*
complete in OpenCorpora, and the basis of our forms data should be OpenCorpora.
There are words missing in OC that I would expect to be there, such as
"вспомния" or "автомойка", but sorting these out later is going to be much
easier than going in the other direction.

It would be interesting to know how many overlapping forms (i.e. same
grammatical shape) map to _different_ lemmata between the corpora, but this is
difficult to determine for now.

One more weird question is how many lemmata do not literally appear in the set
of their forms?

```sql
-- In OpenCorpora:
SELECT * FROM oc_lemmas WHERE lemma NOT IN (SELECT word FROM oc_words)

-- =>
-- 7513	аксеновы
-- 224504	парфеновы

-- In OpenRussian:
SELECT COUNT(*) FROM or_words WHERE bare NOT IN (SELECT form_bare FROM or_words_forms)
-- => 19005
```

This seems to be extremely common in OpenRussian, but we'd prefer not to lose
this data. How many of the ones that are missing in OpenRussian's forms are also
missing in OC's?

```sql
SELECT COUNT(DISTINCT(bare))
FROM or_words
WHERE bare NOT IN (SELECT form_bare FROM or_words_forms)
AND bare NOT IN (SELECT word FROM oc_words)

-- => 1019
```

That's not too bad. Most of these are *weird* words, too, or entire phrases
(which seem to be mistakes in the OpenRussian corpus?).

## Available information

Some of this information overlaps between corpora, but almost all of it is
structured very differently. Marked `[OR]` for data available in OpenRussian,
`[OC]` for data in OpenCorpora, and `[B]` for data in both.

For lemmata:

* [B] bare, literal lemma itself
* [OC] detailed set of grammemes for each lemma
* [OR] high-level word type (e.g. verb, noun, ...) for each lemma
* [OR] parent lemma (from which this is derived), but only for a small number of
  them (~1000)
* [OR] accent marks!
* [OR] word rank (for over 50k words)
* [OR] word translation (53512 out of 89846)

For word forms:

* [B] bare, literal form itself
* [OC] detailed set of grammemes for each form
* [OR] form_type (condensed grammeme reprsentation, e.g.
  `ru_verb_gerund_present`, see also `src/mapping.rs`)
* [OR] accent marks!

## Normalised grammemes

We can use the `oc_grammemes` table almost literally, though eventually we'd
like to rename some of the things as the names are very ... odd.

## Normalised lemmata

We can create a `lemmata` table with these fields:

* `id`: unique lemma ID (of course)
* `lemma`: the bare lemma itself
* `rank`: the word rank (where present)

This table is enhanced by the relation tables:

* `lemmata_grammemes`: same as `oc_lemma_grammemes`
* `translations`: map lemma IDs to (potentially multiple) translations

## Normalised forms

We can create a `forms` table with these fields:

* `id`: unique form ID
* `lemma`: foreign key to `lemmata`
* `form`: bare word form
* `accented`: word form with accent mark (if present)

Whch is enhanced by the relation tale `form_grammemes` (similar to
`oc_words_grammemes`).

## Normalisation procedure

We can create the tables above using simple selects into new tables. We
initially retain all known IDs from OC and OR as extra fields (which are later
dropped).

## Cleanup considerations

### Data precedence

It seems that the grammeme annotations of OpenCorpora are much more consistent
and thorough than the ones in OpenRussian, so these grammemes should take
precedence.

Howev
