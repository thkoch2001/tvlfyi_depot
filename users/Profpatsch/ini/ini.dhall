let lib = ../dhall/lib.dhall

let NameVal = λ(T : Type) → { name : Text, value : T }

let ValueList = λ(T : Type) → List (NameVal T)

let Section = ValueList Text

let Sections = ValueList Section

let Ini = { globalSection : Section, sections : Sections }

let
    -- Takes to INI files and merges their global sections and their section lists,
    -- without duplicating by section name.
    appendInis =
      λ(inis : List Ini) →
          { globalSection =
              lib.List/concat
                (NameVal Text)
                (lib.List/map Ini Section (λ(i : Ini) → i.globalSection) inis)
          , sections =
              lib.List/concat
                (NameVal Section)
                (lib.List/map Ini Sections (λ(i : Ini) → i.sections) inis)
          }
        : Ini

let
    -- Signatures of functions that are input via FFI.
    Externs =
      { -- given a dsl of functions to create an Ini, render the ini file
        renderIni : Ini → Text
      }

in  { NameVal, ValueList, Section, Sections, Ini, appendInis, Externs }
