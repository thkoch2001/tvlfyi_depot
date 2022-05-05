let
    -- DSL for building INI files
    ToIniFns =
      λ ( Ini
        : { -- A Section in the ini
            Section : Type
          , -- A couple of sections
            SectionList : Type
          }
        ) →
        { -- Create a new section
          newSection : Ini.Section
        , -- Add a key/value pair to the section
          add : Text → Text → Ini.Section → Ini.Section
        , -- Add all these key/value pairs to the section
          addAll :
            List { name : Text, value : Text } → Ini.Section → Ini.Section
        , -- Create a new SectionList
          newSectionList : Ini.SectionList
        , -- Add a section to the list of sections
          addSection : Text → Ini.Section → Ini.SectionList → Ini.SectionList
        }

in  λ ( imports
      : { -- concatenate a list with newlines
          concatNewline : List Text → Text
        , -- Take an aerc filter from the aerc distribution /share directory
          aercFilter : Text → Text
        , -- given a dsl of functions to create an Ini, render the ini file
          toIni :
            ( ∀(Ini : { Section : Type, SectionList : Type }) →
              ToIniFns Ini →
                { globalSection : Ini.Section, sections : Ini.SectionList }
            ) →
              Text
        }
      ) →
      let List/foldLeft
          : ∀(a : Type) →
            List a →
            ∀(list : Type) →
            ∀(cons : list → a → list) →
            ∀(nil : list) →
              list
          = λ(a : Type) →
            λ(xs : List a) →
            λ(list : Type) →
            λ(cons : list → a → list) →
            λ(nil : list) →
              List/fold
                a
                xs
                (list → list)
                (λ(x : a) → λ(f : list → list) → λ(l : list) → f (cons l x))
                (λ(l : list) → l)
                nil

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
          -- A builder is a list of “build methods” that go from (a -> a) and change the a step by step.
          Builder/build =
            λ(a : Type) →
            λ(init : a) →
            λ(builders : List (a → a)) →
              List/foldLeft
                (a → a)
                builders
                a
                (λ(acc : a) → λ(f : a → a) → f acc)
                init

      in  { accounts =
              imports.toIni
                ( λ(Ini : { Section : Type, SectionList : Type }) →
                  λ(ini : ToIniFns Ini) →
                    { globalSection = ini.newSection
                    , sections =
                        ini.addSection
                          "mail"
                          ( ini.addAll
                              [ { name = "archive", value = "Archive" }
                              , { name = "copy-to", value = "Sent" }
                              , { name = "default", value = "INBOX" }
                              , { name = "from"
                                , value = "Profpatsch <mail@profpatsch.de>"
                                }
                              , { name = "source"
                                , value = "maildir://~/.Mail/mail"
                                }
                              , { name = "postpone", value = "Drafts" }
                              ]
                              ini.newSection
                          )
                          ini.newSectionList
                    }
                )
          , aerc =
              imports.toIni
                ( λ(Ini : { Section : Type, SectionList : Type }) →
                  λ(ini : ToIniFns Ini) →
                    { globalSection = ini.newSection
                    , sections =
                        ini.addSection
                          "filters"
                          ( Builder/build
                              Ini.Section
                              ini.newSection
                              [ ini.add "text/html" (imports.aercFilter "html")
                              , let _ =
                                      "-- TODO: this awk should be taken from nix!"

                                in  ini.add
                                      "text/*"
                                      "awk -f ${imports.aercFilter "plaintext"}"
                              ]
                          )
                          ini.newSectionList
                    }
                )
          , binds =
              let
                  -- keybinding and command to run
                  Key =
                    { ctrl : Bool, key : Text, cmd : Text }

              in  let
                      -- render a key to config format
                      renderKey =
                        λ(k : Key) →
                          if    k.ctrl
                          then  "<C-${k.key}> = ${k.cmd}"
                          else  "${k.key} = ${k.cmd}"

                  let

                      -- render a list of keys to config format
                      renderKeys =
                        λ(keys : List Key) → List/map Key Text renderKey keys

                  let
                      -- create a section whith a name and a list of keys
                      sect =
                        λ(section : Text) →
                        λ(keys : List Key) →
                          { section, keys = renderKeys keys }

                  let

                      -- set key without modifiers
                      key =
                        λ(key : Text) → { key }

                  let
                      -- set special key without modifiers
                      special =
                        λ(key : Text) → { key = "<${key}>" }

                  let
                      -- no modifier
                      none =
                        { ctrl = False }

                  let
                      -- set control key
                      ctrl =
                        { ctrl = True }

                  let
                      -- set a command to execute
                      cmd =
                        λ(cmd : Text) → { cmd = ":${cmd}<Enter>" }

                  let
                      -- set a command, but stay on the prompt
                      prompt =
                        λ(cmd : Text) → { cmd = ":${cmd}<Space>" }

                  let config =
                        { globalSection =
                            renderKeys
                              [ ctrl ∧ key "p" ∧ cmd "prev-tab"
                              , ctrl ∧ key "n" ∧ cmd "next-tab"
                              , ctrl ∧ key "t" ∧ cmd "term"
                              ]
                        , sections =
                          [ sect
                              "messages"
                              [ ctrl ∧ key "q" ∧ cmd "quit"
                              , none ∧ special "Up" ∧ cmd "prev"
                              , none ∧ special "Down" ∧ cmd "next"
                              , none ∧ special "PgUp" ∧ cmd "prev 100%"
                              , none ∧ special "PgDn" ∧ cmd "next 100%"
                              , none ∧ key "g" ∧ cmd "select 0"
                              , none ∧ key "G" ∧ cmd "select -1"
                              , ctrl ∧ key "Up" ∧ cmd "prev-folder"
                              , ctrl ∧ key "Down" ∧ cmd "next-folder"
                              , none ∧ key "v" ∧ cmd "mark -t"
                              , none ∧ key "V" ∧ cmd "mark -v"
                              , none ∧ special "Enter" ∧ cmd "view"
                              , none ∧ key "c" ∧ cmd "compose"
                              , none ∧ key "|" ∧ prompt "pipe"
                              , none ∧ key "t" ∧ prompt "term"
                              , none ∧ key "/" ∧ prompt "search"
                              , none ∧ key "n" ∧ cmd "next-result"
                              , none ∧ key "N" ∧ cmd "prev-result"
                              , none ∧ special "Esc" ∧ cmd "clear"
                              ]
                          , sect "view" [ none ∧ key "q" ∧ cmd "close" ]
                          ]
                        }

                  let Section = { section : Text, keys : List Text }

                  let iniToJson =
                        λ ( ini
                          : { globalSection : List Text
                            , sections : List Section
                            }
                          ) →
                          let mkKeys = imports.concatNewline

                          let
                              -- TODO: escaping section header?
                              mkSection =
                                λ(section : Section) →
                                      ''
                                      [${section.section}]
                                      ''
                                  ++  mkKeys section.keys

                          in      mkKeys ini.globalSection
                              ++  mkKeys
                                    ( List/map
                                        Section
                                        Text
                                        mkSection
                                        ini.sections
                                    )

                  in  iniToJson config
          }
