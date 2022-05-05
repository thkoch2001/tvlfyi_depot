let NameVal = λ(T : Type) → { name : Text, value : T }

in  λ ( imports
      : { -- Take an aerc filter from the aerc distribution /share directory
          aercFilter : Text → Text
        , -- given a dsl of functions to create an Ini, render the ini file
          toIni :
            { globalSection : List (NameVal Text)
            , sections : List (NameVal (List (NameVal Text)))
            } →
              Text
        }
      ) →
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

      in  { accounts =
              imports.toIni
                { globalSection = [] : List (NameVal Text)
                , sections =
                  [ { name = "mail"
                    , value =
                      [ { name = "archive", value = "Archive" }
                      , { name = "copy-to", value = "Sent" }
                      , { name = "default", value = "INBOX" }
                      , { name = "from"
                        , value = "Profpatsch <mail@profpatsch.de>"
                        }
                      , { name = "source", value = "maildir://~/.Mail/mail" }
                      , { name = "postpone", value = "Drafts" }
                      ]
                    }
                  ]
                }
          , aerc =
              imports.toIni
                { globalSection = [] : List (NameVal Text)
                , sections =
                  [ { name = "filters"
                    , value =
                      [ { name = "text/html"
                        , value = imports.aercFilter "html"
                        }
                      , let _ = "-- TODO: this awk should be taken from nix!"

                        in  { name = "text/*"
                            , value = "awk -f ${imports.aercFilter "plaintext"}"
                            }
                      ]
                    }
                  ]
                }
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
                          then  { name = "<C-${k.key}>", value = k.cmd }
                          else  { name = k.key, value = k.cmd }

                  let

                      -- render a list of keys to config format
                      renderKeys =
                        λ(keys : List Key) →
                          List/map Key (NameVal Text) renderKey keys

                  let
                      -- create a section whith a name and a list of keys
                      sect =
                        λ(section : Text) →
                        λ(keys : List Key) →
                          { name = section, value = renderKeys keys }

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

                  in  imports.toIni config
          }
