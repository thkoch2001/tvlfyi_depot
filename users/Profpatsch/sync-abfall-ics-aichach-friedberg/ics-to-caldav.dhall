let Ini = ../ini/ini.dhall

let Lib = ../dhall/lib.dhall

in  \(Ini/externs : Ini.Externs) ->
      let Vdirsyncer =
            let StorageType =
                  < FileSystem : { path : Text, fileext : < ICS > }
                  | Http : { url : Text }
                  >

            let Collection = < FromA | FromB | Collection : Text >

            let Collections =
                  < Unspecified | TheseCollections : List Collection >

            let Storage = { storageName : Text, storage : StorageType }

            in  { Storage
                , StorageType
                , Collection
                , Collections
                , Pair =
                    { pairName : Text
                    , a : Storage
                    , b : Storage
                    , collections : Collections
                    }
                }

      let toIniSections
          : Vdirsyncer.Pair -> Ini.Sections
          = \(pair : Vdirsyncer.Pair) ->
              let
                  -- we assume the names are [a-zA-Z_]
                  renderList =
                    \(l : List Text) ->
                          "["
                      ++  Lib.Text/concatMapSep
                            ", "
                            Text
                            (\(t : Text) -> "\"${t}\"")
                            l
                      ++  "]"

              in  let nv = \(name : Text) -> \(value : Text) -> { name, value }

                  let mkStorage =
                        \(storage : Vdirsyncer.Storage) ->
                          { name = "storage ${storage.storageName}"
                          , value =
                              merge
                                { FileSystem =
                                    \ ( fs
                                      : { path : Text, fileext : < ICS > }
                                      ) ->
                                      [ nv "type" "filesystem"
                                      , nv
                                          "fileext"
                                          (merge { ICS = ".ics" } fs.fileext)
                                      , nv "path" fs.path
                                      ]
                                , Http =
                                    \(http : { url : Text }) ->
                                      [ nv "type" "http", nv "url" http.url ]
                                }
                                storage.storage
                          }

                  in  [ { name = "pair ${pair.pairName}"
                        , value =
                          [ nv "a" pair.a.storageName
                          , nv "b" pair.b.storageName
                          , nv
                              "collections"
                              ( merge
                                  { Unspecified = "none"
                                  , TheseCollections =
                                      \(colls : List Vdirsyncer.Collection) ->
                                        renderList
                                          ( Lib.List/map
                                              Vdirsyncer.Collection
                                              Text
                                              ( \ ( coll
                                                  : Vdirsyncer.Collection
                                                  ) ->
                                                  merge
                                                    { FromA = "from a"
                                                    , FromB = "from b"
                                                    , Collection =
                                                        \(t : Text) -> t
                                                    }
                                                    coll
                                              )
                                              colls
                                          )
                                  }
                                  pair.collections
                              )
                          ]
                        }
                      , mkStorage pair.a
                      , mkStorage pair.b
                      ]

      in  { example =
              Ini/externs.renderIni
                ( Ini.appendInis
                    ( Lib.List/map
                        Vdirsyncer.Pair
                        Ini.Ini
                        ( \(pair : Vdirsyncer.Pair) ->
                            { globalSection = [] : Ini.Section
                            , sections = toIniSections pair
                            }
                        )
                        (   [ { pairName = "testPair"
                              , a =
                                { storageName = "mystor"
                                , storage =
                                    Vdirsyncer.StorageType.FileSystem
                                      { path = "./test-ics"
                                      , fileext = < ICS >.ICS
                                      }
                                }
                              , b =
                                { storageName = "mystor"
                                , storage =
                                    Vdirsyncer.StorageType.Http
                                      { url = "https://profpatsch.de" }
                                }
                              , collections = Vdirsyncer.Collections.Unspecified
                              }
                            ]
                          : List Vdirsyncer.Pair
                        )
                    )
                )
          }
