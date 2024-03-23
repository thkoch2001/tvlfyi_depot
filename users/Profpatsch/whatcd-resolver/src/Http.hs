{-# LANGUAGE QuasiQuotes #-}

module Http
  ( doRequestJson,
    RequestOptions (..),
    mkRequestOptions,
    setRequestMethod,
    setRequestBodyLBS,
    setRequestHeader,
    getResponseStatus,
    getResponseHeader,
    getResponseBody,
  )
where

import AppT
import Data.CaseInsensitive (CI (original))
import Data.Char qualified as Char
import Data.Int (Int64)
import Data.List qualified as List
import Data.Text qualified as Text
import Data.Text.Lazy qualified as Lazy.Text
import Data.Text.Punycode qualified as Punycode
import Json.Enc qualified as Enc
import MyPrelude
import Network.HTTP.Client
import Network.HTTP.Simple
import OpenTelemetry.Attributes qualified as Otel
import Optional
import Prelude hiding (span)

data RequestOptions = RequestOptions
  { method :: ByteString,
    host :: Text,
    port :: Optional Int,
    path :: Optional [Text],
    headers :: Optional [Header],
    usePlainHttp :: Optional Bool
  }

mkRequestOptions :: (HasField "method" r ByteString, HasField "host" r Text) => r -> RequestOptions
mkRequestOptions opts =
  RequestOptions
    { method = opts.method,
      port = defaults,
      host = opts.host,
      path = defaults,
      headers = defaults,
      usePlainHttp = defaults
    }

doRequestJson ::
  (MonadOtel m) =>
  RequestOptions ->
  Enc.Enc ->
  m (Response ByteString)
doRequestJson opts val = inSpan' "HTTP Request (JSON)" $ \span -> do
  let x = requestToXhCommandLine opts val
  let attrs = [100, 200 .. fromIntegral @Int @Int64 (x & Text.length)]
  for_ attrs $ \n -> do
    addAttribute span [fmt|request.xh.{n}|] (Lazy.Text.repeat 'x' & Lazy.Text.take n & toStrict & Otel.TextAttribute)
  addAttribute span "request.xh" (requestToXhCommandLine opts val)
  defaultRequest {secure = not (opts & optsUsePlainHttp)}
    & setRequestHost (opts & optsHost)
    & setRequestPort (opts & optsPort)
    -- TODO: is this automatically escaped by the library?
    & setRequestPath (opts & optsPath)
    & setRequestHeaders (opts & optsHeaders)
    & setRequestMethod opts.method
    & setRequestBodyLBS (Enc.encToBytesUtf8Lazy val)
    & httpBS

optsHost :: RequestOptions -> ByteString
optsHost opts =
  if opts.host & Text.isAscii
    then opts.host & textToBytesUtf8
    else opts.host & Punycode.encode

optsUsePlainHttp :: RequestOptions -> Bool
optsUsePlainHttp opts = opts.usePlainHttp.withDefault False

optsPort :: RequestOptions -> Int
optsPort opts = opts.port.withDefault (if opts & optsUsePlainHttp then 80 else 443)

optsPath :: RequestOptions -> ByteString
optsPath opts = opts.path.withDefault [] & Text.intercalate "/" & ("/" <>) & textToBytesUtf8

optsHeaders :: RequestOptions -> [Header]
optsHeaders opts = opts.headers.withDefault []

-- | Create a string that can be pasted on the command line to invoke the same HTTP request via the `xh` tool (curl but nicer syntax)
requestToXhCommandLine :: RequestOptions -> Enc.Enc -> Text
requestToXhCommandLine opts val = do
  let protocol = if opts & optsUsePlainHttp then "http" :: Text else "https"
  let url = [fmt|{protocol}://{opts & optsHost}:{opts & optsPort}{opts & optsPath}|]
  let headers = opts & optsHeaders <&> \(hdr, v) -> hdr.original <> ":" <> v

  prettyArgsForBash $
    mconcat
      [ ["xh", url],
        headers <&> bytesToTextUtf8Lenient,
        ["--raw"],
        [val & Enc.encToBytesUtf8 & bytesToTextUtf8Lenient]
      ]

-- | Pretty print a command line in a way that can be copied to bash.
prettyArgsForBash :: [Text] -> Text
prettyArgsForBash = Text.intercalate " " . map simpleBashEscape

-- | Simple escaping for bash words. If they contain anything that’s not ascii chars
-- and a bunch of often-used special characters, put the word in single quotes.
simpleBashEscape :: Text -> Text
simpleBashEscape t = do
  case Text.find (not . isSimple) t of
    Just _ -> escapeSingleQuote t
    Nothing -> t
  where
    -- any word that is just ascii characters is simple (no spaces or control characters)
    -- or contains a few often-used characters like - or .
    isSimple c =
      Char.isAsciiLower c
        || Char.isAsciiUpper c
        || Char.isDigit c
        -- These are benign, bash will not interpret them as special characters.
        || List.elem c ['-', '.', ':', '/']
    -- Put the word in single quotes
    -- If there is a single quote in the word,
    -- close the single quoted word, add a single quote, open the word again
    escapeSingleQuote t' = "'" <> Text.replace "'" "'\\''" t' <> "'"
