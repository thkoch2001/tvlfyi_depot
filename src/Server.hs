-- Server implementation based on Happstack

module Server where

import           Control.Applicative          (optional, pure, (<$>), (<*>))
import           Control.Monad                (liftM, msum, mzero, unless, when)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Reader         (ask)
import           Data.Acid
import           Data.Acid.Advanced
import           Data.ByteString.Char8        (ByteString, pack, unpack)
import           Data.Char                    (toLower)
import           Data.Maybe                   (fromJust)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Time
import           Happstack.Server             hiding (Session)
import           Happstack.Server.Compression

import Blog
import BlogDB  hiding (updateEntry)
import Locales
import RSS


instance FromReqURI BlogLang where
  fromReqURI sub =
    case map toLower sub of
      "de" -> Just DE
      "en" -> Just EN
      _    -> Nothing

tmpPolicy :: BodyPolicy
tmpPolicy = defaultBodyPolicy "/tmp" 0 200000 1000

runBlog :: AcidState Blog -> Int -> String -> IO ()
runBlog acid port respath =
  simpleHTTP nullConf {port = port} $ tazBlog acid respath

tazBlog :: AcidState Blog -> String -> ServerPart Response
tazBlog acid resDir = do
    compr <- compressedResponseFilter
    msum [ path $ \(lang :: BlogLang) -> blogHandler acid lang
         , nullDir >> showIndex acid EN
         , dir " " $ nullDir >>
            seeOther ("https://plus.google.com/115916629925754851590" :: Text) (toResponse ())
         , path $ \(year :: Int) -> path $ \(month :: Int) -> path $ \(id_ :: String) -> formatOldLink year month id_
         , dir "res" $ serveDirectory DisableBrowsing [] "../res"
         , dir "notice" $ ok $ toResponse showSiteNotice
         {- :Admin handlers -}
         , do dirs "admin/postentry" nullDir
              guardSession acid
              postEntry acid
         , do dirs "admin/entrylist" $ dir (show DE) nullDir
              guardSession acid
              entryList acid DE
         , do dirs "admin/entrylist" $ dir (show EN) nullDir
              guardSession acid
              entryList acid EN
         , do guardSession acid
              dirs "admin/edit" $ path $ \(eId :: Integer) -> editEntry acid eId
         , do guardSession acid
              dirs "admin/updateentry" $ nullDir >> updateEntry acid
         , do dir "admin" nullDir
              guardSession acid
              ok $ toResponse $ adminIndex ("tazjin" :: Text)
         , dir "admin" $ ok $ toResponse adminLogin
         , dir "dologin" $ processLogin acid
         , do dirs "static/blogv40.css" nullDir
              setHeaderM "content-type" "text/css"
              setHeaderM "cache-control" "max-age=630720000"
              setHeaderM "expires" "Tue, 20 Jan 2037 04:20:42 GMT"
              ok $ toResponse blogStyle
         , do setHeaderM "cache-control" "max-age=630720000"
              setHeaderM "expires" "Tue, 20 Jan 2037 04:20:42 GMT"
              dir "static" $ serveDirectory DisableBrowsing [] resDir
         , serveDirectory DisableBrowsing [] resDir
         , notFound $ toResponse $ showError NotFound DE
         ]

blogHandler :: AcidState Blog -> BlogLang -> ServerPart Response
blogHandler acid lang =
    msum [ path $ \(eId :: Integer) -> showEntry acid lang $ EntryId eId
         , nullDir >> showIndex acid lang
         , dir "rss" $ nullDir >> showRSS acid lang
         , dir "rss.xml" $ nullDir >> showRSS acid lang
         , notFound $ toResponse $ showError NotFound lang
         ]

formatOldLink :: Int -> Int -> String -> ServerPart Response
formatOldLink y m id_ =
  flip seeOther (toResponse ()) $
    concat $ intersperse' "/"  ["de", show y, show m, replace '.' '/' id_]

showEntry :: AcidState Blog -> BlogLang -> EntryId -> ServerPart Response
showEntry acid lang eId = do
    entry <- query' acid (GetEntry eId)
    tryEntry entry lang

tryEntry :: Maybe Entry -> BlogLang -> ServerPart Response
tryEntry Nothing lang = notFound $ toResponse $ showError NotFound lang
tryEntry (Just entry) _ = ok $ toResponse $ blogTemplate eLang eTitle $ renderEntry entry
    where
        eTitle = T.append ": " (title entry)
        eLang = lang entry

showIndex :: AcidState Blog -> BlogLang -> ServerPart Response
showIndex acid lang = do
    entries <- query' acid (LatestEntries lang)
    (page :: Maybe Int) <- optional $ lookRead "page"
    ok $ toResponse $ blogTemplate lang "" $
        renderEntries False (eDrop page entries) (topText lang) (Just $ showLinks page lang)
  where
    eDrop :: Maybe Int -> [a] -> [a]
    eDrop (Just i) = drop ((i-1) * 6)
    eDrop Nothing = drop 0

showRSS :: AcidState Blog -> BlogLang -> ServerPart Response
showRSS acid lang = do
    entries <- query' acid (LatestEntries lang)
    feed <- liftIO $ renderFeed lang $ take 6 entries
    setHeaderM "content-type" "text/xml"
    ok $ toResponse feed

{- ADMIN stuff -}

postEntry :: AcidState Blog -> ServerPart Response
postEntry acid = do
    decodeBody tmpPolicy
    now <- liftIO getCurrentTime
    let eId = timeToId now
    lang <- look "lang"
    nBtext <- lookText' "btext"
    nMtext <- lookText' "mtext"
    nEntry <- Entry <$> pure eId
                    <*> getLang lang
                    <*> readCookieValue "sUser"
                    <*> lookText' "title"
                    <*> pure nBtext
                    <*> pure nMtext
                    <*> pure now
    update' acid (InsertEntry nEntry)
    seeOther ("/" ++ lang ++ "/" ++ show eId) (toResponse())
  where
    timeToId :: UTCTime -> EntryId
    timeToId t = EntryId . read $ formatTime defaultTimeLocale "%s" t
    getLang :: String -> ServerPart BlogLang
    getLang "de" = return DE
    getLang "en" = return EN

entryList :: AcidState Blog -> BlogLang -> ServerPart Response
entryList acid lang = do
    entries <- query' acid (LatestEntries lang)
    ok $ toResponse $ adminEntryList entries

editEntry :: AcidState Blog -> Integer -> ServerPart Response
editEntry acid i = do
    (Just entry) <- query' acid (GetEntry eId)
    ok $ toResponse $ editPage entry
  where
    eId = EntryId i

updateEntry :: AcidState Blog -> ServerPart Response -- TODO: Clean this up
updateEntry acid = do
    decodeBody tmpPolicy
    (eId :: Integer) <- lookRead "eid"
    (Just entry) <- query' acid (GetEntry $ EntryId eId)
    nTitle <- lookText' "title"
    nBtext <- lookText' "btext"
    nMtext <- lookText' "mtext"
    let nEntry = entry { title = nTitle
                        , btext = nBtext
                        , mtext = nMtext}
    update' acid (UpdateEntry nEntry)
    seeOther (concat $ intersperse' "/" [show $ lang entry, show eId])
             (toResponse ())

guardSession :: AcidState Blog -> ServerPartT IO ()
guardSession acid = do
    (sId :: Text) <- readCookieValue "session"
    (uName :: Text) <- readCookieValue "sUser"
    now <- liftIO getCurrentTime
    mS <- query' acid (GetSession $ SessionID sId)
    case mS of
      Nothing -> mzero
      (Just Session{..}) -> unless ((uName == username user) && sessionTimeDiff now sdate)
                                   mzero
  where
    sessionTimeDiff :: UTCTime -> UTCTime -> Bool
    sessionTimeDiff now sdate = diffUTCTime now sdate < 43200


processLogin :: AcidState Blog -> ServerPart Response
processLogin acid = do
    decodeBody tmpPolicy
    account <- lookText' "account"
    password <- look "password"
    login <- query' acid (CheckUser (Username account) password)
    if login
      then createSession account
      else ok $ toResponse adminLogin
  where
    createSession account = do
      now <- liftIO getCurrentTime
      let sId = hashString $ show now
      addCookie (MaxAge 43200) (mkCookie "session" $ unpack sId)
      addCookie (MaxAge 43200) (mkCookie "sUser" $ T.unpack account)
      (Just user) <- query' acid (GetUser $ Username account)
      let nSession = Session (T.pack $ unpack sId) user now
      update' acid (AddSession nSession)
      seeOther ("/admin?do=login" :: Text) (toResponse())
