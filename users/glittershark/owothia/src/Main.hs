{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Network.IRC.Client
import           Control.Lens
import           NLP.POS
import           NLP.Types (POSTagger)
import           NLP.Types.Tree
import qualified NLP.Corpora.Conll as Conll
import           NLP.Corpora.Conll (Tag)
import qualified Data.ByteString as BS
import           System.Random
import           System.Envy
import           Data.Maybe
--------------------------------------------------------------------------------

data Config = Config
  { _owoChance :: Int
  , _ircServer :: ByteString
  , _ircPort :: Int
  , _ircServerPassword :: Maybe Text
  , _nickservPassword :: Maybe Text
  , _ircNick :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
makeLenses ''Config

instance FromEnv Config where
  fromEnv _ =
    Config <$> envMaybe "OWO_CHANCE" .!= (error "define OWO_CHANCE")
       <*> envMaybe "IRC_SERVER" .!= (error "define IRC_SERVER")
       <*> envMaybe "IRC_PORT" .!= (error "define IRC_PORT")
       <*> envMaybe "IRC_SERVER_PASSWORD" .!= Nothing
       <*> envMaybe "NICKSERV_PASSWORD" .!= Nothing
       <*> envMaybe "IRC_NICK" .!= Nothing

stopWord :: Text -> Bool
stopWord "'s"   = True
stopWord "is"   = True
stopWord "are"  = True
stopWord "am"   = True
stopWord "were" = True
stopWord "was"  = True
stopWord "be"   = True
stopWord _      = False

verbs :: POSTagger Tag -> Text -> [Text]
verbs tagger s
  = filter (not . stopWord)
  . mapMaybe pickVerb
  $ tag tagger s >>= \(TaggedSent ps) -> ps
  where
    pickVerb (POS Conll.VB (Token verb)) = Just verb
    pickVerb (POS Conll.VBD (Token verb)) = Just verb
    pickVerb (POS Conll.VBG (Token verb)) = Just verb
    pickVerb (POS Conll.VBN (Token verb)) = Just verb
    pickVerb (POS Conll.VBZ (Token verb)) = Just verb
    pickVerb _ = Nothing

randomVerb :: POSTagger Tag -> Text -> IO (Maybe Text)
randomVerb tagger txt = do
  let vs = verbs tagger txt
  i <- randomRIO (0, length vs - 1)
  pure $ vs ^? ix i

owo :: Text -> Text
owo = (<> " me owo")

doOwo :: MonadIO m => Config -> m Bool
doOwo conf = do
  n <- liftIO (randomRIO @Int (0, conf ^. owoChance))
  liftIO $ putStrLn $ "rolled " <> show n
  pure $ n == 0

owothiaHandler :: Config -> Text -> IORef Bool -> POSTagger Tag -> EventHandler s
owothiaHandler conf nick state tagger = EventHandler Just $ \src ev -> do
  hasIdentified <- readIORef state
  when (not hasIdentified) $ do
    nickservAuth
    send $ Join "##tvl"
    writeIORef state True

  when ("You are now identified" `BS.isInfixOf` (ev ^. raw)) $
    send $ Join "##tvl"

  case (src, ev ^. message) of
    (Channel "##tvl" nick, Privmsg _ (Right m)) -> do
      willOwo <- doOwo conf
      when willOwo $ owoMessage m
    _ -> pure ()

  pure ()

  where
    owoMessage m = do
      mVerb <- liftIO $ randomVerb tagger m
      for_ mVerb $ \verb -> send $ Privmsg "##tvl" $ Right $ owo verb
    nickservAuthMsg = "IDENTIFY " <> nick <> " " <> fromJust (conf ^. nickservPassword)
    nickservAuth = send $ Privmsg "NickServ" $ Right nickservAuthMsg

main :: IO ()
main = do
  Right conf <- decodeEnv
  tagger <- defaultTagger
  state <- newIORef $ not . isJust $ (conf ^. nickservPassword)
  let nick = fromMaybe "owothia" (conf ^. ircNick) 
      conn =
        plainConnection (conf ^. ircServer) (conf ^. ircPort)
          & realname .~ "Owothia Revströwö"
          & password .~ (conf ^. ircServerPassword)
          & logfunc .~ stdoutLogger
      cfg =
        defaultInstanceConfig nick
          & channels .~ ["##tvl"]
          & handlers %~ (owothiaHandler conf nick state tagger : )
  runClient conn cfg ()
