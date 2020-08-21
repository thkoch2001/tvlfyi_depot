{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Network.IRC.Client
import           Control.Lens
import           NLP.POS
import           NLP.Types (POSTagger)
import qualified NLP.Types.Tags as Tags
import           NLP.Types.Tree
import qualified NLP.Corpora.Conll as Conll
import           NLP.Corpora.Conll (Tag)
import qualified Data.ByteString as BS
import           System.Random
import           System.Envy
import           Data.Maybe
import qualified Data.Text
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
    Config <$> env "OWO_CHANCE"
       <*> env "IRC_SERVER"
       <*> env "IRC_PORT"
       <*> envMaybe "IRC_SERVER_PASSWORD"
       <*> envMaybe "NICKSERV_PASSWORD"
       <*> envMaybe "IRC_NICK"

stopWord :: Text -> Bool
stopWord "'s"   = True
stopWord "\""   = True
stopWord "is"   = True
stopWord "are"  = True
stopWord "am"   = True
stopWord "were" = True
stopWord "was"  = True
stopWord "be"   = True
stopWord _      = False

pickVerb :: POS Tag -> Maybe Text
pickVerb (POS Conll.VB (Token verb)) = Just verb
pickVerb (POS Conll.VBD (Token verb)) = Just verb
pickVerb (POS Conll.VBG (Token verb)) = Just verb
pickVerb (POS Conll.VBN (Token verb)) = Just verb
pickVerb (POS Conll.VBZ (Token verb)) = Just verb
pickVerb _ = Nothing

pickNoun :: POS Tag -> Maybe Text
pickNoun (POS Conll.NN (Token noun)) = Just noun
pickNoun _ = Nothing

randomPOS
  :: Tags.Tag tag
  => (POS tag -> Maybe Text)
  -> POSTagger tag
  -> Text
  -> IO (Maybe Text)
randomPOS pickPOS tagger s = do
  let candidates
        = filter (not . stopWord)
        . mapMaybe pickPOS
        $ tag tagger s >>= \(TaggedSent ps) -> ps
  i <- randomRIO (0, length candidates - 1)
  pure $ candidates ^? ix i

doOwo :: MonadIO m => Config -> m Bool
doOwo conf = do
  n <- liftIO (randomRIO @Int (0, conf ^. owoChance))
  pure $ n == 0

data OwoType = Noun | Verb
  deriving stock (Show, Eq)

instance Random OwoType where
  random = over _1 (bool Noun Verb) . random
  randomR = const random

vowels :: [Char]
vowels = "aeiou"

article :: Text -> Text
article (x <| _) | x `elem` vowels = "an"
article _                     = "a"

owo :: OwoType -> Text -> Text
owo Noun n = mconcat
  [ "I'm "
  , article n
  , " "
  , n
  , if "o" `Data.Text.isSuffixOf` n
    then "wo"
    else " owo"
  ]
owo Verb v = v <> " me owo"

pickOwo :: OwoType -> POS Tag -> Maybe Text
pickOwo Verb = pickVerb
pickOwo Noun = pickNoun

randomOwo :: OwoType -> POSTagger Tag -> Text -> IO (Maybe Text)
randomOwo = randomPOS . pickOwo

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
      owoType <- liftIO randomIO
      mWord <- liftIO $ randomOwo owoType tagger m
      for_ mWord $ \word -> send $ Privmsg "##tvl" $ Right $ owo owoType word
    nickservAuthMsg = "IDENTIFY " <> nick <> " " <> fromJust (conf ^. nickservPassword)
    nickservAuth = send $ Privmsg "NickServ" $ Right nickservAuthMsg

main :: IO ()
main = do
  conf <- either fail pure =<< decodeEnv
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
