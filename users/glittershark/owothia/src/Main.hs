{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Network.IRC.Client
import           Control.Lens
import           NLP.POS
import           NLP.Types (POSTagger)
import           NLP.Types.Tree
import qualified NLP.Types.Tree
import qualified NLP.Corpora.Conll as Conll
import           NLP.Corpora.Conll (Tag)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import           System.Random
import           System.Envy
--------------------------------------------------------------------------------

data Config = Config
  { _nickservPassword :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromEnv)
makeLenses ''Config

verbs :: POSTagger Tag -> Text -> [Text]
verbs tagger s = mapMaybe pickVerb $ tag tagger s >>= \(TaggedSent ps) -> ps
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

owoChance = 1000

doOwo :: MonadIO m => m Bool
doOwo = (== 0) <$> liftIO (randomRIO @Int (0, 1000))

owothiaHandler :: Config -> IORef Bool -> POSTagger Tag -> EventHandler s
owothiaHandler conf state tagger = EventHandler Just $ \src ev -> do
  hasIdentified <- readIORef state
  when (not hasIdentified) $ do
    nickservAuth
    send $ Join "##tvl"
    writeIORef state True

  when ("You are now identified" `BS.isInfixOf` (ev ^. raw)) $
    send $ Join "##tvl"

  case (src, ev ^. message) of
    (Channel "##tvl" nick, Privmsg _ (Right m)) -> do
      willOwo <- doOwo
      when willOwo $ do
        mVerb <- liftIO $ randomVerb tagger m
        for_ mVerb $ \verb -> send $ Privmsg "##tvl" $ Right $ owo verb
    _ -> pure ()

  pure ()

  where
    nickservAuthMsg = "IDENTIFY " <> myNick <> " " <> conf ^. nickservPassword
    nickservAuth = send $ Privmsg "NickServ" $ Right nickservAuthMsg

myNick :: Text
myNick = "owothia"

run :: ByteString -> Int -> IO ()
run host port = do
  Right conf <- decodeEnv
  tagger <- defaultTagger
  state <- newIORef False
  let conn =
        plainConnection host port
          & logfunc .~ stdoutLogger
      cfg =
        defaultInstanceConfig myNick
          & channels .~ ["##tvl"]
          & handlers %~ (owothiaHandler conf state tagger : )
  runClient conn cfg ()

main :: IO ()
main = do
  run "irc.freenode.net" 6667
