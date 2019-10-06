{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
--------------------------------------------------------------------------------
module Xanthous.Game.Prompt
  ( PromptType(..)
  , SPromptType(..)
  , SingPromptType(..)
  , PromptCancellable(..)
  , PromptResult(..)
  , PromptState(..)
  , MenuOption(..)
  , mkMenuItems
  , PromptInput
  , Prompt(..)
  , mkPrompt
  , mkMenu
  , isCancellable
  , submitPrompt
  ) where
--------------------------------------------------------------------------------
import Xanthous.Prelude
--------------------------------------------------------------------------------
import           Brick.Widgets.Edit (Editor, editorText, getEditContents)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.Generic
--------------------------------------------------------------------------------
import           Xanthous.Util (smallestNotIn)
import           Xanthous.Data (Direction, Position)
import           Xanthous.Resource (Name)
import qualified Xanthous.Resource as Resource
--------------------------------------------------------------------------------

data PromptType where
  StringPrompt    :: PromptType
  Confirm         :: PromptType
  Menu            :: Type -> PromptType
  DirectionPrompt :: PromptType
  PointOnMap      :: PromptType
  Continue        :: PromptType
  deriving stock (Generic)

instance Show PromptType where
  show StringPrompt = "StringPrompt"
  show Confirm = "Confirm"
  show (Menu _) = "Menu"
  show DirectionPrompt = "DirectionPrompt"
  show PointOnMap = "PointOnMap"
  show Continue = "Continue"

data SPromptType :: PromptType -> Type where
  SStringPrompt    ::      SPromptType 'StringPrompt
  SConfirm         ::      SPromptType 'Confirm
  SMenu            :: forall a. SPromptType ('Menu a)
  SDirectionPrompt ::      SPromptType 'DirectionPrompt
  SPointOnMap      ::      SPromptType 'PointOnMap
  SContinue        ::      SPromptType 'Continue

class SingPromptType pt where singPromptType :: SPromptType pt
instance SingPromptType 'StringPrompt where singPromptType = SStringPrompt
instance SingPromptType 'DirectionPrompt where singPromptType = SDirectionPrompt
instance SingPromptType 'Continue where singPromptType = SContinue

instance Show (SPromptType pt) where
  show SStringPrompt    = "SStringPrompt"
  show SConfirm         = "SConfirm"
  show SMenu            = "SMenu"
  show SDirectionPrompt = "SDirectionPrompt"
  show SPointOnMap      = "SPointOnMap"
  show SContinue        = "SContinue"

data PromptCancellable
  = Cancellable
  | Uncancellable
  deriving stock (Show, Eq, Ord, Enum, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)

instance Arbitrary PromptCancellable where
  arbitrary = genericArbitrary

data PromptResult (pt :: PromptType) where
  StringResult     :: Text      -> PromptResult 'StringPrompt
  ConfirmResult    :: Bool      -> PromptResult 'Confirm
  MenuResult       :: forall a. a    -> PromptResult ('Menu a)
  DirectionResult  :: Direction -> PromptResult 'DirectionPrompt
  PointOnMapResult :: Position  -> PromptResult 'PointOnMap
  ContinueResult   ::             PromptResult 'Continue

data PromptState pt where
  StringPromptState    :: Editor Text Name -> PromptState 'StringPrompt
  DirectionPromptState ::                    PromptState 'DirectionPrompt
  ContinuePromptState  ::                    PromptState 'Continue
  MenuPromptState      :: forall a.               PromptState ('Menu a)

deriving stock instance Show (PromptState pt)

data MenuOption a = MenuOption Text a

mkMenuItems :: (MonoFoldable f, Element f ~ (Char, MenuOption a))
            => f
            -> Map Char (MenuOption a)
mkMenuItems = flip foldl' mempty $ \items (chr, option) ->
  let chr' = if has (ix chr) items
             then smallestNotIn $ keys items
             else chr
  in items & at chr' ?~ option

instance Show (MenuOption a) where
  show (MenuOption m _) = show m

type family PromptInput (pt :: PromptType) :: Type where
  PromptInput ('Menu a) = Map Char (MenuOption a)
  PromptInput _ = ()

data Prompt (m :: Type -> Type) where
  Prompt
    :: forall (pt :: PromptType)
        (m :: Type -> Type).
      PromptCancellable
    -> SPromptType pt
    -> PromptState pt
    -> PromptInput pt
    -> (PromptResult pt -> m ())
    -> Prompt m

instance Show (Prompt m) where
  show (Prompt c pt ps pri _)
    = "(Prompt "
    <> show c <> " "
    <> show pt <> " "
    <> show ps <> " "
    <> showPri
    <> " <function>)"
    where showPri = case pt of
            SMenu -> show pri
            _ -> "()"

mkPrompt :: (PromptInput pt ~ ()) => PromptCancellable -> SPromptType pt -> (PromptResult pt -> m ()) -> Prompt m
mkPrompt c pt@SStringPrompt cb =
  let ps = StringPromptState $ editorText Resource.Prompt (Just 1) ""
  in Prompt c pt ps () cb
mkPrompt c pt@SDirectionPrompt cb = Prompt c pt DirectionPromptState () cb
mkPrompt c pt@SContinue cb = Prompt c pt ContinuePromptState () cb
mkPrompt _ _ _ = undefined

mkMenu
  :: forall a m.
    PromptCancellable
  -> Map Char (MenuOption a) -- ^ Menu items
  -> (PromptResult ('Menu a) -> m ())
  -> Prompt m
mkMenu c = Prompt c SMenu MenuPromptState

isCancellable :: Prompt m -> Bool
isCancellable (Prompt Cancellable _ _ _ _)   = True
isCancellable (Prompt Uncancellable _ _ _ _) = False

submitPrompt :: Applicative m => Prompt m -> m ()
submitPrompt (Prompt _ pt ps _ cb) =
  case (pt, ps) of
    (SStringPrompt, StringPromptState edit) ->
      cb . StringResult . mconcat . getEditContents $ edit
    (SDirectionPrompt, DirectionPromptState) ->
      pure () -- Don't use submit with a direction prompt
    (SContinue, ContinuePromptState) ->
      cb ContinueResult
    (SMenu, MenuPromptState) ->
      pure () -- Don't use submit with a menu prompt
    _ -> undefined

-- data PromptInput :: PromptType -> Type where
--   StringInput :: PromptInput 'StringPrompt
