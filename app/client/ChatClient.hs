{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Lib (User)

import Control.Monad (void, forever)
import Control.Concurrent (threadDelay, forkIO)

import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl
import qualified Graphics.Vty as V

import Brick.BChan
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Widgets.Core
  ( (<+>)
  , (<=>)
  , hLimit
  , vLimit
  , str
  )
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import Brick.Util (on)

data Name = Edit1
          | Edit2
          deriving (Ord, Show, Eq)

data Screen = Login | Chat | MainMenu


data St =
    St { 
        _screen :: Screen
       , _focusRing :: F.FocusRing Name
       , _edit1 :: E.Editor String Name
       , _edit2 :: E.Editor String Name
       , _user :: Maybe User
       , _selectedChatUserId :: Maybe String
       }

data Counter = Counter

makeLenses ''St

drawUI :: St -> [T.Widget Name]
drawUI st = [ui]
    where

        e1 = F.withFocusRing (st^.focusRing) (E.renderEditor (str . unlines)) (st^.edit1)
        e2 = F.withFocusRing (st^.focusRing) (E.renderEditor (str . unlines)) (st^.edit2)

        -- ui = C.center $
        --     (str "Input 1 (unlimited): " <+> (hLimit 30 $ vLimit 5 e1)) <=>
        --     str " " <=>
        --     (str "Input 2 (limited to 2 lines): " <+> (hLimit 30 e2)) <=>
        --     str " " <=>
        --     str "Press Tab to switch between editors, Esc to quit."
        
        ui = case st^.screen of
            Login -> C.center $ C.vCenter $ C.hCenter $
                str "Login" <=>
                str "" <=>
                (str "Username: " <+> (hLimit 30 $ vLimit 1 e1))
            Chat -> C.center $ str "Chat"
            MainMenu -> C.center $ str "Main Menu" <=>
                str "" <=>
                str "1. Chat" <=>
                str "2. Logout"

appEvent :: T.BrickEvent Name e -> T.EventM Name St ()
appEvent (T.VtyEvent (V.EvKey V.KEsc [])) =
    M.halt
--- handle enter key 
appEvent (T.VtyEvent (V.EvKey V.KEnter [])) = do
    sc <- use screen 
    case sc of
        Login -> do
            username <- use edit1 >>= return . E.getEditContents >>= return . head
            if username == "admin" then
                screen .= MainMenu
            else
                screen .= Login
        _ -> return ()

appEvent (T.VtyEvent (V.EvKey (V.KChar '\t') [])) =
    focusRing %= F.focusNext
appEvent (T.VtyEvent (V.EvKey V.KBackTab [])) =
    focusRing %= F.focusPrev
appEvent ev = do
    r <- use focusRing
    case F.focusGetCurrent r of
      Just Edit1 -> zoom edit1 $ E.handleEditorEvent ev
      Just Edit2 -> zoom edit2 $ E.handleEditorEvent ev
      Nothing -> return ()

initialState :: St
initialState =
    St Login
       (F.focusRing [Edit1, Edit2])
       (E.editor Edit1 Nothing "")
       (E.editor Edit2 (Just 2) "")
       Nothing
       Nothing  

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (E.editAttr,                   V.white `on` V.blue)
    , (E.editFocusedAttr,            V.black `on` V.yellow)
    ]

appCursor :: St -> [T.CursorLocation Name] -> Maybe (T.CursorLocation Name)
appCursor = F.focusRingCursor (^.focusRing)

theApp :: M.App St e Name
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = appCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return ()
          , M.appAttrMap = const theMap
          }


main :: IO ()
main = do
    chan <- newBChan 10

    void $ forkIO $ forever $ do
        writeBChan chan Counter
        threadDelay 1000000

    finalState <- M.customMainWithDefaultVty (Just chan) theApp initialState
    putStrLn "Counter value:"