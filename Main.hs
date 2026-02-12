{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Monomer
import Optics
import Relude
import TextShow

data Module = CmdModule
  { frequency :: Int
  , command :: Text
  }
  deriving stock (Eq, Show)

data AppModel = AppModel
  { clickCount :: Int
  , modules :: [Module]
  }
  deriving stock (Eq, Show, Generic)

data AppEvent
  = AppInit
  | AppIncrease
  deriving stock (Eq, Show)

buildUI :: WidgetEnv AppModel AppEvent -> AppModel -> WidgetNode AppModel AppEvent
buildUI _wenv model = widgetTree
  where
    widgetTree =
      hstack
        [ label $ "Click count: " <> showt (model ^. #clickCount)
        , spacer
        , button "Increase count" AppIncrease
        ]
        `styleBasic` [padding 2]

handleEvent ::
  WidgetEnv AppModel AppEvent ->
  WidgetNode AppModel AppEvent ->
  AppModel ->
  AppEvent ->
  [AppEventResponse AppModel AppEvent]
handleEvent _wenv _node model evt = case evt of
  AppInit -> []
  AppIncrease -> [Model (model & #clickCount %~ (+ 1))]

main :: IO ()
main = do
  startApp model handleEvent buildUI config
  where
    config =
      [ appWindowTitle "Hello world"
      , appTheme darkTheme
      , appFontDef "Regular" "./assets/font/OpenSans-Regular.ttf"
      , appInitEvent AppInit
      , appWindowResizable False
      , appWindowState $ MainWindowNormal (1920, 30) -- Bar size
      ]
    model = AppModel 0
