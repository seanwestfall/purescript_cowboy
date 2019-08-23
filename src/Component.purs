module Component where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Int (toNumber)

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console (log, logShow)

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Elements as HES

import Halogen.Query.EventSource as ES
import Halogen.HTML.Properties as HP

import Web.Event.Event as E
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document) as Web
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

data Action
  = Init
  | HandleKey H.SubscriptionId KeyboardEvent
  | ConsoleLog

type State = { on :: Boolean
             , chars :: String
             , buttonClassName :: String
             , spritePosition :: Int
             , spriteStyle :: String
             , innerStyle :: String
             , spriteHero :: String
             , spriteReverse :: Boolean
             , spriteIndex :: Int
             }

initialState :: State
initialState =
  { on: false
  , chars: ""
  , buttonClassName: "button-not-changed"
  , spritePosition: 0
  , spriteStyle: "position: absolute; left: 0;"
  , innerStyle: "display: block; position: relative; margin-left: 20px; margin-right: 20px;"
  , spriteHero: "sprite-hero-0"
  , spriteReverse: false
  , spriteIndex: 0
  }

component :: forall f i o. H.Component HH.HTML f i o Aff
component =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval (H.defaultEval { handleAction = handleAction, initialize = Just Init })
    }
  where

  render :: forall m. State -> H.ComponentHTML Action () m
  render state =
    HH.div_
      [ HH.h1_
          [ HH.text "Hello Cowboy" ]
      , HH.div
          [ HP.class_ (H.ClassName "inner")
          , HP.attr (HH.AttrName "style") state.innerStyle
          ]
          [ HH.div
              [ HP.class_ (H.ClassName state.spriteHero)
              , HP.attr (HH.AttrName "style") state.spriteStyle
              ]
          []
          ]
      ]

handleAction :: forall o. Action -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  Init -> do
    document <- H.liftEffect $ Web.document =<< Web.window
    H.subscribe' \sid ->
      ES.eventListenerEventSource
        KET.keyup
        (HTMLDocument.toEventTarget document)
        (map (HandleKey sid) <<< KE.fromEvent)
  HandleKey sid ev
    | KE.shiftKey ev -> do
        H.liftEffect $ E.preventDefault (KE.toEvent ev)
        _ <- H.modify (\state -> { on: not state.on
                                   , chars: state.chars
                                   , buttonClassName: "button-changed"
                                   , spriteStyle: state.spriteStyle
                                   , spritePosition: state.spritePosition
                                   , innerStyle: state.innerStyle
                                   , spriteIndex: state.spriteIndex
                                   , spriteReverse: state.spriteReverse
                                   , spriteHero: "sprite-hero-gun" <> if state.spriteReverse then "r" else ""
                                 })
        H.liftEffect $ log "shiftKey pressed!"
    | KE.code ev == "ArrowLeft" -> do
        _ <- H.modify (\state -> { on: not state.on
                                   , chars: state.chars
                                   , buttonClassName: "button-changed"
                                   , spriteStyle: "position: absolute; left: " <> (show state.spritePosition) <> "px;"
                                   , spritePosition: state.spritePosition - 10
                                   , innerStyle: state.innerStyle
                                   , spriteIndex: if state.spriteIndex /= 3 then state.spriteIndex + 1 else 0
                                   , spriteReverse: true
                                   , spriteHero : "sprite-hero-" <> (show state.spriteIndex) <> "r"
                                 })
        H.liftEffect $ log "Arrow Left Button Clicked!"
    | KE.code ev == "ArrowRight" -> do
        _ <- H.modify (\state -> { on: not state.on
                                   , chars: state.chars
                                   , buttonClassName: "button-changed"
                                   , spriteStyle: "position: absolute; left: " <> (show state.spritePosition) <> "px;"
                                   , spritePosition: state.spritePosition + 10
                                   , innerStyle: state.innerStyle
                                   , spriteIndex: if state.spriteIndex /= 3 then state.spriteIndex + 1 else 0
                                   , spriteReverse: false
                                   , spriteHero : "sprite-hero-" <> (show state.spriteIndex)
                                 })
        H.liftEffect $ log "Arrow Right Button Clicked!"
    | otherwise ->
        pure unit
  ConsoleLog -> do
    _ <- H.modify (\state -> { on: not state.on
                               , chars: state.chars
                               , buttonClassName: "button-changed"
                               , spriteStyle: "position: absolute; left: 0;"
                               , spritePosition: 0
                               , innerStyle: state.innerStyle
                               , spriteIndex: state.spriteIndex
                               , spriteReverse: state.spriteReverse
                               , spriteHero : state.spriteHero
                             })
    H.liftEffect $ log "Button Clicked!"

