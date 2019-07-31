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

type State = { on :: Boolean
             , chars :: String
             , buttonClassName :: String
             , spritePosition :: Int
             , spriteStyle :: String
             }

data Action
  = Init
  | HandleKey H.SubscriptionId KeyboardEvent
  | ConsoleLog

initialState :: State
initialState =
  { on: false
  , chars: ""
  , buttonClassName: "button-not-changed"
  , spritePosition: 0
  , spriteStyle: "position: absolute; right: 0;"
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
          [ HP.class_ (H.ClassName "sprite-hero")
          , HP.attr (HH.AttrName "style") state.spriteStyle
          ]
          []
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
        let char = KE.key ev
        when (String.length char == 1) do
          H.modify_ (\st -> st { chars = st.chars <> char })
    | KE.key ev == "Enter" -> do
        H.liftEffect $ E.preventDefault (KE.toEvent ev)
        H.modify_ (_ { chars = "" })
        H.unsubscribe sid
    | KE.code ev == "ArrowUp" -> do
        H.liftEffect $ log "Arrow Up Button Clicked!"
    | KE.code ev == "ArrowDown" -> do
        H.liftEffect $ log "Arrow Down Button Clicked!"
    | KE.code ev == "ArrowLeft" -> do
        _ <- H.modify (\state -> { on: not state.on, chars: state.chars, buttonClassName: "button-changed", spriteStyle: "position: absolute; right: " <> (show state.spritePosition) <> "px;", spritePosition: state.spritePosition + 10})
        H.liftEffect $ log "Arrow Left Button Clicked!"
    | KE.code ev == "ArrowRight" -> do
        _ <- H.modify (\state -> { on: not state.on, chars: state.chars, buttonClassName: "button-changed", spriteStyle: "position: absolute; right: " <> (show state.spritePosition) <> "px;", spritePosition: state.spritePosition - 10})
        H.liftEffect $ log "Arrow Right Button Clicked!"
    | otherwise ->
        pure unit
  ConsoleLog -> do
    _ <- H.modify (\state -> { on: not state.on, chars: state.chars, buttonClassName: "button-changed", spriteStyle: "position: absolute; right 0;", spritePosition: 0})
    H.liftEffect $ log "Button Clicked!"

