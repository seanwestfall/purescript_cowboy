module Component where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Console (log)

import Halogen as H
import Halogen.HTML as HH
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


-- sprite Position marks the positions of the sprite in aboslute pxs.
-- sprite style keeps the style attribute properties.
-- innerStyle keeps the style properties of the div that holds the sprite into place.
-- spriteHero is the class name of the current sprite frame
-- spriteReverse keeps track of the direction of the sprite, true means right facing, and false means left facing.
-- spriteIndex keeps track of the current frame
type State = { spritePosition :: Int
             , spriteStyle :: String
             , innerStyle :: String
             , spriteHero :: String
             , spriteReverse :: Boolean
             , spriteIndex :: Int
             }

initialState :: State
initialState =
  { spritePosition: 0
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
      , HH.div
          [ HP.class_ (H.ClassName "instructions")
            , HP.attr (HH.AttrName "style") "display: block; position: fixed; margin-top: 200px;"
          ]
          [ HH.p_
              [ HH.text ("Tap the Left or Right Arrow keys to make the cowboy walk.")]
            , HH.p_
              [ HH.text ("Hit the Shfit key plux any other key to make the cowboy hold up his gun.") ]
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
    -- The event handler when hitting shift, or when the sprite is holding up his gun
    | KE.shiftKey ev -> do
        H.liftEffect $ E.preventDefault (KE.toEvent ev)
        _ <- H.modify \state -> state 
                                { spriteHero = "sprite-hero-gun" <> if state.spriteReverse then "r" else ""
                                }
        H.liftEffect $ log "shiftKey pressed!"
    -- The event handler for when tapping the left arrow key
    | KE.code ev == "ArrowLeft" -> do
        _ <- H.modify \state -> state 
                                { spriteStyle = "position: absolute; left: " <> (show state.spritePosition) <> "px;"
                                  , spritePosition = state.spritePosition - 10
                                  , spriteIndex = if state.spriteIndex /= 3 then state.spriteIndex + 1 else 0
                                  , spriteReverse = true
                                  , spriteHero = "sprite-hero-" <> (show state.spriteIndex) <> "r"
                                }
        H.liftEffect $ log "Arrow Left Button Clicked!"
    -- The event handler for when tapping the right arrow key
    | KE.code ev == "ArrowRight" -> do
        _ <- H.modify \state -> state 
                                { spriteStyle = "position: absolute; left: " <> (show state.spritePosition) <> "px;"
                                  , spritePosition = state.spritePosition + 10
                                  , spriteIndex = if state.spriteIndex /= 3 then state.spriteIndex + 1 else 0
                                  , spriteReverse = false
                                  , spriteHero = "sprite-hero-" <> (show state.spriteIndex)
                                }
        H.liftEffect $ log "Arrow Right Button Clicked!"
    | otherwise ->
        pure unit


