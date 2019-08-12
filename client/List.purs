module List (component) where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import StaticBooks

type State = { enabled :: Boolean }

data Action = Toggle

component :: forall q i o m. H.Component HH.HTML q i o m
component = 
  H.mkComponent { initialState
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }

initialState :: forall i. i -> State
initialState _ = { enabled : false }


render :: forall m. State -> H.ComponentHTML Action () m
render state =
   HH.ul
   []
   (map (\x -> HH.li [] [HH.text (x.title <> ": " <> x.author)]) books)

handleAction :: forall o m. Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Toggle ->
    H.modify_ \st -> st { enabled = not st.enabled }
