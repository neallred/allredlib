module Main where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Control.Monad.Except
import Data.Traversable
import Data.Either (hush, Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromMaybe)
-- import Data.Newtype (un)
import Effect (Effect)
import Effect.Console (log)
import Effect.Aff.Class (class MonadAff)
import Foreign
import Foreign.Generic
import Foreign.Class
import Foreign.JSON
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Web.Event.Event (Event)
import Web.Event.Event as Event

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI component unit body

newtype Title = Title
  { seriesId :: Maybe Int
  , seriesPart :: Maybe String
  , subseriesId :: Maybe Int
  , subseriesPart :: Maybe String
  , synopsis :: Maybe String
  , title :: String
  , year :: Maybe Int
  }

derive instance genericTitle :: Generic Title _
instance decodeTitle :: Decode Title where
  decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })
instance encodeTitle :: Encode Title where
  encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })
instance showTitle :: Show Title where
  show = genericShow


type State =
  { loading :: Boolean
  , username :: String
  , result :: Maybe String
  , titles :: Array Title
  }

data Action
  = SetUsername String
  | MakeRequest Event

component :: forall q i o m. MonadAff m => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ = { loading: false, username: "", result: Nothing, titles: [] }

render :: forall m. State -> H.ComponentHTML Action () m
render st =
  HH.form
    [ HE.onSubmit \ev -> Just (MakeRequest ev) ]
    [ HH.h1_ [ HH.text "Look up books" ]
    , HH.label_
        [ HH.div_ [ HH.text "Enter book name:" ]
        , HH.input
            [ HP.value st.username
            , HE.onValueInput \str -> Just (SetUsername str)
            ]
        ]
    , HH.button
        [ HP.disabled st.loading
        , HP.type_ HP.ButtonSubmit
        ]
        [ HH.text "Fetch info" ]
    , HH.p_
        [ HH.text (if st.loading then "Working..." else "") ]
    , HH.div_
        case st.result of
          Nothing -> []
          Just res ->
            [ HH.h2_
                [ HH.text "Response:" ]
            , HH.pre_
                [ HH.code_ [ HH.text res ] ]
            ]
--    ,  HH.ul [] (map (\x -> HH.li [] [HH.text (x title)]) st.titles)
    ]

apiDomain :: String
apiDomain = "http://localhost:3000/titles"

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  SetUsername username -> do
    H.modify_ _ { username = username, result = Nothing }

  MakeRequest event -> do
    H.liftEffect $ Event.preventDefault event
    username <- H.gets _.username
    H.modify_ _ { loading = true }
    response <- H.liftAff $ AX.get AXRF.string (apiDomain <> "")
    let bodyStr = fromMaybe "" (map _.body (hush response))
    let possiblyTitles = runExcept (decodeJSON bodyStr :: F (Array Title))

    H.liftEffect $ log $ show possiblyTitles
    H.modify_ _ { loading = false
    , result = map _.body (hush response)
    , titles = case possiblyTitles of
                    Left _ -> []
                    Right titles -> titles
    }
