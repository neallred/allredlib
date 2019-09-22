{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module BookBinding where

import Database.Persist.TH
import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)

-- See: https://www.designersinsights.com/designer-resources/choosing-the-right-binding-type/

data BookBinding = SaddleStitched
    | LoopStitched
    | StabStitched -- also called side stitched
    | SewnBound
    | PerfectBound
    | TapeBound
    | ScrewBound -- also called stud bound & post bound
    | HardCover -- also called case bound
    | PlasticGrip
    | CombBound -- also called plastic bound
    | SpiralBound -- also called coil bound
    | WireOBound -- also called wire bound
    | ThermalBound
    | ThreeHolePunch
    | BradelBound
    | VeloBind
  
    deriving (Generic, Show, Read, Eq)
derivePersistField "BookBinding"

instance ToJSON BookBinding
instance FromJSON BookBinding

