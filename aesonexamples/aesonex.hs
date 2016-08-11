{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson
import GHC.Generics (Generic)
import Control.Monad (mzero)
import Data.Text (Text)
import Data.Scientific (Scientific)
import Data.ByteString.Lazy (ByteString)


data Adres = Adres { straat :: String
                   , huisnr :: Int
                   , stad   :: String
                   , land   :: String
                   } deriving (Show, Generic)

instance FromJSON Adres where
     parseJSON (Object v) = Adres <$> v .: "street"
                                  <*> v .: "no"
                                  <*> v .: "city"
                                  <*> v .: "country"
     parseJSON _          = mzero

instance ToJSON Adres


data Person = Person { naam         :: String
                     , leeftijd     :: Int
                     , lustPizza    :: Bool
                     , lotto        :: [Int]
                     , woorden      :: [String]
                     , adres        :: [Adres]
                     , type_        :: Int
                     } deriving (Show)

instance FromJSON Person where
     parseJSON (Object v) = Person <$> v .: "name"
                                   <*> v .: "age"
                                   <*> v .: "likespizza"
                                   <*> v .: "lottonumbers"
                                   <*> v .: "passphrase"
                                   <*> v .: "addresses"
                                   <*> v .: "type"
     parseJSON _ = mzero

instance ToJSON Person where
    toJSON (Person naam leeftijd lustPizza lotto woorden adres type_) =
        object [ "name"         .= naam
               , "age"          .= leeftijd
               , "likespizza"   .= lustPizza
               , "lottonumbers" .= lotto
               , "passphrase"   .= woorden
               , "addresses"    .= adres
               , "type"         .= type_
               ]

-- Using {-# LANGUAGE DeriveGeneric #-} allows us to use
-- instance FromJSON Person
-- instance ToJSON Person
-- I haven't used it here because "type" is a haskell keyword.
--

-------------------- Being ------------------------
data Being = Human { name :: String
                   , age:: Int
                   } |

             Animal { species :: String
                    , herbivor :: Bool
                    , age :: Int
                    } deriving Show

--instance FromJSON Being where
--    parseJSON (Object v) = Being


-------------------- Coordinates ------------------------

data Coord = Coord { x :: Double
                   , y :: Double
                   } deriving (Show)


instance FromJSON Coord where
  parseJSON (Object v) = Coord <$>
                         v .: "x" <*>
                         v .: "y"
  parseJSON _          = empty

instance ToJSON Coord where
  toJSON (Coord xV yV) = object [ "x" .= xV,
                                  "y" .= yV ]

--  toEncoding Coord{..} = pairs $
--    "x" .= x <>
--    "y" .= y
