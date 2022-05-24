{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module ShoeService (
  Shoe (..),
  mkShoe,
  readShoes,
  findById,
  addShoe,
  saveShoes,
) where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.List (find)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics

-- data Shoe = Shoe String Int deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- mkShoe :: String -> Int -> Shoe
-- mkShoe = Shoe
data Shoe = Shoe
  { id :: Int
  , brand :: T.Text
  , model :: T.Text
  , size :: Maybe Int
  , img :: T.Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

mkShoe :: Int -> T.Text -> T.Text -> Int -> T.Text -> Shoe
mkShoe id' brand' model' size' img' =
  Shoe
    { ShoeService.id = id'
    , brand = brand'
    , model = model'
    , size = Just size'
    , img = img'
    }

readShoes :: IO B.ByteString
readShoes =
  B.readFile "./data/shoes.json"

saveShoes :: B.ByteString -> IO ()
saveShoes = B.writeFile "./data/shoes1.json"

findById :: Int -> [Shoe] -> Maybe Shoe
findById shoeId = find (\shoe -> shoeId == ShoeService.id shoe)

addShoe :: Shoe -> [Shoe] -> [Shoe]
addShoe shoeToAdd shoes =
  if shoeToAdd `elem` shoes
    then shoes
    else shoeToAdd : shoes

aShoe :: [Int] -> [Int]
aShoe = undefined
