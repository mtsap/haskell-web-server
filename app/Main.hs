{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Acme.Missiles
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Aeson as A
import Data.Fixed (showFixed)
import Data.Foldable (Foldable (fold))
import Data.List
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text.Lazy (Text)
import Lib
import ShoeService
import qualified ShoeService
import System.IO.Error (fullErrorType)
import Text.Regex.Applicative
import Text.Regex.Applicative.Common (decimal)
import Web.Scotty

main :: IO ()
main = scotty 3000 $ do
  get "/" $ do
    text "Shoe service is up and running"
  get "/shoes/" $ do
    shoesFromFile <- liftIO ShoeService.readShoes
    let parsedShoes = A.decode shoesFromFile :: Maybe [ShoeService.Shoe]
    json parsedShoes
  get "/shoe/:shoeId" $ do
    shoeId <- param "shoeId"
    shoesFromFile <- liftIO ShoeService.readShoes
    let shoe = A.decode shoesFromFile >>= ShoeService.findById shoeId
    json shoe
  post "/shoe/" $ do
    liftIO $ putStrLn "Adding a shoe"
    shoeFromBody <- jsonData :: ActionM Shoe
    shoesFromFile <- liftIO ShoeService.readShoes
    let parsedShoes = A.decode shoesFromFile :: Maybe [ShoeService.Shoe]
    let newShoes = fmap (ShoeService.addShoe shoeFromBody) parsedShoes
    liftIO $ ShoeService.saveShoes . A.encode $ newShoes
    text "ok"
