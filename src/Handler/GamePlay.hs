{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.GamePlay where

import Import
import Text.Julius (RawJS (..))
import System.Random  
import Control.Monad.State  
import Data.Char
import Data.List.Split  (divvy,splitOn)
import qualified Data.List as DL (intersect)
import Handler.GenerateMap


retriveFromSessionMap :: GameSettings -> Maybe Text ->[(Char,Int)]
retriveFromSessionMap gameSettings lookupMap= do
                        let mapValues = case lookupMap of
                                                Nothing -> generateMap gameSettings
                                                Just map -> unpack map  
                        zip mapValues $ [0 .. length mapValues - 1]

retriveFromSessionMoves :: Maybe Text -> [Int]
retriveFromSessionMoves lookupMoves = do
                        case lookupMoves of 
                                        Nothing -> []
                                        Just moves -> textToIntList moves

getGamePlayR :: GameSettings -> Handler Html
getGamePlayR (GameSettings bombs gameTime mapSize) = 
    if isPositiveSettings $ GameSettings bombs gameTime mapSize then redirect HomeR
        else do
            let gameSettings = GameSettings bombs gameTime mapSize
            lookupMap <- lookupSession "map"
            lookupMoves <- lookupSession "moves"
            
            let mapFields =  retriveFromSessionMap  gameSettings lookupMap
            let bombsMap =  divvy mapSize mapSize mapFields
            
            let moves = retriveFromSessionMoves lookupMoves
            defaultLayout $ do
                setTitle "Game!"                
                $(widgetFile "game/play")
                

isClickedField :: MapField -> [Int] -> Bool
isClickedField (_,i) moves = i `elem` moves
