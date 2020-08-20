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
import qualified Data.List as DL (intersect,nub,partition)
import Handler.GenerateMap

retriveMap :: GameSettings -> Maybe Text ->[(Char,Int)]
retriveMap gameSettings lookupMap= do
                        let retrived = case lookupMap of    Nothing -> generateMap gameSettings
                                                            Just map -> unpack map
                        let mapValues = if length retrived /= size gameSettings then generateMap gameSettings else retrived
                        zip mapValues $ [0 .. length mapValues - 1]

retriveMovesAsListInt :: Maybe Text -> [Int]
retriveMovesAsListInt lookupMoves = filter (>0) . textToIntList . retriveMovesAsText $ lookupMoves

retriveMovesAsText :: Maybe Text -> Text
retriveMovesAsText lookupMoves = case lookupMoves of Nothing -> pack ""
                                                     Just moves -> moves

retriveMarkedPositions :: Maybe Text -> [Int]
retriveMarkedPositions lookupBombs = case lookupBombs of    Nothing -> []
                                                            Just bombs -> textToIntList bombs                                            

blankFieldsOnMap :: [MapField] -> [Int]
blankFieldsOnMap mapFields = map (\x -> snd x) . filter (\(c,i) -> c=='-') $ mapFields 

findNeighboursBlanks :: Int -> [Int] -> [Int] -> [Int] -> [Int]
findNeighboursBlanks _ _ acc [] = acc
findNeighboursBlanks mapSize blanks acc (x:xs) | x `elem` blanks = findNeighboursBlanks mapSize blanks newAcc newXs  
                                               | otherwise = findNeighboursBlanks mapSize blanks (x:acc) xs
                                                  where neighbours = filter (\e -> e>=0 && e<mapSize * mapSize) . 
                                                                neighboursPositions x $ mapSize
                                                        newFields = filter (not . (flip elem acc)) 
                                                                    . filter (not . (flip elem xs)) $ neighbours
                                                        (blanksNeighbours,restNeighbours) = DL.partition (flip elem blanks) newFields
                                                        newXs = xs ++ blanksNeighbours
                                                        newAcc = acc ++ (x:restNeighbours)






getGamePlayR :: GameSettings -> Handler Html
getGamePlayR (GameSettings bombsNum gameTime mapSize) = 
    if isPositiveSettings $ GameSettings bombsNum gameTime mapSize then redirect HomeR
        else do
            let gameSettings = GameSettings bombsNum gameTime mapSize

            lookupBombs <- lookupSession "bombs"
            let markedPositions = retriveMarkedPositions lookupBombs

            lookupMap <- lookupSession "map"
            let mapFields =  retriveMap  gameSettings lookupMap
            let bombsMap =  divvy mapSize mapSize mapFields            
            
            lookupMoves <- lookupSession "moves"
            let moves = retriveMovesAsListInt lookupMoves
            let discoveredPositions = findNeighboursBlanks mapSize (blankFieldsOnMap mapFields) [] moves

            let movesLen = length discoveredPositions
            let freeFields = length mapFields - bombsNum - movesLen
            let gameEnd = length discoveredPositions == mapSize * mapSize - bombsNum
            let failed = any (=='*') . map (\x -> fst . head . impureNonNull . drop x $ mapFields) $ moves


            
            defaultLayout $ do
                setTitle "Game!"
                $(widgetFile "game/play")
                

isClickedField :: MapField -> [Int] -> Bool
isClickedField (_,i) moves = i `elem` moves

