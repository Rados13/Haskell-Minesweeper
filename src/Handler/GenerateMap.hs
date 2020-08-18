{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.GenerateMap where

import Import
import Text.Julius (RawJS (..))
import System.Random  
import Control.Monad.State  
import Data.Char
import Data.List.Split  (divvy,splitOn)
import qualified Data.List as DL (intersect)

generateBombsPositions :: [Int] -> Int -> Int -> [Int] -> [Int]
generateBombsPositions _ 0 _ acc = sort acc
generateBombsPositions (x:xs) bombsLeft size acc | any (==val) acc == True || val < 0 = generateBombsPositions xs bombsLeft size acc 
                                                 | otherwise               = generateBombsPositions xs newBombsLeft size $ val:acc 
                                                        where   val = x `rem` size
                                                                newBombsLeft = bombsLeft - 1


neighboursPositions :: Int -> Int -> [Int]
neighboursPositions idx size    | idx `rem` size == 0  = (++) [last . impureNonNull $ horizontalNeighbours] $ concat . map (tail . impureNonNull) $ verticalNeighbours
                                | idx `rem` size == size - 1 = (++) [head . impureNonNull $ horizontalNeighbours] $ concat . map (init . impureNonNull) $ verticalNeighbours
                                | otherwise = (++) horizontalNeighbours $ concat verticalNeighbours
                                    where   top = idx - size
                                            bottom = idx + size
                                            verticalNeighbours = [[pred top .. succ top],[pred bottom .. succ bottom]]
                                            horizontalNeighbours = [pred idx,succ idx]     

neighboursBombs :: Int -> Int -> [Int] -> Int
neighboursBombs idx size bombs  = length . DL.intersect bombs $ (neighboursPositions idx  size) 


generateMapChar :: Int -> Int -> [Int] -> Char
generateMapChar idx size bombs  | idx `elem` bombs = '*'
                                | bombsNum == 0 = '-'
                                | otherwise = intToDigit bombsNum
                                    where bombsNum = neighboursBombs idx size bombs


generateMap :: GameSettings -> [Char]
generateMap (GameSettings bombs gameTime mapSize) = do
    let bombPositions = generateBombsPositions (randoms (mkStdGen gameTime)) bombs (mapSize * mapSize) []
    foldr (\x y ->y ++ [generateMapChar x mapSize bombPositions]) "" $ [0 .. mapSize*mapSize-1]

isPositiveSettings :: GameSettings -> Bool
isPositiveSettings (GameSettings bombs gameTime mapSize) = bombs <1 || gameTime < 1 || mapSize < 1 && (bombs < mapSize * mapSize)

getGenerateMapR :: GameSettings -> Handler ()
getGenerateMapR  gameSettings =
    if isPositiveSettings gameSettings then redirectUltDest HomeR
        else do
            setSession "map" $ pack $ generateMap gameSettings
            setSession "moves" $ pack $ ""
            redirectUltDest $ GamePlayR gameSettings
            
                
