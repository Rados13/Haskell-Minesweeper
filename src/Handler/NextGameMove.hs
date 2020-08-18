{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.NextGameMove where

import Import
import System.Random  
import Control.Monad.State  
import Text.Julius (RawJS (..))
import qualified Data.List as DL (nub)
import qualified Data.Text as T
import Handler.GenerateMap
import Handler.GamePlay

getNextGameMoveR :: GameSettings -> MapFieldWrapper -> Handler ()
getNextGameMoveR gameSettings (MapFieldWrapper char num) = do
            let fieldChar = char
            let field = (char,num)
            lookupMap <- lookupSession "map"
            lookupMoves <- lookupSession "moves"
            let oldMoves = retriveFromSessionMoves lookupMoves
            let moves = case checkIfBlank fieldChar of  False -> oldMoves ++ [snd field]
                                                        True -> do
                                                            let mapFields =  retriveFromSessionMap gameSettings lookupMap       
                                                            let blankFields = blankFieldsOnMap mapFields
                                                            (++) oldMoves $ findNeighboursBlankFields (size gameSettings) 
                                                                blankFields [snd field] 
            setSession "moves" $ T.init . T.tail . T.pack . show $ moves
            redirectUltDest $ GamePlayR gameSettings



blankFieldsOnMap :: [MapField] -> [Int]
blankFieldsOnMap mapFields = map (\x -> snd x) . filter (\(c,i) -> c=='-') $ mapFields 

findNeighboursBlankFields :: Int -> [Int] -> [Int] -> [Int]

findNeighboursBlankFields mapSize blankFields acc | length acc == length newAcc = acc
                                                  | otherwise = findNeighboursBlankFields mapSize blankFields newAcc
                                                    where newAcc = filter (\x ->x `elem` blankFields) . 
                                                            DL.nub . concat .
                                                            map (\x -> [pred x .. succ x] ++
                                                             [x-mapSize-1 .. x-mapSize +1] ++ 
                                                             [x+mapSize-1 .. x+mapSize+1]) $ acc



checkIfBlank :: Char -> Bool
checkIfBlank '-' = True
checkIfBlank _   = False


