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
import qualified Data.List as DL (union)
import qualified Data.Text as T
import Data.List.Split  (splitOn)
import Handler.GamePlay
import Handler.GenerateMap (neighboursPositions)

getNextGameMoveR :: GameSettings -> MapFieldWrapper -> Handler ()
getNextGameMoveR gameSettings (MapFieldWrapper char num) = do

            lookupMoves <- lookupSession "moves"
            let oldMoves = retriveMovesAsListInt lookupMoves


            let newMoves = intListToText $ if length oldMoves == 0 then  [num] 
                                            else DL.union oldMoves $ [num]

            deleteSession "moves"
            setSession "moves"  newMoves            

            redirect $ GamePlayR gameSettings

postNextGameMoveR :: GameSettings -> MapFieldWrapper -> Handler ()
postNextGameMoveR _ (MapFieldWrapper char num) = do
            lookupBombs <- lookupSession "bombs"
            let markedPositions = retriveMarkedPositions lookupBombs
            let newMarkedPositions  = if num `elem` markedPositions then filter (/= num) markedPositions else markedPositions ++ [num]
            setSession "bombs" $ intListToText newMarkedPositions


