{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.GamePlay where

import Import
-- getGamePlayR :: Handler TypedContent
-- getGamePlayR = selectRep $ do 
--     -- provideRep $ pure $ object ["num" .= toInt 5]
--     provideRep $ defaultLayout $ do
--         setTitle "Hello there!"
--         [whamlet|<p>Hello there 5!|]


getGamePlayR :: GameSettings -> Handler Html
getGamePlayR (GameSettings bombs time size) = do
    defaultLayout $ do
        setTitle "Game!"
        [whamlet| <p> It's work #{bombs} #{time} #{size}|]    

-- postGamePlayR :: Int -> Int -> Int -> Handler Value
-- postGamePlayR bombs time size = do
--     defaultLayout $ do
--         setTitle "Game!"
--         [whamlet| <p> It's work #{bombs} #{time} #{size}|]