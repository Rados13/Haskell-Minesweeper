{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))

-- Define our data that will be used for creating the form.


gameAForm :: AForm Handler GameSettings
gameAForm = GameSettings
    <$> areq naturalField "Number of Bombs" (Just 10)
    <*> areq naturalField "Time in minutes" (Just 10)
    <*> areq naturalField "Size" (Just 10)
    where 
        errorMessage :: Text
        errorMessage = "All fields must be natural numbers"
        naturalField = checkBool (> 0) errorMessage intField


-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes.yesodroutes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm gameAForm
    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

                
postHomeR :: Handler ()
postHomeR = do
    ((result,formWidget), formEnctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm gameAForm
    case result of
        FormSuccess res -> redirectUltDest $ GamePlayR res
        _ -> redirectUltDest HomeR
