{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Guide where

import Import

getGuideR :: Handler Html
getGuideR = defaultLayout $ do
    setTitle "Guide to minesweeper"
    toWidget [whamlet|<h1> Hello there
        <ul>
            <li> On the home page you choose paramas of the game (number of bombs, size of map and seed for map generator)
            <li> To discover field click left mouse button on it
            <li> To mark position as bomb click right mouse button on it
            <li> You have to discover all fields which don't have bomb on it    
    |]
    toWidget [lucius| li { font-size: medium; padding: 5px} |]
    toWidget [lucius| ul {list-style-type:none;} |]
