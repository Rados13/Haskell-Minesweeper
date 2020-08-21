# Haskell-Minesweeper

## Introduction

How name of this project suggest it is a Minesweeper game working in Haskell. For GUI and handle user input Yesod framework was used.

## Run project

1. If you haven't already, [install Stack](https://haskell-lang.org/get-started)
	* On POSIX systems, this is usually `curl -sSL https://get.haskellstack.org/ | sh`
2. Install the `yesod` command line tool: `stack install yesod-bin --install-ghc`
3. Build libraries: `stack build`

If you have trouble, refer to the [Yesod Quickstart guide](https://www.yesodweb.com/page/quickstart) for additional detail.

To run server
```
yesod devel
```
As your code changes, your site will be automatically recompiled and redeployed to localhost.


## Documentation

In this app you have five routes. Described in table below.

| Routes url	| Handled HTTP Methods 	| Description 		 |
|:--------------|:----------------------|:-------------------|
|`/`			| GET 	POST			| Homepage where you setup parameters of game, POST is for handle form result of game parameters|
|`/game/{"GameSettings"}/generate`	| GET			| On this route the game map is generated, with positions of bombs |
|`/game/{"GameSettings"}/`			| GET			| Here the game map is displayed and client play the game |
|`/game/{"GameSettings"}/{"MapFieldWrapper"}`| GET POST	| GET is used for handling discovering new fields, POST is used for marking field as bomb|


In the handlers folder almost all logic was writed (Handlers in Yesod are equivalent of controllers from MVC). Every route has his own handler. The web files are in templates folder. Yesod equivalent of files: HTML - Hamlet, CSS - Lucious, Javascript - Julius
