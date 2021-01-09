{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -imodules #-}

import Control.Monad.State hiding (state)
import Data.List (intersperse)
import Data.Monoid
import qualified Data.Text as T
import Lens.Micro.Platform
import System.Directory (getCurrentDirectory)
import System.Environment


import Yi.Keymap.Vim.StateUtils (resetCountE)
import Yi hiding (super)
import Yi.Utils (io)
import Yi.Modes (gnuMakeMode, jsonMode, cMode)
import Yi.Mode.Haskell (cleverMode, preciseMode, literateMode, fastMode)

import qualified Yi.Keymap.Vim as V
import qualified Yi.Keymap.Vim.Common as V
import qualified Yi.Keymap.Vim.Ex.Types as V
import qualified Yi.Keymap.Vim.Ex.Commands.Common as V
import qualified Yi.Keymap.Vim.Utils as V
import qualified Yi.Frontend.Vty as Vty

import FuzzyFile
import Make
import qualified Yi.Snippet as Snippet
import MySnippets

import RainbowMode
import PyflakesMode

type Bindings = ((V.EventString -> EditorM ()) -> [V.VimBinding])
type Parsers = [V.EventString -> Maybe V.ExCommand]

main :: IO ()
main = do
    files <- getArgs
    let actions = intersperse (EditorA newTabE) (map (YiA . openNewFile) files)
    startEditor (myConfig actions) Nothing

myConfig :: [Action] -> Config
myConfig actions = defaultConfig
    { modeTable =
        fmap
            (configureModeline . configureIndent)
            (myModes defaultConfig)
    , startFrontEnd = Vty.start
    , defaultKm = myKeymapSet
    , configCheckExternalChangesObsessively = False
    , startActions =
        (EditorA (do
            e <- get
            put e { maxStatusHeight = 30 }))
        : YiA guessMakePrg
        : actions
    }


makeKeymapSet  :: Bindings -> Parsers -> KeymapSet
makeKeymapSet bindings parsers = V.mkKeymapSet $ V.defVimConfig `override` \super this ->
    let eval = V.pureEval this
    in super
        { V.vimBindings = bindings eval ++ V.vimBindings super
        , V.vimExCommandParsers =
            parsers ++ 
            V.vimExCommandParsers super
        }


myKeymapSet :: KeymapSet
myKeymapSet = makeKeymapSet myBindings myParsers


nextWin :: EditorM ()
nextWin = do
  nextWinE
  resetCountE

prevWin :: EditorM ()
prevWin = do
  prevWinE
  resetCountE

activeSnippets :: [Snippet.Snippet]
activeSnippets = mySnippets

findSnippet :: [Snippet.Snippet] -> YiM ()
findSnippet snippets =
  (withEditor $ do
      expanded <- Snippet.expandSnippetE (defEval "<Esc>") snippets
      when (not expanded) (defEval "<Tab>"))

myParsers :: Parsers
myParsers = [exMake , exFlakes , exMakePrgOption , exPwd]

-- useful for defining vim bindings
defEval :: V.EventString -> EditorM ()
defEval = V.pureEval (extractValue V.defVimConfig)

nmap x y = V.mkStringBindingE V.Normal V.Drop (x, y, id)
nmapY x y = V.mkStringBindingY V.Normal (x, y, id)
imapY x y = V.VimBindingY (\evs state -> case V.vsMode state of
                                    V.Insert _ ->
                                        fmap (const (y >> return V.Drop))
                                             (evs `V.matchesString` x)
                                    _ -> V.NoMatch)

myBindings :: Bindings
myBindings eval =
       [ nmap "<BS>" previousTabE
       , nmap "<Tab>" nextTabE
       , nmap " " (eval ":nohlsearch<CR>")
       , nmap ";" (eval ":")
       , nmapY "<C-p>" fuzzyFile
       , nmap "<M-s>" (withCurrentBuffer deleteTrailingSpaceB)
       , nmap "<M-l>" (withCurrentBuffer (transposeB unitWord Forward >> leftB))
       , nmap "<M-h>" (withCurrentBuffer (transposeB unitWord Backward))
       , nmap "<C-@>" showErrorE
       , nmap "<M-d>" debug
       , nmap "<C-w>j" nextWin
       , nmap "<C-w>k" prevWin
       , nmap "<C-w>b" splitE
       , nmapY "s" (jumpToNextErrorInCurrentBufferY Forward)
       , nmapY "S" (jumpToNextErrorY Forward)
       , nmap ",s" insertErrorMessageE
       , imapY "<Tab>" (findSnippet mySnippets)
       , nmapY "<Esc>" (flakes >> withEditor (defEval "<Esc>"))
       ]

configureIndent :: AnyMode -> AnyMode
configureIndent = onMode $ \m ->
    if m ^. modeNameA == "Makefile"
    then m
    else m
        { modeIndentSettings = IndentSettings
            { expandTabs = True
            , shiftWidth = 4
            , tabSize = 4
            }}

configureModeline :: AnyMode -> AnyMode
configureModeline = onMode $ \m -> m {modeModeLine = myModeLine}
    where
    myModeLine prefix = do
        (line, col) <- getLineAndCol
        ro <- use readOnlyA
        mode <- gets (withMode0 modeName)
        unchanged <- gets isUnchangedBuffer
        filename <- gets (shortIdentString (length prefix))
        errorCount <- errorCountB
        return $ T.unwords
            [ if ro then "RO" else ""
            , if unchanged then "--" else "**"
            , filename
            , "L", showT line
            , "C", showT col
            , mode
            , if errorCount > 0
                then (showT errorCount <> " errors")
                else ""
            ]

myModes :: Config -> [AnyMode]
myModes cfg
    = AnyMode gnuMakeMode
    : AnyMode pyflakesMode
    : AnyMode cMode
    : AnyMode jsonMode
    : AnyMode cleverMode
    : AnyMode preciseMode
    : AnyMode literateMode
    : AnyMode fastMode
    : AnyMode rainbowParenMode
    : modeTable cfg

exPwd :: V.EventString -> Maybe V.ExCommand
exPwd "pwd" = Just (V.impureExCommand{V.cmdAction = YiA (io getCurrentDirectory >>= printMsg . T.pack)})
exPwd _ = Nothing

showT :: Show a => a -> T.Text
showT = T.pack . show
