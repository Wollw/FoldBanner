{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

-- This program creates a statistics image banner for a user or team
-- involved in Folding@home.  I used the singleMode.hs example from 
-- http://zuttobenkyou.wordpress.com/2011/04/19/haskell-using-cmdargs-single-and-multi-mode/
-- to make the commandline options for this program look pretty.

import BannerConfig
import Control.Monad (when)
import Data.Maybe
import Graphics.Rendering.Cairo
import Network.Download
import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)
import System.Exit
import Text.XML.Light
import Text.XML.Light.Cursor

data MyOptions = MyOptions
    { config     :: FilePath
    , background :: FilePath
    , output     :: FilePath
    , ident      :: String
    } deriving (Data, Typeable, Show, Eq)

-- Customize your options, including help messages, shortened names, etc.
myProgOpts :: MyOptions
myProgOpts = MyOptions
    { config     = def &= typFile &= help "the banner configuration file"
    , background = def &= typFile &= help "the background image (must be png)"
    , output     = def &= typFile &= help "the output file (must be png)"
    , ident      = def &= typ "ID" &= help "the user or team id" }

getOpts :: IO MyOptions
getOpts = cmdArgs $ myProgOpts
    &= verbosityArgs [explicit, name "Verbose", name "V"] []
    &= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]
    &= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
    &= help _PROGRAM_ABOUT
    &= helpArg [explicit, name "help", name "h"]
    &= program _PROGRAM_NAME

_PROGRAM_NAME = "foldbanner"
_PROGRAM_VERSION = "1.0.1"
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_PROGRAM_ABOUT = "A configurable program for generating statistics banners for Folding@home"
_COPYRIGHT = "(C) David Shere 2012"

main :: IO ()
main = do
    args <- getArgs
    -- If the user did not specify any arguments, pretend as "--help" was given
    opts <- (if null args then withArgs ["--help"] else id) getOpts
    optionHandler opts

-- Before directly calling your main program, you should warn your user about incorrect arguments, if any.
optionHandler :: MyOptions -> IO ()
optionHandler opts@MyOptions{..}  = do
    -- Take the opportunity here to weed out ugly, malformed, or invalid arguments.
    when (null config)     $ putStrLn "--config is blank!"     >> exitWith (ExitFailure 1)
    when (null background) $ putStrLn "--background is blank!" >> exitWith (ExitFailure 1)
    when (null output)     $ putStrLn "--output is blank!"     >> exitWith (ExitFailure 1)
    when (null ident)      $ putStrLn "--ident is blank!"      >> exitWith (ExitFailure 1)
    -- When you're done, pass the (corrected, or not) options to your actual program.
    exec opts

exec :: MyOptions -> IO ()
exec opts@MyOptions{..} = do
    cfg <- readBannerConfig config
    case cfg of
        Nothing  -> putStrLn "Error loading config file." >> exitWith (ExitFailure 1)
        Just cfg -> do
            stats <- getStats cfg ident
            createBanner cfg (fromJust stats) output background

-------------------------------------------------------------------------------

-- Retrieve the statistics from the statistics server
getStats :: BannerConfig -> String -> IO (Maybe Element)
getStats cfg id = do
    xml <- openAsXML $ (queryURL cfg) ++ id
    case xml of
        Left _      -> return Nothing
        Right stats -> return $ Just ((onlyElems stats) !! 1)

-- Create the statistics banner and save it to the output file
createBanner :: BannerConfig -> Element -> FilePath -> FilePath -> IO ()
createBanner cfg stats out bg = withImageSurfaceFromPNG bg $ \surface -> do
    renderWith surface $ do mapM_ writeStat $ (statConfigs cfg)
    surfaceWriteToPNG surface out
    return ()
  where
    writeStat (StatConfig key name fontFace size key_color val_color stroke_width pos) = do
        selectFontFace fontFace FontSlantNormal FontWeightNormal
        setFontSize size
        writeText name (getData key) key_color val_color stroke_width pos
    getData key = case findElementByName key stats of
        Nothing -> "Error"
        Just e  -> strContent e
    findElementByName key stats = findElement (unqual key) stats

-- Write a string with a color and position to the surface.
writeText :: String -> String -> Color -> Color -> Double -> Position -> Render ()
writeText keyString valString keyColor valColor strokeWidth (Position x y) = do
    save

    moveTo x y
    textPath keyString
    setSourceRGBA (red keyColor) (green keyColor) (blue keyColor) (alpha keyColor)
    fillPreserve
    (x, y) <- getCurrentPoint -- save current position
    setLineWidth strokeWidth
    stroke

    moveTo x y
    textPath valString
    setSourceRGBA (red valColor) (green valColor) (blue valColor) (alpha valColor)
    fillPreserve
    setLineWidth strokeWidth
    stroke

    restore
