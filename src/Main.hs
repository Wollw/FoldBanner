{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

-- This program creates a statistics image banner for a user or team
-- involved in Folding@home.  I used the singleMode.hs example from 
-- http://zuttobenkyou.wordpress.com/2011/04/19/haskell-using-cmdargs-single-and-multi-mode/
-- to make the commandline options for this program look pretty.

import BannerConfig
import Control.Monad (when)
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import Graphics.Rendering.Cairo
import Network.Curl.Download
import System.Console.CmdArgs
import qualified System.Console.CmdArgs as CA
import System.Directory
import System.Environment (getArgs, withArgs)
import System.Exit
import Text.XML.Light
import Text.XML.Light.Cursor
import Text.XML.Light.Input

data MyOptions = MyOptions
    { config     :: FilePath
    , background :: FilePath
    , output     :: FilePath
    , stats      :: FilePath
    , ident      :: String
    } deriving (Data, Typeable, Show, Eq)

-- Customize your options, including help messages, shortened names, etc.
myProgOpts :: MyOptions
myProgOpts = MyOptions
    { config     = def &= typFile &= help "the banner configuration file"
    , background = def &= typFile &= help "the background image (must be png)"
    , output     = def &= typFile &= help "the output file (must be png)"
    , stats      = def &= typFile &= help "the xml statistics file (from extremeoverclocking.com)"
    , ident      = def &= typ "ID" &= help "the user or team id" }

getOpts :: IO MyOptions
getOpts = cmdArgs $ myProgOpts
    &= verbosityArgs [explicit, CA.name "Verbose", CA.name "V"] []
    &= versionArg [explicit, CA.name "version", CA.name "v", summary _PROGRAM_INFO]
    &= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
    &= help _PROGRAM_ABOUT
    &= helpArg [explicit, CA.name "help", CA.name "h"]
    &= program _PROGRAM_NAME

_PROGRAM_NAME = "foldbanner"
_PROGRAM_VERSION = "1.0.4"
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_PROGRAM_ABOUT = "A configurable program for generating statistics banners for Folding@home"
_COPYRIGHT = "(c) 2012-2013 David Shere"

main :: IO ()
main = do
    args <- getArgs
    -- If the user did not specify any arguments, pretend as "--help" was given
    opts <- (if null args then withArgs ["--help"] else id) getOpts
    optionHandler opts

-- Before directly calling your main program, you should warn your user about incorrect arguments, if any.
optionHandler :: MyOptions -> IO ()
optionHandler opts@MyOptions{..}  = do
    -- Get some error values
    bgExists <- doesFileExist background
    cfgExists <- doesFileExist config
    -- Take the opportunity here to weed out ugly, malformed, or invalid arguments.
    when (null config)     $ putStrLn "--config is blank!"     >> exitWith (ExitFailure 1)
    when (null background) $ putStrLn "--background is blank!" >> exitWith (ExitFailure 1)
    when (not cfgExists) $ putStrLn ("config file \"" ++ config ++ "\" doesn't exist.") >> exitWith (ExitFailure 1)
    when (not bgExists) $ putStrLn ("background file \"" ++ background ++ "\" doesn't exist") >> exitWith (ExitFailure 1)
    when (not (isSuffixOf ".png" (map toLower background))) $ putStrLn "background file isn't a PNG image" >> exitWith (ExitFailure 1)
    when (null output)     $ putStrLn "--output is blank!"     >> exitWith (ExitFailure 1)
    when (not (null ident) && not (null stats)) $ putStrLn "--ident and --stats are both defined! Use one!"      >> exitWith (ExitFailure 1)
    when (null ident && null stats) $ putStrLn "--ident and --stats are blank! Use one!"      >> exitWith (ExitFailure 1)
    -- When you're done, pass the (corrected, or not) options to your actual program.
    exec opts

exec :: MyOptions -> IO ()
exec opts@MyOptions{..} = do
    cfg <- readBannerConfig config
    case cfg of
        Nothing  -> putStrLn "Error loading config file." >> exitWith (ExitFailure 1)
        Just cfg -> do
            case (null stats) of
                True -> do
                    stats <- getStats cfg (StatID ident)
                    case stats of
                        Nothing -> putStrLn "Error retrieving stats."
                        Just s  -> createBanner cfg s output background
                False -> do
                    stats <- getStats cfg (StatFile stats)
                    case stats of
                        Nothing -> putStrLn "Error reading stats."
                        Just s  -> createBanner cfg s output background

-------------------------------------------------------------------------------

data StatSource = StatFile FilePath | StatID String

-- Retrieve the statistics from the statistics server or local file
getStats :: BannerConfig -> StatSource -> IO (Maybe Element)
getStats cfg (StatID id) = do
    case queryURL cfg of
        Nothing  -> putStrLn "No query_url in config file." >> exitWith (ExitFailure 1);
        Just val -> do
            xml <- openAsXML $ val ++ id
            case xml of
                Left e      -> do
                    putStrLn e
                    return Nothing
                Right stats -> return $ Just ((onlyElems stats) !! 1)

getStats cfg (StatFile file) = do
    xmlFile <- readFile file
    return $ parseXMLDoc xmlFile

-- Create the statistics banner and save it to the output file
createBanner :: BannerConfig -> Element -> FilePath -> FilePath -> IO ()
createBanner mainConfig stats out bg = withImageSurfaceFromPNG bg $ \surface -> do
    renderWith surface $ do mapM_ writeStat $ statConfigs mainConfig
    surfaceWriteToPNG surface out
    return ()
  where
    getData keyType key = case findElementByName keyType key stats of
        Nothing -> "Error"
        Just e  -> strContent e
    findElementByName keyType key stats = findElement (unqual key) $
        fromJust $ findElement (unqual keyType) stats
    addCommas x = h++t -- addCommas function based on code from here:
      where            -- http://stackoverflow.com/a/3753207/816685
        sp = break (== '.') x
        h  = reverse (intercalate "," $ chunksOf 3 $ reverse $ fst sp)
        t = snd sp
    writeStat cfg = do
        selectFontFace statFontFace FontSlantNormal FontWeightNormal
        setFontSize statFontSize
        let value = if fromJust $ useCommas cfg
                then do addCommas $ getData (keyType cfg) (key cfg)
                else getData (keyType cfg) (key cfg)
        writeText (BannerConfig.name cfg) value statKeyColor statValueColor statStrokeWidth (originPosition mainConfig) (position cfg)
      where
        statFontFace = case fontFace cfg of
            Nothing  -> defaultFontFace mainConfig
            Just val -> val
        statFontSize = case fontSize cfg of
            Nothing  -> defaultFontSize mainConfig
            Just val -> val
        statKeyColor = case keyColor cfg of
            Nothing  -> defaultKeyColor mainConfig
            Just val -> val
        statValueColor = case valueColor cfg of
            Nothing  -> defaultValueColor mainConfig
            Just val -> val
        statStrokeWidth = case strokeWidth cfg of
            Nothing  -> defaultStrokeWidth mainConfig
            Just val -> val

-- Write a string with a color and position to the surface.
writeText :: String -> String -> Color -> Color -> Double -> Position -> Position -> Render ()
writeText keyString valString keyColor valColor strokeWidth (Position orgX orgY) (Position x y) = do

    s <- status
    case s of 
        StatusSuccess -> render
        StatusNoMemory -> liftIO $ putStrLn "Error rendering image.  Was the background image a valid PNG image?" >> exitWith (ExitFailure 1)

  where
    render = do
        save

        moveTo (orgX + x) (orgY + y)
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
