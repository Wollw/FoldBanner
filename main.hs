{-# LANGUAGE DeriveDataTypeable, RecordWildCards, OverloadedStrings #-}
import Graphics.Rendering.Cairo

import Network.Download

import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)
import System.Exit
import System.IO

import Control.Monad (when)

import qualified Data.ByteString.Lazy.Char8 as BS

import Text.XML.Light
import Text.XML.Light.Cursor

import Data.Yaml.YamlLight

-- Text settings
textFont  = "monospace"
textSize  = 16
strokeSize = 0.75

--teamURL = "http://folding.extremeoverclocking.com/xml/team_summary.php?t="
--userURL = "http://folding.extremeoverclocking.com/xml/user_summary.php?u="

data StatPosition = StatPosition String String Color Double Double deriving (Show)
data Color = Color Double Double Double Double deriving (Show)

data Config = Config {
      getQueryURL :: String
    , getId :: Int
    , getBackground :: String
    --, getStatConfigs :: [StatPosition]
  } deriving (Show)

instance FromJSON Config where
    parseJSON (Object v) =
        Config <$>
        (v .: "query_url")  <*>
        (v .: "id")         <*>
        (v .: "background")

-- Command Line Parsing
-- CmdArgs example code from:
-- http://zuttobenkyou.wordpress.com/2011/04/19/haskell-using-cmdargs-single-and-multi-mode/
data Options = Options
    { config :: String
    } deriving (Data, Typeable, Show, Eq)

myOpts :: Options
myOpts = Options
    { config = "config.json" &= help "config file"
    }

getOpts :: IO Options
getOpts = cmdArgs $ myOpts
    &= verbosityArgs [explicit, name "Verbose", name "V"] []
    &= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]
    &= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
    &= help _PROGRAM_ABOUT
    &= helpArg [explicit, name "help", name "h"]
    &= program _PROGRAM_NAME

_PROGRAM_NAME = "foldrank"
_PROGRAM_VERSION = "0.1"
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_PROGRAM_ABOUT = "Generate an image banner "
_COPYRIGHT = "(C) Your Name Here 2011"

optionHandler :: Options -> IO ()
optionHandler opts@Options{..}  = do
    when (null config) $ putStrLn "warning: no config file"
    exec opts

exec :: Options -> IO ()
exec opts@Options{..} = createBanner teamURL teamId "background.png" "output.png"

-- Generates a new PNG banner with current extremeoverclocking Folding@Home
-- stats using the specified background image.
createBanner :: String -> Int -> String -> String -> IO ()
createBanner url id bgFile outputFile = do

    stats <- getStats $ url ++ (show id)

    jsonBS <- BS.readFile "config.json"
    let json = decode jsonBS :: Maybe Config
    print json

    withImageSurfaceFromPNG bgFile $ \surface -> do
    renderWith surface $ do
        case stats of 
            Just e -> do
                selectFontFace textFont FontSlantNormal FontWeightNormal
                setFontSize textSize
                mapM_ (writeStat) displayList
              where
                getData key  = case findElementByName key e of
                    Nothing  -> "Error"
                    Just el  -> strContent el
                writeStat (StatPosition id name color x y) = do
                    writeText (name ++ ": " ++ (getData id)) color x y
            Nothing -> return ()

    surfaceWriteToPNG surface outputFile
    return ()

main :: IO ()
main = do
    args <- getArgs
    -- If the user did not specify any arguments, pretend as "--help" was given
    opts <- (if null args then withArgs ["--help"] else id) getOpts
    optionHandler opts

-- Retrieve Folding@Home statistics for a team from extremeoverclocking.com
-- Returns the root element of the main XML tree
getStats :: String -> IO (Maybe Element)
getStats url = do
    xml <- openAsXML url
    case xml of
        Left message -> return Nothing
        Right contents -> return $ Just $ (onlyElems contents) !! 1

-- Find a child XML element from an XML element by the child's name
findElementByName :: String -> Element -> Maybe Element
findElementByName name root = findElement (unqual name) root

-- Write a string at position (x,y) on the surface
writeText :: String -> Color -> Double -> Double -> Render ()
writeText text color x y = do
    save

    lineWidth <- getLineWidth

    (TextExtents xb yb w h _ _) <- textExtents text

    moveTo x y
    textPath text
    setColor color
    fillPreserve
    setLineWidth strokeSize
    stroke

    restore
  where
    setColor (Color r g b a) = setSourceRGBA r g b a
