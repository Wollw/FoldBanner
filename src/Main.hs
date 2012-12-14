{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)
import System.Exit
import Control.Monad (when)

import Data.Yaml.YamlLight

import BannerConfig

data MyOptions = MyOptions
    { config :: FilePath
    } deriving (Data, Typeable, Show, Eq)

-- Customize your options, including help messages, shortened names, etc.
myProgOpts :: MyOptions
myProgOpts = MyOptions
    {config = def &= typFile &= help "the banner configuration file" }

getOpts :: IO MyOptions
getOpts = cmdArgs $ myProgOpts
    &= verbosityArgs [explicit, name "Verbose", name "V"] []
    &= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]
    &= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
    &= help _PROGRAM_ABOUT
    &= helpArg [explicit, name "help", name "h"]
    &= program _PROGRAM_NAME

_PROGRAM_NAME = "foldbanner"
_PROGRAM_VERSION = "0.1"
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
    when (null config) $ putStrLn "--config is blank!" >> exitWith (ExitFailure 1)
    -- When you're done, pass the (corrected, or not) options to your actual program.
    exec opts

exec :: MyOptions -> IO ()
exec opts@MyOptions{..} = do
    yamlConfig <- parseYamlFile config
    (print . getBannerConfig) yamlConfig
