{-# LANGUAGE OverloadedStrings #-}

module BannerConfig
    (BannerConfig(
         BannerConfig
        ,queryURL
        ,defaultFontFace
        ,defaultFontSize
        ,defaultKeyColor
        ,defaultValueColor
        ,defaultStrokeWidth
        --,defaultUseCommas
        ,statConfigs)
    ,StatConfig(
         StatConfig
        ,key
        ,keyType
        ,name
        ,position
        ,fontFace
        ,fontSize
        ,keyColor
        ,valueColor
        ,strokeWidth
        ,useCommas)
    ,Color(
         Color
        ,red
        ,green
        ,blue
        ,alpha)
    ,Position(
         Position
        ,x
        ,y)
    ,readBannerConfig)
  where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import Data.Yaml.YamlLight

data BannerConfig = BannerConfig
    { queryURL           :: Maybe String
    , originPosition     :: Position -- The origin point for writing stats.
    , defaultFontFace    :: String   -- Default settings
    , defaultFontSize    :: Double   -- 
    , defaultKeyColor    :: Color    -- 
    , defaultValueColor  :: Color    -- 
    , defaultStrokeWidth :: Double   --
    --, defaultUseCommas       :: Bool     --
    , statConfigs     :: [StatConfig]
    } deriving (Show)

data StatConfig = StatConfig
    { key          :: String   -- The statistic's key, ie: Team_Name or Points
    , keyType      :: String   -- The section the key is in: user, team, status
    , name         :: String   -- The name to display for the key
    , position     :: Position -- The x,y position for this statistic
    , fontFace     :: Maybe String   -- The font to use
    , fontSize     :: Maybe Double   -- The font size
    , keyColor     :: Maybe Color    -- The color to use for the key name
    , valueColor   :: Maybe Color    -- The color to use for the value
    , strokeWidth  :: Maybe Double   -- The amount to stroke the text
    , useCommas    :: Maybe Bool   -- Wether or not to add commas
    } deriving (Show)

data Color = Color
    { red   :: Double
    , green :: Double
    , blue  :: Double
    , alpha :: Double
    } deriving (Show)

data Position = Position
    { x :: Double
    , y :: Double
    } deriving (Show)


-- Converts a configuration file into a BannerConfig value
readBannerConfig :: FilePath -> IO (Maybe BannerConfig)
readBannerConfig configFile = do
    yaml <- parseYamlFile configFile
    return $ getBannerConfig yaml
  where
    getBannerConfig map  = unMap map >>= makeBannerConfig
    makeBannerConfig map = Just
        ( BannerConfig
            { queryURL           = getString "query_url" map
            , originPosition     = fromJust $ getPosition map
            , defaultFontFace    = fromJust $ getString "font_face" map
            , defaultFontSize    = fromJust $ getString "font_size" map >>= toDouble
            , defaultKeyColor    = fromJust $ getColor "key_color" map
            , defaultValueColor  = fromJust $ getColor "val_color" map
            , defaultStrokeWidth = fromJust $ getString "stroke_width" map >>= toDouble
            --, defaultUseCommas   = getString "use_commas" map >>= toBool
            , statConfigs        = getStatConfigs map })
    toDouble val = Just (read val :: Double)
    toBool   val = Just (read val :: Bool)



-- The following functions are all used to access fields of the
-- configuration file and convert them into their proper types.
getStatConfigs :: Map YamlLight YamlLight -> [StatConfig]
getStatConfigs map = case lookup of 
    Nothing  -> []
    Just val -> val
  where
    lookup = M.lookup (YStr "stat_configs") map >>= unSeq
             >>= mapM (\cfgMap -> Just (getStatConfig (unMap cfgMap)))
    getStatConfig map = StatConfig {
             key          = fromJust $ getString "key" (fromJust map),
             keyType      = fromJust $ getString "type" (fromJust map),
             name         = fromJust $ getString "name" (fromJust map),
             position     = fromJust $ getPosition (fromJust map),
             fontFace     = getString "font_face" (fromJust map),
             fontSize     = getString "font_size" (fromJust map) >>= toDouble,
             keyColor     = getColor  "key_color" (fromJust map),
             valueColor   = getColor  "val_color" (fromJust map),
             strokeWidth  = getString "stroke_width" (fromJust map) >>= toDouble,
             useCommas    = getString "use_commas" (fromJust map) >>= toBool
             }
    toDouble val = Just (read val :: Double)
    toBool   val = Just (read val :: Bool)

getValue :: BS.ByteString -> Map YamlLight YamlLight -> Maybe YamlLight
getValue key map = M.lookup (YStr key) map

getString :: BS.ByteString -> Map YamlLight YamlLight -> Maybe String
getString key map = getValue key map >>= unStr >>= (\bs -> (Just (BS.unpack bs)))

getColor :: BS.ByteString -> Map YamlLight YamlLight -> Maybe Color
getColor key map = getValue key map >>= unMap >>= makeColor
  where
    makeColor map = Just (Color
                         (fromJust (getString "r" map >>= toDouble))
                         (fromJust (getString "g" map >>= toDouble))
                         (fromJust (getString "b" map >>= toDouble))
                         (fromJust (getString "a" map >>= toDouble)))
    toDouble val = Just (read val :: Double)

getPosition :: Map YamlLight YamlLight -> Maybe Position
getPosition map = M.lookup (YStr "position") map >>= unMap >>= makePosition
  where
    makePosition map = Just (Position
                            (fromJust (getString "x" map >>= toDouble))
                            (fromJust (getString "y" map >>= toDouble)))
    toDouble val = Just (read val :: Double)
