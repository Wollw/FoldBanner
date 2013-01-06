{-# LANGUAGE OverloadedStrings #-}

module BannerConfig
    ( BannerConfig(BannerConfig, queryURL, statConfigs)
    , StatConfig(StatConfig)
    , Color(Color, red, green, blue, alpha)
    , Position(Position)
    , readBannerConfig
    ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import Data.Yaml.YamlLight

data BannerConfig = BannerConfig
    { queryURL        :: Maybe String
    , statConfigs     :: [StatConfig]
    } deriving (Show)

data StatConfig = StatConfig
    { key          :: String   -- The statistic's key, ie: Team_Name or Points
    , key_type     :: String   -- The section the key is in: user, team, status
    , name         :: String   -- The name to display for the key
    , fontFace     :: String   -- The font to use
    , size         :: Double   -- The font size
    , key_color    :: Color    -- The color to use for the key name
    , val_color    :: Color    -- The color to use for the value
    , stroke_width :: Double   -- The amount to stroke the text
    , use_commas   :: String   -- Wether or not to add commas
    , position     :: Position -- The x,y position for this statistic
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
            { queryURL        = getMaybeString "queryURL" map
            , statConfigs     = getStatConfigs map })
    getMaybeString key map = do
        case getString key map of
            "NOT_FOUND" -> Nothing
            val         -> Just val


-- The following functions are all used to access fields of the
-- configuration file and convert them into their proper types.
getDouble :: BS.ByteString -> Map YamlLight YamlLight -> Double
getDouble key map = read $ getString key map :: Double

getString :: BS.ByteString -> Map YamlLight YamlLight -> String
getString key map = case lookup of
    Nothing  -> "NOT_FOUND"
    Just val -> val
  where
    lookup = M.lookup (YStr key) map >>= unStr >>= (\bs -> (Just (BS.unpack bs)))

getStatConfigs :: Map YamlLight YamlLight -> [StatConfig]
getStatConfigs map = case lookup of 
    Nothing  -> []
    Just val -> val
  where
    lookup = M.lookup (YStr "statConfigs") map >>= unSeq
             >>= mapM (\cfgMap -> Just (getStatConfig (unMap cfgMap)))
    getStatConfig map = StatConfig {
             key          = getString "key" (fromJust map),
             key_type     = getString "type" (fromJust map),
             name         = getString "name" (fromJust map),
             fontFace     = getString "font" (fromJust map),
             size         = getDouble "size" (fromJust map),
             key_color    = getColor "key_color" (fromJust map),
             val_color    = getColor "val_color" (fromJust map),
             stroke_width = getDouble "stroke_width" (fromJust map),
             use_commas   = getString "comma" (fromJust map),
             position     = getPosition (fromJust map)}

getColor :: BS.ByteString -> Map YamlLight YamlLight -> Color
getColor key map = case lookup of
    Nothing -> Color 0 0 0 1
    Just color -> color
  where
    lookup        = M.lookup (YStr key) map >>= unMap >>= makeColor
    makeColor map = Just (Color
                         (getDouble "r" map)
                         (getDouble "g" map)
                         (getDouble "b" map)
                         (getDouble "a" map))

getPosition :: Map YamlLight YamlLight -> Position
getPosition map = case lookup of
    Nothing  -> Position 0 0
    Just pos -> pos
  where
    lookup           = M.lookup (YStr "position") map >>= unMap >>= makePosition
    makePosition map = Just (Position
                            (getDouble "x" map)
                            (getDouble "y" map))
