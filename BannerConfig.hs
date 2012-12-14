{-# LANGUAGE OverloadedStrings #-}

module BannerConfig (BannerConfig, StatConfig, Color, Position, getBannerConfig) where

import Data.Yaml.YamlLight
import qualified Data.ByteString.Char8 as BS
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as M

data BannerConfig = BannerConfig
    { id          :: String
    , queryURL    :: String
    , bgImage     :: String
    , statConfigs :: [StatConfig]
    } deriving (Show)

data StatConfig = StatConfig
    { key      :: String
    , name     :: String
    , color    :: Color
    , position :: Position
    } deriving (Show)

data Color = Color
    { red   :: Double
    , green :: Double
    , blue  :: Double
    , alpha :: Double
    } deriving (Show)

data Position = Position
    { x :: Int
    , y :: Int
    } deriving (Show)

getBannerConfig :: YamlLight -> Maybe BannerConfig
getBannerConfig map = unMap map >>= makeBannerConfig

makeBannerConfig :: Map YamlLight YamlLight -> Maybe BannerConfig
makeBannerConfig map = Just
    (BannerConfig {
        BannerConfig.id = getId map,
        queryURL        = getQueryURL map,
        bgImage         = getBGImage map,
        statConfigs     = getStatConfigs map
      }
    )

getString :: BS.ByteString -> Map YamlLight YamlLight -> String
getString key map = case lookup of
    Nothing  -> "Not found."
    Just val -> val
  where
    lookup = M.lookup (YStr key) map >>= unStr >>= (\bs -> (Just (BS.unpack bs)))

getId          = getString "ida"
getQueryURL    = getString "queryURL"
getBGImage     = getString "bgImage"

getStatConfigs :: Map YamlLight YamlLight -> [StatConfig]
getStatConfigs map = case lookup of 
    Nothing  -> []
    Just val -> val
  where
    lookup = M.lookup (YStr "statConfigs") map >>= unSeq
             >>= mapM (\cfgMap -> Just (getStatConfig (unMap cfgMap)))
    getStatConfig map = StatConfig {
             key  = getString "key" (fromJust map),
             name = getString "name" (fromJust map),
             color = getColor (fromJust map),
             position = getPosition (fromJust map)}

getColor :: Map YamlLight YamlLight -> Color
getColor map = case lookup of
    Nothing -> Color 0 0 0 1
    Just color -> color
  where
    lookup        = M.lookup (YStr "color") map >>= unMap >>= makeColor
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
                            (getInt "x" map)
                            (getInt "y" map))


getDouble :: BS.ByteString -> Map YamlLight YamlLight -> Double
getDouble key map = read $ getString key map :: Double

getInt :: BS.ByteString -> Map YamlLight YamlLight -> Int
getInt key map = read $ getString key map :: Int
