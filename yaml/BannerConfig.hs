{-# LANGUAGE OverloadedStrings #-}

module BannerConfig (BannerConfig, StatConfig, Color, Position, makeBannerConfig) where

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

makeBannerConfig :: Map YamlLight YamlLight -> Maybe BannerConfig
makeBannerConfig map = Just
    (BannerConfig {
        BannerConfig.id          = fromJust $ getId map,
        queryURL    = fromJust $ getQueryURL map,
        bgImage     = fromJust $ getBGImage map,
        statConfigs = []
      }
    )

getId       = getString "id"
getQueryURL = getString "queryURL"
getBGImage  = getString "bgImage"

getString :: BS.ByteString -> Map YamlLight YamlLight -> Maybe String
getString key map = M.lookup (YStr key) map >>= unStr >>= (\bs -> (Just (BS.unpack bs)))
