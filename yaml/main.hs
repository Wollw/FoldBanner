import Data.Yaml.YamlLight
import BannerConfig

main :: IO ()
main = do
    yaml <- parseYamlFile "config.yaml"
    print $ unMap yaml >>= makeBannerConfig


