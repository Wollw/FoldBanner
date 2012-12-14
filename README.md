# FoldBanner #

This is a configurable Folding@home banner generator written in Haskell and
configurable with [YAML](http://www.yaml.org/) config files.

Additional required Haskell modules:
* [cmdargs](http://hackage.haskell.org/package/cmdargs)
* [download](http://hackage.haskell.org/package/download)
* [yaml-light](http://hackage.haskell.org/package/yaml-light)
* [xml](http://hackage.haskell.org/package/xml)
* [cairo](http://hackage.haskell.org/package/cairo)

To run:

    $ foldbanner -c CONFIG.yaml -o OUTPUT.png -i TEAM_OR_USER_ID
