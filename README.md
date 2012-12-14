# FoldBanner #

This is a Folding@home banner generator configurable with
[YAML](http://www.yaml.org/) config files.

Additional required Haskell modules:
* [cmdargs](http://hackage.haskell.org/package/cmdargs)
* [download](http://hackage.haskell.org/package/download)
* [yaml-light](http://hackage.haskell.org/package/yaml-light)
* [xml](http://hackage.haskell.org/package/xml)
* [cairo](http://hackage.haskell.org/package/cairo)

## Usage ##
For querying the server:

    $ foldbanner -c CONFIG.yaml -b BACKGROUND.png -o OUTPUT.png -i TEAM_OR_USER_ID

or for using a local file:

    $ foldbanner -c CONFIG.yaml -b BACKGROUND.png -o OUTPUT.png -s STATSFILE.xml
