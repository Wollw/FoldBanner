# FoldBanner #

This is a Folding@home banner generator configurable with
[YAML](http://www.yaml.org/) config files.

## Install ##
Installation should be as simple as running

    $ cabal install

while in the project's main directory.  This will install it in cabal's
default bin directory.  Ordinary users probably will find the binary
in $HOME/.cabal/bin/ and may want to add that directory to their path
if they haven't already.

## Usage ##
For querying the server:

    $ foldbanner -c CONFIG.yaml -b BACKGROUND.png -o OUTPUT.png -i TEAM_OR_USER_ID

or for using a local file:

    $ foldbanner -c CONFIG.yaml -b BACKGROUND.png -o OUTPUT.png -s STATSFILE.xml
