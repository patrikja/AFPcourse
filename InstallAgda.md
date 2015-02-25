# Installing Agda

## Ubuntu pre-built package

See [the Agda wiki](http://wiki.portal.chalmers.se/agda/pmwiki.php?n=Main.Linux)

Agda is best used with the emacs mode - both are installed by

```Shell
sudo apt-get install agda-mode
```

The [Ubuntu package description](https://launchpad.net/ubuntu/+source/agda).

Currently (2015-02-25) Ubuntu 14.10 comes with Agda-2.4.0.2.

## From Hackage (using cabal install)

First [Install Haskell](InstallHaskell.md), then 

```Shell
sudo cabal install agda
```

will build and install Agda from source.

https://hackage.haskell.org/package/Agda

Currently (2015-02-25) Hackage comes with Agda-2.4.2.2.
