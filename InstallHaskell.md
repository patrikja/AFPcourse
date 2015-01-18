# Installing a Haskell development environment

## Ubuntu

### haskell-platform 2013.2

The easiest way is to install the latest haskell-platform package from
you distribution's standard package manager. For Ubuntu 14.04 and
14.10 that would give you haskell-platform 2013.2 including the
compiler ghc-7.6.3 and cabal-install version 1.20.0.3.

```Shell
sudo apt-get install haskell-platform
```

### haskell-platform 2014.2

To get version 2014-2 (which I recommend for the AFP course), you need to activate another [package archive
for ghc](ppa:hvr/ghc), then install cabal-install and ghc from this
ppa, followed by alex and happy using cabal.  (Instructions copied
from [stackage](http://www.stackage.org/install#ubuntu).)

```Shell
sudo apt-get update
sudo apt-get install -y software-properties-common
sudo add-apt-repository -y ppa:hvr/ghc
sudo apt-get update
sudo apt-get install -y cabal-install-1.20 ghc-7.8.4
cat >> ~/.bashrc <<EOF
export PATH=~/.cabal/bin:/opt/cabal/1.20/bin:/opt/ghc/7.8.4/bin:$PATH
EOF
export PATH=~/.cabal/bin:/opt/cabal/1.20/bin:/opt/ghc/7.8.4/bin:$PATH
cabal update
cabal install alex happy
```

To avoid confusion with different versions of packages I recommend you
to instruct cabal to use stackage LTS Haskell 1.2:

```Shell
cd .cabal
wget 'http://www.stackage.org/snapshot/lts-1.2/cabal.config?global=true' -o cabal.global.config
mv config config.old
cat config.old cabal.global.config > config
cabal update
```

Cabal will the point out that cabal-install in LTS Haskell 1.2 has a
slightly newer version than in the ppa. So if you want you can update
(but that requires another ubuntu package):

```Shell
sudo apt-get install zlib1g-dev
cabal install cabal-install
```

You can check your versions as follows:

```Shell
patrikj@etyd:~$ ghc --version
The Glorious Glasgow Haskell Compilation System, version 7.8.4
patrikj@etyd:~$ cabal --version
cabal-install version 1.18.0.8
using version 1.18.1.5 of the Cabal library 
```

