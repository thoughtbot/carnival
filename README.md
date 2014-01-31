# Carnival

A disqus-like commenting service powering http://robots.thoughtbot.com.

## Getting started

* Install ghc, alex, happy, and cabal-install. See 
  [here](http://www.haskell.org/platform/mac.html).

* Update Cabal:

```
$ cabal update
$ cabal install cabal-install
```

* Create a sandbox for the project

```
$ cabal sandbox init
```

* Install the Yesod Platform

```
$ cabal install yesod-platform yesod-bin
```

* Run the site:

```
$ yesod devel
```
