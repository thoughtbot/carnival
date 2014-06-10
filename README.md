# Carnival

A medium/disqus-like commenting service powering http://robots.thoughtbot.com.

## Getting started

Install ghc, alex, happy, cabal, Postgresql. On OS X, the easiest way is to
install Haskell Platform using homebrew:

    $ brew install haskell-platform

Run the setup script first:

    $ bin/setup

Then boot the site:

    $ yesod devel

And run the tests:

    $ yesod test

## Deployment

Staging is the default deploy target:

```
$ ./bin/deploy
```

The production app can be passed as an argument if desired:

```
$ ./bin/deploy carnival-production
```

Deploys require a valid `./.env` file, see `./.env.sample` for details.

## Managing Dependencies

The `bin/setup` script will create a [cabal sandbox][cabal-sandbox] and 
install dependencies into it. If new dependencies are added to the 
project, they'll need to be installed via:

[cabal-sandbox]: http://coldwa.st/e/blog/2013-08-20-Cabal-sandbox.html

```
$ cabal install --dependencies-only --enable-tests
```

The `bin/setup` script is idempotent, so you can simply re-run that to 
install all of the dependencies for you if you prefer.

It is entirely possible that the introduction of a new dependency 
requires upgrades of existing dependencies. This situation can be 
difficult to manage depending on the exact circumstances and may result 
in "cabal hell".

Thanks to the introduction of sandboxes, one can always reset from 
scratch:

```
$ cabal sandbox delete
$ ./bin/setup
```

This process should always work as long as there is a valid set of 
dependency versions available.
