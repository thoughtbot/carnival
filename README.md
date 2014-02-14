# Carnival

A disqus-like commenting service powering http://robots.thoughtbot.com.

## Getting started

Install ghc, alex, happy, cabal, and Postgresql. On OS X, the easiest way is to
install Haskell Platform using homebrew:

    $ brew install haskell-platform

Run the setup script first:

    $ bin/setup

Then boot the site:

    $ yesod devel

And run the tests:

    $ yesod test
