# Carnival

A disqus-like commenting service powering http://robots.thoughtbot.com.

## Getting started

* Install ghc, alex, happy, and cabal-install.

*See [here](http://www.haskell.org/platform/mac.html).*

* Update Cabal:

```
$ cabal update
$ cabal install cabal-install
```

* Create a sandbox for the project:

```
$ cabal sandbox init
```

*Ensure `./.cabal-sandbox/bin` is in your path.*

* Install Yesod:

```
$ cabal install yesod-platform yesod-bin
```

* Setup the database:

```
$ psql
postgres=# CREATE USER carnival WITH PASSWORD 'carnival';
postgres=# CREATE DATABASE carnival;
postgres=# CREATE DATABASE carnival_test;
postgres=# GRANT ALL PRIVILEGES ON DATABASE carnival to carnival;
postgres=# GRANT ALL PRIVILEGES ON DATABASE carnival_test to carnival;
```

* Run the site:

```
$ yesod devel
```

* Run the tests:

```
$ yesod test
```
