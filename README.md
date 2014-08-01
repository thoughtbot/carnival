# Carnival

A medium/disqus-like commenting service powering http://robots.thoughtbot.com.

## Credits

![thoughtbot](http://thoughtbot.com/logo.png)

Carnival is maintained and funded by [thoughtbot, inc](http://thoughtbot.com/community)

Thank you to all [the contributors](https://github.com/thoughtbot/paperclip/contributors)!

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

## Managing Dependencies

To mitigate so-called *cabal hell*, we're using two features available in recent
versions of Cabal: sandboxes and freezing.

### Sandbox

Sandboxing means that all packages will be installed into a project-local
sandbox. This prevents working on multiple projects with conflicting
dependencies from causing issues.

The `bin/setup` script will create a [cabal sandbox][cabal-sandbox] and install
current dependencies into it. If new dependencies are added to the project,
they'll need to be installed via:

[cabal-sandbox]: http://coldwa.st/e/blog/2013-08-20-Cabal-sandbox.html

```
$ cabal install --dependencies-only --enable-tests
```

The `bin/setup` script is idempotent, so you can also re-run that to install the
new dependencies.

If you run into dependency issues, you can always reset from scratch:

```
$ cabal sandbox delete
$ ./bin/setup
```

This process should always work as long as there is a valid set of dependency
versions available.

### Freeze

Freezing means that after dependencies are resolved, exact versions for all
packages are written to a `config.cabal` file. This ensures that all developers,
the CI server, and deployments will use the *exact* same versions of all
dependencies.

There is one notable exception: `base`.

This package represents GHC itself and enforcing all developers to use the same
exact version it too strict. For this reason, after running `cabal freeze`, we
manually replace the exactly specified `base` version in `cabal.config` with a
more lenient constraint.

If you need to re-run `cabal freeze` be sure to retain the more lenient
constraint on `base`.

## License

The names and logos for thoughtbot are trademarks of thoughtbot, inc.

Carnival is Copyright © 2014 thoughtbot, inc. It is free software, and may be
redistributed under the terms specified in the LICENSE file.
