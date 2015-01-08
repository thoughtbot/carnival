# Carnival

A medium/disqus-like commenting service powering http://robots.thoughtbot.com.

## Credits

![thoughtbot](http://thoughtbot.com/logo.png)

Carnival is maintained and funded by [thoughtbot, inc](http://thoughtbot.com/community)

Thank you to all [the contributors](https://github.com/thoughtbot/carnival/contributors)!

## Pre-requisites

Carnival is built locally, on CI, and deployed using [halcyon][]. Therefore,
there is **no pre-requisite of a Haskell environment**, though you will need the
following tools:

- PostgreSQL
- CoffeeScript
- Heroku Toolbelt (needed for setup and deployment)
- jq (needed for deployment)

[halcyon]: https://halcyon.sh/

## Getting started

Carnival can be setup with one command:

```
$ ./bin/setup
```

How long this takes depends on a number of things. All build artifacts are
cached between anyone who builds Carnival (including Travis and Heroku). This
means many builds can complete in under a minute. Unfortunately, the artifacts
are specific to your platform. If you are the first person to build a given
version of Carnival on your platform, it can take closer to 45 minutes.

## Developing

Before doing anything:

```
$ source ./bin/halcyon-env
```

This sets the needed environment variables to use halcyon-built dependencies. If
you prefer to set these persistently from your shell profile file, that works
too.

Run a development instance:

```
$ yesod devel
```

Run the tests:

```
$ yesod test
```

Do anything else:

```
$ ghci Model.hs
```

## Deployment

Staging is the default deploy target:

```
$ ./bin/deploy
```

The production app can be passed as an argument if desired:

```
$ ./bin/deploy carnival-production
```

Read more about deploying Carnival in [Ship You A Haskell][ship-you].

[ship-you]: http://robots.thoughtbot.com/ship-you-a-haskell

## License

The names and logos for thoughtbot are trademarks of thoughtbot, inc.

Carnival is Copyright © 2014 thoughtbot, inc. It is free software, and may be
redistributed under the terms specified in the LICENSE file.
