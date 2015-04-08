Carnival is built locally, on CI, and deployed using [halcyon][]. Therefore,
there is **no pre-requisite of a Haskell environment**, though you will need the
following tools:

- PostgreSQL
- CoffeeScript
- Heroku Toolbelt (needed for setup and deployment)
- `brew install bash coreutils` (if on OS X)

[halcyon]: https://halcyon.sh/

## Getting started

Carnival can be set up with one command:

```
$ ./bin/setup
```

How long this takes depends on a number of things. All build artifacts are
cached between anyone who builds Carnival (including CircleCI and Heroku). This
means many builds can complete in under a minute. Unfortunately, the artifacts
are specific to your platform. If you are the first person to build a given
version of Carnival on your platform, it can take closer to 45 minutes.

## Developing

Before doing anything:

```
$ eval "$(/app/halcyon/halcyon paths)"
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

Open up a REPL:

```
$ cabal exec -- ghci Model.hs
```

## Sass

- In a separate terminal, run `sass --watch sass:static/css`
- Edit files under `sass/`
- Commit both the Sass files and the generated CSS files

## Maitre-d

To use Carnival from a local instance of [maitre-d][]:

- Find the id of your development Site (it'll probably be `1`):

```
% echo "select id from site where base_url LIKE '%localhost%';" | psql carnival
 id
----
  1
(1 row)

```

- Set `CARNIVAL_HOST` for maitre-d:

```
CARNIVAL_ENABLED=true
CARNIVAL_HOST='localhost:3000/sites/1'
```

[maitre-d]: https://github.com/thoughtbot/maitre-d
