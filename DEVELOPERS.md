Carnival is built using [Stack].

You will need the following tools to develop locally:

- Stack
- PostgreSQL
- CoffeeScript
- Heroku Toolbelt (needed for setup and deployment)
- `brew install bash coreutils` (if on OS X)

[Stack]: http://docs.haskellstack.org/en/stable/

## Getting started

Carnival can be set up with one command:

```
$ ./bin/setup
```

The first time you run this command, it may take a long time. Subsequent builds
will benefit from Stack's intelligent caching and reuse.

## Developing

Run a development instance:

```
$ stack exec yesod devel
```

Run the tests:

```
$ stack test
```

Open up a REPL:

```
$ stack exec -- ghci Model.hs
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
