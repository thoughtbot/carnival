Enable and install the [Heroku Pipelines][heroku-pipelines] plugin:

[heroku-pipelines]: https://devcenter.heroku.com/articles/labs-pipelines

```
$ heroku labs:enable pipelines
$ heroku plugins:install git://github.com/heroku/heroku-pipeline.git
```

Deploy to **staging**:

```
$ ./bin/deploy
```

Deploy to **production**:

```
$ ./bin/deploy carnival-production
```

## Migrations

While Yesod will handle some migrations (e.g. new fields) automatically for you,
anything potentially destructive requires manual intervention. Yesod also has no
support for certain database features (e.g. custom indexes). These things need
to be done by hand at a `psql` prompt.

The approach used in this project is to write such migrations as vanilla SQL
scripts, then automatically run them as part of deployment.

The process is as follows:

- Write the needed migration under `migrations/`
- Test the migration locally: `psql carnival < migrations/foo.sql` (Note: You'll
  need to either drop/recreate your test database, or run the migration on it as
  well)
- Deploy to staging with `bin/migrate-deploy carnival-staging foo.sql`
- Deploy to production with `bin/migrate-deploy carnival-production foo.sql`
- Multiple migration files can be passed, they will be run in the order given
