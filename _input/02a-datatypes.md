
# Connecting a Database (Postgres)

[GitHub][sfb-git-odoo-postgres]

To build out Oodo, we'll need to connect a database. For this we'll
use PostgreSQL; a free, open source, relational database.

For more detailed information on `snaplet-postgres-simple`, refer to
the Postgres chapter later in this book.

## Quick Run

```
git clone git@github.com:snapforbeginners/odoo-postgres.git
cd oodo-postgres
docker-compose build
docker-compose up -d pg
docker-compose up oodo
```

## libpq-dev

We'll have to install `pg_config`, which comes as part of `libpq-dev`
on debian. You can see this in the repo's Dockerfile as:

```bash
apt-get install libpq-dev
```

## oodo.cabal

We'll need to add a snaplet to our `oodo.cabal` file:
`snaplet-postgres-simple`.

```haskell
Build-depends:
  snaplet-postgresql-simple >= 1.0
```

## src/Application.hs
in `src/Application.hs` add the import for the Postgres Snaplet:

```haskell
import Snap.Snaplet.PostgresqlSimple
```

and add a `Snaplet Postgres` to our `App` datatype:

```haskell
data App = App
    { _heist :: Snaplet (Heist App)
    , _sess :: Snaplet SessionManager
    , _auth :: Snaplet (AuthManager App)
    , _pg :: Snaplet Postgres
    }
```

## src/Site.hs

We'll need a couple imports again in `src/Site.hs`. One is the
Postgres Snaplet and the other is the Auth Backend adapter for the
Postgres Snaplet:

```haskell
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Auth.Backends.PostgresqlSimple
```

We can also *remove* the import for the JSON File Auth Backend, since
we'll be replacing it with Postgres. Remove this line:

```haskell
import           Snap.Snaplet.Auth.Backends.JsonFile
```

In Snap, we must do our Snaplet Initialization in a `SnapletInit`
function. Ours currently looks like this:

```haskell
------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
          initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    
    -- NOTE: We're using initJsonFileAuthManager here because it's easy and
    -- doesn't require any kind of database server to run.  In practice,
    -- you'll probably want to change this to a more robust auth backend.
    a <- nestSnaplet "auth" auth $
          initJsonFileAuthManager defAuthSettings sess "users.json"
    addRoutes routes
    addAuthSplices h auth
    return $ App h s a
```

We are going to delete the comment about using
`initJsonFileAuthManager`, add the Postgres Snaplet initialization and
initialize the Postgres Auth Backend.

Here are the two lines we're going to end up adding:

```haskell
d <- nestSnaplet "db" pg pgsInit
a <- nestSnaplet "auth" auth $ initPostgresAuth sess d
```

and here is the completed code:

```haskell
------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
          initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    
    d <- nestSnaplet "db" pg pgsInit
    a <- nestSnaplet "auth" auth $ initPostgresAuth sess d
    addRoutes routes
    addAuthSplices h auth
    return $ App h s a d
```

### A bit more

The two main things we did are

1. Add the Postgres Snaplet Initialization Code.

Put simply, Our `App` datatype is also a Snaplet so when initializing
Postgres, we use [`nestSnaplet`][nestSnaplet].

```haskell
d <- nestSnaplet "db" pg pgsInit
```

The type signature for `nestSnaplet` will help us understand what
we're doing:

```haskell
nestSnaplet
  :: ByteString
  -> SnapletLens v v1
  -> SnapletInit b v1
  -> Initializer b v (Snaplet v1)
```

The first argument, `ByteString`, is a String-like datatype that is
the root url that all of the Snaplet's routes will use. With the
help of `{-# LANGUAGE OverloadedStrings #-}`, we can treat it exactly
as we would a normal String literal. In our case, we used `"db"`. If
the Postgres Snaplet provided any routes, they would all be prefixed
by `db`. For Example: `"/a-postgres-route"` would become
`"/db/a-postgres-route"`.

The next argument is `SnapletLens v v1`. Luckily, we already created
those by including the Snaplets in our App datatype in
`src/Application.hs`. The `makeLenses` function in
`src/Application.hs` makes `_pg` into `pg`. `pg` is a `SnapletLens`,
so we can use it in our Initializer.

The third argument is `SnapletInit b v1`. Once again, we can find this
function provided for us in `Snap.Snaplet.PostgresqlSimple`. It turns
out that there are two functions we can use to fufill this type;
[`pgsInit`][pgsInit] and `pgsInit'`. The only difference is that
`pgsInit'` takes a [configuration][PGSConfig]. The type signature for
`pgsInit` is `SnapletInit b Postgres`. Since we need a `SnapletInit b
v1`, the `v1` type variable was left to our discretion and `Postgres`
fills in nicely for what we want.

All of this gives us back a `Initializer b v (Snaplet v1)`. We need an
Initializer to run the initialization for our Snaplet, so this is
perfect. Since we're working inside `SnapletInit`, we can pull out the
`Snaplet v1` (also known as `Snaplet Postgres`) and store the result in
`d`.

2. Add the Postgres Auth Backend init code.

```haskell
a <- nestSnaplet "auth" auth $ initPostgresAuth sess d
```

Since we just went over `nestSnaplet`, the only difference here is
that we're using [`initPostgresAuth`][initPostgresAuth]. The type
signature for `initPostgresAuth` takes two arguments and returns us a
`SnapletInit`, which is precisely what `pgsInit` did for us before.

```haskell
initPostgresAuth
  :: SnapletLens b SessionManager
  -> Snaplet Postgres
  -> SnapletInit b (AuthManager b)
```

The first argument, `SnapletLens b SessionManager`, is something we
already have. When we set up our App datatype in `src/Application.hs`,
we included the session Snaplet as `_sess`. `sess`, as a result of
`makeLenses` in `src/Application.hs`, is a `SnapletLens b
SessionManager`.

The second argument is a `Snaplet Postgres`. We also have this on
hand, since we just stored it in `d`!

3. Initialize the App

We also need to use the initalized Postgres Snaplet (`d`), when we
create an `App`. In `src/Application.hs`, we said we would include the
Postgres Snaplet last, so that's what we'll do:

```haskell
return $ App h s a d
```

## Configuring Postgres

Snaplet configuration is stored in the `snaplets` folder. Since we'll
be using docker to run, and link to, Postgres, we'll set up some
environment variables in our development config.

Create a `devel.config` for our development environment postgres configuration:

```bash
mkdir snaplets/postgresql-simple
touch snaplets/postgresql-simple/devel.cfg
$EDITOR snaplets/postgresql-simple/devel.cfg
```

This is the information we'll use. Our Postgres server is running in
trust mode locally, so we'll ignore the password field and take the
connection details from our environment using `$(ENV_VAR)` syntax. We
also connect to the `testdb` database.

```
host = "$(POSTGRES_PORT_5432_TCP_ADDR)"
port = "$(POSTGRES_PORT_5432_TCP_PORT)"
user = "postgres"

db = "testdb"

# Nmuber of distinct connection pools to maintain.  The smallest
acceptable
# value is 1.
numStripes = 1

# Number of seconds an unused resource is kept open.  The smallest
acceptable
# value is 0.5 seconds.
idleTime = 5

# Maximum number of resources to keep open per stripe.  The smallest
# acceptable value is 1.
maxResourcesPerStripe = 20
```

We are using the Docker Official Image for Postgres and the
configuration (set up the database, user, etc) can be found in
`postgres-container/`.

To run Postgres (and our Snap server), we can use
`docker-compose`. The `docker-compose.yml` file can be found in the
[GitHub Repo][sfb-git-odoo-postgres].

```bash
docker-compose up -d pg
```

If we check `docker-compose logs` we will see something like this the
first time we run our Postgres server.

```
pg_1   | The files belonging to this database system will be owned by
user "postgres".
pg_1   | This user must also own the server process.
pg_1   |
pg_1   | The database cluster will be initialized with locale
"en_US.utf8".
pg_1   | The default database encoding has accordingly been set to
"UTF8".
pg_1   | The default text search configuration will be set to
"english".
pg_1   |
pg_1   | Data page checksums are disabled.
pg_1   |
pg_1   | fixing permissions on existing directory
/var/lib/postgresql/data ... ok
pg_1   | creating subdirectories ... ok
pg_1   | selecting default max_connections ... 100
pg_1   | selecting default shared_buffers ... 128MB
pg_1   | selecting dynamic shared memory implementation ... posix
pg_1   | creating configuration files ... ok
pg_1   | creating template1 database in
/var/lib/postgresql/data/base/1 ... ok
pg_1   | initializing pg_authid ... ok
pg_1   | initializing dependencies ... ok
pg_1   | creating system views ... ok
pg_1   | loading system objects' descriptions ... ok
pg_1   | creating collations ... ok
pg_1   | ok
pg_1   | creating dictionaries ... ok
pg_1   | setting privileges on built-in objects ... ok
```

Now that the Postgres server is running, let's boot up oodo:

```bash
docker-compose up odoo
```

The logs will stream to `stdout` this time, since we didn't run the
container with `-d`. The logs will look like this:

```
oodo_1 | no port specified, defaulting to port 8000
oodo_1 | Initializing app @ /
oodo_1 | Initializing heist @ /
oodo_1 | ...loaded 7 templates from /opt/odoo/snaplets/heist/templates
oodo_1 | Initializing CookieSession @ /sess
oodo_1 | Initializing postgresql-simple @ /db
oodo_1 | Initializing postgresql-auth @ /auth
oodo_1 | ...setting up filesystem
oodo_1 |
oodo_1 | Listening on http://0.0.0.0:8000
```

## Fin

We can now access the site on `localhost:8000` in a browser. The Snap
server is now using Postgres to store user signup information.

To prove this, first sign up for an account on the website.

Then we'll run `docker ps` to find the name (or id) of our postgres
container. The output here is truncated for brevity.

```bash
> docker ps
CONTAINER ID   IMAGE                            NAMES
54450fff52fc   connectingpostgres_oodo:latest   connectingpostgres_oodo_1
f1c4956192d2   connectingpostgres_pg:latest     connectingpostgres_pg_1
```

we can grab the name (or `CONTAINER ID`) of our Postgres container, in
this case, `connectingpostgres_pg_1`, and exec into it:

```bash
docker exec -it connectingpostgres_pg_1 bash
```

Once we're in, we can run `psql` to connect to the database:

```bash
psql -h "$POSTGRES_PORT_5432_TCP_ADDR" \
  -p "$POSTGRES_PORT_5432_TCP_PORT" -U postgres
```

We can list the available databases with `\l`, and connect with
`\connect`:

```bash
postgres=# \l
  Name    |  Owner   | Encoding...
----------+----------+-----------
testdb    | postgres | UTF8
postgres=# \connect testdb
You are now connected to database "testdb" as user "postgres".
```

Once connected to `testdb`, we can list the relations with `\d`:

```bash
postgres=# \d
List of relations
Schema |          Name          |   Type   |  Owner
-------+------------------------+----------+----------
public | snap_auth_user         | table    | postgres
public | snap_auth_user_uid_seq | sequence | postgres
(2 rows)
```

and search through the `snap_auth_user` relation with some SQL:

```bash
postgres=# SELECT uid,login FROM snap_auth_user;
uid |  login
-----+----------
1 | biscarch
(1 row)
```

That's it! In this chapter we've:

1. Connected our application to Postgres
2. Swapped out our JSON users file for a Postgres Database
3. Ran our Application (and database) using docker-compose
4. Examined the results of our handiwork in the database by hand
