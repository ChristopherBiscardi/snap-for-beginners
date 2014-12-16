
# Snaplet PostgreSQL Simple

Snaplet PostgreSQL Simple offers a connection to the PostgreSQL database via the PostgreSQL Simple package.

## Basics

Before installing `snaplet-postgresql-simple` you must have postgres installed on your system. Specifically, `pg_config` must be available on your path, which can come in `postgresql-devel`, `libpq-dev` or `postgresql` depending on your operating system of choice.

### Add to .cabal

```haskell
  Build-depends:
    bytestring                >= 0.9.1   && < 0.11,
    heist                     >= 0.13    && < 0.14,
    MonadCatchIO-transformers >= 0.2.1   && < 0.4,
    mtl                       >= 2       && < 3,
    snap                      >= 0.13    && < 0.14,
    snap-core                 >= 0.9     && < 0.11,
    snap-server               >= 0.9     && < 0.11,
    snap-loader-static        >= 0.9     && < 0.10,
    text                      >= 0.11    && < 1.2,
    time                      >= 1.1     && < 1.5,
    xmlhtml                   >= 0.1,
    snaplet-postgresql-simple >= 0.4     && < 0.5
```

### Adding to App

We need to import the snaplet:

```haskell
import Snap.Snaplet.PostgresqlSimple
```

and then we can add `snaplet-postgresql-simple` to our app definition as such.

```haskell
data App = App
    { _heist :: Snaplet (Heist App)
    , _sess :: Snaplet SessionManager
    , _db :: Snaplet Postgres
    , _auth :: Snaplet (AuthManager App)
    }
```

### Initialization

We need to add Postgres to our app initialization:

```haskell
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    d <- nestSnaplet "db" db pgsInit
    a <- nestSnaplet "auth" auth $
           initJsonFileAuthManager defAuthSettings sess "users.json"
    addRoutes routes
    addAuthSplices h auth
    return $ App h s d a
```

The important parts to note are the inclusion of an additional `nestSnaplet` call for the database and the inclusion of the initialized Snaplet in the returned `App` value.

```haskell
d <- nestSnaplet "db" db pgsInit
-- and
return $ App h s d a
```

### Configuration

By default, Snaplets create their filesystem on first run of the application *if there are no files already there*. The Postgres files live in `snaplets/postgresql-simple/` and look like this by default:

```haml
host = "localhost"
port = 5432
user = "postgres"
pass = ""
db = "testdb"

# Nmuber of distinct connection pools to maintain.  The smallest acceptable
# value is 1.
numStripes = 1

# Number of seconds an unused resource is kept open.  The smallest acceptable
# value is 0.5 seconds.
idleTime = 5

# Maximum number of resources to keep open per stripe.  The smallest
# acceptable value is 1.
maxResourcesPerStripe = 20
```

Customize this file to adjust connection preferences and other options.

### Instances

Instead of typing `with db` like we do when we need to use the Auth Snaplet, we can write an instance of `HasPostgres`, just like we did with Heist. From `src/Site.hs`:

```haskell
instance HasPostgres (Handler b App) where
    getPostgresState = with db get
```

## Getting Data In

### ToRow

### ToField

## Getting Data Out


### query

We can use the already set up tables for the auth snaplet (if we are using the Postgres backend) to see an example query.

```haskell
getFromPostgres :: Handler App (AuthManager App) ()
getFromPostgres = do
    results <- query_ "select * from snap_auth_user"
    writeJSON (results :: [AuthUser])
```