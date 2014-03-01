
# Authentication Snaplet

The Auth Snaplet handles user signup, login and route restriction. This chapter uses code from `code/auth-app`.

## Basics

### Adding to App Definition

Simply add `_auth` with a type of `Snaplet (AuthManager App)`, we also need the Session Snaplet so we'll add that too. The heist snaplet is not strictly necessary, but we will use it to render splices from the Auth Snaplet.

```haskell
data App = App
    { _heist :: Snaplet (Heist App)
    , _sess :: Snaplet SessionManager
    , _auth :: Snaplet (AuthManager App)
    }
```

### Initialization

First we will initialize the Session Snaplet, then use the initialized Session Snaplet to initialize the Authentication Snaplet.

```haskell
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    a <- nestSnaplet "auth" auth $
           initJsonFileAuthManager defAuthSettings sess "users.json"
    addRoutes routes
    addAuthSplices h auth
    return $ App h s a
```

### Adding Auth to Routes

To use auth-specific functions in routes we use `with`:

```haskell
  ("/login",    with auth handleLoginSubmit)
, ("/logout",   with auth handleLogout)
, ("/new_user", with auth handleNewUser)
```

### Handler Type

`with auth` takes a handler with a slightly different signature as an argument and returns a handler of the normal `Handler App App ()` type. This means that the `handle*` functions in the example above are of this type:

```haskell
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"
```

We could rewrite the `"/logout"` handler to make this a bit more clear. We will add a new route `"/hlogout"`, split out `with auth handleLogout` into it's own function (with type signature) and use the same `handleLogout` function to see the difference in handler types.

```haskell
, ("/hlogout",   hLogout)
```

```haskell
hLogout :: Handler App App ()
hLogout = with auth handleLogout

handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"
```
If we look at our `App` declaration in `code/auth-app/src/Application.hs` we can see that the new type signature for our handlers includes the type of our Auth Snaplet:

```haskell
, _auth :: Snaplet (AuthManager App)
```

## Backends

Backends for the Authentication Snaplet are pluggable. Some of the current options include a flat JSON file and PostgreSQL.

### JSON File

The default backend (given when you run `snap init`) is a flat JSON file. It is useful for examining how the system works, but should be replaced by the PostgreSQL backend or another database in production. One reason for this is that the users are stored in a flat file and this can cause issues.

#### Init with JSON

To initialize Auth with a JSON backend we will need to add the following import.

```haskell
import           Snap.Snaplet.Auth.Backends.JsonFile
```
Then we can use `initJsonFileAuthManager` to create the Auth backend inside of our app init code:

```haskell
s <- nestSnaplet "sess" sess $
       initCookieSessionManager "site_key.txt" "sess" (Just 3600)
a <- nestSnaplet "auth" auth $
       initJsonFileAuthManager defAuthSettings sess "users.json"
```

Remember that `nestSnaplet` takes a `ByteString` (the name of our snaplet), a Lensed Snaplet value (the ones we created when we ran `mkLenses` in `Application.hs`), and an init function.

`initJsonFileAuthManager` takes an `AuthSettings`, the Lensed Session Snaplet and the filepath we want to use to store the users.

### PostgreSQL

PostgresSQL is one of the other backends available. It is more robust than the JSON file. The Postgres Chapter has more information on configuration.

#### snaplet-postgresql-simple

Add this to `Build-depends` in our `.cabal` file.

```haskell
snaplet-postgresql-simple >= 0.4     && < 0.5
```

#### Adding to App Definition

We need to import the snaplet in `Application.hs`:

```haskell
import Snap.Snaplet.PostgresqlSimple
```

Then we can add `snaplet-postgresql-simple` to our app definition as such.

```haskell
data App = App
    { _heist :: Snaplet (Heist App)
    , _sess :: Snaplet SessionManager
    , _db :: Snaplet Postgres
    , _auth :: Snaplet (AuthManager App)
    }
```

#### Initializing the Backend

In `Site.hs` we will add a few imports.

```haskell
import           Snap.Snaplet.Auth.Backends.PostgresqlSimple
import           Snap.Snaplet.PostgresqlSimple
```

Then we can initialize the database with `pgsInit` and the backend as part of the Auth initialization.

```haskell
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    d <- nestSnaplet "db" db pgsInit
    a <- nestSnaplet "auth" auth $
           initPostgresAuth sess d
    addRoutes routes
    addAuthSplices h auth
    return $ App h s d a
```

#### Instances

After setting up the initialization we can write an instance that is much like our regular instance:

```haskell
instance HasPostgres (Handler b App) where
    getPostgresState = with db get
```

The new instance will be used inside of handlers with Auth type signatures.

```haskell
instance HasPostgres (Handler App (AuthManager App)) where
    getPostgresState = withTop db get
```

These instances wil need a `{-# LANGUAGE FlexibleInstances #-}` declaration at the top of `Site.hs`.

## Restricted Routes

To restrict a route to only logged in users, we can use `requireUser`. First we'll add a route at `/restricted` that uses the auth snaplet:

```haskell
("/restricted", with auth restrictedHandler)
```

Then we'll write the handler with the auth snaplet in the type signature and a call to `requireUser`. `requireUser` takes a lensed auth snaplet value, such as `auth`, a handler to execute if there is no user logged in and a handler to execute if there is a user logged in.

```haskell
restrictedHandler :: Handler App (AuthManager App) ()
restrictedHandler = requireUser auth noUserHandler userExistsHandler
```

We'll write each of these handlers as a simple ByteString response:

```haskell
noUserHandler :: Handler App (AuthManager App) ()
noUserHandler = writeBS "No User"

userExistsHandler :: Handler App (AuthManager App) ()
userExistsHandler = writeBS "User Exists"
```

Note that `requireUser` just checks to see if there is a user_id in the session. This means there is no database cost.
