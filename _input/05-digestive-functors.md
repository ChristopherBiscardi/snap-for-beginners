
# Digestive Functors

Digestive functors are one way to do form processing in Haskell. In this chapter we will build up a sample application to see how to accept and validate form input and render forms and errors with the Digestive Functors package.

Then, we'll move into a deeper exploration and examine all of the possible options digestive functors gives us.

## Building a Digestive Functors Flow

To start off, we need a scaffold. We'll create a new folder named `df-one` and create a new scaffolding app inside it:

```bash
mkdir df-one
cd df-one
snap init
```

Alternatively, check out the code in `code/digestive-functors/df-one`, which has the completed code.

Our `src/Site.hs` currently looks as such:

```haskell
{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------
import           Control.Applicative
import           Data.ByteString (ByteString)
import           Data.Maybe
import qualified Data.Text as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Heist
import qualified Heist.Interpreted as I
------------------------------------------------------------------------
import           Application


------------------------------------------------------------------------
-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = [("loginError", I.textSplice c) | c <- maybeToList authError]


------------------------------------------------------------------------
-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (redirect "/")
  where
    err = Just "Unknown user or password"


------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"


------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = registerUser "login" "password" >> redirect "/"


------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/login",    with auth handleLoginSubmit)
         , ("/logout",   with auth handleLogout)
         , ("/new_user", with auth handleNewUser)
         , ("",          serveDirectory "static")
         ]


------------------------------------------------------------------------
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

We are going to keep the handlers around so that later we can use the `auth` snaplet, which is already set up for us, to secure our form.

### Define a new Datatype: Tweet

The very first thing we need is a new datatype to define the data we will be capturing in our form. In our case, Twitter is down and the world is in a panic, so we will create a `Tweet` in a new file `src/Twitter.hs` to build a new Twitter.

We will also include some module boilerplate so we can export our awesome new datatypes and functions.

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Twitter
(Tweet
  ) where

import qualified Data.Text as T

data Tweet = Tweet {
  username :: T.Text,
  timestamp :: Int,
  content :: T.Text
} deriving (Show)
```

In this example[^4], we have created a new `Tweet` datatype using [record syntax][rsyntax]. Our `Tweet` has a field for `username` and `content`, which are both of type `T.Text`[^1], and a field for a `timestamp` which we are going to deal with as an Integer (for example, the current epoch time, for me writing this, is 1391490698).

The beginning of the file says that our module name is `Twitter` (so if we were going to import it, we would write `import Twitter`). We also export the `Tweet` data type so that after importing it (in some other file), we can use `Tweet` to create new Tweets.

Also, since we are using `Data.Text` we import it `as T`.

We can check out our fancy new datatype using `ghci src/Twitter.hs`. This starts up Haskell's interpreter for the compiler we are using (ghc). Once inside the prompt, we can run `Tweet "MyAwesomeUsername" 1234567 "42 is the answer!"` to see how a `Tweet` is constructed. It looks somthing like this:

```haskell
*Twitter> Tweet "MyAwesomeUsername" 1234567 "42 is the answer!"
Tweet {username = "MyAwesomeUsername",
       timestamp = 1234567,
       content = "42 is the answer!"}
```

`:q` will get us out of the prompt.

### Creating the Form

Now we can create our forms. Since we are using Digestive Functors with Snap and Heist, we will need a couple imports. Digestive Functors can be used in other contexts, including against JSON data.

```Haskell
import           Text.Digestive
import           Text.Digestive.Heist
import           Text.Digestive.Snap
```

Along with these imports, we need to tell cabal what packages to include when installing. Add these digestive-functor imports to `df-one.cabal`.

```Haskell
  Build-depends:
    bytestring                >= 0.9.1   && < 0.11,
    heist                     >= 0.13    && < 0.14,
    MonadCatchIO-transformers >= 0.2.1   && < 0.4,
    mtl                       >= 2       && < 3,
    snap                      >= 0.11    && < 0.13,
    snap-core                 >= 0.9     && < 0.11,
    snap-server               >= 0.9     && < 0.11,
    snap-loader-static        >= 0.9     && < 0.10,
    text                      >= 0.11    && < 0.12,
    time                      >= 1.1     && < 1.5,
    xmlhtml                   >= 0.1,
    digestive-functors        >=0.6.1    && <0.7,
    digestive-functors-snap   >= 0.6.0.0 && < 0.7.0.0,
    digestive-functors-heist  == 0.7.0.0
```

Now we are going to construct the actual form. We'll use a helper function `isNotEmpty` to check the inputs and make sure they aren't empty. `isNotEmpty` will take a `T.Text` and return a `Bool` to us.

```Haskell
isNotEmpty :: T.Text -> Bool
isNotEmpty = not . T.null
```

We will also define some error strings to display if the input isn't quite right.

```Haskell
userErrMsg :: T.Text
userErrMsg = "Username can not be empty"
tsErrMsg :: T.Text
tsErrMsg = "timestamp must be an Int"
contentErrMsg :: T.Text
contentErrMsg = "Tweet can not be empty"
```

We can then use these in our form:

```Haskell
tweetForm :: (Monad m) => Form T.Text m Tweet
tweetForm = Tweet
  <$> "username" .: check userErrMsg isNotEmpty (text Nothing)
  <*> "timestamp" .: stringRead tsErrMsg Nothing
  <*> "content"   .: check contentErrMsg isNotEmpty (text Nothing)
```

We are using a simple `check` function which takes an error string, a test function and a form to validate. This may seem a little confusing, until we examine that `text`[^2] returns a `Formlet` for us.

In the case of `text`, we can choose to specify a default value (for use as the username or content). Currently we have `Nothing`, or no default value. The alternative is `Just "sometext"`, which is a default value of `sometext`.

`stringRead` is similar to `text`, but for parseable and serializable values, such as our `Int`. `stringRead` takes an error string. After giving it an error string, the combination of `stringRead "error string"` acts exactly the same as `text`, which means we get to specify a default value. Again either `Nothing` or `Just 5`[^3].

The `<$>` and `<*>` operators come from Control.Applicative and we have to import it to use them:

```Haskell
import Control.Applicative
```

Our `Twitter.hs` now looks like this:

```Haskell
{-# LANGUAGE OverloadedStrings #-}
module Twitter
(Tweet
  ) where

import qualified Data.Text as T
import           Text.Digestive
import           Text.Digestive.Heist
import           Text.Digestive.Snap
import           Control.Applicative

data Tweet = Tweet {
  username :: T.Text,
  timestamp :: Int,
  content :: T.Text
} deriving (Show)

isNotEmpty :: T.Text -> Bool
isNotEmpty = not . T.null

userErrMsg :: T.Text
userErrMsg = "Username can not be empty"
tsErrMsg :: T.Text
tsErrMsg = "timestamp must be an Int"
contentErrMsg :: T.Text
contentErrMsg = "Tweet can not be empty"

tweetForm :: (Monad m) => Form T.Text m Tweet
tweetForm = Tweet
  <$> "username" .: check userErrMsg isNotEmpty (text Nothing)
  <*> "timestamp" .: stringRead tsErrMsg Nothing
  <*> "content" .: check contentErrMsg isNotEmpty (text Nothing)
```

In ghci, we can play around with the form a bit and see what happens.

```bash
ghci src/Twitter.hs
```

gets us into the prompt and `getForm` will give us the resulting view:

```Haskell
*Twitter> getForm "MyTweetForm" tweetForm

View "thing" [] App
  App
    Map _
      Ref "username"
        Map _
          Pure (Text "")
    Ref "timestamp"
      Map _
        Pure (Text "")
  Ref "content"
    Map _
      Pure (Text "")
 [] [] Get
```

### Building The Heist Templates

Before we can render out our form we should write a template. `Text.Digestive.Heist` gives us some splices (bits of Heist templates) that we can use to render out our form.

In a new template file at `snaplets/heist/templates/tweetform.tpl`

```Haskell
<apply template="base">
  <dfForm action="/tweet">
    <dfChildErrorList ref="" />

    <dfLabel ref="username">Username: </dfLabel>
    <dfInputText ref="username" />
    <br>

    <dfLabel ref="timestamp">Timestamp: </dfLabel>
    <dfInput ref="timestamp" type="number" min="0" step="1" pattern="\d+" />
    <br>

    <dfLabel ref="content">Content: </dfLabel>
    <dfInputTextArea ref="content" />
    <br>

    <dfInputSubmit value="Submit" />
  </dfForm>
</apply>
```

The tags that start with `df` are processed by Digestive Functors before displaying. We are using a couple different tags: `dfForm`, `dfChildErrorList`, `dfLabel`, `dfInputText`, `dfInput`, `dfInputTextArea` and `dfInputSubmit`. We will go into these a bit more at the end of this chapter, but for now the important part is `ref`, which Digestive Functors uses to identify form elements.

When rendered without errors (and a form name of "tweet", more on that in a sec), this template will look like this:

```html
<form action="/tweet" method="POST"
 enctype="application/x-www-form-urlencoded">

    <label for="tweet.username">Username: </label>
    <input type="text" id="tweet.username"
     name="tweet.username" value="">
    <br>

    <label for="tweet.timestamp">Timestamp: </label>
    <input type="number" min="0" step="1" pattern="\d+"
     id="tweet.timestamp" name="tweet.timestamp" value="">
    <br>

    <label for="tweet.content">Content: </label>
    <textarea id="tweet.content" name="tweet.content"></textarea>
    <br>

    <input value="Submit" type="submit">
</form>
```

You'll notice that the field ids and names are all namespaced by the form name (tweet). We also get a couple things for free, including encodingtype, method, some types and values.

These values will keep the values that are input if there are errors in other fields and the errors will be displayed at the top of the form.

### The FormHandler Routing Function

We can now write our Snap Form Handler:

```Haskell
tweetFormHandler :: Handler App App ()
tweetFormHandler = do
  (view, result) <- runForm "tweet" tweetForm
  case result of
    Just x  -> writeText $ T.pack $ show x
    Nothing -> heistLocal (bindDigestiveSplices view) $ render "tweetform"
```

We now need to export `tweetFormHandler` so we can use it later.

```Haskell
module Twitter
(Tweet
,tweetFormHandler  ) where
```

Remember when we used `getForm` to test our form in ghci? Well it so happens that Digestive Functors Snap has a function that will automatically choose between `getForm` and `postForm` for us based on the type of request. It is called `runForm`. In addition, runForm takes the form name that we saw in the html before ("tweet"). This name can be any string we want and our forms will automatically be namespaced by it.

Essentially what's going on here is that if we can parse a `Tweet` datatype, `result` is where it will be stored and `Just x` will match in our case statement. If we can't parse a `Tweet`, `result` will be `Nothing` and the `view` will be rendered out with our `tweetform` template. `bindDigestiveSplices` is what allows us to use the `dfInput` and other `df` tags in our html.

We also need some more imports. I've imported only what we need from Snap.Core and Snap.Snaplet.Heist to make it more obvious where these functions are coming from. In the future, to import the whole modules, you can delete the parentheses and the text inside them.

```Haskell
import           Snap.Core (writeText)
import           Snap.Snaplet
import           Snap.Snaplet.Heist (heistLocal, render)
import           Application
```

### Final Steps

Now that we have everything set up in `src/Twitter.hs` and our template written, let's go into `src/Site.hs` and create a route. First we'll add Twitter to our list of imports.

```Haskell
import           Control.Applicative
import           Data.ByteString (ByteString)
import           Data.Maybe
import qualified Data.Text as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Heist
import qualified Heist.Interpreted as I
import           Twitter
```

Now we can add our `tweetFormHandler` to our routes. Since our form is already set up to POST to /tweet, we'll use that as our route:

```Haskell
------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/logins",    with auth handleLoginSubmit)
         , ("/logout",   with auth handleLogout)
         , ("/new_user", with auth handleNewUser)
         , ("/tweet", tweetFormHandler)
         , ("",          serveDirectory "static")
         ]
```

That's it. Run `cabal install` and then we can run `df-one` to run our app.

Visit `localhost:8000/tweet` in a browser to see our form and error handling in action!

Here is our finalized `src/Twitter.hs`

```Haskell
{-# LANGUAGE OverloadedStrings #-}
module Twitter
(Tweet
,tweetFormHandler  ) where

import qualified Data.Text as T
import           Text.Digestive
import           Text.Digestive.Heist
import           Text.Digestive.Snap
import           Control.Applicative
import           Snap.Core (writeText)
import           Snap.Snaplet
import           Snap.Snaplet.Heist (heistLocal, render)
import           Application

data Tweet = Tweet {
  username :: T.Text,
  timestamp :: Int,
  content :: T.Text
} deriving (Show)

isNotEmpty :: T.Text -> Bool
isNotEmpty = not . T.null

userErrMsg :: T.Text
userErrMsg = "Username can not be empty"
tsErrMsg :: T.Text
tsErrMsg = "timestamp must be an Int"
contentErrMsg :: T.Text
contentErrMsg = "Tweet can not be empty"

tweetForm :: (Monad m) => Form T.Text m Tweet
tweetForm = Tweet
  <$> "username" .: check userErrMsg isNotEmpty (text Nothing)
  <*> "timestamp" .: stringRead tsErrMsg Nothing
  <*> "content" .: check contentErrMsg isNotEmpty (text Nothing)

tweetFormHandler :: Handler App App ()
tweetFormHandler = do
  (view, result) <- runForm "tweet" tweetForm
  case result of
    Just x  -> writeText $ T.pack $ show x
    Nothing -> heistLocal (bindDigestiveSplices view) $ render "tweetform"
```


[^1]: If you don't know what this is, it would be a good idea to look up the difference between `ByteString`, `Text` and `String` at some point.

[^2]: and a bunch of other functions we will examine later such as `bool`, `optionalText` and `utcTimeFormlet`

[^3]: More on Maybe here: http://learnyouahaskell.com/a-fistful-of-monads#getting-our-feet-wet-with-maybe

[^4]: OverloadedStrings is a commonly used language extension that makes it easier to write string literals and use them in our application.

[rsyntax]: http://learnyouahaskell.com/making-our-own-types-and-typeclasses#record-syntax
