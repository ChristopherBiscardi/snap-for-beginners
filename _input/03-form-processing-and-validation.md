
# Form Processing and Validation

Digestive functors are one way to do form processing in Haskell. In
this chapter we will build up our Pulsar application to accept and
validate form input and render forms and errors with the Digestive
Functors package.

## Building a Digestive Functors Flow

### Creating the Form

Since we are using Digestive Functors with Snap and Heist, we will
need a couple imports. Digestive Functors can be used in other
contexts, including against JSON data.

#### src/Pulsar/Form.hs

```Haskell
import           Text.Digestive
import           Text.Digestive.Heist
import           Text.Digestive.Snap
```

Along with these imports, we need to tell cabal what packages to
include when installing. Add these digestive-functor imports to
`pulsar.cabal`.

```Haskell
Build-depends:
  bytestring                >= 0.9.1   && < 0.11,
  heist                     >= 0.14    && < 0.15,
  map-syntax                >= 0.2     && < 0.3,
  MonadCatchIO-transformers >= 0.2.1   && < 0.4,
  mtl                       >= 2       && < 3,
  snap                      >= 0.13    && < 0.15,
  snap-core                 >= 0.9     && < 0.10,
  snap-server               >= 0.9     && < 0.10,
  snap-loader-static        >= 0.9     && < 0.10,
  text                      >= 0.11    && < 1.3,
  time                      >= 1.1     && < 1.6,
  xmlhtml                   >= 0.1     && < 0.3,
  snaplet-postgresql-simple >= 0.6.0.2 && < 0.7,
  postgresql-simple         >= 0.3     && < 0.5,
  digestive-functors        >= 0.8.0.0 && < 0.9,
  digestive-functors-snap   >= 0.6.1.2 && < 0.7,
  digestive-functors-heist  >= 0.8.6.2 && < 0.9
```

Now we are going to construct the actual form. We'll use a helper
function `isNotEmpty` to check the inputs and make sure they aren't
empty. `isNotEmpty` will take a `Text` and return a `Bool` to us.

```Haskell
isNotEmpty :: Text -> Bool
isNotEmpty = not . T.null
```

We will also define some error strings to display if the input isn't
quite right.

```Haskell
blogErrMsg :: T.Text
blogErrMsg = "Microblog can not be empty"
```

We can then use these in our form:

```Haskell
blogForm :: (Monad m) => Form Text m Blog
blogForm = Blog
  <$> "blog" .: check "blog cannot be empty" isNotEmpty (text Nothing)
```

The result we want from this form is a `Blog`. The form content we
expect from the client will include a `"blog"` key, so we use the `.:`
operator to match the key with a validation function. For the
validation function, we are using a `check` function which takes an
error string, a test function and a form to validate.

It is interesting to note two of the types we are working with right
now. One is a `Form`, which we can see is the type of `blogForm`. The
other is `Formlet`, which is what `text`[^2] returns.

Starting at the beginning, `check` has the following type signature:

```
check
  :: (Monad m, Monoid v)
  => v
  -> (a -> Bool)
  -> Form v m a
  -> Form v m a
```

Where:

* `v` is an error string.
* `(a -> Bool)` is our validation function that takes a value and
  returns a True or False.
* The first `Form v m a` is a Form just like the return value
* The last `Form v m a` is a new Form created from the previous
  arguments.

If we focus on the third argument (the first `Form v m a`), we see
that it is the `(text Nothing)` in our `check` function. If we go look
up the type signature for `text`, we can see that it does not match
`Form v m a` like we might expect it to, but rather is `Formlet v m
Text`:

```
text :: (Monad m, Monoid v) => Formlet v m Text
```

This is because a `Formlet` is a `Form` with an optional default
value. If we look at the type signature for `Formlet`, we can see that
it takes a `Maybe` value and returns a `Form`:

```
type Formlet v m a = Maybe a -> Form v m a
```

In the case of `text`, we chose to specify a default value of
`Nothing`, or no default value. The alternative is `Just "sometext"`,
which is a default value of `sometext`.

Our `Forms.hs` now looks like this:

```Haskell
{-# LANGUAGE OverloadedStrings #-}
module Pulsar.Forms where

import           Data.Text      (Text)
import qualified Data.Text      as T (null)
import           Pulsar.Types   (Blog (..))
import           Text.Digestive (Form, check, text, (.:))

isNotEmpty :: Text -> Bool
isNotEmpty = not . T.null

blogErrMsg :: Text
blogErrMsg = "Microblog can not be empty"

blogForm :: (Monad m) => Form Text m Blog
blogForm = Blog
  <$> "blog" .: check blogErrMsg isNotEmpty (text Nothing)
```

### Building The Heist Templates

Before we can render out our form we should write a
template. `Text.Digestive.Heist` gives us some splices (bits of Heist
templates) that we can use to render out our form.

In a new template file at `snaplets/heist/templates/blog_form.tpl`

```Haskell
<apply template="base">
  <dfForm action="/blog">
    <dfChildErrorList ref="" />

    <dfLabel ref="blog">Blog: </dfLabel>
    <dfInputTextArea ref="blog" />
    <br>

    <dfInputSubmit value="Submit" />
  </dfForm>
</apply>
```

The tags that start with `df` are processed by Digestive Functors
before displaying. We are using a couple different tags: `dfForm`,
`dfChildErrorList`, `dfLabel`, `dfInputTextArea` and
`dfInputSubmit`. We will go into these a bit more at the end of this
chapter, but for now the important part is `ref`, which Digestive
Functors uses to identify form elements.

When rendered without errors (and a form name of "microblog", more on that
in a sec), this template will look like this:

```html
<form action="/blog" method="POST"
 enctype="application/x-www-form-urlencoded">

    <label for="microblog.blog">Blog: </label>
    <textarea id="microblog.blog" name="microblog.blog"></textarea>
    <br>

    <input value="Submit" type="submit">
</form>
```

Notice that the field ids and names are all namespaced by the form
name (`microblog`). We also get a couple things for free, including
encodingtype, method, some types and values.

These values will keep the values that are input if there are errors
in other fields and the errors will be displayed at the top of the
form.

### src/Pulsar/Handlers

#### The FormHandler Routing Function

We can now write our Snap Form Handler in `src/Pulsar/Handlers`:

```Haskell
blogFormHandler :: (HasHeist b) => Handler b v ()
blogFormHandler = do
  (view, result) <- runForm "microblog" blogForm
  case result of
    Just x  -> writeText $ T.pack $ show x
    Nothing -> heistLocal (bindDigestiveSplices view) $ render "blog_form"
```

We are already exporting everything in the `Handlers` file, so just
including the form handler should be enough.

Digestive Functors Snap has a function that will automatically choose
between `getForm` and `postForm` for us based on the type of
request. It is called `runForm`. In addition, runForm takes the form
name that we saw in the html before (`"microblog"`). This name can be
any string we want and our forms will automatically be namespaced by
it.

Essentially what's going on here is that if we can parse a `Blog`
datatype, `result` is where it will be stored and `Just x` will match
in our case statement. If we can't parse a `Blog`, `result` will be
`Nothing` and the `view` will be rendered out with our `blog_form`
template. `bindDigestiveSplices` is what allows us to use the
`dfInput` and other `df` tags in our html.

We also need some more imports added to the list:

```Haskell
import           Snap.Snaplet.Heist            (HasHeist (..), heistLocal,
                                                render)
import           Text.Digestive.Heist          (bindDigestiveSplices)
import           Text.Digestive.Snap           (runForm)
```

### Final Steps

Now that we have everything set up in `src/Forms.hs`,
`src/Handlers.hs` and our template written, let's go into
`src/Site.hs` and create a route. First we'll add `blogFormHandler` to
our list of imports.

```Haskell
import           Pulsar.Handlers                             (blogFormHandler,
                                                              getAllTest,
                                                              getOneTest,
                                                              insertTest)
```

Now we can add our `blogFormHandler` to our routes. Since our form is
already set up to POST to /blog, we'll use that as our route:

```Haskell
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/login",    with auth handleLoginSubmit)
         , ("/logout",   with auth handleLogout)
         , ("/new_user", with auth handleNewUser)
         , ("/insert", method POST $ with pg $ insertTest)
         , ("/show", method GET $ with pg $ getAllTest)
         , ("/show/one", method GET $ with pg $ getOneTest)
         , ("/blog", blogFormHandler)
         , ("",          serveDirectory "static")
         ]
```

Visit `localhost:8000/blog` in a browser to see our form and error handling in action!
