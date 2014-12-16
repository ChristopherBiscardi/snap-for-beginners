
# Scaffolding a New Project

[GitHub Repo][git-scaffolding]

We've created an awesome new startup called Odoo. It's a new
microblogging service. The first thing we'll do is scaffold a new Snap
app to start the platform.

Create a new directory:

```bash
mkdir oodo
```

Then enter the directory and initialize a project with the `default`
template:

```bash
cd oodo
cabal sandbox init
snap init default
```

We have initialized a sandbox as well as the project
template. Sandboxes help to separate different projects with different
dependencies from each other. It is not strictly necessary that we use
a sandbox for this project, but it is a good practice that will help
us a great deal in the future by separating the dependencies for each
project.

We now have a default Snap app with a basic user authentication
scheme. Build the app by running:

```bash
cabal build --only-dependencies
```

This uses the `odoo.cabal` file in the current directory to install
our dependencies. We can build an executable by running `cabal build`
again without the `--only-dependencies` flag:

```bash
cabal build
```

We can now run the app by with:

TODO: Check this command for accuracy

```bash
.cabal-sandbox/bin/odoo
```

The server defaults to port 8000, so by navigating to `localhost:8000` we should
see a running instance of the app. From the homepage, we can create a user and
then log in to see the demo website.

## code/odoo

The two files we are concerned with are `src/Application.hs` and
`src/Site.hs`. `src/Application.hs` includes some basic setup code for
the Authorization, Session and Heist Snaplets. We will go into more
detail with Snaplets in later chapters.

`src/Site.hs` is where most of our development will happen. It includes the
routing, initialization and some route handlers. The handlers can be split out
into other files, but we will keep them in a single file for now.

## src/Application.hs

`src/Application.hs` starts off with something that tells our compiler that we
are using an extension to the haskell language[^prag]. In this case, it is the
`TemplateHaskell` extension. This won't actually affect us much, as the only
place we use Template Haskell is in the call to `makeLenses` later in this file.

```haskell
{-# LANGUAGE TemplateHaskell #-}
```

The next bit of code defines the module for this file. We will use this in our
`src/Site.hs` to import this file. In this case, `import Application` is what
we will write.

```haskell
module Application where
```

The imports list is next and defines some of the modules we'll be using in our
code in this file. `Control.Lens` will be used as part of our call to
`makeLenses` and the rest are Snaplet modules, since we are defining some of
our Snaplet code in this file.

```haskell
import Control.Lens
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Session
```

Next is the most important part of this file, our `App` datatype[^rec]. This
defines the Snaplets we will be using as part of a data structure so that we
can initialize and access them later on in `src/Site.hs`.

We are using the Heist (`_heist`), Session (`_sess`) and Authentication
(`_auth`) Snaplets. Each comes with it's own type declaration so that we can be
assured that we are putting the right Snaplets in the right places when we
initialize our app.

```haskell
data App = App
    { _heist :: Snaplet (Heist App)
    , _sess :: Snaplet SessionManager
    , _auth :: Snaplet (AuthManager App)
    }
```

`makeLenses` is next. Basically, this automatically creates getters/setters and
some other things for us so we don't have to write a bunch of boilerplate. We
are calling it on our `App` datatype, so when we use our Snaplets in
`src/Site.hs` we can call them without the underscores in front (ie: `_heist`
becomes `heist`).

```haskell
makeLenses ''App
```

Writing an instance of `HasHeist` for our App allows us to write less
boilerplate code. If we didn't write this instance, we would have to write
`with heist doSomething` whenever we wanted to render a template. The instance
defines how to access the Heist Snaplet when we are in a route. In this case,
since `App` is also a Snaplet, we can use `subSnaplet heist`.

```haskell
instance HasHeist App where
    heistLens = subSnaplet heist
```

This is a simple alias. `AppHandler` and `Handler App App` mean exactly the same
thing. If we were writing a handler for a URL, either one of these would
be acceptable as the type signature.

```haskell
type AppHandler = Handler App App
```

## src/Site.hs

### Language Pragma

`Site.hs` starts off with an extension to the Haskell language[^prag]. This one
makes it easier to work with string literals in our source code files.
Typically, a String literal is of type `String`. Using `OverloadedStrings`
allows us to write string literals (a string literal is `"like this"`) of type
`Text` or other types that implement the `isString` type class. Their type is
determined by the function that uses them. So if we use a function that has
`myFunction :: Text -> Text` as the type signature we could use it as
`myFunction "My Awesome String (which is of type Text)"`.

```haskell
{-# LANGUAGE OverloadedStrings #-}
```

### Module Declaration and Imports

Then we declare our module (`Site`) and a few imports. This includes the
`src/Application.hs` module, which is imported as `import Application`.

```haskell
module Site
  ( app
  ) where

-------------------------------------------------------------
import           Control.Applicative
import           Data.ByteString (ByteString)
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
-------------------------------------------------------------
import           Application
```

### handleLogin

Next, we set up the rendering of the login form template (with errors).

```haskell
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = maybe noSplices splice authError
    splice err = "loginError" ## I.textSplice err
```

The type signature breaks down into two pieces split by `->`. The first:

```haskell
Maybe T.Text
```

is the type of the argument to this function. It says that we might get some
text or we might get nothing. The second type:

```haskell
Handler App (AuthManager App) ()
```

is what the function returns. In this case it returns a Snap handler that uses
the Authentication Snaplet. A basic handler (without Authentication) has the
type `Handler App App ()`.[^auththing]

The next part starts the function definition.

```haskell
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
```

`handleLogin` takes one argument, which we've named `authError`. `heistLocal` is
a function that lets us bind custom splices[^splices] to be used in the
`"login"` template and then use them.

`errs` defines our custom splice:
```haskell
errs = maybe noSplices splice authError
```

`maybe` takes a default values (`noSplices` in this case), our custom splice
(defined as `splice` on the line below) and the `authError`. If the `authError`
is `Nothing` (no errors) we use `noSplices`, otherwise we use our custom splice.

```haskell
splice err = "loginError" ## I.textSplice err
```

Here we define our splice. If the `authError` exists it gets passed to this
function as `err`. We then bind the name `"loginError"` to our `textSplice`,
which we created from the `err` text. The splice we just created displays the
error using the tag `<loginError/>` in our heist templates (specifically
`snaplets/heist/templates/_login.tpl`).

### handleLoginSubmit

`handleLoginSubmit` handles retrieving values from a login form submission using
the Authentication Snaplet's `loginUser` function.

```haskell
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (redirect "/")
  where
    err = Just "Unknown user or password"
```

`loginUser` takes the names of the username and password form fields (`"login"`
and `"password"` in our case), the "Remember Me" field (In our case,  `Nothing`
since we aren't using one), a failure function and a success function.

Our failure function is

```haskell
(\_ -> handleLogin err)
```

Which is an anonymous function that takes anything (the `_` is Haskell for "we
don't care what this argument is", in this case because we aren't using any
arguments) and returns `handleLogin` with the error value `err`.

`err` is `Just "Unknown user or password"`. We put `Just` in front of the value
because as we saw before, `handleLogin` takes `Maybe T.Text` as an argument.
The two possible values being `Nothing` and `Just "some text"`.

The success function, `(redirect "/")` simply redirects a successful login to
the homepage.

### handleLogout

`handleLogout` uses the Authentication Snaplet's `logout` function and then
redirects the user to the homepage.

```haskell
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"
```

The `>>` operator sequences the two functions, discarding any values produced
by `logout`.

### handleNewUser

`handleNewUser` splits a request into two different functions for `GET` and
`POST`.

```haskell
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = registerUser "login" "password" >> redirect "/"
```

For a `GET` request, we use `handleForm`, which just renders the `"new_user"`
template.

For a `POST` request, we use the Authentication Snaplet's `registerUser`.
`registerUser` takes the username and password fields (In our case `"login"`
and `"password"`).

### Routing

Our routes are defined next. `with auth` is how we say "this route is going to
be using the Authentication Snaplet's functions".

```haskell
routes :: [(ByteString, Handler App App ())]
routes = [ ("/login",    with auth handleLoginSubmit)
         , ("/logout",   with auth handleLogout)
         , ("/new_user", with auth handleNewUser)
         , ("",          serveDirectory "static")
         ]
```

We also serve static files from the `static` folder.

### Initialization

The most involved code is the `app` initialization code.

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

First we say that `app` will hold our initialized `App` (from
`src/Application.hs`). `makeSnaplet` takes an id (`"app"` in this case), a
description, a `Maybe (IO FilePath)` (which we'll just set to `Nothing` since
this isn't a packaged Snaplet) and an Initializer.

In this case our Initializer is our `do` statement.

Common to all of the Snaplets we are about to initialize is `nestSnaplet`.
`nestSnaplet` takes a root url for any routes defined in the Snaplet, the name
of the Snaplet as defined in `src/Application.hs` without the underscore (also
known as a Lens because we ran `makeLenses` on it), and the Snaplet specific
initializer function.

The first thing we do is initialize our Heist Snaplet.

```haskell
h <- nestSnaplet "" heist $ heistInit "templates"
```

Using a call to `nestSnaplet` we pass in: The root path for the routes (`""`),
`heist` (which is the Lens value we made from `_heist`) and the result of
`heistInit "templates"`, which is our Heist initializer. `heistInit`'s argument
is the folder that we are storing our templates in (in this case the Heist
Snaplet is located in `snaplets/heist` and our templates are in
`snaplets/heist/templates` so we pass in `"templates"`).

The next Snaplet to be initialized is the Session Snaplet. This will be used with the Authentication Snaplet to give us sessions.

```haskell
s <- nestSnaplet "sess" sess $
       initCookieSessionManager "site_key.txt" "sess" (Just 3600)
```

Once again we call `nestSnaplet` with the base route and Lens value (`sess` because we used `_sess` in `src/Application.hs`). We then initialize a Cookie-based backend with `initCookieSessionManager`.

`initCookieSessionManager` takes an encryption key (generated for us in `site_key.txt`), a name (`"sess"`) and a session timeout for replay attack protection (`Just 3600`).

The Authorization Snaplet is initialized next.

```haskell
a <- nestSnaplet "auth" auth $
       initJsonFileAuthManager defAuthSettings sess "users.json"
```

Again a call to `nestSnaplet`. The Authentication Snaplet has support for multiple backends, such as a flat json file or PostgreSQL. In this case, we initialize a JSON file with the default authentication settings (`defAuthSettings`), the Session Snaplet we just initialized (`sess`) and a filename to store the data in (`"users.json"`).

`defAuthSettings` contains a few fields:

```yaml
asMinPasswdLen = 8
asRememberCookieName = "_remember"
asRememberPeriod = Just (2*7*24*60*60) = 2 weeks
asLockout = Nothing
asSiteKey = "site_key.txt"
```

Currently, `asMinPasswdLen` is not used by the Auth Snaplet. More information about these fields is availible in the Snap docs on [Hackage][snaphack].

Finally:

```haskell
addRoutes routes
addAuthSplices h auth
return $ App h s a
```

We add our routes, add some splices from the Auth Snaplet and return an instance of the `App` definition from `src/Application.hs` that includes the heist (`h`), session (`s`) and auth (`a`) instances.

## snaplets/heist/templates/

This folder holds our Heist templates. `snaplets/heist` is the base directory for the Heist Snaplet and templates is a directory that has been created so that Heist has access to our templates.

### _login.tpl

The `_login` template is rendered as a sub-piece of the `login.tpl` template.

```html
<h1>Snap Example App Login</h1>

<p><loginError/></p>

<bind tag="postAction">/login</bind>
<bind tag="submitText">Login</bind>
<apply template="userform"/>

<p>Don't have a login yet? <a href="/new_user">Create a new user</a></p>
```

`<loginError/>` is a splice we created in `handleLogin` in our `src/Site.hs` file. The splice, as we defined it, shows the error message if it exists.

We have two `<bind>` tags next. These function a bit like defining variables and are used later on in our template. Specifically in the `userform` section specified by the apply tag below.

The next line is an `<apply>` tag. It is used to render `userform.tpl` as part of this template.

### \_new_user.tpl

```html
<h1>Register a new user</h1>

<bind tag="postAction">/new_user</bind>
<bind tag="submitText">Add User</bind>
<apply template="userform"/>
```

`_new_user.tpl` is similar to `_login.tpl`. The only difference is that the values of the `<bind>` tags are different. This shows how a template can be modified by the context in which it is rendered.

### base.tpl

`base.tpl` is the base outline of our templates. It includes all the scaffolding such as `<html>`, `<head>` and `<body>`.

```html
<html>
  <head>
    <title>Snap web server</title>
    <link rel="stylesheet" type="text/css" href="/screen.css"/>
  </head>
  <body>
    <div id="content">

      <apply-content/>

    </div>
  </body>
</html>
```

Inside of the `<div id="content">` is `<apply-content>`. This allows us to use `base.tpl` as a wrapper for whatever content we want, as we will see in `index.tpl`.

### index.tpl

The `index.tpl` template is a little more interesting. The first tag applies the base template. Anything inside the `<apply template="base">` tag will go where we wrote `<apply-content>` in `base.tpl`.

```html
<apply template="base">

  <ifLoggedIn>
    <p>
      This is a simple demo page served using
      <a href="http://snapframework.com/docs/tutorials/heist">Heist</a>
      and the <a href="http://snapframework.com/">Snap</a> web framework.
    </p>

    <p>Congrats!  You're logged in as '<loggedInUser/>'</p>

    <p><a href="/logout">Logout</a></p>
  </ifLoggedIn>

  <ifLoggedOut>
    <apply template="_login"/>
  </ifLoggedOut>

</apply>
```

`<ifLoggedIn>` is one of the Auth Splices we added in `src/Site.hs` when we initialized our app. The content inside this tag only renders if the user is logged in.

`<loggedInUser/>` is similar, but it displays the username of the logged in user.

`<ifLoggedOut>` is also an Auth Splice. It renders it's content if the user is not logged in. In this case, it renders the `_login.tpl` template.

### login.tpl

The `login.tpl` template is super simple. It applies the base template and uses `_login.tpl` as the content.

```html
<apply template="base">
  <apply template="_login"/>
</apply>
```

### new_user.tpl

The `new_user.tpl` template is very similar to `login.tpl`. It applies the base template and uses `_new_user.tpl` as the content.

```html
<apply template="base">
  <apply template="_new_user" />
</apply>
```

### userform.tpl

`userform.tpl` uses the content of the `<bind>` tags from the other templates. To access the value of the bind tag, we use `${tag}`. In the case of `postAction` it looks like `${postAction}`.

```html
<form method="post" action="${postAction}">
  <table id="info">
    <tr>
      <td>Login:</td><td><input type="text" name="login" size="20" /></td>
    </tr>
    <tr>
      <td>Password:</td><td><input type="password"
      name="password" size="20" /></td>
    </tr>
    <tr>
      <td></td>
      <td><input type="submit" value="${submitText}" /></td>
    </tr>
  </table>
</form>
```

## Fin

That's it for the default template. From here use the other chapters to learn more about various pieces of Snap. Later in the book we will go over Digestive Functors, which can be used to render and process forms with validation, and Heist, which has more splices (such as Markdown) an Interpreted and a Compiled library.

[^prag]: This is a Language Pragma. There is plenty of information on them online if you search for "haskell language pragmas".
[^rec]: The way we are writing this datatype is called "Record Syntax".
[^auththing]: More on this in the Authentication and Routing chapters.
[^splices]: More on splices in the Heist chapter

[snaphack]: http://hackage.haskell.org/package/snap-0.13.2.2/docs/Snap-Snaplet-Auth.html#t:AuthSettings
