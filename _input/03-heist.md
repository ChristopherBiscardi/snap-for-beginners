
# Heist Snaplet

The Heist Snaplet handles the rendering of templates. We will be going over Interpreted Heist in this chapter. There is another usage of Heist called Compiled Heist that is higher performance but also slightly more difficult to work with.

## Initializing the Snaplet

As we can see in `code/scaffolding`, and in the scaffolding chapter, we have to first add Heist to our app definition:

```haskell
 data App = App { _heist :: Snaplet (Heist App) }
```

Then we write a simple instance so we don't have to write `with heist` in front of all our routes that render templates:

```haskell
instance HasHeist App where
    heistLens = subSnaplet heist
```

Here we declare an instance of `HasHeist App`, which is to say that we are telling the compiler that our `App` does indeed have an instance of Heist accessible. We then define `heistLens`, which is the function that will be called to access heist from our `App`, to be `subSnaplet heist`. This is because in our initialization code (as seen in the scaffolding chapter) we define `heist` to be a `subSnaplet` of `App`.

This instance is totally optional, but if we don't write it we will have to prefix the routes that use `heist` with `with heist`.

To finish off the initialization, we will go over how we set up `heist` as a `subSnaplet` to `App`:

```haskell
appInit = makeSnaplet "app" "" Nothing $ do
     h <- nestSnaplet "heist" $ heistInit "templates"
     return $ App h
```

We can see that our app is initialized as `makeSnaplet "app" "" Nothing`, making it a Snaplet. Then, when we return our initialized `App` structure with the initialized `heist` Snaplet.

## Handler Functions

### render

`render` renders a template. It is a specialized version of `renderAs`.

#### render usage
```haskell
myhandler :: Handler App App ()
myhandler = render "mytemplate.tpl"
```

### heistLocal

`heistLocal` can be used, as seen in `code/scaffold-app/Site.hs`, to use cutomized splices for specific routes.

```haskell
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = maybe noSplices splice authError
    splice err = "loginError" ## I.textSplice err
```

`heistLocal` takes a function that modifies the Heist state (`bindSplices` does this), and a Handler to run. In this case our handler is rendering the `login.tpl` template.

### renderWithSplices

We can simplify the above code using `renderWithSplices`. `renderWithSplices` is sugar for the combination of `heistLocal`, `bindSplices` and `render` that we just used. The simplified version:

```haskell
handleLogin authError = renderWithSplices "login" errs
  where
    errs = maybe noSplices splice authError
    splice err = "loginError" ## I.textSplice err
```

`renderWithSplices` takes a template name and some splices and returns a Handler for us that will render the template with the included splices.

## Splices

Bind and Apply are the main splices that come with Heist.

### Bind

The Bind tag is used to bind a value to a tag, such as:

```html
<bind tag="postAction">/login</bind>
<bind tag="submitText">Login</bind>
```

After using the `<bind>` tag we can use the values later on in our template by using `${tag}` syntax.

```html
<form method="post" action="${postAction}">
  <input type="submit" value="${submitText}" />
</form>
```

This form with those bound tags would render as:

```html
<form method="post" action="/login">
  <input type="submit" value="Login" />
</form>
```

### Apply

The Apply tag is used to apply templates as to the current template.

Given `something.tpl` as such:

```html
<h1>In Something Template</h1>
<apply template="_something"/>
```

and a template called `_something.tpl` (the underscore is purely convention for a template we won't use when calling `render`, but is used as a sub-template) as such:

```html
<p>Content from _something template</p>
```

When we `render "something"` the output will look like:

```html
<h1>In Something Template</h1>
<p>Content from _something template</p>
```

### apply-content

`apply-content` is used to allow an `apply` tag to wrap content. For example if we have this `base.tpl`:

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

We can use it as the "container" like this:

```html
<apply template="base">
  <h1>All Your Base</h1>
</apply>
```

The rendered template will look like this:

```html
<html>
  <head>
    <title>Snap web server</title>
    <link rel="stylesheet" type="text/css" href="/screen.css"/>
  </head>
  <body>
    <div id="content">
      <h1>All Your Base</h1>
    </div>
  </body>
</html>
```
