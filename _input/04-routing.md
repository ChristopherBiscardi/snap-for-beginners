
# Routing

Snap allows writing routes in fairly familiar way. It then takes these routes in the `addRoutes` function and turns them into a trie that gives us O(log n) dispatching time.

## Route Definitions

The route definitions from `code/routing-app/src/Site.hs`:

```haskell
routes :: [(ByteString, Handler App App ())]
routes = [ ("/logins",    with auth handleLoginSubmit)
         , ("/logout",   with auth handleLogout)
         , ("/new_user", with auth handleNewUser)
         , ("",          serveDirectory "static")
         ]
```

which we then add in our app initialization:

```haskell
addRoutes routes
```

We can see that we define our routes with a list of tuples. Each tuple consists of a URL fragment and a function.

Similar routes are combined using Control.Applicative's Alternative class (`<|>`). To get some basic intuition for how `<|>` works, we can run some experiments in `ghci`.

Enter ghci:
```bash
ghci
```
Import `Control.Applicative`
```bash
:m Control.Applicative
```
Now we can use the `<|>` operator to test. In this example `>` is used to represent the prompt, everything after `>` is typed into ghci and content without a `>` at the beginning is the return value of the previous line.
```haskell
>Nothing <|> Just 4
Just 4
>Nothing <|> Just 4 <|> Just 5
Just 4
> Nothing <|> Nothing <|> Just 5
Just 5
>Nothing <|> Nothing <|> Nothing
Nothing
```

We use `<|>` later in this chapter to match routes based on method.

### Parameters

Parameters can be in three places: `rqQueryParams` for the query string, `rqPostParams` for `POST` bodies and `rqParams` for a union of the two previous maps.

### URL Parameters

We can also use the `:paramname` form in the route to get parameters from the URI. We'll use a sample handler to echo back the parameter in the url:

```haskell
echoHandler :: Handler App App ()
echoHandler = do
  param <- getParam "echoparam"
  maybe (writeBS "must specify echo/param in URL")
         writeBS param
```
`getParam` will get the parameter from either a GET or POST request and then we respond with either `"must specify echo/param in URL"` if there is no `param` or the value of the `param`. Here is the route we use:
```haskell
routes = [("/echo/:echoparam", echoHandler)]
```

It will be used for `/echo/something/` and for `/echo/something/many/things/` but not for `/echo/`. Both times it will respond with `"something"`.

### ifTop

We can solve this issue with `ifTop`. We can create a second route that will only respond to the base route we define, in this case `/echotwo/parameter`. `/echotwo/` and `/echotwo/something/anythinghere/` will fail.

```haskell
("/echotwo/:echoparam", ifTop echoHandler)
```

### method VERB

For additional restriction we can use `method`. `method` allows us to restrict route handlers to specific verbs, such as GET or POST.

We can define two handlers for GET and POST that simply respond with "getHandler" and "postHandler" respectively.

```haskell
getHandler :: Handler App App ()
getHandler = writeBS "getHandler"

postHandler :: Handler App App ()
postHandler = writeBS "postHandler"
```
We can then set up `method GET getHandler`, which will only run GET requests to the `getHandler` we can then chain it with `method POST postHandler` using `<|>`. Note that this will behave very similar to the example at the beginning of the chapter. Behind the scenes `method` uses `unless` from `Control.Monad` to determine whether or not to "pass" to the next handler.
```haskell
("/getorpost", method GET getHandler <|> method POST postHandler)
```

We could then run curl to test the routes:

```bash
curl localhost:8000/getorpost
```
should return `"getHandler"` while:
```bash
curl -XPOST localhost:8000/getorpost -d "stuff"
```
will return `"postHandler"`.

## Sending Data Back

There are many ways to send data in the response. A few of them are here. If we use the OverloadedStrings language pragma we can write string literals as below. If we don't we would have to write the respective pack functions for each data type.

It is important to note that `writeBS` doesn't actually write to the socket, but rather adds to the closure in the Response that *will* be called. This allows us to use multiple calls to `writeBS` in the same handler. The 1.0 release of Snap will be based on streams (using io-streams). A future version of this book will cover that.

### writeBS

Writes a ByteString back to the client.

```haskell
writeBS "data here"
```

### writeText

Writes Text back to the client.
```haskell
writeText "data here"
```

### writeJSON

`writeJSON` is from the Snap.Extras.JSON package and can be used in conjunction with Data.Aeson to more easily write JSON responses. It will set the MIME to `'application/json'` and write the given object into the response body.

If we have a custom datatype and a `ToJSON` instance from `Data.Aeson` we can use writeJSON` to send it as a JSON representation. From  `code/routing-app/src/Site.hs`:

```haskell
data Person  = Person {
  name :: String
  } deriving (Show)
instance ToJSON Person where
  toJSON (Person s) = object ["name" .= s]
```
and in our route/handler we create a new `Person` and pass it to `writeJSON`:

```haskell
("/json", writeJSON $ Person "me")
```

When we hit `http://localhost:8000/json` we should get:

```javascript
{"name":"me"}
```
