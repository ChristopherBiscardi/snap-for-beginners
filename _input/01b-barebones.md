
## Hello Barebones

* [GitHub Repo][sfb-git-barebones]
* Docker Image: `snapforbeginners/barebones`

Handling a single request is nice, but in the real world we will be
using many more. Snap comes with a couple template projects, one of
which is `barebones`.

If we run `snap init barebones`, we end up with a `src/Main.hs` that
looks very similar to this:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (writeBS "hello world") <|>
    route [ ("foo", writeBS "bar")
          , ("echo/:echoparam", echoHandler)
          ] <|>
    dir "static" (serveDirectory "/opt/server/static")

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param
```

We already know about `quickHttpServe` from last chapter. This time,
`site` is our Snap action.

`site` handles routes in a couple ways. The least efficient is `<|>`
and the most efficient is `route`. `<|>` will try whatever is on the
left, if whatever is on the left fails `<|>` will move to whatever is
on the right. This is an intuitive way to structure routes since if
route #1 fails, we move to route #2; If route #2 fails, we move to
route #3, etc.

In the above code, route #1 is `ifTop (writeBS "hello world")`. This
route will execute if the user is hitting the root URL with nothing
after it. (ex: `snapforbeginners.com/`). If that fails, `<|>` uses
`route` to try matching `/foo` and `/echo/:echoparam`. If `route`
fails to find a suitable handler, we fall down to serving static files
from `/static`. If nothing matches, an error will be served.

### route

The `route` function is interesting because it constructs a very
efficient (`O(log n)`) routing structure. This is how we will
construct the vast majority of our routes in the future.

## Exercises

1. Run the Docker Image as a container. How can we access the server
   using a web browser such as Chrome or Firefox?
2. Using the running container, try to figure out what `getParam`
   does. How might this help us build sites in the future?
3. Add a route to the site that will show an error (so that we don't
   expose handler errors to users).

Answers in the [GitHub Repo][sfb-git-barebones]
