
# Hello World

## A Single Handler

The smallest possible Snap app has a single handler.

```Haskell
{-# LANGUAGE OverloadedStrings #-}

import Snap.Core
import Snap.Http.Server

main :: IO ()
main = quickHttpServe handler

handler :: Snap ()
handler = writeBS "Handled"
```

This Application responds to every request on every URL with
`"Handled"`. Let's run a ghci session to learn a bit more about what
we're doing.

Using docker on the Command Line, we can get a ghci session by running
the `snapforbeginners/basic` image interactively (`-i -t`) and mapping
the the port (`-p 8000:8000`) `8000` in the container to port `8000`
on our computer.

`snapforbeginners/basic` already has Snap installed as well as the
code we just wrote available.

```bash
docker run -i -t -p 8000:8000 snapforbeginners/basic
```

If you used the Docker Container, the output will look like this:

```bash
GHCi, version 7.8.3: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
[1 of 1] Compiling Main  ( /opt/server/Site.hs, interpreted )
Ok, modules loaded: Main.
*Main>
```

`GHCi` tells us that we are using `ghc` version `7.8.3` and that we've
loaded the `Main` module from `Site.hs`. The `Main` module is the
default module, since we didn't specify one in `Site.hs`.

Now that we have a prompt, let's take a look at some of our types with
`:t` or `:type`. Don't type the `*Main>` part since that signifies our
prompt.

```haskell
*Main> :t main
main :: IO ()
```

We can see that our `main` function is of type `IO ()` or in other
words is an `IO` action that returns `()` (pronounced
"unit"). Roughly, this means the IO action is only used for it's
effect with essentially no return value. This makes sense since
receiving and responding to http requests inherently has side effects,
*and* we have defined `main` to be of this type in `Site.hs`

Let's check `quickHttpServe`, which is more interesting since it comes
from Snap itself, specifically the [`Snap.Http.Server`][quickHttpServe] module.

```haskell
*Main> :t quickHttpServe
quickHttpServe :: Snap () -> IO ()
```

Here, we see that `quickHttpServe` takes a `Snap` action and returns
an `IO` action. Both actions are used for their side effects so we
return `()` from both.

`handler` is the Snap action we can give to `quickHttpServe`. It only
handles building the `ByteString` response to the client.

```haskell
*Main> :t handler
handler :: Snap ()
```

The last piece of our puzzle is `writeBS`. Looking at `writeBS` gives
us it's type.

```haskell
*Main> :t writeBS
writeBS :: MonadSnap m => ByteString -> m ()
```

As it turns out, `writeBS` takes a [`ByteString`][bytestring] and
returns an action that is an instance of `MonadSnap`. If we look at
the [`Snap.Core` Documentation][monadsnap] We can see the the `Snap`
datatype we've been using is in fact an instance of `MonadSnap`, so
we're good here. What `writeBS` actually does is append a ByteString
to the end of the response.

We can rewrite the `writeBS` type signature into `writeBS ::
ByteString -> Snap ()`. It isn't written this way because later on we
will use `writeBS` with other datatypes which are instances of
`MonadSnap`. This will be useful when we start talking about Snaplets.

Finally, we can run the app directly from GHCi by calling our `main`
function.

```haskell
*Main> main
no port specified, defaulting to port 8000
Listening on http://0.0.0.0:8000/
Can't open log file "log/access.log".
Can't open log file "log/error.log".
Exception: log/access.log: openFile: does not exist (No such file or
directory)
Exception: log/error.log: openFile: does not exist (No such file or
directory)
Logging to stderr instead. **THIS IS BAD, YOU OUGHT TO FIX THIS**

Logging to stderr instead. **THIS IS BAD, YOU OUGHT TO FIX THIS**

[19/Sep/2014:21:16:23 +0000] Server.httpServe: START, binding to
[http://0.0.0.0:8000/]
```

Note that because we haven't talked about logging, we are getting two
errors about not having a `log` directory. Since we don't have a
logging directory everything will log to stdout/stderr. This is ok for
now and will be covered later.

Since we're now running the app, we can hit `localhost:8000` or
`${boot2docker ip}:8000` to see our handler in action.

