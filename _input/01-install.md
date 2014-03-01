
# Getting Started

This chapter will be about getting started with Snap. We will begin by giving
some backround and progress through a working scaffolding app. A passing
familiarity with Haskell development and the command line is assumed, but a
brief rundown is given for those that do not have the background.

## How to Read this Book

This book strives to place example code in your hands rather than
lead you through a project. I suggest that you pick an idea such as a simple
blog, where you input some data in a form and can render posts at different
urls. Then you can use the code examples in the book to build up your
project little by little.

The chapters may depend on each other (The Authorization Snaplet references the PostgreSQL chapter for example) and you are encouraged to bounce around while reading the book.

## What Snap is

Snap is a web framework for the Haskell language that is most similar to Express
or Sinatra. As contrast, Yesod (Another Haskell framework), can be viewed as a
more Rails-like framework.

From SnapFramework.com:

Snap is a simple web development framework for unix systems, written in the
Haskell programming language. Snap has a high level of test coverage and is
well-documented. Features include:

* A fast HTTP server library
* A sensible and clean monad for web programming
* An HTML-based templating system for generating pages

Snap also contains `snaplets`, which are modular units of stateful code that are
usable between applications. For example, there are snaplets for [heist][heist],
[authentication][auth] and [Postgres][postgres]. Snaplets also come with some nice extras such as a unified configuration format.

One of the nice things about the Haskell web framework landscape is that many of
the components are replaceable with components from other projects. We will not
be going into that in this book but you can read more [here][web]

## Where to Get Help

There are a few places to get help or submit issues. This book is available in
source form on [github][book-github] for issues. Also useful are the official snap
[website][snap] and irc channel (#snapframework).

## Install Haskell

By far the easiest way to install Haskell is the Haskell Platform, which is availible from [http://www.haskell.org/platform/][hsplat]. Some package managers also include haskell-platform as well, including homebrew and apt-get. This book is written using the 2013.2.0.0 Haskell Platform, which includes GHC 7.6.3.

## A Note on Sandboxes

It is a good idea to separate Haskell projects from each other if there are multiple Haskell projects on a machine. This makes it easier to manage dependencies and enables a simpler workflow.

There are a few options for creating environments, we will go over `cabal sandbox`.

After installing Haskell Platform, we need to upgrade cabal to ~v1.18. Run:

```bash
cabal update
cabal install cabal-install
```

Then you will see something like this at the end:

```bash
Installing executable(s) in
/Library/Haskell/ghc-7.6.3/lib/cabal-install-1.18.0.2/bin
Installed cabal-install-1.18.0.2
Updating documentation index /Library/Haskell/doc/index.html
```

According to the code we just ran, the updated `cabal` is in `/Library/Haskell/ghc-7.6.3/lib/cabal-install-1.18.0.2/bin` so we need to add that to our path:

```bash
export PATH="/Library/Haskell/ghc-7.6.3/lib/cabal-install-1.18.0.2/bin:$PATH"
```

Note that this path will be different based on your system.

### cabal sandbox

Now that we have a recent cabal version, we can use `cabal sandbox`. For a deeper understanding of `cabal sandbox`, refer to this [post](http://coldwa.st/e/blog/2013-08-20-Cabal-sandbox.html).

```bash
cabal sandbox init
```

will create a directory and a file in the current directory that will hold packages and other settings for the sandbox.

```bash
.cabal-sandbox
cabal.sandbox.config
```

Now, when we run `cabal install` for this project, the executable will be in `.cabal-sandbox/bin/`. For example, if we are in `code/heist-app` (one of the sample projects that comes with the book) and we created a sandbox in `heist-app`, we can install and run the executable without affecting our other projects as such:

```bash
cd code/heist-app
cabal sandbox init
cabal install
.cabal-sandbox/bin/heist-app
```
which will result in

```bash
Initializing app @ /
Initializing heist @ /
...loaded 7 templates from /heist-app/snaplets/heist/templates
Initializing CookieSession @ /sess
Initializing JsonFileAuthManager @ /auth

Listening on http://0.0.0.0:8000/
```


## Install Snap

Installing Snap is easy because of cabal, Haskell's package manager. Just run:

``` {.bash}
cabal install snap
```

[heist]: https://github.com/snapframework/snap/blob/master/src/Snap/Snaplet/Heist.hs "Heist"
[auth]: https://github.com/snapframework/snap/blob/master/src/Snap/Snaplet/Auth.hs
[postgres]: https://github.com/mightybyte/snaplet-postgresql-simple/
[web]: http://www.haskell.org/haskellwiki/Web/Deploy)
[hsenv]: https://github.com/Paczesiowa/hsenv
[hsplat]: http://www.haskell.org/platform/
[ghc]: http://www.haskell.org/ghc/download_ghc_7_6_3
[snap]: http://snapframework.com/
[book-github]: https://github.com/ChristopherBiscardi/snap-for-beginners/tree/master
[vagrant-downloads]: http://www.vagrantup.com/downloads
[vbox]: https://www.virtualbox.org/wiki/Downloads
[vbguest]: https://github.com/dotless-de/vagrant-vbguest
