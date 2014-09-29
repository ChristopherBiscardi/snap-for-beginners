
# Starting Up

This chapter will be mostly about getting a development environment set up
and running a `Hello World` Snap application.

## Haskell

If you are new to Haskell, I highly recommend [@bitemyapp][bma]'s
[Learn Haskell][lh] repository. It contains a wealth of information and it
contains up-to-date methods for installing Haskell on various platforms.

## How to Read this Book

This book strives to place example code in your hands. First we will walk
through the construction of a sample project which looks a lot like Twitter.
This project will lead us into various topics which can be referenced in their
respective chapters. I suggest that you pick an idea such as a simple
blog, where you input some data in a form and can render posts at different
urls. Then you can use the code examples in the book to build up your
project little by little.

The chapters may depend on each other (The Authorization Snaplet references the
PostgreSQL chapter for example) and you are encouraged to bounce around while
reading the book.

## What Snap is

Snap is a web framework for the Haskell language that is most similar to Express
or Sinatra.

From [snapframework.com][snap]:

Snap is a simple web development framework for unix systems, written in the
Haskell programming language. Snap has a high level of test coverage and is
well-documented. Features include:

* A fast HTTP server library
* A sensible and clean monad for web programming
* An HTML-based templating system for generating pages

Snap also contains `snaplets`, which are modular units of stateful code that are
usable between applications. For example, there are snaplets for [heist][heist],
[authentication][auth] and [Postgres][postgres]. Snaplets also come with some
nice extras such as a unified configuration format.

One of the nice things about the Haskell web framework landscape is that many of
the components are replaceable with components from other projects. If it turns
out [Heist][heist] isn't the best solution for your templating needs it can be
replaced. We will not be going into that in this book but you can read more
about other packages on [Hackage][hackage-templates].

## Where to Get Help

There are a few places to get help or submit issues. This book is available in
source form on [github][book-github] for issues. Also useful are the official
[snap website][snap] and irc channel on [freenode][webchat] (#snapframework).

For newcomers to Haskell, there is an irc channel at #haskell-beginners on
[freenode][webchat].

## A note about Docker

Using [Docker][docker] has it's own chapter, but it's worth mentioning that each
project mentioned in this book has an associated Dockerfile and Docker Image on
the [hub][hub]. Whenever a project is mentioned, there will be a mention of the
image name (such as `biscarch/ghc-7.8.3`) and a link. This makes it easy to
`docker pull` and `docker run` the projects at different stages to see how they
work.

## Install Snap

Assuming you either have a Haskell installation, or followed [Learn Haskell][lh]
, Snap can be installed with cabal, Haskell's package manager. Open a terminal
and run:

``` {.bash}
cabal install snap
```

[heist]: https://github.com/snapframework/snap/blob/master/src/Snap/Snaplet/Heist.hs
[auth]: https://github.com/snapframework/snap/blob/master/src/Snap/Snaplet/Auth.hs
[postgres]: https://github.com/mightybyte/snaplet-postgresql-simple/
[web]: http://www.haskell.org/haskellwiki/Web/Deploy)
[ghc]: http://www.haskell.org/ghc/download_ghc_7_6_3
[snap]: http://snapframework.com/
[book-github]: https://github.com/ChristopherBiscardi/snap-for-beginners/tree/master
[vagrant-downloads]: http://www.vagrantup.com/downloads
[vbox]: https://www.virtualbox.org/wiki/Downloads
[vbguest]: https://github.com/dotless-de/vagrant-vbguest
[lh]: https://github.com/bitemyapp/learnhaskell/blob/master/README.md
[webchat]: http://webchat.freenode.net/
[bma]: https://twitter.com/bitemyapp
[hackage-templates]: https://hackage.haskell.org/packages/search?terms=templating
[docker]: https://docker.com/
[hub]: https://hub.docker.com
