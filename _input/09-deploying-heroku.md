
# Deploying to Heroku

Quickly deploying to Heroku is fairly simple. We simply need to use a build-pack that has been prepared by the Haskell community.

## Procfile

First, we need to put a `Procfile` in the root of our project; Save this as `Procfile`

```
web: cabal run -- -p $PORT
```

## Build Pack

It is preferable to have a git repo initialized before creating the app with the build pack.

This command will create a new Heroku app with a Haskell buildpack. You can find more information on the buildpack <a href"https://github.com/ChristopherBiscardi/heroku-buildpack-ghc">here</a>

```bash
heroku create --stack=cedar --buildpack https://github.com/ChristopherBiscardi/heroku-buildpack-ghc.git
```
## Pushing

If you had a git repo initialized before running `heroku create`, you now have a `heroku` remote. Just push to Heroku and watch it build.

```bash
git push heroku
```
