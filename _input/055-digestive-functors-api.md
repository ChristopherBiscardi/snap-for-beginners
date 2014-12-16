
# Building the Digestive Functors API

The goal of form validation is twofold: Error Handling and Conversion. That is to say we want to take some data coming in as a form, make sure it is the right format and then end up with a verified Haskell data structure so we can then do whatever we need with it (ex: put it in a database).

To that end, we want our end result to be a Haskell data structure as such:

```haskell
data Tweet = Tweet { name      :: Text
                   , timestamp :: ClockTime
                   , content   :: Text
                   } deriving (Show)
```

We can see that we have upgraded our Tweet to use ClockTime. We will use this to fix the issue in the last chapter, where we generated the timestamp on the client, by generating the timestamp on the server.