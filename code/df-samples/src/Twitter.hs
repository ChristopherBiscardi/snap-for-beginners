{-# LANGUAGE OverloadedStrings #-}
module Twitter
(Tweet
,tweetFormHandler
,thingFormHandler
,thingRadioFormHandler
,runnerFormHandler
,mRunnerFormHandler
 ) where

import qualified Data.Text as T
import           Text.Digestive
import           Text.Digestive.Heist
import           Text.Digestive.Snap
import           Control.Applicative
import           Snap.Core (writeText)
import           Snap.Snaplet
import           Snap.Snaplet.Heist (heistLocal, render)
import           Application

-- Twitter
data Tweet = Tweet {
  username :: T.Text,
  timestamp :: Int,
  content :: T.Text
} deriving (Show)

isNotEmpty :: T.Text -> Bool
isNotEmpty = not . T.null

userErrMsg :: T.Text
userErrMsg = "Username can not be empty"
tsErrMsg :: T.Text
tsErrMsg = "timestamp must be an Int"
contentErrMsg :: T.Text
contentErrMsg = "Tweet can not be empty"

tweetForm :: (Monad m) => Form T.Text m Tweet
tweetForm = Tweet
  <$> "username" .: check userErrMsg isNotEmpty (text Nothing)
  <*> "timestamp" .: stringRead tsErrMsg Nothing
  <*> "content" .: check contentErrMsg isNotEmpty (text Nothing)

tweetFormHandler :: Handler App App ()
tweetFormHandler = do
  (view, result) <- runForm "tweet" tweetForm
  case result of
    Just x  -> writeText $ T.pack $ show x
    Nothing -> heistLocal (bindDigestiveSplices view) $ render "tweetform"

-- choice
data Thing = ThingOne | ThingTwo
           deriving (Show, Eq)
                    
thingForm :: (Monad m) => Form T.Text m Thing
thingForm = "thething"  .: choice [(ThingOne, "t1"),
                                   (ThingTwo, "t2")] Nothing

thingFormHandler :: Handler App App ()
thingFormHandler = do
  (view, result) <- runForm "thing" thingForm
  case result of
    Just x  -> writeText $ T.pack $ show x
    Nothing -> heistLocal (bindDigestiveSplices view) $ render "thingform"

thingRadioFormHandler :: Handler App App ()
thingRadioFormHandler = do
  (view, result) <- runForm "thing" thingForm
  case result of
    Just x  -> writeText $ T.pack $ show x
    Nothing -> heistLocal (bindDigestiveSplices view) $ render "thingradioform"

-- choiceWith (uses Thing)

thingWithForm :: (Monad m) => Form T.Text m Thing
thingWithForm = "thething" .: choiceWith [("TheFirstThing", (ThingOne, "t1")),
                                          ("TheSecondThing", (ThingTwo, "t2"))] Nothing

thingWithFormHandler :: Handler App App ()
thingWithFormHandler = do
  (view, result) <- runForm "thing" thingWithForm
  case result of
    Just x  -> writeText $ T.pack $ show x
    Nothing -> heistLocal (bindDigestiveSplices view) $ render "thingwithform"

 -- bool

data Runner = Runner {
  isRunner :: Bool,
  name :: T.Text
  } deriving (Show)

runnerForm :: (Monad m) => Form T.Text m Runner
runnerForm = Runner
  <$> "isrunner" .: bool Nothing
  <*> "name" .: text Nothing

runnerFormHandler :: Handler App App ()
runnerFormHandler = do
  (view, result) <- runForm "runner" runnerForm
  case result of
    Just x -> writeText $ T.pack $ show x
    Nothing -> heistLocal (bindDigestiveSplices view) $ render "runnerform"

-- optionalText
-- The mName field is optional.

data MRunner = MRunner {
  isMRunner :: Bool,
  mName :: Maybe T.Text
  } deriving (Show)

mRunnerForm :: (Monad m) => Form T.Text m MRunner
mRunnerForm = MRunner
  <$> "isrunner" .: bool Nothing
  <*> "name" .: optionalText Nothing

mRunnerFormHandler :: Handler App App ()
mRunnerFormHandler = do
  (view, result) <- runForm "mrunner" mRunnerForm
  case result of
    Just x -> writeText $ T.pack $ show x
    Nothing -> heistLocal (bindDigestiveSplices view) $ render "mrunnerform"
