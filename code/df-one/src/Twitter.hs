{-# LANGUAGE OverloadedStrings #-}
module Twitter
(Tweet
,tweetFormHandler  ) where

import qualified Data.Text as T
import           Text.Digestive
import           Text.Digestive.Heist
import           Text.Digestive.Snap
import           Control.Applicative
import           Snap.Core (writeText)
import           Snap.Snaplet
import           Snap.Snaplet.Heist (heistLocal, render)
import           Application

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
