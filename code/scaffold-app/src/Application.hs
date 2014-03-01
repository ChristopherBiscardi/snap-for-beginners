{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Control.Lens
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Session

------------------------------------------------------------------------------
-- After we call `makeLenses` below, we will be able to refer to these
-- snaplets by their names with the underscores removed. Such as `with auth` 
-- in the `routes` function in Site.hs
data App = App
    { _heist :: Snaplet (Heist App)
    , _sess :: Snaplet SessionManager
    , _auth :: Snaplet (AuthManager App)
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist


------------------------------------------------------------------------------
-- an alias so that we could write our routes with a type of `AppHandler` 
-- instead of `Handler App App`
type AppHandler = Handler App App


