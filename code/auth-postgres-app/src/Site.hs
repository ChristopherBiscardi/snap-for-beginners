{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.State.Class
import           Data.ByteString                             (ByteString)
import qualified Data.Text                                   as T
import           Heist
import qualified Heist.Interpreted                           as I
import           Snap.Core
import           Snap.Extras.JSON
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Auth.Backends.PostgresqlSimple
import           Snap.Snaplet.Heist
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe

------------------------------------------------------------------------------
import           Application

instance HasPostgres (Handler b App) where
    getPostgresState = with db get

instance HasPostgres (Handler App (AuthManager App)) where
    getPostgresState = withTop db get
------------------------------------------------------------------------------
-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = maybe noSplices splice authError
    splice err = "loginError" ## I.textSplice err


------------------------------------------------------------------------------
-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (redirect "/")
  where
    err = Just "Unknown user or password"


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"


------------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = registerUser "login" "password" >> redirect "/"

hLogout :: Handler App App ()
hLogout = with auth handleLogout

restrictedHandler :: Handler App (AuthManager App) ()
restrictedHandler = requireUser auth noUserHandler userExistsHandler

noUserHandler :: Handler App (AuthManager App) ()
noUserHandler = writeBS "No User"

userExistsHandler :: Handler App (AuthManager App) ()
userExistsHandler = writeBS "User Exists"

getFromPostgres :: Handler App (AuthManager App) ()
getFromPostgres = do
  results <- query_ "select * from snap_auth_user"
  writeJSON (results :: [AuthUser])
------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/login",    with auth handleLoginSubmit)
         , ("/logout",   with auth handleLogout)
         , ("/new_user", with auth handleNewUser)
         , ("/hlogout", hLogout)
         , ("/restricted", with auth restrictedHandler)
         , ("/get", with auth getFromPostgres)
         , ("",          serveDirectory "static")
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    d <- nestSnaplet "db" db pgsInit
    a <- nestSnaplet "auth" auth $
           initPostgresAuth sess d
    addRoutes routes
    addAuthSplices h auth
    return $ App h s d a

