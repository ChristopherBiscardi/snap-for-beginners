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
import           Data.Aeson
import           Data.ByteString                             (ByteString)
import           Data.Text                                   (Text)
import qualified Data.Text                                   as T
import           Heist
import qualified Heist.Interpreted                           as I
import           Snap.Core
import           Snap.Extras.JSON
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
------------------------------------------------------------------------------
import           Application

instance HasPostgres (Handler b App) where
    getPostgresState = with db get


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

data Name = Name { name :: Text } deriving Show

instance FromRow Name where
  fromRow = Name <$> field

instance ToJSON Name where
  toJSON (Name n) = object ["name" .= n]

-- nameSplice :: Name -> I.Splice
-- nameSplice aName = I.runChildrenWith [("nameOfPerson", name aName)]

insertHandler :: Handler App App ()
insertHandler = do
  p <- getPostParam "thing"
  maybe (writeBS "no value") (writeBS) p

allNames :: HasPostgres m => m [Name]
allNames = query_ "SELECT * FROM names"

selectHandler :: Handler App App ()
selectHandler = do
  --n <- query_ "SELECT 2 + 2"
  --writeJSON (n :: Int)
  d <- allNames
  writeJSON d
  -- renderWithSplices "names" users
  --   where
  --     users = maybe noSplices splice
  --     splice = "names" ## (I.mapSplices nSplice d)
  --     nSplice aName = I.runChildrenWith [("nameOfPerson", name aName)]

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/login",    with auth handleLoginSubmit)
         , ("/logout",   with auth handleLogout)
         , ("/new_user", with auth handleNewUser)
         , ("/insert", ifTop $ method POST insertHandler)
         , ("/select", ifTop $ method GET selectHandler)
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
           initJsonFileAuthManager defAuthSettings sess "users.json"
    addRoutes routes
    addAuthSplices h auth
    return $ App h s d a

