{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Happstack.Lite
import Happstack.Server.Auth (basicAuth)
import Happstack.Server.Monads (require)
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value, onclick)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5.Attributes as A
import Database.PostgreSQL.Simple
import Data.Text (Text, concat)
import Data.Text.Lazy (unpack)
import Data.Map (fromList)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.ByteString.UTF8 (fromString)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

main :: IO ()
main = do
  putStrLn "starting admin server"
  connectionString <- readFile "../connection_string.txt"
  conn <- connectPostgreSQL . fromString . trim $ connectionString
  serve (Just ServerConfig{port=3000,ramQuota=1*10^6,diskQuota=20*10^6,tmpDir="/tmp"}) $ myApp conn

get_username :: Connection -> IO (Maybe String)
get_username conn = do
  [Only username] <- query_ conn "select username from users where uid = 0"
  return $ Just username

get_password :: Connection -> IO (Maybe String)
get_password conn = do
  [Only password] <- query_ conn "select password from users where uid = 0"
  return $ Just password

update_username :: Connection -> String -> IO (Maybe ())
update_username conn new_name = do
  execute conn "update users set username = ? where uid = 0" [new_name]
  return $ Just ()
update_password :: Connection -> String -> IO (Maybe ())
update_password conn new_pw = do
  execute conn "update users set password = ? where uid = 0" [new_pw]
  return $ Just ()

myApp :: Connection -> ServerPart Response
myApp conn =
    msum [ dir "auth-change" $ do
            username <- require (get_username conn) return
            password <- require (get_password conn) return
            basicAuth "admin.librorumadyton.net" (fromList [(username,password)]) $ authChange conn
         , dir "static"    $ serveDirectory EnableBrowsing [] "../static"
         , do
            username <- require (get_username conn) return
            password <- require (get_password conn) return
            basicAuth "admin.librorumadyton.net" (fromList [(username,password)]) $ homePage username
    ]

template :: Text -> Html -> Response
template title body = toResponse $
  H.html $ do
    H.head $ do
      H.link ! A.rel "stylesheet" ! href "static/css/main.css"
      H.title . toHtml $ title
    H.body $ do
      H.table ! A.id "nav-bar" $ do
        H.tr $ do
          H.td $ a ! href "/"             $ "home"
          H.td $ a ! href "/auth-change"  $ "change credentials"
          H.td $ a ! href "https://www.librorumadyton.net/"  $ "public page"
      H.div ! A.id "body-div" $ do
        body

homePage :: String -> ServerPart Response
homePage admin_name =
  ok $ template "admin page" $ do
    H.h1 $ "Welcome, " >> H.string admin_name

authChange :: Connection -> ServerPart Response
authChange conn = msum [ viewForm, processForm conn ]
  where
    viewForm :: ServerPart Response
    viewForm = do
      method GET
      ok $ template "update auth" $
        form ! action "/auth-change" ! enctype "multipart/form-data" ! A.method "POST" $ do
        H.div $ do
          label ! A.for "username" $ "username"
          input ! type_ "text" ! A.id "username" ! name "username"
        H.div $ do
          label ! A.for "password" $ "password"
          input ! type_ "password" ! A.id "password" ! name "password"
        H.div $ do
          input ! type_ "submit" ! value "Update"

    processForm :: Connection -> ServerPart Response
    processForm conn = do
      method POST
      username <- lookText "username"
      password <- lookText "password"
      require (update_username conn (unpack username)) return
      require (update_password conn (unpack password)) return
      ok $ template "Updated" $ do
        H.p "credentials updated"
