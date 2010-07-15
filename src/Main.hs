{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Snap.Types
import Snap.Util.FileServe
import Text.Templating.Heist
import Text.Templating.Heist.TemplateDirectory
import Database.HDBC
import Database.HDBC.PostgreSQL
import Prelude hiding ((++), (.))
import Data.Monoid
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Char8 as B
import qualified Data.List as L
import Data.Maybe
import Network.Websocket
import Control.Concurrent
import Debug.Trace
import Control.Exception

import Glue
import Server

-- Some Snap helper functions:
f . g = fmap f g

x ++ y = mappend x y

pack = B.pack

unpack = B.unpack

showBS = pack . show

codeString 400 = "Bad Request"
codeString 500 = "Internal Server Error"

snapError n t = do
  putResponse $ setResponseStatus n (codeString n) emptyResponse
  writeBS $ pack t
  getResponse >>= finishWith
  return undefined

snapAssert n t x
  | x = return ()
  | True = snapError n t

snapHead n t [] = snapError n t
snapHead _ _ (x : _) = return x

snapTail n t [] = snapError n t
snapTail _ _ (_ : xs) = return xs

snapJust n t Nothing = snapError n t
snapJust _ _ (Just x) = return x

getJustParam n = getParam (pack n) >>=
  snapJust 400 ("Could not get parameter: " ++ n)

getJustParams = mapM getJustParam

getCategory = readCategory . getJustParam "ctg"

getArg = (snapTail 400 "No format specified." .) .
  break (== '.') .
  unpack . getJustParam "arg"

getCookieValue n =
  filter ((== pack n) . cookieName) .
  rqCookies . getRequest >>=
  (cookieValue .) .
  snapHead 400 ("Could not get cookie: " ++ n)

--

-- Database functions:
commafy = concat . L.intersperse ", "

tuplify xs = "(" ++ commafy xs ++ ")"

dbFunc f c xs = do
  r <- liftIO $ handleSql (return . Left) $ Right . f ?db c (map toSql xs)
  case r of
    Left e -> snapError 500 $ show e
    Right v -> return v

dbRun cmd vals = do
  r <- dbFunc run cmd vals
  snapAssert 500 "Database transaction failed." (r >= 1)
  liftIO $ commit ?db

dbQuery = dbFunc quickQuery'

dbSelect tbl x i = dbQuery
  ( "SELECT " ++ x ++ " FROM " ++ tbl ++
    " WHERE " ++ i )

dbGet tbl x id =
  snapHead 500 "Does not exist." =<<
  dbSelect tbl x "id = ?" [id]

dbInsert tbl flds = dbRun
  ( "INSERT INTO " ++ tbl ++
    tuplify flds ++ " VALUES " ++
    tuplify (map (const "?") flds) )

dbUpdate tbl flds vals id = dbRun
  ( "UPDATE " ++ tbl ++ " SET " ++
    commafy (map (++ " = ?") flds) ++
    " WHERE id = ?"
  ) (vals ++ [id])

dbDelete tbl id = dbRun
  ( "DELETE FROM " ++ tbl ++
    " WHERE id = ?"
  ) [id]

permit ctg id nick = do
  poster <- snapHead 500 "No 'poster' field." =<<
    dbGet (table ctg) "poster" id
  snapAssert 400 "Operation not permitted."
    ((ctg == User &&
      nick == id) ||
     (ctg /= User &&
      nick == fromSql poster))

identify = do
  nick <- getCookieValue "nick"
  pass <- getCookieValue "pass"
  [id, password] <- dbGet "users" "(id, password)" nick
  snapAssert 400 "Identification failed."
    (fromSql id /= nick || fromSql password /= pass)
  return nick

getTheGist = do
  ctg <- getCategory
  id <- getJustParam "id"
  nick <- identify
  return (ctg, id, nick)
--

-- Helper types:
class InDatabase c where
  table :: c -> String
  columns :: c -> [String]

data Category
  = User
  | Want
  | Have
  deriving Eq

readCategory "user" = User
readCategory "want" = Want
readCategory "have" = Have
readCategory _ = error "Bad category."

instance InDatabase Category where
  table User = "users"
  table Want = "wants"
  table Have = "haves"
  -- NOTE: One HAS to make sure that these fields
  -- match the columns described in init_db.sql
  columns User =
    [ "id"
    , "email"
    , "phone"
    , "birthday"
    , "sex"
    , "password"
    , "about"
    , "locations" --TODO: allow for location input
    ]
  columns Want =
    [ "poster"
    , "keywords"
    , "details"
    , "highprice"
    , "lowprice"
    ]
  columns Have =
    [ "poster"
    , "title"
    , "description"
    , "price"
    , "condition"
    ]
--

-- GET functions:
fetch = do
  ctg <- getCategory
  (id, fmt) <- getArg
  r <- dbGet (table ctg) "*" id
  writeBS $ showBS r -- Needs correct formatting.

fetchComments = do
  ctg <- getCategory
  id <- getJustParam "id"
  (x, fmt) <- getArg
  snapAssert 500 "This should never be printed." (x == "comments")
  r <- dbGet (table ctg) "replies" id
  writeBS $ showBS r

search = undefined
--

-- POST functions:
create = do
  ctg <- getCategory
  let flds = columns ctg
  vals <- getJustParams flds
  if ctg == User
    then dbInsert (table ctg) flds vals
    else do
      nick <- identify
      dbInsert (table ctg) ("poster" : flds) (nick : vals)
  writeBS "Created."

comment = do
  (ctg, id, nick) <- getTheGist
  content <- getJustParam "content"
  (ix :: Int) <- succ . fromSql .
    (snapHead 500 "Missing field: replies" =<<
    dbGet (table ctg) "array_length(replies, 1)" id)
  dbUpdate (table ctg)
    ["replies[" ++ show ix ++ "]"]
    [tuplify [show nick, "now", show content]] $ unpack id
  writeBS "Comment posted."
--

-- PUT functions:
edit = do
  (ctg, id, nick) <- getTheGist
  let flds = columns ctg
  vals <- getJustParams flds
  permit ctg id nick
  dbUpdate (table ctg) flds vals id
  writeBS "Edited."

editField = do
  (ctg, id, nick) <- getTheGist
  let flds = columns ctg
  fld <- unpack . getJustParam "fld"
  snapAssert 400 "Field does not exist." (elem fld flds)
  val <- getJustParam fld
  permit ctg id nick
  dbUpdate (table ctg) [fld] [val] id
  writeBS "Field edited."
--

-- DELETE functions:
remove = do
  (ctg, id, nick) <- getTheGist
  permit ctg id nick
  dbDelete (table ctg) id
  writeBS "Deleted."
--

-- The main site:
site =
  ifTop (fileServeSingle "pages/main.html") <|>
  method GET  -- Fetch.
    (route
      [ ("scripts", fileServe "scripts")
      , ("styles", fileServe "styles")
      , ("signup", fileServeSingle "pages/signup.html")
      , (":ctg/:arg", fetch)
      , (":ctg/:id/:arg", fetchComments)
      , (":ctg", search)
      ]) <|>
  method POST  -- Create.
    (route
      [ (":ctg", create)
      , (":ctg/:id", comment)
      ]) <|>
  method PUT  -- Modify.
    (route
      [ (":ctg/:id", edit)
      , (":ctg/:id/:fld", editField)
      ]) <|>
  method DELETE  -- Remove.
    (route
      [ (":ctg/:id", remove)
      ]) <|>
  writeBS "Something went wrong. The server hates you." --TODO: display our own 404 page.
--

-- The Websocket server:
wsConf = Config
  { configPort = 10000
  , configOrigins = Nothing
  , configDomains = Nothing
  , configOnOpen = wsOpen
  , configOnMessage = wsMessage
  , configOnClose = wsClose
  }

wsOpen ws = putStrLn "Websocket connection opened."

wsMessage ws msg = putStrLn ("Websocket connection received message:" ++ msg)

wsClose ws = putStrLn "Websocket connection closed."

-- Program entry point:
main = bracket
  (connectPostgreSQL "")
  disconnect $ \pdb -> do
  let ?db = pdb
  forkIO $ startServer wsConf
  quickServer site
--
