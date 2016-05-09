{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE QuasiQuotes     #-}
module Main where

import           Data.JSString as JS
import           Data.Proxy
import           GHCJS.Types
import           Servant.API
import           Servant.GHCJS.Internal
import           Servant.GHCJS.Redirect
import           Servant.GHCJS.Router


import GHCJS.Foreign.QQ
import Control.Concurrent

printMaybePage :: Either [JSString] Page -> IO ()
printMaybePage (Right (Page v)) = v
printMaybePage (Left err) = js_log $ "Error: " `append` (intercalate ", " err)

main :: IO ()
main = do
  [js| console.log("Hello") |]
  initRouter (Proxy :: Proxy MyApi) anAPI
  let redirectBooks :<|> redirectAuthor :<|> redirectISBN = redirectClient (Proxy :: Proxy MyApi)

  redirectBooks "aTitle"


  threadDelay $ 10 ^ 6

  redirectISBN "123" "pageNumber" (Just "aJustPage")
  return ()


-- | An example API that uses Capture and QueryParam
type MyApi = "api" :> ("books" :> Capture "title" JSString :> Page
                      :<|> "author" :> Capture "author" JSString :> Page
                      :<|> "isbn" :> Capture "isbn" JSString :> Capture "page" JSString :> QueryParam "page" JSString :> Page
                      )



hashOne = HashRoute ["api","books","aTitle"] []

hashTwo = HashRoute ["api","author","anAuthor"] []

hashThree = HashRoute ["api","isbn","someISBN", "somePage"] [("page", "anotherPage")]

hashFour = HashRoute ["api","isbn","someISBN", "somePage"] []

hashFive = HashRoute ["api","isbn","someISBN"] []


-- | Stand in router
anAPI :: Router MyApi
anAPI = books :<|> author :<|> isbn
  where books title = Page $ js_log $ "Books: " `append` title
        author a = Page $ js_log $ "Author: " `append` a
        isbn i page (Just page') = Page $ js_log $ JS.concat ["ISBN: ", i, " Page: ", page, " Page': ", page']
        isbn i page _ = Page $ js_log $ "Need a page"


foreign import javascript unsafe "console.log($1)" js_log :: JSString -> IO ()