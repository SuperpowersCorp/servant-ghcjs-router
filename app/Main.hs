{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Main where

import           Data.JSString
import           Data.Proxy
import           GHCJS.Types
import           Servant.API
import           Servant.GHCJS.Internal
import           Servant.GHCJS.Redirect
import           Servant.GHCJS.Router


printMaybePage :: Either [JSString] Page -> IO ()
printMaybePage (Right (Page v)) = js_log v
printMaybePage (Left err) = js_log $ "Error: " `append` (intercalate ", " err)

main :: IO ()
main = do
  initRouter (Proxy :: Proxy MyApi) anAPI
  let redirectBooks :<|> redirectAuthor :<|> redirectISBN = redirectClient (Proxy :: Proxy MyApi)

  redirectBooks "aTitle"
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
  where books title = Page $ "Books: " `append` title
        author a = Page $ "Author: " `append` a
        isbn i page (Just page') = Page $ i `append` page `append` page'
        isbn i page _ = Page "Need a page"


foreign import javascript unsafe "console.log($1)" js_log :: JSString -> IO ()