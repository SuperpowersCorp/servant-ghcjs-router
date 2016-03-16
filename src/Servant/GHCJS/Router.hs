{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Servant.GHCJS.Router where

import           Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import           Data.Proxy                (Proxy (..))
import           GHC.TypeLits              (KnownSymbol, symbolVal)
import           Servant.API               ((:<|>) (..), (:>), Capture,
                                            QueryParam)

import           Data.JSString             as JS (concat, cons, pack, snoc,
                                                  unpack)
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal             (FromJSVal (..))
import           GHCJS.Types               (JSVal)

import           Data.JSString
import           Servant.GHCJS.Internal


import           Data.Monoid
import           Unsafe.Coerce

import           GHCJS.Hasher

import           Control.Concurrent
import           Control.Monad


initRouter :: (HasRouter route) => Proxy route -> Router route -> IO ()
initRouter proxy router = do
  let runHasher new = do
        case runHashRouter (route proxy router) (parseHashRoute (unsafeCoerce new)) of
          Left errs -> putStrLn . unpack . JS.concat $ errs
          Right (Page suc) -> putStrLn . unpack $ JS.concat ["Success: ", suc]
  onChange <- syncCallback2 ContinueAsync (\x _-> runHasher x)
  onChange' <- syncCallback1 ContinueAsync runHasher
  hasherAddOnChange onChange
  hasherOnInitialized onChange'
  hasherInit
  return ()


foreign import javascript unsafe "window[$1] = $2" export :: JSString -> Callback a -> IO ()

data Router' =
    WithParams ([(JSString, JSString)] -> Router')
  | NextIs JSString (Router')
  | WithNext (JSString -> Router')
  | Choice Router' Router'
  | LeafRouter Page


runHashRouter :: Router' -> HashRoute -> Either [JSString] Page
runHashRouter (WithParams f) hr = runHashRouter (f $ hashParams hr) hr
runHashRouter (WithNext f) (HashRoute (p:ps) params) = runHashRouter (f p) $ HashRoute ps params
runHashRouter (Choice r1 r2) h = tryEither (runHashRouter r1 h) (runHashRouter r2 h)
runHashRouter (LeafRouter p) (HashRoute [] _) = Right p
runHashRouter (NextIs path next) (HashRoute (p:ps) params) =
    if path == p
      then runHashRouter next $ HashRoute ps params
      else Left $ [path `append` " != " `append` p]
runHashRouter _ _ = Left ["Empty case?"]


tryEither :: Either [a] b -> Either [a] b -> Either [a] b
tryEither (Right x) _ = Right x
tryEither _ (Right y) = Right y
tryEither (Left err1) (Left err2) = Left $ err1 ++ err2



-- | Handle creating a router for a single page app
class HasRouter route where
  type RouterT route (m :: * -> *) :: *

  route :: Proxy route -> Router route -> Router'

-- | Default Router type
type Router route = RouterT route IO

-- | Base case for the router
instance HasRouter Page where
  type RouterT Page m = Page

  route _ p = LeafRouter p

-- | Build a router for disjoint routes
-- NOTE - Can be optimized on some cases
instance (HasRouter a, HasRouter b) => HasRouter (a :<|> b) where
  type RouterT (a :<|> b) m = RouterT a m :<|> RouterT b m

  route _ (arouter :<|> brouter) = Choice (route aproxy arouter) (route bproxy brouter)
    where aproxy = Proxy :: Proxy a
          bproxy = Proxy :: Proxy b

-- | Build a router to handle the 'capture' of some param ie
-- "v1" :> Capture "name" JSString :> Page would match for all {str}
-- v1/{str} and pass the string in to the rendering function
instance (KnownSymbol capture, HasRouter subroute)
                => HasRouter (Capture capture JSString :> subroute) where
  type RouterT (Capture capture JSString :> subroute) m = JSString -> RouterT subroute m

  route _ router = WithNext (\next -> route subProxy (router next))
    where subProxy = Proxy :: Proxy subroute

-- | Build a router to add query parameters that are optional
-- these are looked up and aren't require for the function to be ran
instance (KnownSymbol query, HasRouter subroute)
                => HasRouter (QueryParam query JSString :> subroute) where
  type RouterT (QueryParam query JSString :> subroute) m = Maybe JSString -> RouterT subroute m

  route _ router = WithParams (route subProxy . router')
    where queryKey = pack $ symbolVal (Proxy :: Proxy query)
          router' params = router (lookup queryKey params)
          subProxy = Proxy :: Proxy subroute

-- | Build a router to handle paths and match
-- them exactly
instance (KnownSymbol path, HasRouter subroute) => HasRouter (path :> subroute) where
  type RouterT (path :> subroute) m = RouterT subroute m

  route _ subrouter = NextIs nextPath $ route subProxy subrouter
    where nextPath = pack $ symbolVal (Proxy :: Proxy path)
          subProxy = Proxy :: Proxy subroute

