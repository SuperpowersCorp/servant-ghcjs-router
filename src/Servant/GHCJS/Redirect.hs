{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Servant.GHCJS.Redirect where

import           Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import           Data.Proxy                (Proxy (..))
import           GHC.TypeLits              (KnownSymbol, symbolVal)
import           Servant.API               ((:<|>) (..), (:>), Capture,
                                            QueryParam)

import           Data.JSString             (cons, pack, snoc, unpack)
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal             (FromJSVal (..))
import           GHCJS.Types               (JSVal)

import           Data.JSString
import           Servant.GHCJS.Internal

import           GHCJS.Hasher

import           Data.Monoid
import           Unsafe.Coerce

-- | Build combinators for each route to redirect the client
-- using Hasher's setHash
redirectClient :: (HasRedirect route) => Proxy route -> Redirect route
redirectClient p = redirect p emptyHashRoute

-- | Class to handle redirecting and changing the current
-- client side url
class HasRedirect route where
  type Redirect route :: *

  redirect :: Proxy route -> HashRoute -> Redirect route

-- | Redirect to a page just renders the current HashRoute
instance HasRedirect Page where
  type Redirect Page = IO ()

  redirect _ hroute = hasherSetHash $ renderRoute hroute

-- | Disjoint routes create two redirects
instance (HasRedirect a, HasRedirect b) => HasRedirect (a :<|> b) where
  type Redirect (a :<|> b) = Redirect a :<|> Redirect b

  redirect _ hroute = redirect aproxy hroute :<|> redirect bproxy hroute
    where aproxy = Proxy :: Proxy a
          bproxy = Proxy :: Proxy b

-- | A capture requires a JSString to fill in the
-- required parameter
instance (KnownSymbol capture, HasRedirect subroute)
                => HasRedirect (Capture capture JSString :> subroute) where
  type Redirect (Capture capture JSString :> subroute) = JSString -> Redirect subroute

  redirect _ (HashRoute paths ps) param = redirect subProxy (HashRoute (paths ++ [param]) ps)
    where subProxy = Proxy :: Proxy subroute

-- | A query param optionally sets a parameter
instance (KnownSymbol query, HasRedirect subroute)
                => HasRedirect (QueryParam query JSString :> subroute) where
  type Redirect (QueryParam query JSString :> subroute) = Maybe JSString -> Redirect subroute

  redirect _ (HashRoute paths ps) (Just val) = redirect subProxy (HashRoute paths (ps ++ [(queryKey, val)]))
    where queryKey = pack $ symbolVal (Proxy :: Proxy query)
          subProxy = Proxy :: Proxy subroute
  redirect _ hroute Nothing = redirect subProxy hroute
    where subProxy = Proxy :: Proxy subroute

-- | A path adds the path to the end of the paths list to be rendered
instance (KnownSymbol path, HasRedirect subroute) => HasRedirect (path :> subroute) where

  type Redirect (path :> subroute) = Redirect subroute

  redirect _ (HashRoute paths ps) = redirect subProxy (HashRoute (paths ++ [nextPath]) ps)
    where nextPath = pack $ symbolVal (Proxy :: Proxy path)
          subProxy = Proxy :: Proxy subroute
