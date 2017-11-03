{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Servant.GHCJS.Redirect where

import           Data.Proxy                (Proxy (..))
import           GHC.TypeLits              (KnownSymbol, symbolVal)
import           Servant.API               ((:<|>) (..), (:>), Capture,
                                            QueryParam)

import           Data.JSString             (JSString, pack)

import           Servant.GHCJS.Internal

import           GHCJS.Hasher

-- | Build combinators for each route to redirect the client
-- using Hasher's setHash
redirectClient :: (HasRedirect route) => Proxy route -> Redirect route
redirectClient p = redirect p emptyHashRoute


class ToRouteParam val where
  toRouteParam :: val -> JSString

instance ToRouteParam JSString where
  toRouteParam x = x

instance ToRouteParam Int where
  toRouteParam x = pack $ show x

instance ToRouteParam Route where
  toRouteParam (Route s) = s

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
instance (KnownSymbol capture, HasRedirect subroute, ToRouteParam val)
                => HasRedirect (Capture capture val :> subroute) where
  type Redirect (Capture capture val :> subroute) = val -> Redirect subroute

  redirect _ (HashRoute paths ps) param = redirect subProxy (HashRoute (paths ++ [toRouteParam param]) ps)
    where subProxy = Proxy :: Proxy subroute

-- | A query param optionally sets a parameter
instance (KnownSymbol query, HasRedirect subroute, ToRouteParam val)
                => HasRedirect (QueryParam query val :> subroute) where
  type Redirect (QueryParam query val :> subroute) = Maybe val -> Redirect subroute

  redirect _ (HashRoute paths ps) (Just val) = redirect subProxy (HashRoute paths (ps ++ [(queryKey, toRouteParam val)]))
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
