{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Servant.GHCJS.Render where

import           Data.Proxy                (Proxy (..))
import           GHC.TypeLits              (KnownSymbol, symbolVal)
import           Servant.API               ((:<|>) (..), (:>), Capture,
                                            QueryParam)

import           Data.JSString
import           Servant.GHCJS.Internal
import           Servant.GHCJS.Redirect

-- | Build combinators for each route to redirect the client
-- using Hasher's setHash
renderAPI :: (HasRender route) => Proxy route -> Render route
renderAPI p = render p emptyHashRoute

-- | Class to handle rendering and changing the current
-- client side url
class HasRender route where
  type Render route :: *

  render :: Proxy route -> HashRoute -> Render route

-- | Render to a page just renders the current HashRoute
instance HasRender Page where
  type Render Page = Route

  render _ hroute = Route $ renderRoute hroute

-- | Disjoint routes create two Renders
instance (HasRender a, HasRender b) => HasRender (a :<|> b) where
  type Render (a :<|> b) = Render a :<|> Render b

  render _ hroute = render aproxy hroute :<|> render bproxy hroute
    where aproxy = Proxy :: Proxy a
          bproxy = Proxy :: Proxy b

-- | A capture requires a JSString to fill in the
-- required parameter
instance (KnownSymbol capture, HasRender subroute, ToRouteParam val)
                => HasRender (Capture capture val :> subroute) where
  type Render (Capture capture val :> subroute) = val -> Render subroute

  render _ (HashRoute paths ps) param = render subProxy (HashRoute (paths ++ [toRouteParam param]) ps)
    where subProxy = Proxy :: Proxy subroute

-- | A query param optionally sets a parameter
instance (KnownSymbol query, HasRender subroute, ToRouteParam val)
                => HasRender (QueryParam query val :> subroute) where
  type Render (QueryParam query val :> subroute) = Maybe val -> Render subroute

  render _ (HashRoute paths ps) (Just val) = render subProxy (HashRoute paths (ps ++ [(queryKey, toRouteParam val)]))
    where queryKey = pack $ symbolVal (Proxy :: Proxy query)
          subProxy = Proxy :: Proxy subroute
  render _ hroute Nothing = render subProxy hroute
    where subProxy = Proxy :: Proxy subroute

-- | A path adds the path to the end of the paths list to be rendered
instance (KnownSymbol path, HasRender subroute) => HasRender (path :> subroute) where

  type Render (path :> subroute) = Render subroute

  render _ (HashRoute paths ps) = render subProxy (HashRoute (paths ++ [nextPath]) ps)
    where nextPath = pack $ symbolVal (Proxy :: Proxy path)
          subProxy = Proxy :: Proxy subroute
