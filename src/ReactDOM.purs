module ReactDOM
  ( render
  , unmountComponentAtNode
  , findDOMNode
  , renderToString
  , renderToStaticMarkup
  ) where

import Control.Monad.Eff (Eff())

import Data.Function (Fn4(), runFn4)
import Data.Maybe (Maybe(..))

import DOM (DOM())
import DOM.Node.Types (Element())

import React (ReactElement, ReactElementImpl(..), ReactElementRaw(), ReactComponent(), TaggedReactElement(..))

-- | Render a React element in a document element. Returns Nothing for stateless components.
render :: forall eff. Partial => ReactElement -> Element -> Eff (dom :: DOM | eff) (Maybe ReactComponent)
render (ReactElement [StaticElement e] _) = runFn4 renderFn Nothing Just e

renderToString :: Partial => ReactElement -> String
renderToString (ReactElement [StaticElement e] _) = renderToStringImpl e

renderToStaticMarkup :: Partial => ReactElement -> String
renderToStaticMarkup (ReactElement [StaticElement e] _) = renderToStaticMarkupImpl e

foreign import renderFn
  :: forall eff. Fn4 (Maybe ReactComponent)
                     (ReactComponent -> Maybe ReactComponent)
                     ReactElementRaw
                     Element
                     (Eff (dom :: DOM | eff) (Maybe ReactComponent))

-- | Removes a mounted React element in a document element. Returns true if it was unmounted, false otherwise.
foreign import unmountComponentAtNode :: forall eff. Element -> Eff (dom :: DOM | eff) Boolean

-- | Finds the DOM node rendered by the component.
foreign import findDOMNode :: forall eff. ReactComponent -> Eff (dom :: DOM | eff) Element

-- | Render a React element as a string.
foreign import renderToStringImpl :: ReactElementRaw -> String

-- | Render a React element as static markup string without extra DOM attributes.
foreign import renderToStaticMarkupImpl :: ReactElementRaw -> String
