module Demo where

import Prelude

import Data.Invariant.Optics (projection, propertyInvLensTagged)
import Data.Plus ((^))
import Data.String (toUpper)
import Effect (Effect)
import Specular.Dom.Builder (runMainBuilderInBody)
import Type.Proxy (Proxy(..))
import Web (Component, buildComponent, inside, staticText, text)
import Web.MDC as MDC

type Order =
      { customer :: Customer
      }

type Customer =
      { firstName :: String
      , lastName :: String
      }

customer = propertyInvLensTagged (Proxy :: Proxy "customer")
firstName = propertyInvLensTagged (Proxy :: Proxy "firstName")
lastName = propertyInvLensTagged (Proxy :: Proxy "lastName")
upperCase = projection toUpper

app ∷ Component Order
app =
  (
    MDC.filledText "first name" # inside "div" mempty mempty # firstName
    ^
    MDC.filledText "last name" # inside "div" mempty mempty # lastName
    ^
    text # inside "div" mempty mempty # firstName
    ^
    text # inside "div" mempty mempty # lastName
  ) # inside "div" mempty mempty # customer
  ^
  staticText "Summary: " ^ text # upperCase # lastName # customer

main :: Effect Unit
main = do
  update <- runMainBuilderInBody $ buildComponent app mempty
  update $ { customer: { firstName: "Joe", lastName: "Doe"}}
  pure unit
