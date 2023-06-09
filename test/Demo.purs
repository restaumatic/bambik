module Demo where

import Prelude

import Data.Invariant.Optics (projection, propertyInvLensTagged)
import Data.Plus ((^))
import Data.String (toUpper)
import Effect (Effect)
import Type.Proxy (Proxy(..))
import Web (Component, buildMainComponent)
import Web.HTML as HTML
import Web.MDC as MDC

type Order =
      { id :: String
      , customer :: Customer
      }

type Customer =
      { firstName :: String
      , lastName :: String
      }

id = propertyInvLensTagged (Proxy :: Proxy "id")
customer = propertyInvLensTagged (Proxy :: Proxy "customer")
firstName = propertyInvLensTagged (Proxy :: Proxy "firstName")
lastName = propertyInvLensTagged (Proxy :: Proxy "lastName")
upperCase = projection toUpper

order ∷ Component Order
order =
  (
    MDC.filledText "first name" # HTML.inside "div" mempty mempty # firstName
    ^
    MDC.filledText "last name" # HTML.inside "div" mempty mempty # lastName
    ^
    HTML.text # HTML.inside "div" mempty mempty # firstName
    ^
    HTML.text # HTML.inside "div" mempty mempty # lastName
  ) # HTML.inside "div" mempty mempty # customer
  ^
  HTML.staticText "Summary: " ^ HTML.text # id ^ HTML.staticText " " ^ HTML.text # firstName # customer ^ HTML.staticText " " ^ HTML.text # upperCase # lastName # customer

main :: Effect Unit
main = do
  updateOrder <- buildMainComponent order
  updateOrder { id: "6178", customer: { firstName: "John", lastName: "Doe"}}
