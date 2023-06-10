module Demo1 where

import Prelude

import Data.Invariant (class Cartesian, class Invariant)
import Data.Invariant.Optics (class Tagged, projection, property)
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

id :: forall i a b . Invariant i => Cartesian i => Tagged i => i a → i { id ∷ a | b }
id = property (Proxy :: Proxy "id")

customer :: forall i a b . Invariant i => Cartesian i => Tagged i => i a → i { customer ∷ a | b }
customer = property (Proxy :: Proxy "customer")

firstName :: forall i a b . Invariant i => Cartesian i => Tagged i => i a → i { firstName ∷ a | b }
firstName = property (Proxy :: Proxy "firstName")

lastName :: forall i a b . Invariant i => Cartesian i => Tagged i => i a → i { lastName ∷ a | b }
lastName = property (Proxy :: Proxy "lastName")

upperCase :: forall i . Invariant i => Cartesian i => i String → i String
upperCase = projection toUpper

--

orderComponent ∷ Component Order
orderComponent =
  customerComponent # HTML.inside "div" # customer
  ^
  HTML.staticText "Summary: " ^ HTML.text # id ^ HTML.staticText " " ^ HTML.text # firstName # customer ^ HTML.staticText " " ^ HTML.text # upperCase # lastName # customer

customerComponent :: Component Customer
customerComponent =
  MDC.filledText "first name" # HTML.inside "div" # firstName
  ^
  MDC.filledText "last name" # HTML.inside "div" # lastName
  ^
  HTML.text # HTML.inside "div" # firstName
  ^
  HTML.text # HTML.inside "div" # lastName

main :: Effect Unit
main = do
  updateOrder <- buildMainComponent orderComponent
  updateOrder { id: "61710", customer: { firstName: "John", lastName: "Doe"}}
