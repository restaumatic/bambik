module Demo1 where

import Prelude

import Data.Invariant (class Cartesian, class Invariant)
import Data.Invariant.Optics (projection)
import Data.Invariant.Optics.Tagged (class Tagged, property)
import Data.Plus ((^))
import Data.String (toUpper)
import Effect (Effect)
import Type.Proxy (Proxy(..))
import Web (Component, inside, runMainComponent)
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
  customerComponent # inside "div" # customer
  ^
  HTML.staticText "Summary: " ^ HTML.text # id ^ HTML.staticText " " ^ HTML.text # firstName # customer ^ HTML.staticText " " ^ HTML.text # upperCase # lastName # customer

customerComponent :: Component Customer
customerComponent =
  MDC.filledText "first name" # inside "div" # firstName
  ^
  MDC.filledText "last name" # inside "div" # lastName
  ^
  HTML.text # inside "div" # firstName
  ^
  HTML.text # inside "div" # lastName

main :: Effect Unit
main = do
  updateOrder <- runMainComponent orderComponent
  updateOrder { id: "61710", customer: { firstName: "John", lastName: "Doe"}}
