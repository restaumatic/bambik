module Demo1 (main) where

import Prelude hiding (show)

import Demo1Business
import Web

import Data.Invariant.Transformers.Scoped (Scoped(..))
import Data.Maybe (maybe)
import Data.Show as S
import Effect (Effect)
import Effect.Console (log)
import Web.MDC as MDC

main :: Effect Unit
main = runMainComponent order defaultOrder

order ∷ Component Order Order
order =
  div' (
    MDC.filledText "Id" # id) ^
  div' (
    customer # orderedBy) ^
  div' (
    MDC.checkbox # paid) ^
  (div' (
      MDC.radioButton # isDineIn ^
      text "Dine in") ^
    div' (
      MDC.radioButton # isTakeaway ^
      text "Takeaway") ^
    div' (
      MDC.radioButton # isDelivery ^
      text "Delivery")) # fulfillment ^
  div' (
    div' (
      text "Delivery details") ^
    div' (
      MDC.filledText "Address") # address) # delivery # fulfillment ^
  div' (
    text "Summary: " ^
    text # value # id ^
    text " " ^ (
      text # value # firstName ^
      text " " ^
      text # value # lastName ^ (
        text " (" ^
        text # value # forename ^
        text " " ^
        text # value # surname ^
        text ") ") # formal) # orderedBy ^
    text ", paid: " ^
    text # value # show # paid ^
    text ", fulfillment: " ^
    text # value # show # fulfillment) ^
  div' (
    MDC.button (maybe (pure unit) (\(Scoped _ a )-> log $ S.show a)) (
      text "Submit"
    ))

customer :: Component CustomerInformal CustomerInformal
customer =
  div' (
    MDC.filledText "First name" # firstName ^
    MDC.filledText "Last name" # lastName) ^
  text "or more formally:" ^
  div' (
    MDC.filledText "Forename" # forename ^
    MDC.filledText "Surename" # surname) # formal
