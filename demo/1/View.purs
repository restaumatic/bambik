module View
  ( order
  ) where


-- commons
import Prelude hiding (div)
import Data.Lens (_Just)
import QualifiedDo.Alt as A
import QualifiedDo.Semigroup as S
import QualifiedDo.Semigroupoid as T

-- model
import ViewModel (NameInformal, Order, address, customer, delivery, dineIn, firstName, forename, formal, fulfillment, lastName, paid, payment, shortId, submitOrder, surname, table, takeaway, time, total, uniqueId)

-- ui framweork
import Widget (Widget, fixed)
import Web (Web, at', div, slot, text)
import MDC (body1, card, checkbox, containedButton, dialog, elevation20, filledTextField, headline6, radioButton, snackbar, subtitle1, subtitle2)


order :: Widget Web Order Order
order =
  elevation20 S.do
    headline6 S.do
      text # fixed "Order "
      text # shortId
    card S.do
      subtitle1 (text # fixed "Identifier")
      filledTextField { floatingLabel: (_ # fixed "Short ID") } shortId
      filledTextField { floatingLabel: (_ # fixed "Unique ID") } uniqueId
    card S.do
      subtitle1 S.do
        text # fixed "Customer"
        name # customer
    card ( S.do
      radioButton { labelContent: text # fixed "Dine in", default: { table: "1"} } dineIn
      radioButton { labelContent: text # fixed "Takeaway", default: { time: "15:30"} } takeaway
      radioButton { labelContent: text # fixed "Delivery", default: { address: "Mulholland Drive 2001, Los Angeles" } } delivery
      filledTextField { floatingLabel: (_ # fixed "Table" )} table # slot # dineIn
      filledTextField { floatingLabel: (_ # fixed "Time" )} time # slot # takeaway
      filledTextField { floatingLabel: (_ # fixed "Address" )} address # slot # delivery ) # fulfillment
    card S.do
      subtitle1 (text # fixed "Total")
      filledTextField { floatingLabel: (_ # fixed "Total" )} total
    card S.do
      checkbox { labelContent: text # fixed "Payment", default: { paid: "0" } } payment
      filledTextField { floatingLabel: (_ # fixed "Paid" )} paid # slot # _Just # payment
    card S.do
      body1 S.do
        text # fixed "Summary: Order "
        text # shortId
        text # fixed " (uniquely "
        text # uniqueId
        text # fixed ") for "
        ( S.do
          text # firstName
          text # fixed " "
          text # lastName
          ( S.do
            text # fixed " (formally "
            text # surname
            text # fixed " "
            text # forename
            text # fixed ")" ) # formal ) # customer
        text # fixed ", fulfilled as "
        ( S.do
            ( S.do
              text # fixed "dine in at table "
              text ) # table # slot # dineIn
            ( S.do
              text # fixed "takeaway at "
              text ) # time # slot # takeaway
            ( S.do
              text # fixed "delivery to "
              text ) # address # slot # delivery ) # fulfillment
        ( S.do
          text # fixed ", paid "
          text # paid ) # slot # _Just # payment
      T.do
        div
          (A.do
            containedButton { label: S.do
              text # fixed "Submit order "
              text # shortId
              text # fixed " as draft" }
            containedButton { label: S.do
              text # fixed "Submit order "
              text # shortId }) # at' "style" "display: flex; justify-content: space-between; align-items: center; width: 100%;"
        dialog { title: S.do
          text # fixed "Submit order "
          text # shortId
          text # fixed "?" } S.do
            body1 (text # fixed "Are you sure?")
            containedButton { label: text # fixed "Submit order" }
        submitOrder
        snackbar { label: S.do
          text # fixed "Order "
          text # shortId
          text # fixed " submitted"}

name :: Widget Web NameInformal NameInformal
name = S.do
  subtitle2 (text # fixed "Informal")
  filledTextField { floatingLabel: (_ # fixed "First name") } firstName
  filledTextField { floatingLabel: (_ # fixed "Last name") } lastName
  ( S.do
    subtitle2 (text # fixed "Formal")
    filledTextField { floatingLabel: (_ # fixed "Surname" )} surname
    filledTextField { floatingLabel: (_ # fixed "Forename" )} forename ) # formal
