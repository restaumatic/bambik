module MyApp (myApp) where

import Prelude ((#), ($), (+), (<<<), Unit, const, show)

import Data.Variant (match)
import Effect (Effect)
import PUI (completed, forField, mvu, updated)
import PUI.Web.HTML (body, text)
import PUI.Web.MDC2 (button, card, elevation20, headline4)
import QualifiedDo.Semigroupoid as Semigroupoid

myApp :: Effect Unit
myApp =
  body $
    elevation20 $
      card { caption: "myapp" } $ ( Semigroupoid.do
          headline4 text # forField @"count" show # completed
          button { label: "Count" } # updated (match { clicked: const <<< increment })
      ) # mvu freshCount

increment :: { count :: Int } -> { count :: Int }
increment { count } = { count: count + 1 }

freshCount :: { count :: Int }
freshCount = { count: 0 }
