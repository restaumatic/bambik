module HelloWorld (helloWorld) where

import Prelude (($), Unit)

import Effect (Effect)
import PUI.Web (staticText)
import PUI.Web.HTML (body)

helloWorld :: Effect Unit
helloWorld = body $ staticText "Hello, World!"
