module HelloWorld (helloWorld) where

import Prelude (($), Unit)

import Effect (Effect)
import PUI.HTML (body, staticText)

helloWorld :: Effect Unit
helloWorld = body $ staticText greeting

greeting :: String
greeting = "Hello, World!"
