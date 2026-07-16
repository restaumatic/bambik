-- | 7GUIs task 5: **CRUD** — a filterable name list with create, update
-- | and delete.
-- |
-- | One custom leaf (`listBox`, MDC list markup, delegated clicks) joins
-- | the standard vocabulary: the filter and name fields are `×→×` editors,
-- | the list is an editor of the `selected` field whose entries are
-- | *derived* input (`lcmap` from the whole model), `people` rides the
-- | echo wire through the merge, the three buttons are `×→+` events, and
-- | one fold (`handle`) is the entire business logic. `looped` keeps the
-- | ensemble consistent: filtering re-renders the list, mutations
-- | re-render everything.
-- |
-- | Deviation from some reference implementations: selecting an entry
-- | enables update/delete against it but does not prefill the text fields.
module Main (main) where

import Prelude

import Data.Array (deleteAt, filter, mapWithIndex, snoc, updateAt)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.Profunctor (lcmap, rmap)
import Data.Profunctor.Row.RecordToRecord (field)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant (recordToCase)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.String (joinWith, replaceAll, stripPrefix)
import Data.String (Pattern(..), Replacement(..)) as String
import Data.Variant (case_, on) as Variant
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Control.Monad.State (gets)
import MDC as MDC
import QualifiedDo.Semigroupoid as Semigroupoid
import Type.Proxy (Proxy(..))
import UI (UI, looped, silence)
import Web (Node, Web, body, staticHTML, text)

foreign import setInnerHTML :: Node -> String -> Effect Unit
foreign import onEntryClick :: Node -> (Int -> Effect Unit) -> Effect Unit

type Person = { name :: String, surname :: String }

type Model =
  { prefix :: String
  , name :: String
  , surname :: String
  , people :: Array Person
  , selected :: Maybe Int
  }

main :: Effect Unit
main = body @Unit $ MDC.elevation20 $ MDC.card { caption: Just "CRUD" } Semigroupoid.do
  lcmap (const initial) $ looped Semigroupoid.do
    RecordToRecord.do
      MDC.filledTextField @"prefix" { floatingLabel: "Filter prefix (surname)" }
      lcmap entries listBox
      MDC.filledTextField @"name" { floatingLabel: "Name" }
      MDC.filledTextField @"surname" { floatingLabel: "Surname" }
      field @"people" identity
    RecordToVariant.do
      MDC.button @"create" { label: Just "Create", icon: Nothing }
      MDC.button @"update" { label: Just "Update", icon: Nothing }
      MDC.button @"delete" { label: Just "Delete", icon: Nothing }
      (recordToCase @"state" identity :: UI Web Model [ state :: Model ])
    rmap handle identity
  silence

initial :: Model
initial =
  { prefix: ""
  , name: ""
  , surname: ""
  , people:
      [ { name: "Hans", surname: "Emil" }
      , { name: "Max", surname: "Mustermann" }
      , { name: "Roman", surname: "Tisch" }
      ]
  , selected: Nothing
  }

handle :: [ create :: Model, update :: Model, delete :: Model, state :: Model ] -> Model
handle = Variant.case_
  # Variant.on (Proxy @"state") identity
  # Variant.on (Proxy @"create") (\m ->
      m { people = snoc m.people { name: m.name, surname: m.surname } })
  # Variant.on (Proxy @"update") (\m -> case m.selected of
      Just i -> m { people = fromMaybe m.people (updateAt i { name: m.name, surname: m.surname } m.people) }
      Nothing -> m)
  # Variant.on (Proxy @"delete") (\m -> case m.selected of
      Just i -> m { people = fromMaybe m.people (deleteAt i m.people), selected = Nothing }
      Nothing -> m)

-- the list's derived input: absolute keys survive filtering
entries :: Model -> { entries :: Array { key :: Int, label :: String }, selected :: Maybe Int }
entries m =
  { entries: m.people
      # mapWithIndex (\i p -> { key: i, label: p.surname <> ", " <> p.name, surname: p.surname })
      # filter (\e -> hasPrefix m.prefix e.surname)
      # map (\e -> { key: e.key, label: e.label })
  , selected: m.selected
  }
  where
  hasPrefix p s = case stripPrefix (String.Pattern p) s of
    Just _ -> true
    Nothing -> false

-- | The custom leaf: an MDC list rendering derived entries, editing the
-- | `selected` field. Echoes on `toUser` (the leaf protocol), emits on
-- | delegated entry clicks.
listBox :: UI Web { entries :: Array { key :: Int, label :: String }, selected :: Maybe Int } { selected :: Maybe Int }
listBox = wrap do
  _ <- unwrap (staticHTML """<ul class="mdc-deprecated-list" style="border: 1px solid #ccc; min-height: 120px; max-height: 200px; overflow-y: auto;"></ul>""")
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new Nothing
  lastRef <- liftEffect $ Ref.new { entries: [], selected: Nothing }
  let render st = setInnerHTML node (joinWith "" (st.entries <#> \e ->
        "<li class=\"mdc-deprecated-list-item" <> (if st.selected == Just e.key then " mdc-deprecated-list-item--selected" else "") <> "\" style=\"cursor: pointer;\" data-key=\"" <> show e.key <> "\">"
          <> escape e.label <> "</li>"))
  pure
    { toUser: \st -> do
        Ref.write st lastRef
        render st
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> void $ prop { selected: st.selected }
    , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        onEntryClick node \key -> do
          st <- Ref.read lastRef
          let st' = st { selected = Just key }
          Ref.write st' lastRef
          render st'
          void $ prop { selected: Just key }
    }

escape :: String -> String
escape s = replaceAll (String.Pattern "<") (String.Replacement "&lt;")
  (replaceAll (String.Pattern "&") (String.Replacement "&amp;") s)
