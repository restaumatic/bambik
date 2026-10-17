module Cells (Cell, Sheet, Value, columnHeaders, commit, edit, orderSheet, sheetRows) where

import Prelude (bind, identity, map, max, min, mod, not, otherwise, pure, show, (&&), (*), (+), (-), (/), (<#>), (<$>), (<=), (<>), (==), (>=), (||))

import Data.Array (concatMap, range, snoc)
import Data.Char (fromCharCode, toCharCode)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Int (fromString, round, toNumber) as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number (fromString)
import Data.String.CodeUnits (charAt, drop, dropWhile, length, singleton, take, takeWhile)
import Data.Variant (match)
import Foreign.Object (Object, delete, empty, fromHomogeneous, insert, keys, lookup, member)

type Sheet =
  { editing :: [ cell :: { name :: String }, none :: {} ]
  , formulas :: Object String
  , values :: Object Value
  }

type Cell =
  { "Formula" :: String
  , key :: String
  , mode :: [ editing :: {}, shown :: {} ]
  , text :: String
  }

type Value = [ errV :: String, numV :: Number, textV :: String ]

orderSheet :: Sheet
orderSheet = { formulas, values: reevaluate formulas empty (keys formulas), editing: .none {} }
  where
  formulas = fromHomogeneous
    { "A0": "Item",     "B0": "Price", "C0": "Qty", "D0": "Total"
    , "A1": "Espresso", "B1": "2.5",   "C1": "2",   "D1": "=B1*C1"
    , "A2": "Cake",     "B2": "4",     "C2": "1",   "D2": "=B2*C2"
    , "A3": "Sum",                                  "D3": "=SUM(D1:D2)"
    }

cols :: Int
cols = 26

rows :: Int
rows = 100

colName :: Int -> String
colName c = fromMaybe "" (singleton <$> fromCharCode (toCharCode 'A' + c))

refKey :: forall r1. { c :: Int, r :: Int | r1 } -> String
refKey { c, r } = colName c <> show r

columnHeaders :: Array { key :: String, text :: String }
columnHeaders = [ { key: "corner", text: "" } ] <> (range 0 (cols - 1) <#> \c -> { key: colName c, text: colName c })

sheetRows :: Sheet -> Array { cells :: Array Cell, rowKey :: String }
sheetRows { formulas, values, editing } = range 0 (rows - 1) <#> \r -> { rowKey: show r, cells: cellsOfRow r }
  where
  cellsOfRow r = range 0 (cols - 1) <#> \c -> cellAt (refKey { c, r })
  cellAt key =
    { key
    , text: maybe "" display (lookup key values)
    , "Formula": fromMaybe "" (lookup key formulas)
    , mode: match { cell: \{ name } -> if name == key then .editing {} else .shown {}, none: \_ -> .shown {} } editing
    }

edit :: { event :: String, model :: Sheet } -> Sheet
edit { event: key, model: m } = m { editing = .cell { name: key } }

commit :: { event :: Cell, model :: Sheet } -> Sheet
commit { event: { key, "Formula": formula }, model: m@{ formulas, values } }
  | fromMaybe "" (lookup key formulas) == formula = m { editing = .none {} }
  | otherwise =
      let edited = if formula == "" then delete key formulas else insert key formula formulas
      in m { formulas = edited, values = reevaluate edited values (dependentsOf edited key), editing = .none {} }

-- the cell and every cell that depends on it, directly or through others
dependentsOf :: Object String -> String -> Array String
dependentsOf formulas start = go [] [ start ]
  where
  referrers key = concatMap (\k -> if refersTo k key then [ k ] else []) (keys formulas)
  refersTo k key = case lookup k formulas of
    Just src -> elemOf key (references src)
    Nothing -> false
  go seen pending = case pending of
    [] -> seen
    _ -> let next = concatMap (\k -> if elemOf k seen then [] else [ k ]) pending
             seen' = seen <> next
         in if next == [] then seen else go seen' (concatMap referrers next)

-- re-evaluate the given cells, each after the cells it reads; every other
-- value is kept as it was
reevaluate :: Object String -> Object Value -> Array String -> Object Value
reevaluate formulas old scope = (foldl evalOne { values: cleared, evaluated: empty } walk.order).values
  where
  inScope = foldl (\acc k -> insert k {} acc) empty scope
  cleared = foldl (\acc k -> delete k acc) old scope
  walk = foldl (visit formulas inScope) { done: empty, active: empty, order: [], cyclic: empty } scope
  evalOne acc key =
    let lookupRef k
          | member k walk.cyclic = .errV "#CYCLE"
          | member k inScope && not (member k acc.evaluated) = .errV "#CYCLE"
          | otherwise = fromMaybe (.textV "") (lookup k acc.values)
        value = if member key walk.cyclic then .errV "#CYCLE" else evalSource lookupRef (lookup key formulas)
    in { values: insert key value acc.values, evaluated: insert key {} acc.evaluated }

type Walk = { active :: Object {}, cyclic :: Object {}, done :: Object {}, order :: Array String }

visit :: Object String -> Object {} -> Walk -> String -> Walk
visit formulas scope w key
  | member key w.done = w
  | member key w.active = w { cyclic = insert key {} w.cyclic }
  | not (member key scope) = w
  | otherwise =
      let entered = w { active = insert key {} w.active }
          walked = foldl (visit formulas scope) entered (fromMaybe [] (references <$> lookup key formulas))
      in walked { active = delete key walked.active, done = insert key {} walked.done, order = snoc walked.order key }

elemOf :: String -> Array String -> Boolean
elemOf x = foldl (\found y -> found || y == x) false

references :: String -> Array String
references src = case charAt 0 src of
  Just '=' -> case parseExpr (drop 1 src) of
    Just { val } -> refsOf val
    Nothing -> []
  _ -> []
  where
  refsOf = case _ of
    Num _ -> []
    Ref key -> [ key ]
    Bin _ l r -> refsOf l <> refsOf r
    Sum { from, to } -> do
      c <- range (min from.c to.c) (max from.c to.c)
      r <- range (min from.r to.r) (max from.r to.r)
      pure (refKey { c, r })

display :: Value -> String
display = match { numV: formatNum, textV: identity, errV: identity }

formatNum :: Number -> String
formatNum n =
  let scaled = Int.round (n * 100.0)
  in if scaled `mod` 100 == 0
    then show (scaled / 100)
    else show (Int.toNumber scaled / 100.0)

evalSource :: (String -> Value) -> Maybe String -> Value
evalSource valueOf = case _ of
  Nothing -> .textV ""
  Just src -> case charAt 0 src of
    Just '=' -> case parseExpr (drop 1 src) of
      Just { val, rest } | length (skipSpace rest) == 0 -> evalExpr valueOf val
      _ -> .errV "#PARSE"
    _ -> case fromString src of
      Just n -> .numV n
      Nothing -> .textV src

numOf :: (String -> Value) -> String -> Either String Number
numOf valueOf key = case valueOf key of
  .numV n -> Right n
  .textV "" -> Right 0.0
  .textV _ -> Left "#REF!"
  .errV e -> Left e
  _ -> Left "#REF!"

type Coord = { c :: Int, r :: Int }

type Parsed = { val :: Expr, rest :: String }

data Expr
  = Num Number
  | Ref String
  | Bin Char Expr Expr
  | Sum { from :: Coord, to :: Coord }

skipSpace :: String -> String
skipSpace = dropWhile (_ == ' ')

parseExpr :: String -> Maybe Parsed
parseExpr s0 = do
  { val: t, rest } <- parseTerm (skipSpace s0)
  chain t rest
  where
  chain acc s = case charAt 0 (skipSpace s) of
    Just op | op == '+' || op == '-' -> case parseTerm (drop 1 (skipSpace s)) of
      Just { val, rest } -> chain (Bin op acc val) rest
      Nothing -> Nothing
    _ -> Just { val: acc, rest: s }

parseTerm :: String -> Maybe Parsed
parseTerm s0 = do
  { val: f, rest } <- parseFactor (skipSpace s0)
  chain f rest
  where
  chain acc s = case charAt 0 (skipSpace s) of
    Just op | op == '*' || op == '/' -> case parseFactor (drop 1 (skipSpace s)) of
      Just { val, rest } -> chain (Bin op acc val) rest
      Nothing -> Nothing
    _ -> Just { val: acc, rest: s }

parseFactor :: String -> Maybe Parsed
parseFactor s0 =
  let s = skipSpace s0
  in case charAt 0 s of
    Just '(' -> do
      { val, rest } <- parseExpr (drop 1 s)
      case charAt 0 (skipSpace rest) of
        Just ')' -> Just { val, rest: drop 1 (skipSpace rest) }
        _ -> Nothing
    Just c
      | isDigit c || c == '.' -> parseNumber s
      | c == 'S' && take 4 s == "SUM(" -> do
        { val: from, rest: r1 } <- parseRef (drop 4 s)
        case charAt 0 r1 of
          Just ':' -> do
            { val: to, rest: r2 } <- parseRef (drop 1 r1)
            case charAt 0 r2 of
              Just ')' -> Just { val: Sum { from, to }, rest: drop 1 r2 }
              _ -> Nothing
          _ -> Nothing
      | isUpper c -> map (\{ val, rest } -> { val: Ref (refKey val), rest }) (parseRef s)
    _ -> Nothing

isDigit :: Char -> Boolean
isDigit c = c >= '0' && c <= '9'

isUpper :: Char -> Boolean
isUpper c = c >= 'A' && c <= 'Z'

parseNumber :: String -> Maybe Parsed
parseNumber s =
  let digits = takeWhile (\c -> isDigit c || c == '.') s
  in case fromString digits of
    Just n -> Just { val: Num n, rest: drop (length digits) s }
    Nothing -> Nothing

parseRef :: String -> Maybe { val :: Coord, rest :: String }
parseRef s = case charAt 0 s of
  Just col | isUpper col ->
    let digits = takeWhile isDigit (drop 1 s)
    in case Int.fromString digits of
      Just r -> Just { val: { c: toCharCode col - toCharCode 'A', r }, rest: drop (1 + length digits) s }
      Nothing -> Nothing
  _ -> Nothing

evalExpr :: (String -> Value) -> Expr -> Value
evalExpr valueOf = go
  where
  go (Num n) = .numV n
  go (Ref key) = case numOf valueOf key of
    Right n -> .numV n
    Left e -> .errV e
  go (Bin op l r) = case go l, go r of
    .numV a, .numV b -> case op of
      '+' -> .numV (a + b)
      '-' -> .numV (a - b)
      '*' -> .numV (a * b)
      _ -> if b == 0.0 then .errV "#DIV0" else .numV (a / b)
    .errV e, _ -> .errV e
    _, .errV e -> .errV e
    _, _ -> .errV "#REF!"
  go (Sum { from, to }) =
    let ks = do
          c <- range (min from.c to.c) (max from.c to.c)
          r <- range (min from.r to.r) (max from.r to.r)
          pure (refKey { c, r })
        step acc k = case acc of
          Left e -> Left e
          Right total -> case numOf valueOf k of
            Right n -> Right (total + n)
            Left e -> Left e
    in case foldl step (Right 0.0) ks of
      Right n -> .numV n
      Left e -> .errV e
