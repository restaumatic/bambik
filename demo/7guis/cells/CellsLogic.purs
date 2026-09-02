module CellsLogic (commit, gridRows, orderSheet, presentCells, selectCell) where

import Prelude ((<>), bind, map, max, min, mod, pure, show, (&&), (*), (+), (-), (/), (/=), (<#>), (<$>), (<=), (==), (>=), (||))

import Data.Array (catMaybes, range)
import Data.Char (fromCharCode, toCharCode)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Int (fromString, round, toNumber) as Int
import Data.List (List(..), elem, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString)
import Data.String.CodeUnits (charAt, drop, singleton, take, takeWhile, dropWhile, length)
import Data.Variant (match)
import Foreign.Object (Object, delete, empty, fromHomogeneous, insert, lookup)

orderSheet :: { cells :: Object String, selected :: [ picked :: { name :: String }, none :: {} ], selectedLine :: String, "Formula (e.g. =SUM(A0:A5)*2)" :: String }
orderSheet = presentCells
  { cells: fromHomogeneous
      { "A0": "Item",     "B0": "Price", "C0": "Qty", "D0": "Total"
      , "A1": "Espresso", "B1": "2.5",   "C1": "2",   "D1": "=B1*C1"
      , "A2": "Cake",     "B2": "4",     "C2": "1",   "D2": "=B2*C2"
      , "A3": "Sum",                                  "D3": "=SUM(D1:D2)"
      }
  , selected: .none {}
  , selectedLine: ""
  , "Formula (e.g. =SUM(A0:A5)*2)": ""
  }

presentCells :: { cells :: Object String, selected :: [ picked :: { name :: String }, none :: {} ], selectedLine :: String, "Formula (e.g. =SUM(A0:A5)*2)" :: String } -> { cells :: Object String, selected :: [ picked :: { name :: String }, none :: {} ], selectedLine :: String, "Formula (e.g. =SUM(A0:A5)*2)" :: String }
presentCells r = r { selectedLine = "Cell " <> match { picked: _.name, none: \_ -> "—" } r.selected }

cols :: Int
cols = 26

rows :: Int
rows = 30

gridRows :: { cells :: Object String, selected :: [ picked :: { name :: String }, none :: {} ] } -> Array { rowKey :: String, cells :: Array { domKey :: String, key :: String, kind :: [ header :: {}, cell :: {} ], text :: String, status :: [ selected :: {}, unselected :: {} ] } }
gridRows m =
  let
    values = evalSheet m.cells
    colName c = fromMaybe "" (singleton <$> fromCharCode (toCharCode 'A' + c))
    colIndices = range 0 (cols - 1)
    headerCells =
      [ { domKey: "h", key: "", kind: .header {}, text: "", status: .unselected {} } ]
        <> (colIndices <#> \c -> { domKey: "h" <> show c, key: "", kind: .header {}, text: colName c, status: .unselected {} })
    rowCells r =
      [ { domKey: "l" <> show r, key: "", kind: .header {}, text: show r, status: .unselected {} } ]
        <> (colIndices <#> \c -> let key = colName c <> show r in { domKey: key, key, kind: .cell {}, text: fromMaybe "" (lookup key values), status: statusOf key })
    statusOf key = match { picked: \p -> if p.name == key then .selected {} else .unselected {}, none: \_ -> .unselected {} } m.selected
  in
    [ { rowKey: "header", cells: headerCells } ]
      <> (range 0 (rows - 1) <#> \r -> { rowKey: show r, cells: rowCells r })

selectCell :: String -> { cells :: Object String, selected :: [ picked :: { name :: String }, none :: {} ], "Formula (e.g. =SUM(A0:A5)*2)" :: String } -> { cells :: Object String, selected :: [ picked :: { name :: String }, none :: {} ], "Formula (e.g. =SUM(A0:A5)*2)" :: String }
selectCell "" m = m
selectCell key m = m { selected = .picked { name: key }, "Formula (e.g. =SUM(A0:A5)*2)" = fromMaybe "" (lookup key m.cells) }

commit :: { cells :: Object String, selected :: [ picked :: { name :: String }, none :: {} ], "Formula (e.g. =SUM(A0:A5)*2)" :: String } -> { cells :: Object String, selected :: [ picked :: { name :: String }, none :: {} ], "Formula (e.g. =SUM(A0:A5)*2)" :: String }
commit m@{ selected, "Formula (e.g. =SUM(A0:A5)*2)": formula } = match
  { picked: \p ->
      if lookup p.name m.cells /= Just formula then m { cells = if formula == "" then delete p.name m.cells else insert p.name formula m.cells } else m
  , none: \_ -> m
  } selected

evalSheet :: Object String -> Object String
evalSheet cells = foldl insertVal empty keys
  where
  keys = catMaybes do
    c <- range 0 (cols - 1)
    r <- range 0 (rows - 1)
    pure ((\ch -> singleton ch <> show r) <$> fromCharCode (toCharCode 'A' + c))
  insertVal acc key = case lookup key cells of
    Nothing -> acc
    Just _ -> insert key (display (evalCell cells Nil key)) acc

display :: [ numV :: Number, textV :: String, errV :: String ] -> String
display = match { numV: formatNum, textV: \t -> t, errV: \e -> e }

formatNum :: Number -> String
formatNum n =
  let scaled = Int.round (n * 100.0)
  in if scaled `mod` 100 == 0
     then show (scaled / 100)
     else show (Int.toNumber scaled / 100.0)

evalCell :: Object String -> List String -> String -> [ numV :: Number, textV :: String, errV :: String ]
evalCell cells visiting key =
  if key `elem` visiting then .errV "#CYCLE"
  else case lookup key cells of
      Nothing -> .textV ""
      Just src -> case charAt 0 src of
        Just '=' -> case parseExpr (drop 1 src) of
          Just { val, rest } | length (skipSpace rest) == 0 -> evalExpr cells (key : visiting) val
          _ -> .errV "#PARSE"
        _ -> case fromString src of
          Just n -> .numV n
          Nothing -> .textV src

numAt :: Object String -> List String -> String -> Either String Number
numAt cells visiting key = case evalCell cells visiting key of
  .numV n -> Right n
  .textV "" -> Right 0.0
  .textV _ -> Left "#REF!"
  .errV e -> Left e
  _ -> Left "#REF!"

data Expr
  = Num Number
  | Ref String
  | Bin Char Expr Expr
  | Sum { from :: { c :: Int, r :: Int }, to :: { c :: Int, r :: Int } }

skipSpace :: String -> String
skipSpace = dropWhile (_ == ' ')

parseExpr :: String -> Maybe { val :: Expr, rest :: String }
parseExpr s0 = do
  { val: t, rest } <- parseTerm (skipSpace s0)
  chain t rest
  where
  chain acc s = case charAt 0 (skipSpace s) of
    Just op | op == '+' || op == '-' -> case parseTerm (drop 1 (skipSpace s)) of
      Just { val, rest } -> chain (Bin op acc val) rest
      Nothing -> Nothing
    _ -> Just { val: acc, rest: s }

parseTerm :: String -> Maybe { val :: Expr, rest :: String }
parseTerm s0 = do
  { val: f, rest } <- parseFactor (skipSpace s0)
  chain f rest
  where
  chain acc s = case charAt 0 (skipSpace s) of
    Just op | op == '*' || op == '/' -> case parseFactor (drop 1 (skipSpace s)) of
      Just { val, rest } -> chain (Bin op acc val) rest
      Nothing -> Nothing
    _ -> Just { val: acc, rest: s }

parseFactor :: String -> Maybe { val :: Expr, rest :: String }
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

parseNumber :: String -> Maybe { val :: Expr, rest :: String }
parseNumber s =
  let digits = takeWhile (\c -> isDigit c || c == '.') s
  in case fromString digits of
    Just n -> Just { val: Num n, rest: drop (length digits) s }
    Nothing -> Nothing

parseRef :: String -> Maybe { val :: { c :: Int, r :: Int }, rest :: String }
parseRef s = case charAt 0 s of
  Just col | isUpper col ->
    let digits = takeWhile isDigit (drop 1 s)
    in case Int.fromString digits of
      Just r -> Just { val: { c: toCharCode col - toCharCode 'A', r }, rest: drop (1 + length digits) s }
      Nothing -> Nothing
  _ -> Nothing

refKey :: { c :: Int, r :: Int } -> String
refKey { c, r } = fromMaybe "" (singleton <$> fromCharCode (toCharCode 'A' + c)) <> show r

evalExpr :: Object String -> List String -> Expr -> [ numV :: Number, textV :: String, errV :: String ]
evalExpr cells visiting = go
  where
  go (Num n) = .numV n
  go (Ref key) = case numAt cells visiting key of
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
          Right total -> case numAt cells visiting k of
            Right n -> Right (total + n)
            Left e -> Left e
    in case foldl step (Right 0.0) ks of
      Right n -> .numV n
      Left e -> .errV e
