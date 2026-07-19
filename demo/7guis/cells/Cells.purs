module Cells (cells) where

import Prelude ((#), ($), (&&), (*), (+), (-), (/), (/=), (<#>), (<$>), (<=), (<>), (==), (>=), (>>>), Unit, bind, const, map, max, min, mod, otherwise, pure, show, (||))

import Data.Array (catMaybes, range)
import Data.Char (fromCharCode, toCharCode)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Int (fromString, round, toNumber) as Int
import Data.List (List(..), elem, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString)
import Data.Profunctor (lcmap, rmap)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.String.CodeUnits (charAt, drop, singleton, take, takeWhile, dropWhile, length)
import Data.Variant (match)
import Effect (Effect)
import Foreign.Object (Object, delete, empty, fromHomogeneous, insert, lookup)
import PUI (asField, completed, forValue, mvu, projection, toCase, updates)
import PUI.HTML (attrWith, body, clicked, div, foreach, table, td, text, tr, (:=))
import PUI.MDC (body1, card, elevation20, filledTextField)
import QualifiedDo.Semigroupoid as Semigroupoid

type Sheet =
  { cells :: Object String
  , selected :: Maybe String
  , formula :: String
  }

cells :: Effect Unit
cells =
  body $
    elevation20 $
      card { caption: "Cells" } $ ( Semigroupoid.do
          ( RecordToRecord.do
              body1 (text # projection selectedCaption # forValue)
              filledTextField { floatingLabel: "Formula (e.g. =SUM(A0:A5)*2)" } # asField @"formula"
          ) # completed # rmap commit
          ( div >>> "style" := "overflow: auto; max-height: 420px; border: 1px solid #ccc; margin-top: 10px;" $
              ( table >>> "style" := "border-collapse: collapse; font-size: 13px;" $
                  ( tr $ ( clicked ( td >>> attrWith "style" cellStyle $ text # lcmap (\c -> { value: c.text }) ) # rmap _.key ) # foreach _.domKey # lcmap _.cells )
                    # foreach _.rowKey # lcmap gridRows
              ) # toCase @"cellClicked"
          ) # updates (match { cellClicked: selectCell })
      ) # mvu orderSheet

cols :: Int
cols = 26

rows :: Int
rows = 30

type GridCell = { domKey :: String, key :: String, header :: Boolean, text :: String, sel :: Boolean }
type GridRow = { rowKey :: String, cells :: Array GridCell }

gridRows :: Sheet -> Array GridRow
gridRows m =
  let
    values = evalSheet m.cells
    colName c = fromMaybe "" (singleton <$> fromCharCode (toCharCode 'A' + c))
    colIndices = range 0 (cols - 1)
    headerCells =
      [ { domKey: "h", key: "", header: true, text: "", sel: false } ]
        <> (colIndices <#> \c -> { domKey: "h" <> show c, key: "", header: true, text: colName c, sel: false })
    rowCells r =
      [ { domKey: "l" <> show r, key: "", header: true, text: show r, sel: false } ]
        <> (colIndices <#> \c -> let key = colName c <> show r in { domKey: key, key, header: false, text: fromMaybe "" (lookup key values), sel: m.selected == Just key })
  in
    [ { rowKey: "header", cells: headerCells } ]
      <> (range 0 (rows - 1) <#> \r -> { rowKey: show r, cells: rowCells r })

cellStyle :: GridCell -> String
cellStyle c
  | c.header = "border: 1px solid #ddd; background: #f4f4f4; padding: 2px 6px; position: sticky; top: 0;"
  | otherwise = "border: 1px solid #eee; padding: 2px 6px; min-width: 48px; height: 18px; cursor: cell;"
      <> (if c.sel then " background: #cde;" else "")

selectCell :: String -> Sheet -> Sheet
selectCell "" m = m -- header cells carry an empty key; clicking one selects nothing
selectCell key m = m { selected = Just key, formula = fromMaybe "" (lookup key m.cells) }

commit :: Sheet -> Sheet
commit m = case m.selected of
  Just k | lookup k m.cells /= Just m.formula ->
    m { cells = if m.formula == "" then delete k m.cells else insert k m.formula m.cells }
  _ -> m

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

data Value = NumV Number | TextV String | ErrV String

display :: Value -> String
display (NumV n) = formatNum n
display (TextV s) = s
display (ErrV e) = e

formatNum :: Number -> String
formatNum n =
  let scaled = Int.round (n * 100.0)
  in if scaled `mod` 100 == 0
     then show (scaled / 100)
     else show (Int.toNumber scaled / 100.0)

evalCell :: Object String -> List String -> String -> Value
evalCell cells visiting key =
  if key `elem` visiting then ErrV "#CYCLE"
  else case lookup key cells of
      Nothing -> TextV ""
      Just src -> case charAt 0 src of
        Just '=' -> case parseExpr (drop 1 src) of
          Just { val, rest } | length (skipSpace rest) == 0 -> evalExpr cells (key : visiting) val
          _ -> ErrV "#PARSE"
        _ -> case fromString src of
          Just n -> NumV n
          Nothing -> TextV src

numAt :: Object String -> List String -> String -> Either String Number
numAt cells visiting key = case evalCell cells visiting key of
  NumV n -> Right n
  TextV "" -> Right 0.0
  TextV _ -> Left "#REF!"
  ErrV e -> Left e

data Expr
  = Num Number
  | Ref String
  | Bin Char Expr Expr
  | Sum { from :: { c :: Int, r :: Int }, to :: { c :: Int, r :: Int } }

type P a = Maybe { val :: a, rest :: String }

skipSpace :: String -> String
skipSpace = dropWhile (_ == ' ')

parseExpr :: String -> P Expr
parseExpr s0 = do
  { val: t, rest } <- parseTerm (skipSpace s0)
  chain t rest
  where
  chain acc s = case charAt 0 (skipSpace s) of
    Just op | op == '+' || op == '-' -> case parseTerm (drop 1 (skipSpace s)) of
      Just { val, rest } -> chain (Bin op acc val) rest
      Nothing -> Nothing
    _ -> Just { val: acc, rest: s }

parseTerm :: String -> P Expr
parseTerm s0 = do
  { val: f, rest } <- parseFactor (skipSpace s0)
  chain f rest
  where
  chain acc s = case charAt 0 (skipSpace s) of
    Just op | op == '*' || op == '/' -> case parseFactor (drop 1 (skipSpace s)) of
      Just { val, rest } -> chain (Bin op acc val) rest
      Nothing -> Nothing
    _ -> Just { val: acc, rest: s }

parseFactor :: String -> P Expr
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

parseNumber :: String -> P Expr
parseNumber s =
  let digits = takeWhile (\c -> isDigit c || c == '.') s
  in case fromString digits of
    Just n -> Just { val: Num n, rest: drop (length digits) s }
    Nothing -> Nothing

parseRef :: String -> P { c :: Int, r :: Int }
parseRef s = case charAt 0 s of
  Just col | isUpper col ->
    let digits = takeWhile isDigit (drop 1 s)
    in case Int.fromString digits of
      Just r -> Just { val: { c: toCharCode col - toCharCode 'A', r }, rest: drop (1 + length digits) s }
      Nothing -> Nothing
  _ -> Nothing

refKey :: { c :: Int, r :: Int } -> String
refKey { c, r } = fromMaybe "" (singleton <$> fromCharCode (toCharCode 'A' + c)) <> show r

evalExpr :: Object String -> List String -> Expr -> Value
evalExpr cells visiting = go
  where
  go (Num n) = NumV n
  go (Ref key) = case numAt cells visiting key of
    Right n -> NumV n
    Left e -> ErrV e
  go (Bin op l r) = case go l, go r of
    NumV a, NumV b -> case op of
      '+' -> NumV (a + b)
      '-' -> NumV (a - b)
      '*' -> NumV (a * b)
      _ -> if b == 0.0 then ErrV "#DIV0" else NumV (a / b)
    ErrV e, _ -> ErrV e
    _, ErrV e -> ErrV e
    _, _ -> ErrV "#REF!"
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
      Right n -> NumV n
      Left e -> ErrV e

selectedCaption :: Sheet -> String
selectedCaption m = "Cell " <> fromMaybe "—" m.selected

orderSheet :: Sheet
orderSheet =
  { cells: fromHomogeneous
      { "A0": "Item",     "B0": "Price", "C0": "Qty", "D0": "Total"
      , "A1": "Espresso", "B1": "2.5",   "C1": "2",   "D1": "=B1*C1"
      , "A2": "Cake",     "B2": "4",     "C2": "1",   "D2": "=B2*C2"
      , "A3": "Sum",                                  "D3": "=SUM(D1:D2)"
      }
  , selected: Nothing
  , formula: ""
  }
