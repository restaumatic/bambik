module Main (main) where

import Prelude

import Data.Array (catMaybes, range)
import Data.Char (fromCharCode, toCharCode)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Int (fromString, round, toNumber) as Int
import Data.List (List(..), elem, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString) as Number
import Data.Profunctor (lcmap, rmap)
import Data.Profunctor.Row.RecordToRecord (completed)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.String (joinWith)
import Data.String.CodeUnits (charAt, drop, singleton, take, takeWhile, dropWhile, length) as S
import Data.Variant (match) as Variant
import Effect (Effect)
import Foreign.Object (Object, delete, empty, fromHomogeneous, insert, lookup) as Obj
import PUI (PUI, looped, updates, with)
import PUI.MDC (body1, card, elevation20, filledTextField) as MDC
import PUI.Web (Web, body, escapeHtml, onKeyClick, text, viewEvents) as Web
import QualifiedDo.Semigroupoid as Semigroupoid

cols :: Int
cols = 26

rows :: Int
rows = 30

type Model =
  { cells :: Obj.Object String
  , selected :: Maybe String
  , formula :: String
  }

main :: Effect Unit
main = Web.body $ MDC.elevation20 $ MDC.card { caption: Just "Cells" } $ looped
  $ with
    { cells: Obj.fromHomogeneous
        { "A0": "Item",     "B0": "Price", "C0": "Qty", "D0": "Total"
        , "A1": "Espresso", "B1": "2.5",   "C1": "2",   "D1": "=B1*C1"
        , "A2": "Cake",     "B2": "4",     "C2": "1",   "D2": "=B2*C2"
        , "A3": "Sum",                                  "D3": "=SUM(D1:D2)"
        }
    , selected: Nothing
    , formula: ""
    }
  $ Semigroupoid.do
  rmap commit $ completed RecordToRecord.do
    MDC.body1 $ lcmap selectedCaption Web.text
    MDC.filledTextField @"formula" { floatingLabel: "Formula (e.g. =SUM(A0:A5)*2)" }
  updates handle grid

handle :: [ cellClicked :: String ] -> Model -> Model
handle e m = Variant.match
  { cellClicked: \key -> m { selected = Just key, formula = fromMaybe "" (Obj.lookup key m.cells) }
  } e

commit :: Model -> Model
commit m = case m.selected of
  Just k | Obj.lookup k m.cells /= Just m.formula ->
    m { cells = if m.formula == "" then Obj.delete k m.cells else Obj.insert k m.formula m.cells }
  _ -> m

grid :: PUI Web.Web Model [ cellClicked :: String ]
grid = Web.viewEvents
  """<div style="overflow: auto; max-height: 420px; border: 1px solid #ccc; margin-top: 10px;"></div>"""
  renderTable
  (\node emit -> Web.onKeyClick node \key -> emit (.cellClicked key))

renderTable :: Model -> String
renderTable m =
  let values = evalSheet m.cells
      colName c = fromMaybe "" (S.singleton <$> fromCharCode (toCharCode 'A' + c))
      header = "<tr><th style=\"" <> thStyle <> "\"></th>"
        <> joinWith "" (range 0 (cols - 1) <#> \c -> "<th style=\"" <> thStyle <> "\">" <> colName c <> "</th>") <> "</tr>"
      row r = "<tr><th style=\"" <> thStyle <> "\">" <> show r <> "</th>"
        <> joinWith "" (range 0 (cols - 1) <#> \c ->
            let key = colName c <> show r
                sel = m.selected == Just key
            in "<td data-key=\"" <> key <> "\" style=\"" <> tdStyle <> (if sel then "background: #cde;" else "") <> "\">"
                 <> Web.escapeHtml (fromMaybe "" (Obj.lookup key values)) <> "</td>") <> "</tr>"
  in "<table style=\"border-collapse: collapse; font-size: 13px;\">" <> header
       <> joinWith "" (range 0 (rows - 1) <#> row) <> "</table>"
  where
  thStyle = "border: 1px solid #ddd; background: #f4f4f4; padding: 2px 6px; position: sticky; top: 0;"
  tdStyle = "border: 1px solid #eee; padding: 2px 6px; min-width: 48px; height: 18px; cursor: cell;"

evalSheet :: Obj.Object String -> Obj.Object String
evalSheet cells = foldl insertVal Obj.empty keys
  where
  keys = catMaybes do
    c <- range 0 (cols - 1)
    r <- range 0 (rows - 1)
    pure ((\ch -> S.singleton ch <> show r) <$> fromCharCode (toCharCode 'A' + c))
  insertVal acc key = case Obj.lookup key cells of
    Nothing -> acc
    Just _ -> Obj.insert key (display (evalCell cells Nil key)) acc

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

evalCell :: Obj.Object String -> List String -> String -> Value
evalCell cells visiting key
  | key `elem` visiting = ErrV "#CYCLE"
  | otherwise = case Obj.lookup key cells of
      Nothing -> TextV ""
      Just src -> case S.charAt 0 src of
        Just '=' -> case parseExpr (S.drop 1 src) of
          Just { val, rest } | S.length (skipSpace rest) == 0 -> evalExpr cells (key : visiting) val
          _ -> ErrV "#PARSE"
        _ -> case Number.fromString src of
          Just n -> NumV n
          Nothing -> TextV src

numAt :: Obj.Object String -> List String -> String -> Either String Number
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
skipSpace = S.dropWhile (_ == ' ')

parseExpr :: String -> P Expr
parseExpr s0 = do
  { val: t, rest } <- parseTerm (skipSpace s0)
  chain t rest
  where
  chain acc s = case S.charAt 0 (skipSpace s) of
    Just op | op == '+' || op == '-' -> case parseTerm (S.drop 1 (skipSpace s)) of
      Just { val, rest } -> chain (Bin op acc val) rest
      Nothing -> Nothing
    _ -> Just { val: acc, rest: s }

parseTerm :: String -> P Expr
parseTerm s0 = do
  { val: f, rest } <- parseFactor (skipSpace s0)
  chain f rest
  where
  chain acc s = case S.charAt 0 (skipSpace s) of
    Just op | op == '*' || op == '/' -> case parseFactor (S.drop 1 (skipSpace s)) of
      Just { val, rest } -> chain (Bin op acc val) rest
      Nothing -> Nothing
    _ -> Just { val: acc, rest: s }

parseFactor :: String -> P Expr
parseFactor s0 =
  let s = skipSpace s0
  in case S.charAt 0 s of
    Just '(' -> do
      { val, rest } <- parseExpr (S.drop 1 s)
      case S.charAt 0 (skipSpace rest) of
        Just ')' -> Just { val, rest: S.drop 1 (skipSpace rest) }
        _ -> Nothing
    Just c
      | isDigit c || c == '.' -> parseNumber s
      | c == 'S' && S.take 4 s == "SUM(" -> do
          { val: from, rest: r1 } <- parseRef (S.drop 4 s)
          case S.charAt 0 r1 of
            Just ':' -> do
              { val: to, rest: r2 } <- parseRef (S.drop 1 r1)
              case S.charAt 0 r2 of
                Just ')' -> Just { val: Sum { from, to }, rest: S.drop 1 r2 }
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
  let digits = S.takeWhile (\c -> isDigit c || c == '.') s
  in case Number.fromString digits of
    Just n -> Just { val: Num n, rest: S.drop (S.length digits) s }
    Nothing -> Nothing

parseRef :: String -> P { c :: Int, r :: Int }
parseRef s = case S.charAt 0 s of
  Just col | isUpper col ->
    let digits = S.takeWhile isDigit (S.drop 1 s)
    in case Int.fromString digits of
      Just r -> Just { val: { c: toCharCode col - toCharCode 'A', r }, rest: S.drop (1 + S.length digits) s }
      Nothing -> Nothing
  _ -> Nothing

refKey :: { c :: Int, r :: Int } -> String
refKey { c, r } = fromMaybe "" (S.singleton <$> fromCharCode (toCharCode 'A' + c)) <> show r

evalExpr :: Obj.Object String -> List String -> Expr -> Value
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

selectedCaption :: Model -> String
selectedCaption m = "Cell " <> fromMaybe "—" m.selected
