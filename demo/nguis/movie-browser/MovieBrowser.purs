module MovieBrowser (movieBrowser) where

import Prelude ((#), ($), (&&), (||), (<>), (==), (>>>), Unit, map, show, unit)

import Data.Array (any, catMaybes, elem, filter, length, null)
import Data.Maybe (Maybe(..))
import Data.Number.Format (fixed, toStringWith)
import Data.Profunctor (lcmap)
import Data.Variant (match)
import Effect (Effect)
import PUI (asField, completed, foreach, forField, forValue, mvu, projection, tapped, toCase, updatesOn, widenRecordInput)
import PUI.HTML (body, clWhen, span, text)
import PUI.MDC (card, chipSet, elevation1, elevation10, filterChip, iconToggle, list, listItem, subtitle1, tabBar)
import QualifiedDo.Semigroupoid as Semigroupoid
import Data.Profunctor.Row.RecordToRecord as RecordToRecord

movieBrowser :: Effect Unit
movieBrowser =
  body $
    elevation10 $
      card { caption: "Movie Browser" } $ ( Semigroupoid.do
          tabBar categoryTabs # asField @"category" # completed
          chipSet ( RecordToRecord.do
              filterChip { label: "Classic" } # asField @"classic"
              filterChip { label: "Cult" } # asField @"cult"
              filterChip { label: "Oscar" } # asField @"oscar") # completed
          elevation1 (subtitle1 text # projection favoritesLine) # widenRecordInput # tapped
          list $
            ( clWhen _.favorite "mdc-deprecated-list-item--selected"
                $ listItem $ ( RecordToRecord.do
                    span text # forValue # forField @"title"
                    span text # projection show # forField @"year"
                    span text # projection ratingLine # forField @"rating"
                    iconToggle { onIcon: "star", offIcon: "star_border", label: "Favorite" } # asField @"favorite") # completed) # foreach @"title" # toCase @"favored" # lcmap visibleMovies # updatesOn (match { favored: markFavorite })
      ) # mvu movieCatalogue

movieCatalogue :: { category :: [ all :: Unit, action :: Unit, drama :: Unit, comedy :: Unit ], classic :: Boolean, cult :: Boolean, oscar :: Boolean, movies :: Array { title :: String, year :: Int, category :: [ all :: Unit, action :: Unit, drama :: Unit, comedy :: Unit ], tags :: Array String, rating :: Number, favorite :: Boolean } }
movieCatalogue =
  { category: .all unit
  , classic: false
  , cult: false
  , oscar: false
  , movies:
      [ { title: "Die Hard", year: 1988, category: .action unit, tags: [ "classic", "cult" ], rating: 8.2, favorite: false }
      , { title: "Mad Max: Fury Road", year: 2015, category: .action unit, tags: [ "oscar" ], rating: 8.1, favorite: false }
      , { title: "The Dark Knight", year: 2008, category: .action unit, tags: [ "oscar" ], rating: 9.0, favorite: false }
      , { title: "John Wick", year: 2014, category: .action unit, tags: [ "cult" ], rating: 7.4, favorite: false }
      , { title: "The Godfather", year: 1972, category: .drama unit, tags: [ "classic", "oscar" ], rating: 9.2, favorite: false }
      , { title: "The Shawshank Redemption", year: 1994, category: .drama unit, tags: [ "classic" ], rating: 9.3, favorite: false }
      , { title: "Parasite", year: 2019, category: .drama unit, tags: [ "oscar" ], rating: 8.5, favorite: false }
      , { title: "Fight Club", year: 1999, category: .drama unit, tags: [ "cult" ], rating: 8.8, favorite: false }
      , { title: "Some Like It Hot", year: 1959, category: .comedy unit, tags: [ "classic" ], rating: 8.2, favorite: false }
      , { title: "The Big Lebowski", year: 1998, category: .comedy unit, tags: [ "cult" ], rating: 8.1, favorite: false }
      , { title: "The Grand Budapest Hotel", year: 2014, category: .comedy unit, tags: [ "oscar" ], rating: 8.1, favorite: false }
      , { title: "Groundhog Day", year: 1993, category: .comedy unit, tags: [ "classic", "cult" ], rating: 8.0, favorite: false }
      ]
  }

categoryTabs :: Array { value :: [ all :: Unit, action :: Unit, drama :: Unit, comedy :: Unit ], label :: String }
categoryTabs =
  [ { value: .all unit, label: "All" }
  , { value: .action unit, label: "Action" }
  , { value: .drama unit, label: "Drama" }
  , { value: .comedy unit, label: "Comedy" }
  ]

visibleMovies :: { category :: [ all :: Unit, action :: Unit, drama :: Unit, comedy :: Unit ], classic :: Boolean, cult :: Boolean, oscar :: Boolean, movies :: Array { title :: String, year :: Int, category :: [ all :: Unit, action :: Unit, drama :: Unit, comedy :: Unit ], tags :: Array String, rating :: Number, favorite :: Boolean } } -> Array { title :: String, year :: Int, rating :: Number, favorite :: Boolean }
visibleMovies m = map card (filter (\movie -> inCategory movie && taggedAsChosen movie) m.movies)
  where
  inCategory movie = m.category == .all unit || movie.category == m.category
  taggedAsChosen movie = null chosenTags || any (\tag -> elem tag movie.tags) chosenTags
  chosenTags = catMaybes [ chosenIf m.classic "classic", chosenIf m.cult "cult", chosenIf m.oscar "oscar" ]
  chosenIf on tag = if on then Just tag else Nothing
  card movie = { title: movie.title, year: movie.year, rating: movie.rating, favorite: movie.favorite }

markFavorite :: { title :: String, year :: Int, rating :: Number, favorite :: Boolean } -> { movies :: Array { title :: String, year :: Int, category :: [ all :: Unit, action :: Unit, drama :: Unit, comedy :: Unit ], tags :: Array String, rating :: Number, favorite :: Boolean } } -> { movies :: Array { title :: String, year :: Int, category :: [ all :: Unit, action :: Unit, drama :: Unit, comedy :: Unit ], tags :: Array String, rating :: Number, favorite :: Boolean } }
markFavorite chosen m = m { movies = map (\movie -> if movie.title == chosen.title then movie { favorite = chosen.favorite } else movie) m.movies }

ratingLine :: Number -> String
ratingLine rating = "★ " <> toStringWith (fixed 1) rating


favoritesLine :: { movies :: Array { title :: String, year :: Int, category :: [ all :: Unit, action :: Unit, drama :: Unit, comedy :: Unit ], tags :: Array String, rating :: Number, favorite :: Boolean } } -> String
favoritesLine m = case length (filter _.favorite m.movies) of
  1 -> "1 favorite"
  n -> show n <> " favorites"
