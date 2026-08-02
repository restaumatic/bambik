module MovieBrowserMDC3 (movieBrowserMDC3) where

import Prelude (identity, (#), ($), (&&), (||), (==), Unit, map, show)

import Data.Array (any, catMaybes, elem, filter, length, null)
import Data.Maybe (Maybe(..))
import Data.Number.Format (fixed, toStringWith)
import Data.Variant (match)
import Effect (Effect)
import PUI (asField, completed, displayed, foreach, forField, informed, mvu, projected, toCase, updated)
import PUI.HTML (body, clWhen, provided, span, staticText, text)
import PUI.MDC3 (card, chipSet, elevation1, elevation3, filterChip, iconToggle, list, listItem, titleMedium, tabBar)
import QualifiedDo.Semigroupoid as Semigroupoid
import Data.Profunctor.Row.RecordToRecord as RecordToRecord

movieBrowserMDC3 :: Effect Unit
movieBrowserMDC3 =
  body $
    elevation3 $
      card { caption: "Movie Browser" } $ ( Semigroupoid.do
          tabBar
            [ { value: .all {}, label: "All" }
            , { value: .action {}, label: "Action" }
            , { value: .drama {}, label: "Drama" }
            , { value: .comedy {}, label: "Comedy" }
            ] # asField @"category" # completed
          chipSet ( RecordToRecord.do
              filterChip { label: "Classic" } # asField @"classic"
              filterChip { label: "Cult" } # asField @"cult"
              filterChip { label: "Oscar" } # asField @"oscar") # completed
          elevation1 ( titleMedium $ RecordToRecord.do
              text # forField @"count" show
              staticText " favorite" ) # provided soleFavorite # displayed
          elevation1 ( titleMedium $ RecordToRecord.do
              text # forField @"count" show
              staticText " favorites" ) # provided severalFavorites # displayed
          list $
            ( clWhen _.favorite "mdc-deprecated-list-item--selected"
                $ listItem $ ( RecordToRecord.do
                    span text # forField @"title" identity
                    span text # forField @"year" show
                    span ( RecordToRecord.do
                        staticText "★ "
                        text # projected ratingText )
                    iconToggle { onIcon: "star", offIcon: "star_border", label: "Favorite" } # asField @"favorite") # completed) # foreach @"title" visibleMovies # toCase @"favored" identity # updated (match { favored: informed markFavorite })
      ) # mvu movieCatalogue

movieCatalogue :: { category :: [ all :: {}, action :: {}, drama :: {}, comedy :: {} ], classic :: Boolean, cult :: Boolean, oscar :: Boolean, movies :: Array { title :: String, year :: Int, category :: [ all :: {}, action :: {}, drama :: {}, comedy :: {} ], tags :: Array [ classic :: {}, cult :: {}, oscar :: {} ], rating :: Number, favorite :: Boolean } }
movieCatalogue =
  { category: .all {}
  , classic: false
  , cult: false
  , oscar: false
  , movies:
      [ { title: "Die Hard", year: 1988, category: .action {}, tags: [ .classic {}, .cult {} ], rating: 8.2, favorite: false }
      , { title: "Mad Max: Fury Road", year: 2015, category: .action {}, tags: [ .oscar {} ], rating: 8.1, favorite: false }
      , { title: "The Dark Knight", year: 2008, category: .action {}, tags: [ .oscar {} ], rating: 9.0, favorite: false }
      , { title: "John Wick", year: 2014, category: .action {}, tags: [ .cult {} ], rating: 7.4, favorite: false }
      , { title: "The Godfather", year: 1972, category: .drama {}, tags: [ .classic {}, .oscar {} ], rating: 9.2, favorite: false }
      , { title: "The Shawshank Redemption", year: 1994, category: .drama {}, tags: [ .classic {} ], rating: 9.3, favorite: false }
      , { title: "Parasite", year: 2019, category: .drama {}, tags: [ .oscar {} ], rating: 8.5, favorite: false }
      , { title: "Fight Club", year: 1999, category: .drama {}, tags: [ .cult {} ], rating: 8.8, favorite: false }
      , { title: "Some Like It Hot", year: 1959, category: .comedy {}, tags: [ .classic {} ], rating: 8.2, favorite: false }
      , { title: "The Big Lebowski", year: 1998, category: .comedy {}, tags: [ .cult {} ], rating: 8.1, favorite: false }
      , { title: "The Grand Budapest Hotel", year: 2014, category: .comedy {}, tags: [ .oscar {} ], rating: 8.1, favorite: false }
      , { title: "Groundhog Day", year: 1993, category: .comedy {}, tags: [ .classic {}, .cult {} ], rating: 8.0, favorite: false }
      ]
  }

visibleMovies :: { category :: [ all :: {}, action :: {}, drama :: {}, comedy :: {} ], classic :: Boolean, cult :: Boolean, oscar :: Boolean, movies :: Array { title :: String, year :: Int, category :: [ all :: {}, action :: {}, drama :: {}, comedy :: {} ], tags :: Array [ classic :: {}, cult :: {}, oscar :: {} ], rating :: Number, favorite :: Boolean } } -> Array { title :: String, year :: Int, rating :: Number, favorite :: Boolean }
visibleMovies { category, classic, cult, oscar, movies } = map card (filter (\movie -> inCategory movie && taggedAsChosen movie) movies)
  where
  inCategory movie = category == .all {} || movie.category == category
  taggedAsChosen movie = null chosenTags || any (\tag -> elem tag movie.tags) chosenTags
  chosenTags = catMaybes [ chosenIf classic (.classic {}), chosenIf cult (.cult {}), chosenIf oscar (.oscar {}) ]
  chosenIf on tag = if on then Just tag else Nothing
  card { title, year, rating, favorite } = { title, year, rating, favorite }

markFavorite :: { title :: String, favorite :: Boolean, movies :: Array { title :: String, year :: Int, category :: [ all :: {}, action :: {}, drama :: {}, comedy :: {} ], tags :: Array [ classic :: {}, cult :: {}, oscar :: {} ], rating :: Number, favorite :: Boolean } } -> { movies :: Array { title :: String, year :: Int, category :: [ all :: {}, action :: {}, drama :: {}, comedy :: {} ], tags :: Array [ classic :: {}, cult :: {}, oscar :: {} ], rating :: Number, favorite :: Boolean } }
markFavorite { title, favorite, movies } = { movies: map (\movie -> if movie.title == title then movie { favorite = favorite } else movie) movies }

ratingText :: { rating :: Number } -> String
ratingText { rating } = toStringWith (fixed 1) rating

favoriteCount :: { movies :: Array { title :: String, year :: Int, category :: [ all :: {}, action :: {}, drama :: {}, comedy :: {} ], tags :: Array [ classic :: {}, cult :: {}, oscar :: {} ], rating :: Number, favorite :: Boolean } } -> Int
favoriteCount { movies } = length (filter _.favorite movies)

soleFavorite :: { movies :: Array { title :: String, year :: Int, category :: [ all :: {}, action :: {}, drama :: {}, comedy :: {} ], tags :: Array [ classic :: {}, cult :: {}, oscar :: {} ], rating :: Number, favorite :: Boolean } } -> Maybe { count :: Int }
soleFavorite { movies } = if favoriteCount { movies } == 1 then Just { count: 1 } else Nothing

severalFavorites :: { movies :: Array { title :: String, year :: Int, category :: [ all :: {}, action :: {}, drama :: {}, comedy :: {} ], tags :: Array [ classic :: {}, cult :: {}, oscar :: {} ], rating :: Number, favorite :: Boolean } } -> Maybe { count :: Int }
severalFavorites { movies } = if favoriteCount { movies } == 1 then Nothing else Just { count: favoriteCount { movies } }
