module MovieBrowserLogic (favorites, markFavorite, movieCatalogue, ratingText, visibleMovies) where

import Prelude ((&&), (||), (==), map, not)

import Data.Array (any, filter, length)
import Data.Number.Format (fixed, toStringWith)
import Data.Variant (match)

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
  taggedAsChosen movie = not (classic || cult || oscar) || any chosenTag movie.tags
  chosenTag = match { classic: \_ -> classic, cult: \_ -> cult, oscar: \_ -> oscar }
  card { title, year, rating, favorite } = { title, year, rating, favorite }

markFavorite :: { title :: String, favorite :: Boolean, movies :: Array { title :: String, year :: Int, category :: [ all :: {}, action :: {}, drama :: {}, comedy :: {} ], tags :: Array [ classic :: {}, cult :: {}, oscar :: {} ], rating :: Number, favorite :: Boolean } } -> { movies :: Array { title :: String, year :: Int, category :: [ all :: {}, action :: {}, drama :: {}, comedy :: {} ], tags :: Array [ classic :: {}, cult :: {}, oscar :: {} ], rating :: Number, favorite :: Boolean } }
markFavorite { title, favorite, movies } = { movies: map (\movie -> if movie.title == title then movie { favorite = favorite } else movie) movies }

ratingText :: { rating :: Number } -> String
ratingText { rating } = toStringWith (fixed 1) rating

favoriteCount :: { movies :: Array { title :: String, year :: Int, category :: [ all :: {}, action :: {}, drama :: {}, comedy :: {} ], tags :: Array [ classic :: {}, cult :: {}, oscar :: {} ], rating :: Number, favorite :: Boolean } } -> Int
favoriteCount { movies } = length (filter _.favorite movies)

favorites :: { movies :: Array { title :: String, year :: Int, category :: [ all :: {}, action :: {}, drama :: {}, comedy :: {} ], tags :: Array [ classic :: {}, cult :: {}, oscar :: {} ], rating :: Number, favorite :: Boolean } } -> [ sole :: { count :: Int }, several :: { count :: Int } ]
favorites { movies } =
  let count = favoriteCount { movies }
  in if count == 1 then .sole { count } else .several { count }
