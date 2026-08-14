module MovieBrowserLogic (favorites, markFavorite, movieCatalogue, ratingText, visibleMovies) where

import Prelude ((&&), (||), (==), map, not)

import Data.Array (any, filter, length)
import Data.Number.Format (fixed, toStringWith)
import Data.Variant (match)

movieCatalogue :: { category :: [ all :: {}, action :: {}, drama :: {}, comedy :: {} ], "Classic" :: Boolean, "Cult" :: Boolean, "Oscar" :: Boolean, movies :: Array { title :: String, year :: Int, category :: [ all :: {}, action :: {}, drama :: {}, comedy :: {} ], tags :: Array [ "Classic" :: {}, "Cult" :: {}, "Oscar" :: {} ], rating :: Number, "Favorite" :: Boolean } }
movieCatalogue =
  { category: .all {}
  , "Classic": false
  , "Cult": false
  , "Oscar": false
  , movies:
      [ { title: "Die Hard", year: 1988, category: .action {}, tags: [ ."Classic" {}, ."Cult" {} ], rating: 8.2, "Favorite": false }
      , { title: "Mad Max: Fury Road", year: 2015, category: .action {}, tags: [ ."Oscar" {} ], rating: 8.1, "Favorite": false }
      , { title: "The Dark Knight", year: 2008, category: .action {}, tags: [ ."Oscar" {} ], rating: 9.0, "Favorite": false }
      , { title: "John Wick", year: 2014, category: .action {}, tags: [ ."Cult" {} ], rating: 7.4, "Favorite": false }
      , { title: "The Godfather", year: 1972, category: .drama {}, tags: [ ."Classic" {}, ."Oscar" {} ], rating: 9.2, "Favorite": false }
      , { title: "The Shawshank Redemption", year: 1994, category: .drama {}, tags: [ ."Classic" {} ], rating: 9.3, "Favorite": false }
      , { title: "Parasite", year: 2019, category: .drama {}, tags: [ ."Oscar" {} ], rating: 8.5, "Favorite": false }
      , { title: "Fight Club", year: 1999, category: .drama {}, tags: [ ."Cult" {} ], rating: 8.8, "Favorite": false }
      , { title: "Some Like It Hot", year: 1959, category: .comedy {}, tags: [ ."Classic" {} ], rating: 8.2, "Favorite": false }
      , { title: "The Big Lebowski", year: 1998, category: .comedy {}, tags: [ ."Cult" {} ], rating: 8.1, "Favorite": false }
      , { title: "The Grand Budapest Hotel", year: 2014, category: .comedy {}, tags: [ ."Oscar" {} ], rating: 8.1, "Favorite": false }
      , { title: "Groundhog Day", year: 1993, category: .comedy {}, tags: [ ."Classic" {}, ."Cult" {} ], rating: 8.0, "Favorite": false }
      ]
  }

visibleMovies :: { category :: [ all :: {}, action :: {}, drama :: {}, comedy :: {} ], "Classic" :: Boolean, "Cult" :: Boolean, "Oscar" :: Boolean, movies :: Array { title :: String, year :: Int, category :: [ all :: {}, action :: {}, drama :: {}, comedy :: {} ], tags :: Array [ "Classic" :: {}, "Cult" :: {}, "Oscar" :: {} ], rating :: Number, "Favorite" :: Boolean } } -> Array { title :: String, year :: Int, rating :: Number, "Favorite" :: Boolean }
visibleMovies { category, "Classic": classic, "Cult": cult, "Oscar": oscar, movies } = map card (filter (\movie -> inCategory movie && taggedAsChosen movie) movies)
  where
  inCategory movie = category == .all {} || movie.category == category
  taggedAsChosen movie = not (classic || cult || oscar) || any chosenTag movie.tags
  chosenTag = match { "Classic": \_ -> classic, "Cult": \_ -> cult, "Oscar": \_ -> oscar }
  card { title, year, rating, "Favorite": favorite } = { title, year, rating, "Favorite": favorite }

markFavorite :: { title :: String, "Favorite" :: Boolean, movies :: Array { title :: String, year :: Int, category :: [ all :: {}, action :: {}, drama :: {}, comedy :: {} ], tags :: Array [ "Classic" :: {}, "Cult" :: {}, "Oscar" :: {} ], rating :: Number, "Favorite" :: Boolean } } -> { movies :: Array { title :: String, year :: Int, category :: [ all :: {}, action :: {}, drama :: {}, comedy :: {} ], tags :: Array [ "Classic" :: {}, "Cult" :: {}, "Oscar" :: {} ], rating :: Number, "Favorite" :: Boolean } }
markFavorite { title, "Favorite": favorite, movies } = { movies: map (\movie -> if movie.title == title then movie { "Favorite" = favorite } else movie) movies }

ratingText :: { rating :: Number } -> String
ratingText { rating } = toStringWith (fixed 1) rating

favoriteCount :: { movies :: Array { title :: String, year :: Int, category :: [ all :: {}, action :: {}, drama :: {}, comedy :: {} ], tags :: Array [ "Classic" :: {}, "Cult" :: {}, "Oscar" :: {} ], rating :: Number, "Favorite" :: Boolean } } -> Int
favoriteCount { movies } = length (filter _."Favorite" movies)

favorites :: { movies :: Array { title :: String, year :: Int, category :: [ all :: {}, action :: {}, drama :: {}, comedy :: {} ], tags :: Array [ "Classic" :: {}, "Cult" :: {}, "Oscar" :: {} ], rating :: Number, "Favorite" :: Boolean } } -> [ sole :: { count :: Int }, several :: { count :: Int } ]
favorites { movies } =
  let count = favoriteCount { movies }
  in if count == 1 then .sole { count } else .several { count }
