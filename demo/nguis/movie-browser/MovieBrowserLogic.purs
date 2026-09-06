module MovieBrowserLogic (favoriteMark, favoritesLine, isFavorite, markFavorite, movieCatalogue, ratingLine, titleLine, visibleMovies, yearLine) where

import Prelude ((&&), (||), (==), (<>), const, map, not, show)

import Data.Array (any, filter, length)
import Data.Number.Format (fixed, toStringWith)
import Data.Variant (match)

movieCatalogue :: { category :: [ "All" :: {}, "Action" :: {}, "Drama" :: {}, "Comedy" :: {} ], "Classic" :: Boolean, "Cult" :: Boolean, "Oscar" :: Boolean, movies :: Array { title :: String, year :: Int, category :: [ "All" :: {}, "Action" :: {}, "Drama" :: {}, "Comedy" :: {} ], tags :: Array [ "Classic" :: {}, "Cult" :: {}, "Oscar" :: {} ], rating :: Number, "Favorite" :: Boolean } }
movieCatalogue =
  { category: ."All" {}
  , "Classic": false
  , "Cult": false
  , "Oscar": false
  , movies:
      [ { title: "Die Hard", year: 1988, category: ."Action" {}, tags: [ ."Classic" {}, ."Cult" {} ], rating: 8.2, "Favorite": false }
      , { title: "Mad Max: Fury Road", year: 2015, category: ."Action" {}, tags: [ ."Oscar" {} ], rating: 8.1, "Favorite": false }
      , { title: "The Dark Knight", year: 2008, category: ."Action" {}, tags: [ ."Oscar" {} ], rating: 9.0, "Favorite": false }
      , { title: "John Wick", year: 2014, category: ."Action" {}, tags: [ ."Cult" {} ], rating: 7.4, "Favorite": false }
      , { title: "The Godfather", year: 1972, category: ."Drama" {}, tags: [ ."Classic" {}, ."Oscar" {} ], rating: 9.2, "Favorite": false }
      , { title: "The Shawshank Redemption", year: 1994, category: ."Drama" {}, tags: [ ."Classic" {} ], rating: 9.3, "Favorite": false }
      , { title: "Parasite", year: 2019, category: ."Drama" {}, tags: [ ."Oscar" {} ], rating: 8.5, "Favorite": false }
      , { title: "Fight Club", year: 1999, category: ."Drama" {}, tags: [ ."Cult" {} ], rating: 8.8, "Favorite": false }
      , { title: "Some Like It Hot", year: 1959, category: ."Comedy" {}, tags: [ ."Classic" {} ], rating: 8.2, "Favorite": false }
      , { title: "The Big Lebowski", year: 1998, category: ."Comedy" {}, tags: [ ."Cult" {} ], rating: 8.1, "Favorite": false }
      , { title: "The Grand Budapest Hotel", year: 2014, category: ."Comedy" {}, tags: [ ."Oscar" {} ], rating: 8.1, "Favorite": false }
      , { title: "Groundhog Day", year: 1993, category: ."Comedy" {}, tags: [ ."Classic" {}, ."Cult" {} ], rating: 8.0, "Favorite": false }
      ]
  }

visibleMovies :: { category :: [ "All" :: {}, "Action" :: {}, "Drama" :: {}, "Comedy" :: {} ], "Classic" :: Boolean, "Cult" :: Boolean, "Oscar" :: Boolean, movies :: Array { title :: String, year :: Int, category :: [ "All" :: {}, "Action" :: {}, "Drama" :: {}, "Comedy" :: {} ], tags :: Array [ "Classic" :: {}, "Cult" :: {}, "Oscar" :: {} ], rating :: Number, "Favorite" :: Boolean } } -> Array { title :: String, year :: Int, rating :: Number, "Favorite" :: Boolean }
visibleMovies { category, "Classic": classic, "Cult": cult, "Oscar": oscar, movies } = map listing (filter (\movie -> inCategory movie && taggedAsChosen movie) movies)
  where
  inCategory movie = category == ."All" {} || movie.category == category
  taggedAsChosen movie = not (classic || cult || oscar) || any chosenTag movie.tags
  chosenTag = match { "Classic": const classic, "Cult": const cult, "Oscar": const oscar }
  listing { title, year, rating, "Favorite": favorite } = { title, year, rating, "Favorite": favorite }

favoriteMark :: { title :: String, year :: Int, rating :: Number, "Favorite" :: Boolean } -> { title :: String, "Favorite" :: Boolean }
favoriteMark { title, "Favorite": favorite } = { title, "Favorite": favorite }

markFavorite :: { title :: String, "Favorite" :: Boolean } -> { movies :: Array { title :: String, year :: Int, category :: [ "All" :: {}, "Action" :: {}, "Drama" :: {}, "Comedy" :: {} ], tags :: Array [ "Classic" :: {}, "Cult" :: {}, "Oscar" :: {} ], rating :: Number, "Favorite" :: Boolean } } -> { movies :: Array { title :: String, year :: Int, category :: [ "All" :: {}, "Action" :: {}, "Drama" :: {}, "Comedy" :: {} ], tags :: Array [ "Classic" :: {}, "Cult" :: {}, "Oscar" :: {} ], rating :: Number, "Favorite" :: Boolean } }
markFavorite { title, "Favorite": favorite } { movies } = { movies: map (\movie -> if movie.title == title then movie { "Favorite" = favorite } else movie) movies }

isFavorite :: { title :: String, year :: Int, rating :: Number, "Favorite" :: Boolean } -> Boolean
isFavorite { "Favorite": favorite } = favorite

titleLine :: { title :: String } -> String
titleLine { title } = title

yearLine :: { year :: Int } -> String
yearLine { year } = show year

ratingLine :: { rating :: Number } -> String
ratingLine { rating } = "★ " <> toStringWith (fixed 1) rating

favoritesLine :: { movies :: Array { title :: String, year :: Int, category :: [ "All" :: {}, "Action" :: {}, "Drama" :: {}, "Comedy" :: {} ], tags :: Array [ "Classic" :: {}, "Cult" :: {}, "Oscar" :: {} ], rating :: Number, "Favorite" :: Boolean } } -> String
favoritesLine { movies } =
  let count = length (filter _."Favorite" movies)
  in if count == 1 then "1 favorite" else show count <> " favorites"
