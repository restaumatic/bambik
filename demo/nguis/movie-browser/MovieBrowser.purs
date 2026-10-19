module MovieBrowser (Catalogue, Category, Listing, Movie, favoritesLine, isFavorite, listingLine, markFavorite, movieCatalogue, visibleMovies) where

import Prelude ((&&), (||), (==), (<>), const, map, not, show)

import Data.Array (any, filter, length)
import Data.Number.Format (fixed, toStringWith)
import Data.Variant (match)

type Catalogue =
  { "Classic" :: Boolean
  , "Cult" :: Boolean
  , "Category" :: Category
  , "Oscar" :: Boolean
  , movies :: Array Movie
  }

type Movie =
  { "Favorite" :: Boolean
  , category :: Category
  , rating :: Number
  , tags :: Array [ "Classic" :: {}, "Cult" :: {}, "Oscar" :: {} ]
  , title :: String
  , year :: Int
  }

type Category = [ "Action" :: {}, "All" :: {}, "Comedy" :: {}, "Drama" :: {} ]

type Listing = { "Favorite" :: Boolean, rating :: Number, title :: String, year :: Int }

movieCatalogue :: Catalogue
movieCatalogue =
  { "Category": ."All" {}
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

visibleMovies :: Catalogue -> Array Listing
visibleMovies { "Category": category, "Classic": classic, "Cult": cult, "Oscar": oscar, movies } = map listing (filter (\movie -> inCategory movie && taggedAsChosen movie) movies)
  where
  inCategory movie = category == ."All" {} || movie.category == category
  taggedAsChosen movie = not (classic || cult || oscar) || any chosenTag movie.tags
  chosenTag = match { "Classic": const classic, "Cult": const cult, "Oscar": const oscar }
  listing { title, year, rating, "Favorite": favorite } = { title, year, rating, "Favorite": favorite }

markFavorite :: { event :: Listing, model :: Catalogue } -> Catalogue
markFavorite { event: { title, "Favorite": favorite }, model: catalogue } = catalogue { movies = map (\movie -> if movie.title == title then movie { "Favorite" = favorite } else movie) catalogue.movies }

isFavorite :: Listing -> Boolean
isFavorite { "Favorite": favorite } = favorite

listingLine :: Listing -> String
listingLine { title, year, rating } = title <> " (" <> show year <> ") · ★ " <> toStringWith (fixed 1) rating

favoritesLine :: Catalogue -> String
favoritesLine { movies } =
  let count = length (filter _."Favorite" movies)
  in if count == 1 then "1 favorite" else show count <> " favorites"
