module MovieBrowser (movieBrowser) where

import Prelude ((#), ($), (&&), (||), (<>), (==), (>>>), class Eq, Unit, map, not, show)

import Data.Array (all, catMaybes, elem, filter, length)
import Data.Maybe (Maybe(..))
import Data.Number.Format (fixed, toStringWith)
import Data.Profunctor (lcmap, rmap)
import Data.Variant (match)
import Effect (Effect)
import PUI (asField, completed, forField, forValue, mvu, projection, tapped, updates)
import PUI.HTML (attr, body, div, span, text)
import PUI.MDC (card, chipSet, elevation1, elevation10, filterChip, iconToggle, listOf, subtitle1, tabBar)
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
              filterChip { label: "Oscar" } # asField @"oscar"
          ) # completed
          elevation1 (subtitle1 (text # projection favoritesLine # forValue)) # tapped
          listOf { selected: _.favorite }
            ( div >>> attr "style" "display: flex; align-items: center; gap: 8px;" $ RecordToRecord.do
                span (text # forField @"title")
                span (text # projection show # forField @"year")
                span (text # projection ratingLine # forField @"rating")
                iconToggle { onIcon: "star", offIcon: "star_border", label: "Favorite" } # asField @"favorite"
            )
            # rmap (\m -> .favorited m :: [ favorited :: MovieCard ])
            # lcmap visibleMovies
            # updates (match { favorited: toggleFavorite })
      ) # mvu movieCatalogue

data Category = All | Action | Drama | Comedy

derive instance Eq Category

type Movie =
  { title :: String
  , year :: Int
  , category :: Category
  , tags :: Array String
  , rating :: Number
  , favorite :: Boolean
  }

type MovieCard =
  { title :: String
  , year :: Int
  , rating :: Number
  , favorite :: Boolean
  }

type MovieCatalogue =
  { category :: Category
  , classic :: Boolean
  , cult :: Boolean
  , oscar :: Boolean
  , movies :: Array Movie
  }

movieCatalogue :: MovieCatalogue
movieCatalogue =
  { category: All
  , classic: false
  , cult: false
  , oscar: false
  , movies:
      [ { title: "Die Hard", year: 1988, category: Action, tags: [ "classic", "cult" ], rating: 8.2, favorite: false }
      , { title: "Mad Max: Fury Road", year: 2015, category: Action, tags: [ "oscar" ], rating: 8.1, favorite: false }
      , { title: "The Dark Knight", year: 2008, category: Action, tags: [ "oscar" ], rating: 9.0, favorite: false }
      , { title: "John Wick", year: 2014, category: Action, tags: [ "cult" ], rating: 7.4, favorite: false }
      , { title: "The Godfather", year: 1972, category: Drama, tags: [ "classic", "oscar" ], rating: 9.2, favorite: false }
      , { title: "The Shawshank Redemption", year: 1994, category: Drama, tags: [ "classic" ], rating: 9.3, favorite: false }
      , { title: "Parasite", year: 2019, category: Drama, tags: [ "oscar" ], rating: 8.5, favorite: false }
      , { title: "Fight Club", year: 1999, category: Drama, tags: [ "cult" ], rating: 8.8, favorite: false }
      , { title: "Some Like It Hot", year: 1959, category: Comedy, tags: [ "classic" ], rating: 8.2, favorite: false }
      , { title: "The Big Lebowski", year: 1998, category: Comedy, tags: [ "cult" ], rating: 8.1, favorite: false }
      , { title: "The Grand Budapest Hotel", year: 2014, category: Comedy, tags: [ "oscar" ], rating: 8.1, favorite: false }
      , { title: "Groundhog Day", year: 1993, category: Comedy, tags: [ "classic", "cult" ], rating: 8.0, favorite: false }
      ]
  }

categoryTabs :: Array { value :: Category, label :: String, icon :: Maybe String }
categoryTabs =
  [ { value: All, label: "All", icon: Nothing }
  , { value: Action, label: "Action", icon: Nothing }
  , { value: Drama, label: "Drama", icon: Nothing }
  , { value: Comedy, label: "Comedy", icon: Nothing }
  ]

visibleMovies :: MovieCatalogue -> Array MovieCard
visibleMovies m = map card (filter (\movie -> inCategory movie && taggedAsRequired movie) m.movies)
  where
  inCategory movie = m.category == All || movie.category == m.category
  taggedAsRequired movie = all (\tag -> elem tag movie.tags) requiredTags
  requiredTags = catMaybes [ requiredIf m.classic "classic", requiredIf m.cult "cult", requiredIf m.oscar "oscar" ]
  requiredIf on tag = if on then Just tag else Nothing
  card movie = { title: movie.title, year: movie.year, rating: movie.rating, favorite: movie.favorite }

toggleFavorite :: MovieCard -> MovieCatalogue -> MovieCatalogue
toggleFavorite chosen m = m { movies = map (\movie -> if movie.title == chosen.title then movie { favorite = not movie.favorite } else movie) m.movies }

ratingLine :: Number -> String
ratingLine rating = "★ " <> toStringWith (fixed 1) rating


favoritesLine :: MovieCatalogue -> String
favoritesLine m = case length (filter _.favorite m.movies) of
  1 -> "1 favorite"
  n -> show n <> " favorites"
