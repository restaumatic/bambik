module MeetingBooker (blankBooking, bookRoom, plan, planLine, roomStars, seatOccupancy, seatsInRoom) where

import Prelude (discard, pure, (<>), show, (/), ($))

import Effect.Aff (Aff, Milliseconds(..), delay)
import Data.Int (round)
import Data.Ord (clamp)
import Data.String (toLower, trim)
import Data.Variant (match)
import Data.Variant.Case (caseText)

blankBooking
  :: { "Attendees" :: { current :: Number
                      , max :: Number
                      , min :: Number
                      , step :: [ continuous :: {}, discrete :: Number ]
                      }
     , "Catering" :: [ none :: {}
                     , ordered :: [ "Coffee and pastries" :: {}, "Sandwich lunch" :: {} ]
                     ]
     , "Duration" :: [ chosen :: [ "15 min" :: {}, "30 min" :: {}, "60 min" :: {} ], unchosen :: {} ]
     , "Teams link" :: Boolean
     , "Meeting title" :: String
     , "Room" :: [ chosen :: [ "Auditorium" :: {}
                             , "Boardroom" :: {}
                             , "Focus pod" :: {}
                             ]
                 , unchosen :: {}
                 ]
     }
blankBooking = { "Meeting title": "", "Room": .unchosen {}, "Duration": .unchosen {}, "Attendees": { current: justTheOrganizer, min: justTheOrganizer, max: justTheOrganizer, step: .discrete 1.0 }, "Teams link": false, "Catering": .none {} }

seatsInRoom
  :: { "Attendees" :: { current :: Number
                      , max :: Number
                      , min :: Number
                      , step :: [ continuous :: {}, discrete :: Number ]
                      }
     , "Catering" :: [ none :: {}
                     , ordered :: [ "Coffee and pastries" :: {}, "Sandwich lunch" :: {} ]
                     ]
     , "Duration" :: [ chosen :: [ "15 min" :: {}, "30 min" :: {}, "60 min" :: {} ], unchosen :: {} ]
     , "Teams link" :: Boolean
     , "Meeting title" :: String
     , "Room" :: [ chosen :: [ "Auditorium" :: {}
                             , "Boardroom" :: {}
                             , "Focus pod" :: {}
                             ]
                 , unchosen :: {}
                 ]
     }
  -> { "Attendees" :: { current :: Number
                      , max :: Number
                      , min :: Number
                      , step :: [ continuous :: {}, discrete :: Number ]
                      }
     , "Catering" :: [ none :: {}
                     , ordered :: [ "Coffee and pastries" :: {}, "Sandwich lunch" :: {} ]
                     ]
     , "Duration" :: [ chosen :: [ "15 min" :: {}, "30 min" :: {}, "60 min" :: {} ], unchosen :: {} ]
     , "Teams link" :: Boolean
     , "Meeting title" :: String
     , "Room" :: [ chosen :: [ "Auditorium" :: {}
                             , "Boardroom" :: {}
                             , "Focus pod" :: {}
                             ]
                 , unchosen :: {}
                 ]
     }
seatsInRoom booking@{ "Room": room, "Attendees": seats } = match
  { chosen: \r -> booking { "Attendees" = seats { current = seatedIn r seats.current, max = roomCapacity r } }
  , unchosen: \_ -> booking
  } room

seatedIn
  :: [ "Focus pod" :: {}, "Boardroom" :: {}, "Auditorium" :: {} ]
  -> Number
  -> Number
seatedIn room n = clamp justTheOrganizer (roomCapacity room) n

plan
  :: { "Attendees" :: { current :: Number
                      , max :: Number
                      , min :: Number
                      , step :: [ continuous :: {}, discrete :: Number ]
                      }
     , "Catering" :: [ none :: {}
                     , ordered :: [ "Coffee and pastries" :: {}, "Sandwich lunch" :: {} ]
                     ]
     , "Duration" :: [ chosen :: [ "15 min" :: {}, "30 min" :: {}, "60 min" :: {} ], unchosen :: {} ]
     , "Teams link" :: Boolean
     , "Meeting title" :: String
     , "Room" :: [ chosen :: [ "Auditorium" :: {}
                             , "Boardroom" :: {}
                             , "Focus pod" :: {}
                             ]
                 , unchosen :: {}
                 ]
     }
  -> [ complete :: {}, incomplete :: {} ]
plan booking = match { complete: \_ -> .complete {}, incomplete: \_ -> .incomplete {} } (planned booking)

planned
  :: forall r1.
     { "Meeting title" :: String
     , "Room" :: [ chosen :: [ "Focus pod" :: {}
                             , "Boardroom" :: {}
                             , "Auditorium" :: {}
                             ]
                 , unchosen :: {}
                 ]
     , "Duration" :: [ chosen :: [ "15 min" :: {}, "30 min" :: {}, "60 min" :: {} ], unchosen :: {} ]
     , "Attendees" :: { current :: Number
                      , min :: Number
                      , max :: Number
                      , step :: [ discrete :: Number, continuous :: {} ]
                      }
     , "Teams link" :: Boolean
     , "Catering" :: [ ordered :: [ "Coffee and pastries" :: {}, "Sandwich lunch" :: {} ]
                     , none :: {}
                     ]
     | r1
     }
  -> [ complete :: { "Meeting title" :: String
                   , room :: [ "Focus pod" :: {}
                             , "Boardroom" :: {}
                             , "Auditorium" :: {}
                             ]
                   , duration :: [ "15 min" :: {}, "30 min" :: {}, "60 min" :: {} ]
                   , attendees :: Number
                   , "Teams link" :: Boolean
                   , catering :: [ ordered :: [ "Coffee and pastries" :: {}
                                              , "Sandwich lunch" :: {}
                                              ]
                                 , none :: {}
                                 ]
                   }
     , incomplete :: {}
     ]
planned { "Meeting title": title, "Room": room, "Duration": duration, "Attendees": seats, "Teams link": online, "Catering": catering } = match
  { chosen: \r -> match
    { chosen: \d -> .complete { "Meeting title": title, room: r, duration: d, attendees: seats.current, "Teams link": online, catering }
    , unchosen: \_ -> .incomplete {}
    } duration
  , unchosen: \_ -> .incomplete {}
  } room

planLine
  :: { "Attendees" :: { current :: Number
                      , max :: Number
                      , min :: Number
                      , step :: [ continuous :: {}, discrete :: Number ]
                      }
     , "Catering" :: [ none :: {}
                     , ordered :: [ "Coffee and pastries" :: {}, "Sandwich lunch" :: {} ]
                     ]
     , "Duration" :: [ chosen :: [ "15 min" :: {}, "30 min" :: {}, "60 min" :: {} ], unchosen :: {} ]
     , "Teams link" :: Boolean
     , "Meeting title" :: String
     , "Room" :: [ chosen :: [ "Auditorium" :: {}
                             , "Boardroom" :: {}
                             , "Focus pod" :: {}
                             ]
                 , unchosen :: {}
                 ]
     }
  -> String
planLine booking = match { complete: \p -> "Plan: " <> planText p, incomplete: \_ -> "" } (planned booking)

planText
  :: { "Meeting title" :: String
     , room :: [ "Focus pod" :: {}
               , "Boardroom" :: {}
               , "Auditorium" :: {}
               ]
     , duration :: [ "15 min" :: {}, "30 min" :: {}, "60 min" :: {} ]
     , attendees :: Number
     , "Teams link" :: Boolean
     , catering :: [ ordered :: [ "Coffee and pastries" :: {}, "Sandwich lunch" :: {} ]
                   , none :: {}
                   ]
     }
  -> String
planText p =
  titleText p."Meeting title" <> " in the " <> roomText p.room <> ", " <> caseText p.duration <> ", " <> headcount p.attendees <> " attendees" <> onlineNote p."Teams link" <> cateringNote p.catering

headcount :: Number -> String
headcount attendees = show (round attendees)

onlineNote :: Boolean -> String
onlineNote online = if online then ", with a Teams link" else ""

cateringNote
  :: [ ordered :: [ "Coffee and pastries" :: {}, "Sandwich lunch" :: {} ], none :: {} ]
  -> String
cateringNote = match { ordered: \c -> ", " <> toLower (caseText c), none: \_ -> "" }

titleText :: String -> String
titleText title = case trim title of
  "" -> "Untitled meeting"
  name -> name

roomText
  :: [ "Focus pod" :: {}, "Boardroom" :: {}, "Auditorium" :: {} ]
  -> String
roomText room = toLower (caseText room)


roomRating
  :: [ "Focus pod" :: {}, "Boardroom" :: {}, "Auditorium" :: {} ]
  -> Number
roomRating = match { "Focus pod": \_ -> 4.5, "Boardroom": \_ -> 3.5, "Auditorium": \_ -> 4.0 }


roomCapacity
  :: [ "Focus pod" :: {}, "Boardroom" :: {}, "Auditorium" :: {} ]
  -> Number
roomCapacity = match { "Focus pod": \_ -> 4.0, "Boardroom": \_ -> 12.0, "Auditorium": \_ -> 40.0 }

justTheOrganizer :: Number
justTheOrganizer = 1.0

roomStars
  :: { "Attendees" :: { current :: Number
                      , max :: Number
                      , min :: Number
                      , step :: [ continuous :: {}, discrete :: Number ]
                      }
     , "Catering" :: [ none :: {}
                     , ordered :: [ "Coffee and pastries" :: {}, "Sandwich lunch" :: {} ]
                     ]
     , "Duration" :: [ chosen :: [ "15 min" :: {}, "30 min" :: {}, "60 min" :: {} ], unchosen :: {} ]
     , "Teams link" :: Boolean
     , "Meeting title" :: String
     , "Room" :: [ chosen :: [ "Auditorium" :: {}
                             , "Boardroom" :: {}
                             , "Focus pod" :: {}
                             ]
                 , unchosen :: {}
                 ]
     }
  -> Number
roomStars { "Room": room } = match { chosen: roomRating, unchosen: \_ -> 0.0 } room

seatOccupancy
  :: { "Attendees" :: { current :: Number
                      , max :: Number
                      , min :: Number
                      , step :: [ continuous :: {}, discrete :: Number ]
                      }
     , "Catering" :: [ none :: {}
                     , ordered :: [ "Coffee and pastries" :: {}, "Sandwich lunch" :: {} ]
                     ]
     , "Duration" :: [ chosen :: [ "15 min" :: {}, "30 min" :: {}, "60 min" :: {} ], unchosen :: {} ]
     , "Teams link" :: Boolean
     , "Meeting title" :: String
     , "Room" :: [ chosen :: [ "Auditorium" :: {}
                             , "Boardroom" :: {}
                             , "Focus pod" :: {}
                             ]
                 , unchosen :: {}
                 ]
     }
  -> Number
seatOccupancy { "Room": room, "Attendees": seats } = match { chosen: \r -> seats.current / roomCapacity r, unchosen: \_ -> 0.0 } room

bookRoom
  :: { "Attendees" :: { current :: Number
                      , max :: Number
                      , min :: Number
                      , step :: [ continuous :: {}, discrete :: Number ]
                      }
     , "Catering" :: [ none :: {}
                     , ordered :: [ "Coffee and pastries" :: {}, "Sandwich lunch" :: {} ]
                     ]
     , "Duration" :: [ chosen :: [ "15 min" :: {}, "30 min" :: {}, "60 min" :: {} ], unchosen :: {} ]
     , "Teams link" :: Boolean
     , "Meeting title" :: String
     , "Room" :: [ chosen :: [ "Auditorium" :: {}
                             , "Boardroom" :: {}
                             , "Focus pod" :: {}
                             ]
                 , unchosen :: {}
                 ]
     }
  -> Aff [ "Room booked" :: { "Attendees" :: { current :: Number
                                        , max :: Number
                                        , min :: Number
                                        , step :: [ continuous :: {}, discrete :: Number ]
                                        }
                       , "Catering" :: [ none :: {}
                                       , ordered :: [ "Coffee and pastries" :: {}
                                                    , "Sandwich lunch" :: {}
                                                    ]
                                       ]
                       , "Duration" :: [ chosen :: [ "15 min" :: {}, "30 min" :: {}, "60 min" :: {} ]
                                             , unchosen :: {}
                                             ]
                       , "Teams link" :: Boolean
                       , "Meeting title" :: String
                       , "Room" :: [ chosen :: [ "Auditorium" :: {}
                                               , "Boardroom" :: {}
                                               , "Focus pod" :: {}
                                               ]
                                   , unchosen :: {}
                                   ]
                       }
         ]
bookRoom booking = do
  delay (Milliseconds 600.0)
  pure $ ."Room booked" booking
