module Evergreen.V1.Internal.Timeline exposing (..)

import Evergreen.V1.Internal.Time


type Occurring event
    = Occurring event Evergreen.V1.Internal.Time.Absolute Evergreen.V1.Internal.Time.Absolute


type Line event
    = Line Evergreen.V1.Internal.Time.Absolute (Occurring event) (List (Occurring event))


type Timetable event
    = Timetable (List (Line event))


type Event event
    = Event Evergreen.V1.Internal.Time.Duration event (Maybe Evergreen.V1.Internal.Time.Duration)


type Schedule event
    = Schedule Evergreen.V1.Internal.Time.Duration (Event event) (List (Event event))


type alias TimelineDetails event = 
    { initial : event
    , now : Evergreen.V1.Internal.Time.Absolute
    , events : (Timetable event)
    , queued : (Maybe (Schedule event))
    , interruption : (List (Schedule event))
    , running : Bool
    }


type Timeline event
    = Timeline (TimelineDetails event)