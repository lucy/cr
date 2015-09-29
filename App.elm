module App (main) where

import Array
import Bootstrap as B
import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes as A
import Html.Events as E
import History
import Http
import Json.Decode as Json
import List exposing ((::))
import Maybe exposing (Maybe(..), withDefault)
import StartApp
import Set
import String
import Signal exposing ((<~))
import Task
import Debug
import Char
import Keyboard

purl : String
purl = "http://127.0.0.1:8000/files.json"

iurl : String
iurl = "http://127.0.0.1:8000/files/"

root : String
root = "/viewer/"

removePrefix : String -> String -> String
removePrefix pre s =
  if String.startsWith pre s
  then String.dropLeft (String.length pre) s
  else s

trimRoot = removePrefix root

app = StartApp.start
  { init = init
  , view = view
  , update = update
  , inputs =
    [ InitialPath <~ initialPath
    , KeysDown <~ Keyboard.keysDown
    ]
  }

main = app.html

port tasks : Signal (Task.Task Never ())
port tasks = app.tasks

appStartMailbox : Signal.Mailbox ()
appStartMailbox = 
  Signal.mailbox ()

port appStart : Signal (Task.Task error ())
port appStart =
  Signal.constant
    (Signal.send appStartMailbox.address ())

initialPath : Signal String
initialPath =
  Signal.sampleOn appStartMailbox.signal History.path

scrollBox : Signal.Mailbox Bool
scrollBox = Signal.mailbox True

port scrollTop : Signal Bool
port scrollTop = scrollBox.signal

preloadBox : Signal.Mailbox String
preloadBox = Signal.mailbox ""

port preloadImg : Signal String
port preloadImg = preloadBox.signal

-- Model

type alias Model =
  { pages : Array.Array Page
  , page : Int
  , uri : Maybe String
  , keys : Set.Set Keyboard.KeyCode
  }

type alias Page =
  { no: Int
  , path : String
  , pre: Bool
  }

type View
  = ViewAll
  | ViewSingle

init : (Model, Effects Action)
init = (Model Array.empty 0 Nothing Set.empty, refresh)

-- Update

type Action
  = NoOp
  | InitialPath String
  | NextPage
  | PrevPage
  | Refresh
  | KeysDown (Set.Set Keyboard.KeyCode)
  | NewPages (Maybe (Array.Array String))

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp -> (model, Effects.none)
    Refresh -> (model, refresh)
    NewPages p ->
      ( case p of
          Just ps -> goto
            { model
            | pages <- Array.indexedMap (\i p -> { no = i, path = p, pre = False }) ps
            , page <- 0
            }
          Nothing -> model
      , Effects.none
      )
    PrevPage -> toPage model (model.page - 1)
    NextPage -> toPage model (model.page + 1)
    InitialPath p -> ({model | uri <- Just (fixPath p)}, Effects.none)
    KeysDown s ->
      let
        diff = Set.diff s model.keys |> Set.toList
        m = {model | keys <- s}
        f k = case k of
          37 {- Left -} -> toPage m (model.page - 1)
          39 {- Right -} -> toPage m (model.page + 1)
          _ -> (m, Effects.none)
      in
        f (withDefault 0 (List.head diff))

fixPath p = Http.uriDecode (trimRoot p)

search : String -> Array.Array Page -> Maybe Int
search x xs =
  binarySearch' (compare x << (!) xs) (0, Array.length xs - 1)

binarySearch' compareAt (low, high) =
  if high < low
  then Nothing
  else let
    mid = (low + high) // 2
    in case compareAt mid of
      LT -> binarySearch' compareAt (low, mid - 1)
      GT -> binarySearch' compareAt (mid + 1, high)
      EQ -> Just mid

(!) xs i = case Array.get i xs of
  Just x -> x.path
  Nothing -> Debug.crash "Array index out of bounds"

goto : Model -> Model
goto model =
  case model.uri of
    Just p ->
      case search (Http.uriDecode (trimRoot p)) model.pages of
        Just i -> { model | page <- i }
        Nothing -> model
    Nothing -> model

maxPage : Model -> Int
maxPage m = Array.length m.pages - 1

preload : Model -> Int -> (Model, List (Effects Action))
preload m page =
  let
    pref p (effs, arr) =
      ( if p.pre
        then effs
        else (Signal.send preloadBox.address (iurl ++ p.path) |> noop |> Effects.task) :: effs
      , if p.pre
        then arr
        else Array.set p.no {p | pre <- True} arr
      )
    (effs, prep) = Array.foldl pref ([], m.pages) (Array.slice (page) (page + 4) m.pages)
  in
    ({m | pages <- prep}, effs)
    

toPage : Model -> Int -> (Model, Effects Action)
toPage m page =
  let
    p = clamp 0 (maxPage m) page
    (ms, effs) = preload m p
  in
  ( {ms | page <- p}
  , Effects.batch
    ( [ Signal.send scrollBox.address True |> noop |> Effects.task
      , withDefault Effects.none
        (Maybe.map
          (\p -> History.setPath (root ++ p.path) |> noop |> Effects.task)
          (Array.get p ms.pages))
      ] ++ effs
    )
  )

noop = Task.map (\_ -> NoOp)

classes : List String -> Attribute
classes list = list |> String.join " " |> A.class

-- View
view : Signal.Address Action -> Model -> Html
view addr model =
  let disableNext = model.page >= maxPage model in
  div [classes ["max"]]
    [ div [classes ["max"]]
      [ div [classes [B.btnGroup, B.pullLeft]]
        [ b addr PrevPage "<" (model.page <= 0)
        , b addr NextPage ">" disableNext
        ]
      , (withDefault (text "nothing")
          (Maybe.map (\url -> mImg addr disableNext url.path)
          (Array.get model.page model.pages)))
      ]
    ]

mImg addr disable src =
  img
    [ A.src (iurl ++ src)
    , E.onClick addr NextPage
    , classes [B.pullRight, B.imgResponsive]
    ]
    []

b : Signal.Address a -> a -> String -> Bool -> Html
b addr ev t disable =
  button
    [ A.disabled disable
    , E.onClick addr ev
    , classes [B.btn, B.btnSecondary, B.btnSm]
    ]
    [ text t ]

bGrp : List Html -> Html
bGrp = div [classes [B.btnGroup]]

-- Effects

refresh : Effects Action
refresh = Http.get decode purl |> Task.toMaybe |> Task.map NewPages |> Effects.task

decode : Json.Decoder (Array.Array String)
decode = Json.array Json.string
