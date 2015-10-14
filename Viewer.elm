module Viewer
  ( init
  , view
  , update
  , inputs
  , appStartBox
  , preloadBox
  , scrollBox
  ) where

import Array as A
import Bootstrap as B
import Debug
import Effects exposing (Effects)
import History
import Html exposing (..)
import Html.Attributes as A
import Html.Events as E
import Http
import Json.Decode as Json
import Keyboard
import List exposing ((::))
import Maybe exposing (Maybe(..), withDefault)
import Set
import Signal exposing ((<~))
import StartApp
import String
import Task

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

fixPath : String -> String
fixPath = removePrefix root >> Http.uriDecode

appStartBox : Signal.Mailbox ()
appStartBox = Signal.mailbox ()

initialPath : Signal String
initialPath = Signal.sampleOn appStartBox.signal History.path

noop = Task.map (always NoOp) >> Effects.task
pure = Task.succeed >> Effects.task

-- Model

type alias Model =
  { pages : A.Array Page
  , page : Int
  , uri : Maybe String
  , keys : Set.Set Keyboard.KeyCode
  , showPath : Bool
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
init = (Model A.empty 0 Nothing Set.empty False, refresh)

inputs : List (Signal Action)
inputs =
  [ InitialPath <~ initialPath
  , KeysDown <~ Keyboard.keysDown
  ]

-- Update

type Action
  = NoOp
  | NextPage
  | PrevPage
  | NextDir
  | PrevDir
  | InitialPath String
  | SetPathNo
  | Refresh
  | NewPages (Maybe (A.Array String))
  | Preload
  | KeysDown (Set.Set Keyboard.KeyCode)

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp -> (model, Effects.none)
    Refresh -> (model, refresh)
    NewPages p ->
      case p of
        Just ps ->
          ( goto
            { model
            | pages <- A.indexedMap (\i p -> { no = i, path = p, pre = False }) ps
            , page <- 0
            }
          , Effects.batch [pure SetPathNo, pure Preload]
          )
        Nothing -> (model, Effects.none)
    PrevPage -> toPage model <| model.page - 1
    NextPage -> toPage model <| model.page + 1
    NextDir -> Maybe.withDefault
      (model, Effects.none)
      (Maybe.map (toPage model)
        (findNext model.pages model.page))
    PrevDir -> Maybe.withDefault
      (model, Effects.none)
      (Maybe.map (toPage model)
        (findPrev model.pages model.page))
    SetPathNo ->
      ( model
      , withDefault Effects.none
        <| Maybe.map (\p -> History.setPath (root ++ p.path) |> noop)
          <| A.get model.page model.pages
      )
    InitialPath p -> ({model | uri <- Just (fixPath p)}, Effects.none)
    Preload -> preload model
    KeysDown s ->
      let
        dd f = Maybe.withDefault
          (model, Effects.none)
          (Maybe.map (toPage model)
            (f model.pages model.page))
        diff = Set.diff s model.keys |> Set.toList
        m = {model | keys <- s}
        f k = case k of
          37 {- Left  -} -> toPage m <| model.page - 1
          39 {- Right -} -> toPage m <| model.page + 1
          38 {- Up    -} -> dd findNext
          40 {- Down  -} -> dd findPrev
          72 {- h -} -> toPage m <| model.page - 1
          76 {- l -} -> toPage m <| model.page + 1
          75 {- k -} -> dd findNext
          74 {- j -} -> dd findPrev
          80 {- P -} -> ({ m | showPath <- not m.showPath }, Effects.none)
          _ -> (m, Effects.none)
      in
        f <| withDefault 0 <| List.head diff

findNext : A.Array Page -> Int -> Maybe Int
findNext a i = find (\i -> i + 1) i a

findPrev : A.Array Page -> Int -> Maybe Int
findPrev a i = find (\i -> i - 1) i a

find : (Int -> Int) -> Int -> A.Array Page -> Maybe Int
find f i a =
  let
    cur : Maybe String
    cur = Maybe.map (String.join "/" << List.reverse)
      (List.tail << List.reverse <| (String.split "/" (a!i)))

    get : Int -> A.Array Page -> Maybe String
    get i a = Maybe.map (\x -> x.path) (A.get i a)

    next : A.Array Page -> Int -> String -> Maybe Int
    next a i c = Maybe.andThen (get i a)
      (\s -> if String.startsWith c s then next a (f i) c else Just i)
  in
    Maybe.andThen cur (\c -> next a i c)

-- Binary search pages
search : String -> A.Array Page -> Maybe Int
search x xs =
  binarySearch' (compare x << (!) xs) (0, A.length xs - 1)

binarySearch' compareAt (low, high) =
  if high < low
  then Nothing
  else let
    mid = (low + high) // 2
    in case compareAt mid of
      LT -> binarySearch' compareAt (low, mid - 1)
      GT -> binarySearch' compareAt (mid + 1, high)
      EQ -> Just mid

(!) xs i = case A.get i xs of
  Just x -> x.path
  Nothing -> Debug.crash "Array index out of bounds"

-- Jump to page specified in model.uri
goto : Model -> Model
goto model =
  case model.uri of
    Just p ->
      case search (fixPath p) model.pages of
        Just i -> { model | page <- i }
        Nothing -> model
    Nothing -> model

-- Max page number
maxPage : Model -> Int
maxPage m = A.length m.pages - 1

-- Change to page, preloading and setting title
toPage : Model -> Int -> (Model, Effects Action)
toPage m page =
  ( {m | page <- clamp 0 (maxPage m) page}
  , Effects.batch
    ( [ Signal.send scrollBox.address True |> noop
      , pure SetPathNo
      , pure Preload
      ]
    )
  )

classes : List String -> Attribute
classes = String.join " " >> A.class

two : Maybe a -> Maybe b -> Maybe (a, b)
two x y = Maybe.andThen x (\f -> Maybe.map (\s -> (f, s)) y)

ht : List a -> Maybe (a, List a)
ht l = two (List.head l) (List.tail l)

ila : List a -> Maybe (List a, a)
ila l = let r = List.reverse l in
  two (Maybe.map List.reverse (List.tail r)) (List.head r) 

mapDef : (a -> b) -> b -> Maybe a -> b
mapDef f d x = Maybe.withDefault d (Maybe.map f x)

mc : Maybe a -> List a -> List a
mc x l = Maybe.withDefault l (Maybe.map (\x -> x :: l) x)

-- View
view : Signal.Address Action -> Model -> Html
view addr m =
  let
    disableNext = m.page >= maxPage m
    renderPath : (List String, String) -> Html
    renderPath (inits, last) =
      ol [classes ["breadcrumb"]]
        ((List.map (\i -> li [] [text i]) inits) ++ [li [classes ["active"]] [text last]])
    main =
        withDefault (text "nothing")
          <| Maybe.map (\url -> mImg addr url.path)
            <| A.get m.page m.pages
    path1 = if m.showPath
      then Maybe.map
        (\p -> renderPath p)
        (ila (String.split "/" (m.pages ! m.page)))
      else Nothing
  in
  div [classes ["max"]]
    (mc path1 [main])

mImg : Signal.Address Action -> String -> Html
mImg addr src =
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

-- Effects

-- Fetch pages
refresh : Effects Action
refresh = Http.get decode purl |> Task.toMaybe |> Task.map NewPages |> Effects.task

decode : Json.Decoder (A.Array String)
decode = Json.array Json.string

-- Preload pages
preload : Model -> (Model, Effects Action)
preload m =
  let
    pref p (effs, arr) =
      ( if p.pre
        then effs
        else Signal.send preloadBox.address (iurl ++ p.path) :: effs
      , if p.pre
        then arr
        else A.set p.no {p | pre <- True} arr
      )
    (effs, prep) = A.foldl pref ([], m.pages) (A.slice m.page (m.page + 5) m.pages)
  in
    ({m | pages <- prep}, effs |> Task.sequence |> Task.spawn |> noop)

-- Scroll to top
scrollBox : Signal.Mailbox Bool
scrollBox = Signal.mailbox True

-- Preload img
preloadBox : Signal.Mailbox String
preloadBox = Signal.mailbox ""
