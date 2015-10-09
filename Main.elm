module Main (main) where

import Effects exposing (Never)
import Signal exposing ((<~))
import StartApp
import Task
import Viewer exposing (appStartBox, preloadBox, scrollBox)

app = StartApp.start
  { init = Viewer.init
  , view = Viewer.view
  , update = Viewer.update
  , inputs = Viewer.inputs
  }

main = app.html

port tasks : Signal (Task.Task Never ())
port tasks = app.tasks

port scrollTop : Signal Bool
port scrollTop = scrollBox.signal

port preloadImg : Signal String
port preloadImg = preloadBox.signal

port appStart : Signal (Task.Task error ())
port appStart =
  Signal.constant
    (Signal.send appStartBox.address ())

