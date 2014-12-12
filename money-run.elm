import Graphics.Element (..)
import Graphics.Collage (..)
import Color (..)
import Keyboard
import Time (..)
import Signal

-- MODEL -----------------------------------------------------------------------

type alias Player = {
    x : Float
  , y : Float
  , vx : Float
  , vy : Float
  }

startPlayer : Player
startPlayer = {
    x = 100
  , y = 50
  , vx = 0
  , vy = 0
  }

type alias World = {
    startx : Float,
    player : Player
  }

startWorld : World
startWorld = {
    startx = 0
  , player = startPlayer
  }
  
type alias Arrows = {
    x : Int
  , y : Int
  }

-- STATE UPDATE ----------------------------------------------------------------

dvx = 1

walk : Arrows -> Player -> Player
walk {x} p = { p | vx <- toFloat x }

physics : Float -> Player -> Player
physics dt p = { p | x <- p.x + p.vx * dvx * dt }

step : (Float, Arrows) -> Player -> Player
step (dt, keys) =
  walk keys >> physics dt

-- RENDERING -------------------------------------------------------------------

screenWidth  = 320
screenHeight = 240

black : Color
black = greyscale 0.5

bg : Form
bg =
  let url = "http://172.21.137.90:8000/screens.png"
      img = croppedImage (0, 0) screenWidth screenHeight url
  in toForm img

playerPicture : Form
playerPicture =
  let url = "http://172.21.137.90:8000/tiles.png"
      img = croppedImage (0, 20) 20 28 url
  in toForm img

positionedPlayer : Player -> Form
positionedPlayer p = playerPicture |> move (p.x, p.y)

renderWithBorder : List Form -> Element
renderWithBorder forms =
  let w = screenWidth + 2
      h = screenHeight + 2
      line = solid black
      shape = rect w h
      border = outlined line shape
      formsWithBorder = [border] ++ forms
  in collage w h formsWithBorder

render : Player -> Element
render p = renderWithBorder [
    bg,
    positionedPlayer p
  ]

-- MAIN ------------------------------------------------------------------------

input : Signal (Float, Arrows)
input =
  let delta = Signal.map (\x -> x / 10) (fps 25)
      inputTuples = Signal.map2 (,) delta Keyboard.arrows
  in Signal.sampleOn delta inputTuples

main : Signal Element
main =
  let updates = Signal.foldp step startPlayer input
  in Signal.map render updates
