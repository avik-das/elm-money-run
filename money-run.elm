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
  
isGrounded : Player -> Bool
isGrounded { y } = y <= bottomY

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
dvy = 1
g = -9.8 / 300

walk : Arrows -> Player -> Player
walk {x} p = { p | vx <- toFloat x }

jump : Arrows -> Player -> Player
jump {y} p = { p | vy <- if isGrounded p then p.vy + 2.8 * toFloat y else p.vy }

gravity : Float -> Player -> Player
gravity dt p = { p | vy <- p.vy + g * dt }

updateY : Float -> Player -> Float
updateY dt p = if isFallingThroughFloor p then bottomY else p.y +  p.vy * dvy * dt

isFallingThroughFloor : Player -> Bool
isFallingThroughFloor p = isGrounded p && p.vy < 0

physics : Float -> Player -> Player
physics dt p = { p | x <- p.x + p.vx * dvx * dt, y <- updateY dt p, vy <- if isFallingThroughFloor p then 0 else p.vy }

step : (Float, Arrows) -> Player -> Player
step (dt, keys) =
  walk keys >> jump keys >> gravity dt >> physics dt

-- RENDERING -------------------------------------------------------------------

playerWidth = 20
playerHeight = 28

screenWidth  = 320
screenHeight = 240

bottomY = -screenHeight / 2 + playerHeight / 2

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
      img = croppedImage (0, 20) playerWidth playerHeight url
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