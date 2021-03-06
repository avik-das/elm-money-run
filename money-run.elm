import Graphics.Element (..)
import Graphics.Collage (..)
import Color (..)
import Keyboard
import Time (..)
import Signal
import Random (..)
import List (..)

-- MODEL -----------------------------------------------------------------------

type alias Player = {
    x : Float
  , y : Float
  , vx : Float
  , vy : Float
  }

startPlayer : Player
startPlayer = {
    x = -screenWidth / 2 + blockWidth + playerWidth / 2
  , y = screenHeight / 2
  , vx = 0
  , vy = 0
  }
  
infixl 9 !!
(!!) : List a -> Int -> a
xs !! n  = head (drop n xs)

xToColumn : Float -> (Int, Int)
xToColumn x = 
  let columnFloat = (x - (blockWidth / 2)) / blockWidth + 8
  in (floor columnFloat, ceiling columnFloat)

playerColumns : Player -> List Int -> (Int, Int)
playerColumns {x} bs = 
  let (lc, rc) = xToColumn x
  in (bs !! lc, bs !! rc) 

columnToY : Int -> Float
columnToY c = (toFloat <| (c - 6) * blockHeight) - (blockWidth / 2)

columnToHeight : Int -> Float
columnToHeight c = columnToY c + (blockHeight / 2) + (playerHeight / 2)

bottomBlockHeight : World -> Float
bottomBlockHeight w =
 let p = w.player
     (lc, rc) = playerColumns p w.blocks
     (lh, rh) = (columnToHeight lc, columnToHeight rc)
 in (if lh <= rh then rh else lh)

isGrounded : World -> Bool
isGrounded w =
   let p = w.player
       (lc, rc) = playerColumns p w.blocks
       (lh, rh) = (columnToHeight lc, columnToHeight rc)
   in p.y <= lh || p.y <= rh

canMoveInto : Float -> Int -> World -> Bool
canMoveInto dt dx w =
  let p = w.player
      (lb, rb) = playerColumns { p | x <- xUpdatedByVx dt (toFloat dx) p } w.blocks
      (lh, rh) = (columnToHeight lb, columnToHeight rb)
  in if | dx < 0 -> lh <= p.y
        | dx > 0 -> rh <= p.y
        | otherwise -> True

type alias World = {
    startx : Float
  , player : Player
  , blocksGenerator : Generator Int
  , blocks : List Int
  , blocksSeed : Seed
  }

blocks : (Generator Int, List Int, Seed)
blocks =
  let g = int 1 10
      (ls, s) = generateBlocks g (initialSeed 12345)
  in (g, ls, s)

startWorld : World
startWorld =
  let (blocksGenerator, blocksList, blocksSeed) = blocks
  in {
    startx = 0
  , player = startPlayer
  , blocksGenerator = blocksGenerator
  , blocks = blocksList
  , blocksSeed = blocksSeed
  }

generateBlocks : Generator Int -> Seed -> (List Int, Seed)
generateBlocks g s = generateBlocks' g s 18 []

generateBlocks' : Generator Int -> Seed -> Int -> List Int -> (List Int, Seed)
generateBlocks' g s n blocks =
  if | n == 0 -> (blocks, s)
     | otherwise -> let (blocks', s') = newBlock g s blocks
                    in generateBlocks' g s' (n - 1) blocks'

safeLast : a -> List a -> a
safeLast z = foldl (\x y -> x) z

newBlock : Generator Int -> Seed -> List Int -> (List Int, Seed)
newBlock g s blocks =
  let (b, s') = generate g s
      lastBlock = safeLast 0 blocks
      b' = if b - lastBlock > 4 then lastBlock + 4 else b
  in (blocks ++ [b'], s')

type alias Arrows = {
    x : Int
  , y : Int
  }

-- STATE UPDATE ----------------------------------------------------------------

dvx = 1
dvy = 1
g = -9.8 / 300

updatePlayer : (Player -> Player) -> World -> World
updatePlayer f w =
  let p = w.player
      p' = f p
  in { w | player <- p' }

walk' : Arrows -> Float -> World -> Player
walk' {x} dt w =
  let p = w.player
      leftOfScreen  = p.x - playerWidth / 2 <= -screenWidth / 2
      rightOfScreen = p.x + playerWidth / 2 >=  screenWidth / 2
  in { p |
    x <- if | leftOfScreen  -> -screenWidth / 2 + playerWidth / 2
            | rightOfScreen ->  screenWidth / 2 - playerWidth / 2
            | otherwise -> p.x,
    vx <- if | leftOfScreen && x < 0 -> 0
             | rightOfScreen && x > 0 -> 0
             | not <| canMoveInto dt x w -> 0
             | otherwise -> toFloat x }

walk : Arrows -> Float -> World -> World
walk arrows dt w = { w | player <- walk' arrows dt w }

jump : Arrows -> World -> World
jump {y} w = updatePlayer
  (\p -> { p | vy <- if | isGrounded w -> p.vy + 2.8 * toFloat y
                        | otherwise -> p.vy }) w

gravity : Float -> World -> World
gravity dt = updatePlayer (\p -> { p | vy <- p.vy + g * dt })

updatedY : Float -> World -> Float
updatedY dt w =
  let p = w.player
  in if | isFallingThroughFloor w -> bottomBlockHeight w
        | otherwise -> p.y +  p.vy * dvy * dt

isFallingThroughFloor : World -> Bool
isFallingThroughFloor w =
  let p = w.player in isGrounded w && p.vy < 0

xUpdatedByVx : Float -> Float -> Player -> Float
xUpdatedByVx dt vx p = p.x + vx * dvx * dt

physics : Float -> World -> World
physics dt w = updatePlayer
  (\p -> { p | x <- xUpdatedByVx dt p.vx p,
               y <- updatedY dt w,
               vy <- if isFallingThroughFloor w then 0 else p.vy }) w

step : (Float, Arrows) -> World -> World
step (dt, keys) =
  walk keys dt >> jump keys >> gravity dt >> physics dt

-- RENDERING -------------------------------------------------------------------

playerWidth = 20
playerHeight = 28

blockWidth = 20
blockHeight = 20

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

blockPicture : Form
blockPicture =
  let url = "http://172.21.137.90:8000/tiles.png"
      img = croppedImage (0, 0) blockWidth blockHeight url
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

renderedBlockColumn : Int -> Int -> Form
renderedBlockColumn col n =
  let x col = (toFloat <| (col - 8) * blockWidth) + (blockWidth / 2)
      y i = columnToY i
      pictures = map (\i -> blockPicture |> move (x col, y i)) [1..n]
  in group pictures

renderedBlocks : List Int -> Form
renderedBlocks blocks =
  group <| indexedMap renderedBlockColumn blocks

render : World -> Element
render w = renderWithBorder [
    bg,
    renderedBlocks w.blocks,
    positionedPlayer w.player
  ]

-- MAIN ------------------------------------------------------------------------

input : Signal (Float, Arrows)
input =
  let delta = Signal.map (\x -> x / 10) (fps 25)
      inputTuples = Signal.map2 (,) delta Keyboard.arrows
  in Signal.sampleOn delta inputTuples

main : Signal Element
main =
  let updates = Signal.foldp step startWorld input
  in Signal.map render updates
