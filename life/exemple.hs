
module Main where
import Drawing.Activity

main :: IO ()
main =
  activityOf
    [ onMouseDown SetPoint, onKeyDown (\_ _ -> Rotate) ]
    initial
    handle
    draw

initial :: Point
initial = (40, 0)

data Action = Rotate | SetPoint Point

handle :: Action -> UpdateM Point Action ()
handle Rotate =
  modify $ transformPoint $ rotation $ pi / 4
handle (SetPoint p) =
  put p

draw :: Point -> Drawing
draw p =
    polyline [(0, 0), p] <>
    translated p (colored red $ solidCircle 10)

