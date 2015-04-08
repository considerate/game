import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import Keyboard
import Signal
import Time (..)
import List
import Window
import Debug
-- MODEL
player = { x=0, y=0, vx=0, vy=0, dir="right" }


jumpSpeed = 8
moveSpeed = 8

-- UPDATE -- ("m" is for player)
jump {y} m = if y > 0 && m.y == 0 then { m | vy <- jumpSpeed } else m

gravity t m = if m.y > 0 then { m | vy <- m.vy - t/4 } else m

physics t m = { m | x <- m.x + t*m.vx , y <- max 0 (m.y + t*m.vy) }

walk {x} m = { m | vx <- moveSpeed*toFloat x
     , dir <- if x < 0 then "left" else
                 if x > 0 then "right" else m.dir }

step (dt, keys) =
    jump keys >> gravity dt >> walk keys >> physics dt

fitIn: (Float,Float) -> (Float,Float) -> (Float,Float,Float)
fitIn (w,h) (screenW,screenH) =
    let ratio = w/h
        screenRatio = screenW/screenH
    in
       if screenRatio > ratio then (w * screenH/h, screenH,screenH/h) else
          (screenW, h * screenW/w, screenW/w)

-- DISPLAY
render (w',h') player =
    let (w,h,scaling) = fitIn (900,500) (toFloat w',toFloat h')
        pright_src = "res/pright.png"
        pleft_src = "res/pleft.png"
    in Debug.watch (toString (scaling)) <| container w' h' middle <|
        collage (round w) (round h) [
            toForm (image (round (900*scaling)) (round (500*scaling)) "res/bg_s4.png"),

            -- This section displays the image of the player depending on what way he is turning. 
            if player.dir == "right" then toForm (image (round (52*scaling)) (round (82*scaling)) pright_src) |> move (player.x, player.y-80) else
              toForm (image (round (52*scaling)) (round (82*scaling)) pleft_src) |> move (player.x, player.y-80)
        ]

input = let delta = Signal.map (\t -> t/20) (fps 25)
        in  Signal.sampleOn delta (Signal.map2 (,) delta Keyboard.arrows)

main = Signal.map2 render Window.dimensions (Signal.foldp step player input)
