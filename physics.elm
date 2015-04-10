import Text(..)
import Graphics.Element(..)
import Signal
import Keyboard
import Time(fps)

earth  = {m=5.972e24, pos=(0, -6371000),a=(0,0), v=(0,0)}
ball = {m=0.5, pos=(0, 10),a = (0,0), v=(0,0)}
g_con = 6.67384e-11

sub (x1,y1) (x2,y2) = (x1-x2, y1-y2)
add (x1,y1) (x2,y2) = (x1+x2, y1+y2)
dot (x1,y1) (x2,y2) = x1*x2 + y1*y2
scale k (x,y) = (k*x, k*y)
len v = sqrt (dot v v)
norm v = scale (1/(len v)) v

gravity o1 o2 = 
    let p1=o1.pos
        p2=o2.pos
        m1=o1.m
        m2=o2.m
        r = sub p2 p1
    in 
    scale ((-g_con*m1*m2)/(dot r r)/m2) (norm r)

-- FPS loop
input =
    let delta = fps 10
    in
    Signal.sampleOn delta (Signal.map2 (,) delta Keyboard.arrows)

step (t,keys) [tellus,me] =
    let
        acc = gravity tellus me
        vel = add me.v (scale t acc)
        position = add me.pos (scale t vel)
        newMe = {me | a <-acc, v <- vel, pos <- position}
    in
        [tellus,newMe]

render [tellus,me] =
    asText (me.a, me.v, me.pos)

-- Main
main  =
    Signal.map render (Signal.foldp step [earth,ball] input)


