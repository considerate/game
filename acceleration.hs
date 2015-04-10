-- var g = 9.82;

-- function(vy0, time){
-- 	vy = vy0 + g*t
--}

g :: Float
g = (-9.82)

velocity :: Float -> Float -> Float
velocity vy0 t = (fromInteger $ round $ (vy0 + g*t) * (10^2)) / (10.0^^2)

distance :: Float -> Float -> Float
distance vy0 t = (fromInteger $ round $ (vy0*t + (g*(t^2)/2)) * (10^2)) / (10.0^^2)

vel :: Float -> IO()
vel vy0 = putStrLn ("t(s) v(m/s) d(m)\n" ++ 
		  			"0.0   " ++ show (velocity vy0 0) ++ "   " ++ show (distance vy0 0) ++ "\n" ++
		  			"0.5   " ++ show (velocity vy0 0.5) ++ "   " ++ show (distance vy0 0.5) ++ "\n" ++
					"1.0   " ++ show (velocity vy0 1.0) ++ "   " ++ show (distance vy0 1.0))
					
  