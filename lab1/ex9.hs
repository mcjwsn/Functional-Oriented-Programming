roots :: (Double, Double, Double) -> (Double, Double)
roots (a, b, c) = ( (-b - d) / e, (-b + d) / e )
   where d = sqrt (b * b - 4 * a * c)
         e = 2 * a

unitVec2D :: (Double, Double) -> (Double, Double)
unitVec2D (x, y) = (x / norm, y / norm)
  where
    norm = sqrt (x^2 + y^2) 

pole :: (Double, Double, Double)->Double
pole (a,b,c) = sqrt(p*(p-a)*(p-b)*(p-c))
	where
		p = ( a + b + c)/2
