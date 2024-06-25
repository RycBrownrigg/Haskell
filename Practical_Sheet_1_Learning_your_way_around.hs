import Type.Reflection (Con') -- Importing the Con' type from the Type.Reflection module.

'{-

Practical Sheet 1:
Learning your way around

The aim of this tutorial is to help you familiarise yourselves with Haskell and GHC. Try to write the types of all your functions, so as to get used to thinking within the confines of a strictly typed language. It is also best to write your programs as literate scripts, with ample comments in between functions. Remember that programming requires practice.
-}'


-- 1.	Define the function circleArea, which calculates the area of a circle with a given radius.

-- The Floating a => constraint means that the function works for any type that is an instance of the Floating type class. The function takes a radius of type a and returns an area of type a.
-- Formula: π * r^2


circleArea :: Floating a => a -> a -- A type class constraint that restricts the type of 'a' to Floating types and specifies that the function takes an argument of type 'a' (the radius) and returns a value of type 'a' (the area).
circleArea radius = pi * radius ^ 2 -- This function calculates the area of a circle given its radius.

-- 2.	Define the function dbl, which given an integer as input, returns twice the value. Using dbl, define quad, which multiplies the input by four.

-- | Double the value of an integer.
dbl :: Num a => a -> a -- A type class constraint indicating that a must be a number and specifies that the function takes an argument of type 'a' and returns a value of type 'a'.
dbl x = x * 2 --The function 'dbl' doubles a given number.

-- | Multiply the input by four using dbl.
quad :: Num a => a -> a-- A type class constraint indicating that a must be a number and specifies that the function takes an argument of type 'a' and returns a value of type 'a'.
quad x = dbl (dbl x) -- The function 'quad' quadruples a given number by doubling it twice.

-- 3.	Define a function sumN which, given an integer n, recursively calculates the sum of the first n integers.

-- | Calculate the sum of the first n integers recursively.
sumN :: (Eq a, Num a) => a -> a --  A type class constraint indicating that 'a' must be both an instance of the 'Eq' (equality) type class and the 'Num' (numeric) type class. This means 'a' can be any type that supports numeric operations and equality checks
sumN 0 = 0  -- Base case: When n is 0, the sum is 0.
sumN n = n + sumN (n - 1) -- Recursive case: For any positive integer n, the sum is n plus the sum of the first n-1 integers.

-- 4.	A rectangle can be described by giving the coordinates of the bottom left and top right hand corners.
--    (a)	Define a type synonym Rectangle to describe rectangles.
--    (b)	Define functions to calculate (i) the length of a rectangle; (ii) the breadth of a rectangle;
--    (iii) the top-left, bottom-left, top-right and bottom-right points of a rectangle (as four separate functions); (iv) given a point and a rectangle, return whether the point lies inside the rectangle.
--    (c)	Define a function areaR, which, given a rectangle, returns its area.
--    (d)	Define a function intersect, which given two rectangles returns whether they intersect (hint: an easy way is to check whether a corner of one rectangle lies inside the other).


-- | (a) Define a type synonym Rectangle to describe rectangles.
type Rectangle = ((Int, Int), (Int, Int)) -- A type synonym for ((Int, Int), (Int, Int)) and represents a pair of pairs of integers.

-- | (b)(i) Define a function to calculate the length of a rectangle. The function 'lengthR' effectively computes the horizontal distance (or length) between the two x-coordinates of the rectangle's corners, which is the length of the rectangle along the x-axis.
lengthR :: Rectangle -> Int -- A type signature 'lengthR :: Rectangle -> Int' specifies that the function lengthR takes a Rectangle as input and returns an Int as output.
lengthR ((x1, _), (x2, _)) = abs (x2 - x1) -- The function takes a tuple ((x1, _), (x2, _)) as its argument. This tuple represents a rectangle defined by two corners. Each corner is a pair of integers (Int, Int) with '(x1, _)' represents the coordinates of the first corner, where x1 is the x-coordinate, and _ is a wildcard that ignores the y-coordinate and '(x2, _)' represents the coordinates of the second corner, where x2 is the x-coordinate, and _ is a wildcard that ignores the y-coordinate. The function computes the absolute difference between x2 and x1 using abs (x2 - x1). This difference represents the length of the rectangle along the x-axis.

-- | (b)(ii) Define a function to calculate the breadth of a rectangle. The function 'breadthR' effectively computes the vertical distance (or height) between the two y-coordinates of the rectangle's corners, which is the breadth (or height) of the rectangle along the y-axis.
breadthR :: Rectangle -> Int-- A type signature 'breadthR :: Rectangle -> Int' specifies that the function lengthR takes a Rectangle as input and returns an Int as output.
breadthR ((_, y1), (_, y2)) = abs (y2 - y1) -- The function takes a tuple ((_, y1), (_, y2)) as its argument. This tuple represents a rectangle defined by two corners. Each corner is a pair of integers (Int, Int) '(_, y1)' represents the coordinates of the first corner, where _ is a wildcard that ignores the x-coordinate, and 'y1' is the y-coordinate. '(_, y2)' represents the coordinates of the second corner, where _ is a wildcard that ignores the x-coordinate, and 'y2' is the y-coordinate. The function computes the absolute difference between 'y2' and 'y1' using 'abs (y2 - y1)'.

-- | (b)(iii) Get the top-left point of a rectangle.
topLeft :: Rectangle -> (Int, Int) -- topLeft is a function that takes a Rectangle as input and returns a tuple (Int, Int) representing the coordinates of the top-left corner of the rectangle.
topLeft ((x1, y1), _) = (x1, y1) -- The function takes a tuple '((x1, y1), _)' as its argument. This tuple represents a rectangle defined by two corners. '(x1, y1)' is one corner of the rectangle, which we consider to be the top-left corner. The second part of the tuple (_) is a wildcard pattern, which means ignore the second corner. The function simply extracts the coordinates (x1, y1) of the top-left corner and returns them as a tuple.

-- | (b)(iii) Get the bottom-left point of a rectangle.
bottomLeft :: Rectangle -> (Int, Int) -- bottomLeft is a function that takes a Rectangle as input and returns a tuple (Int, Int) representing the coordinates of the bottom-left corner of the rectangle.
bottomLeft ((x1, _), (_, y2)) = (x1, y2) -- The function takes a tuple '((x1, _), (_, y2))' as its argument. This tuple represents a rectangle defined by two corners. '(x1, _)' represents the coordinates of the first corner, where 'x1' is the x-coordinate, and '_' is a wildcard that ignores the y-coordinate.'(_, y2)' represents the coordinates of the second corner, where '_' is a wildcard that ignores the x-coordinate, and 'y2' is the y-coordinate. The function returns a tuple (x1, y2) representing the bottom-left corner of the rectangle.

-- | (b)(iii) Get the top-right point of a rectangle.
topRight :: Rectangle -> (Int, Int) -- topRight is a function that takes a Rectangle as input and returns a tuple (Int, Int) representing the coordinates of the top-right corner of the rectangle.
topRight (_, (x2, y2)) = (x2, y2) -- The function takes a tuple (_, (x2, y2)) as its argument. This tuple represents a rectangle defined by two corners. '_' represents the first corner, where '_' is a wildcard that ignores both the x-coordinate and the y-coordinate. '(x2, y2)' represents the coordinates of the second corner, where 'x2' is the x-coordinate, and 'y2' is the y-coordinate. The function returns a tuple '(x2, y2)' representing the top-right corner of the rectangle.

-- | (b)(iii) Get the bottom-right point of a rectangle.
bottomRight :: Rectangle -> (Int, Int) -- bottomRight is a function that takes a Rectangle as input and returns a tuple (Int, Int) representing the coordinates of the bottom-right corner of the rectangle.
bottomRight ((_, y1), (x2, _)) = (x2, y1) -- The function takes a tuple '((_, y1), (x2, _))' as its argument. This tuple represents a rectangle defined by two corners. '(_, y1)' represents the coordinates of the first corner, where '_' is a wildcard that ignores the x-coordinate, and 'y1' is the y-coordinate. '(x2, _)' represents the coordinates of the second corner, where 'x2' is the x-coordinate, and '_' is a wildcard that ignores the y-coordinate. The function returns a tuple '(x2, y1)' representing the bottom-right corner of the rectangle.

-- | (b)(iv) Check if a point lies inside a rectangle.
pointInside :: (Int, Int) -> Rectangle -> Bool -- pointInside is a function that takes a point and a rectangle as input and returns a boolean value indicating whether the point lies inside the rectangle or not.
pointInside (x, y) ((x1, y1), (x2, y2)) =
    x >= x1 && x <= x2 && y >= y1 && y <= y2 -- Defines a function pointInside that takes a point '(x, y)' and a rectangle '((x1, y1), (x2, y2))' as input and returns a boolean value indicating whether the point lies inside the rectangle or not. It checks if the x-coordinate of the point 'x' is within the range defined by the x-coordinates of the rectangle's corners, 'x1' and 'x2'. It also checks if the y-coordinate of the point 'y' is within the range defined by the y-coordinates of the rectangle's corners, 'y1' and 'y2'. If both conditions are met, the point is inside the rectangle, and the function returns 'True'; otherwise, it returns 'False'.

-- | (c) Define a function areaR, which, given a rectangle, returns its area.
areaR :: Rectangle -> Int -- A type signature 'areaR :: Rectangle -> Int' specifies that the function areaR takes a Rectangle as input and returns an Int as output.
areaR rect = lengthR rect * breadthR rect -- The function areaR calculates the area of a rectangle by multiplying its length and breadth. It takes a rectangle 'rect' as input and computes the area by multiplying the length of the rectangle (calculated using the 'lengthR' function) by the breadth of the rectangle (calculated using the 'breadthR' function).

-- | (d) Define a function intersect, which given two rectangles returns whether they intersect.
intersect :: Rectangle -> Rectangle -> Bool -- A type signature 'intersect :: Rectangle -> Rectangle -> Bool' specifies that the function intersect takes two Rectangles as input and returns a Bool as output.
intersect rect1 rect2 =
    let (x1, y1) = bottomLeft rect1
        (x2, y2) = topRight rect1
        (x3, y3) = bottomLeft rect2
        (x4, y4) = topRight rect2
    in x1 <= x4 && x2 >= x3 && y1 <= y4 && y2 >= y3 -- The function intersect takes two rectangles 'rect1' and 'rect2' as input and returns a boolean value indicating whether the rectangles intersect or not. It extracts the coordinates of the bottom-left and top-right corners of both rectangles and checks if the x-coordinates and y-coordinates of the corners overlap. If there is an overlap in both x and y directions, the rectangles intersect, and the function returns 'True'; otherwise, it returns 'False'.

-- 5.	An trapezoid is a quadrilateral with two parallel sides. An orthogonal trapezoid is a trapezoid with two orthogonal internal angles:
--(a)	The area of an orthogonal trapezoid is 1 δ(h1 + h2). Write a function which calculates this. 

-- An orthogonal trapezoid has two parallel sides and two orthogonal (right) angles. The formula for its area is given as Area = .5 δ(h1+h2), where h1 and h2 are the lengths of the two parallel sides, and δ is the distance between them.

-- A function to calculate the area of an orthogonal trapezoid
areaOrthogonalTrapezoid :: Double -> Double -> Double -> Double -- A type signature 'areaOrthogonalTrapezoid :: Double -> Double -> Double -> Double' specifies that the function areaOrthogonalTrapezoid takes three Doubles as input and returns a Double as output.
areaOrthogonalTrapezoid delta h1 h2 = 0.5 * delta * (h1 + h2) -- The function areaOrthogonalTrapezoid calculates the area of an orthogonal trapezoid using the formula 0.5 * delta * (h1 + h2), where delta is the distance between the parallel sides and h1 and h2 are the lengths of the parallel sides.

-- (b)Haskell functions can be given other functions as parameters. We will be talking more about this later in the course. For the moment, try to define a function approxArea1 which given a function (from Double to Double) calculates an approximation of the area under the graph between 0 and 1 by reducing it to a trapezoid:

-- The function definition would look something like:
-- approxArea1 f = ...(f 0) ...(f 1) ...

-- A function approxArea1 which approximates the area under a curve 𝑓 between 0 and 1 by treating it as a trapezoid.

-- A function to approximate the area under a curve using one trapezoid between 0 and 1.
approxArea1 :: (Double -> Double) -> Double -- A type signature 'approxArea1 :: (Double -> Double) -> Double' specifies that the function approxArea1 takes a function from Double to Double as input and returns a Double as output.
approxArea1 f = areaOrthogonalTrapezoid 1 (f 0) (f 1) -- The function approxArea1 calculates an approximation of the area under the curve represented by the function f between 0 and 1 by treating it as a trapezoid. It uses the areaOrthogonalTrapezoid function to calculate the area of the trapezoid with delta = 1, h1 = f 0, and h2 = f 1.

-- (c)	Now define approx1 which also takes the left and right bounds (which would replace 0 and 1). I will extend the previous function to accept arbitrary bounds l and r instead of just 0 and 1.

-- Function to approximate the area under a curve between given bounds l and r
approx1 :: (Double -> Double) -> Double -> Double -> Double -- A type signature 'approx1 :: (Double -> Double) -> Double -> Double -> Double' specifies that the function approx1 takes a function from Double to Double and two Doubles as input and returns a Double as output.
approx1 f l r = areaOrthogonalTrapezoid (r - l) (f l) (f r) -- The function approx1 calculates an approximation of the area under the curve represented by the function f between the bounds l and r by treating it as a trapezoid. It uses the areaOrthogonalTrapezoid function to calculate the area of the trapezoid with delta = r - l, h1 = f l, and h2 = f r.

-- (d)	Define approx2 which uses two trapezoids to approximate the area under a curve between two given bounds.

-- Function to approximate the area under a curve between given bounds l and r using two trapezoids
approx2 :: (Double -> Double) -> Double -> Double -> Double -- A type signature 'approx2 :: (Double -> Double) -> Double -> Double -> Double' specifies that the function approx2 takes a function from Double to Double and two Doubles as input and returns a Double as output.
approx2 f l r = 
    let mid = (l + r) / 2
    in approx1 f l mid + approx1 f mid r -- The function approx2 approximates the area under the curve represented by the function f between the bounds l and r using two trapezoids. It calculates the midpoint of the interval (l + r) / 2 and then calculates the area under the curve using two trapezoids: one from l to mid and the other from mid to r. The total area is the sum of the areas of the two trapezoids.

-- (e)	Define approxArea which takes a number ε, two bounds l and r, and a function f, and then, if l and r are sufficiently close to each other (their difference is less than ε) returns the area using approx1, otherwise splits the range into two (from l to (l+r)/2 and from (l+r)/2 to r) and returns the sum of the approximate areas for both ranges (using approxArea) itself.

-- Define a recursive function approxArea that continues to divide the interval until the subintervals are smaller than a given tolerance ϵ.

-- Recursive function to approximate the area under a curve with a given tolerance ε, between bounds l and r
approxArea :: (Double -> Double) -> Double -> Double -> Double -> Double -- A type signature 'approxArea :: (Double -> Double) -> Double -> Double -> Double -> Double' specifies that the function approxArea takes a function from Double to Double, a Double (tolerance ε), and two Doubles (bounds l and r) as input and returns a Double as output.
approxArea f epsilon l r
    | (r - l) < epsilon = approx1 f l r
    | otherwise =
        let mid = (l + r) / 2
        in approxArea f epsilon l mid + approxArea f epsilon mid r -- The function approxArea approximates the area under the curve represented by the function f between the bounds l and r with a given tolerance ε. If the difference between the bounds is less than ε, it calculates the area using approx1. Otherwise, it divides the interval into two subintervals (l to mid and mid to r) and recursively calculates the approximate areas for each subinterval. The total area is the sum of the approximate areas of the subintervals.












