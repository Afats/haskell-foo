module Ex01 where

-- needed to display the picture in the playground
import Codec.Picture

-- our line graphics programming interface
import ShapeGraphics

-- Part 1
-- picture of a house


chimneyHouse :: Picture
chimneyHouse = [window, chimney, house, door]
  where
    window :: PictureObject
    window = Path windowCOs cyan Solid

    chimney :: PictureObject
    chimney = Path chimneyCOs green Solid

    house :: PictureObject
    house = Path houseCOs green Solid

    door :: PictureObject
    door = Path doorCOs red Solid

windowCOs :: [(Float, Float)]
windowCOs = [(350, 650), (350,550), (450,550), (450,650), (350,650)]

chimneyCOs :: [(Float, Float)]
chimneyCOs = [(615,325), (615,250), (650,250), (650,363)]

houseCOs :: [(Float, Float)]
houseCOs = [(300,750), (300,450), (270,450), (500,200),
         (730,450), (700,450) (700,750)]

doorCOs :: [(Float, Float)]
doorCOs = [(550,750), (550,550), (650,550), (650,750)]

----------------------------------------------------------------------------------

housePic :: Picture
housePic = [door, house]
  where
    house :: PictureObject
    house = Path houseCOs green Solid
    door :: PictureObject
    door = Path doorCOs red Solid

cyan :: Colour
cyan = Colour 96 192 255 255




-- Part 2
movePoint :: Point -> Vector -> Point
movePoint (Point x y) (Vector xv yv)
  = Point (x + xv) (y + yv)

movePictureObject :: Vector -> PictureObject -> PictureObject
movePictureObject vec (Path points colour lineStyle) = error "'movePictureObject' unimplemented"

-- add other cases



-- Part 3


-- generate the picture consisting of circles:
-- [Circle (Point 400 400) (400/n) col Solid SolidFill,
--  Circle (Point 400 400) 2 * (400/n) col Solid SolidFill,
--  ....
--  Circle (Point 400 400) 400 col Solid SolidFill]
simpleCirclePic :: Colour -> Float -> Picture
simpleCirclePic col n = error "'simpleCirclePic' unimplemented"


-- use 'writeToFile' to write a picture to file "ex01.png" to test your
-- program if you are not using Haskell for Mac
-- e.g., call
-- writeToFile [house, door]

writeToFile pic
  = writePng "ex01.png" (drawPicture 3 pic)