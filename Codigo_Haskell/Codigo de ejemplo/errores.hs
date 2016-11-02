import Data.Maybe
import Data.Either


--    divisionSegura 6 2  ==  Just 3
--    divisionSegura 6 0  ==  Nothing
divisionSegura :: Int -> Int -> Maybe Int
divisionSegura _ 0 = Nothing
divisionSegura m n = Just (m `div` n)

-- λ> fromJust (divisionSegura 6 2)
-- 3

--    divisionSegura2 6 2  ==  Right 3
--    divisionSegura2 6 0  ==  Left "No se puede dividir 6 por cero"
--    divisionSegura2 8 0  ==  Left "No se puede dividir 8 por cero"
divisionSegura2 :: Int -> Int -> Either String Int
divisionSegura2 m 0 = Left  ("No se puede dividir " ++ show m ++ " por cero")
divisionSegura2 m n = Right (m `div` n)

--    divisionSegura3 6 2  ==  Right 3
--    divisionSegura3 6 0  ==  Left 6
divisionSegura3 :: Int -> Int -> Either Int Int
divisionSegura3 m 0 = Left  m
divisionSegura3 m n = Right (m `div` n)

data ERROR = DivisionSegura
             deriving Show

--    divisionSegura4 6 2  ==  Right 3
--    divisionSegura4 6 0  ==  Left DivisionSegura
divisionSegura4 :: Int -> Int -> Either ERROR Int
divisionSegura4 m 0 = Left  DivisionSegura
divisionSegura4 m n = Right (m `div` n)

-- λ> let (Right x) = divisionSegura4 6 2
-- λ> x
-- 3

--    extrae (divisionSegura4 6 2)  ==  3
extrae :: Either a b -> b
extrae (Right x) = x
