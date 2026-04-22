module Tlon.Core.Rng
  ( drawBounded,
    drawFromList,
    nextSeed,
  )
where

nextSeed :: Int -> Int
nextSeed seed =
  let modulus = 2147483647
      multiplier = 48271
      raw = (seed * multiplier) `mod` modulus
   in if raw == 0 then 1 else raw

drawBounded :: Int -> Int -> (Int, Int)
drawBounded upperBound seed
  | upperBound <= 0 = error "drawBounded requires a positive upper bound."
  | otherwise =
      let seed' = nextSeed seed
       in (seed' `mod` upperBound, seed')

drawFromList :: [a] -> Int -> (a, Int)
drawFromList values seed =
  case values of
    [] -> error "drawFromList requires a non-empty list."
    _ ->
      let (index, seed') = drawBounded (length values) seed
       in (values !! index, seed')
