module Maths exposing (..)

sum : Int -> Int
sum n =
  if n <= 0 then
    0
  else
    n + sum (n - 1)

sum2 : Int -> Int
sum2 n =
  if n < 0 then 0 else
  case n of
    0 -> 0
    _ -> n + sum2 (n - 1)

sumWithoutRecursion : Int -> Int
sumWithoutRecursion n =
  round (1 / 2 * toFloat n * toFloat (n + 1))

factor : Int -> Int
factor n =
  if n == 0 || n == 1 then
    1
  else
    n * factor (n - 1)

tailRecFactorHelp : Int -> Int -> Int
tailRecFactorHelp n acc =
  if n == 0 || n == 1 then
    acc
  else
    tailRecFactorHelp (n - 1) (n * acc)

tailRecFactor : Int -> Int
tailRecFactor n = tailRecFactorHelp n 1
