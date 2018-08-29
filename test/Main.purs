module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (logShow)
import Data.List.Infinite (List, delete, deleteAt, drop, dropWhile, head, insert, iterate, merge, repeat, (!!))

nats :: List Int
nats = iterate (add 1) 0

nats2 :: List (List Int)
nats2 = repeat nats

zeroes :: List Int
zeroes = repeat 0

nats3 :: List (List Int)
nats3 = insert zeroes nats2

test1 :: Boolean
test1 = nats !! 100 == 100

test2 :: Boolean
test2 = head (drop 100 nats) == 100

test3 :: Int
test3 = head $ dropWhile (_ < 50) nats

test4 :: Int
test4 = head $ drop 20 $ merge nats nats

test5 :: Int
test5 = head $ delete 0 nats

test6 :: Int
test6 = (deleteAt 10 nats) !! 10

test7 :: Int
test7 = head $ head $ nats2

test8 :: Int
test8 = head $ head $ nats3

main :: Effect Unit
main = do
  logShow test1
  logShow test2
  logShow test3
  logShow test4
  logShow test5
  logShow test6
  logShow test7
  logShow test8
