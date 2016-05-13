module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, print)

import Data.List.Infinite (List, (!!), drop, dropWhile, iterate, head, merge, delete, deleteAt, repeat, insert)

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

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  print test1
  print test2
  print test3
  print test4
  print test5
  print test6
  print test7
  print test8
