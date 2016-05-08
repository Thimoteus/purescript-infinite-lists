module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, print)

import Data.List.Infinite (List, (!!), drop, dropWhile, iterate, head, merge, delete, deleteAt)

nats :: List Int
nats = iterate (add 1) 0

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

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  print test1
  print test2
  print test3
  print test4
  print test5
  print test6
