module Data.List.Infinite where

import Prelude

import Control.Lazy as Z

import Data.Lazy as L
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

data Step a = Cons a (List a)

derive instance eqStep :: Eq a => Eq (Step a)

derive instance ordStep :: Ord a => Ord (Step a)

newtype List a = List (L.Lazy (Step a))

derive instance eqList :: Eq a => Eq (List a)

derive instance ordList :: Ord a => Ord (List a)

instance lazyList :: Z.Lazy (List a) where
  defer f = List $ L.defer (step <<< f)

instance functorList :: Functor List where
  map f xs = List (go <$> runList xs)
    where
      go (Cons x xs) = Cons (f x) (f <$> xs)

instance semigroupList :: Semigroup (List a) where append = merge

instance applyList :: Apply List where
  apply fs as = List $ go <$> runList fs <*> runList as
    where
      go (Cons f ft) (Cons a at) = Cons (f a) (apply ft at)

traverse :: forall a b f. Applicative f => (a -> f b) -> List a -> f (List b)
traverse f xs = go $ step xs
  where
    go (Cons x xs) = cons <$> f x <*> traverse f xs

sequence :: forall a f. Applicative f => List (f a) -> f (List a)
sequence = traverse id

runList :: forall a. List a -> L.Lazy (Step a)
runList (List xs) = xs

step :: forall a. List a -> Step a
step = L.force <<< runList

fromStep :: forall a. Step a -> List a
fromStep = List <<< pure

index :: forall a. List a -> Int -> a
index xs = go (step xs)
  where
    go (Cons x _) 0 = x
    go (Cons _ rest) n = go (step rest) (n - 1)

infixl 8 index as !!

cons :: forall a. a -> List a -> List a
cons x xs = List $ L.defer \_ -> Cons x xs

infixr 6 cons as :

insertBy :: forall a. (a -> a -> Ordering) -> a -> List a -> List a
insertBy cmp x xs = List $ go <$> runList xs
  where
    go ys@(Cons y ys') =
      case cmp x y of
           GT -> Cons y $ insertBy cmp x ys'
           _ -> Cons x $ fromStep ys

insert :: forall a. Ord a => a -> List a -> List a
insert = insertBy compare

iterate :: forall a. (a -> a) -> a -> List a
iterate f x = Z.fix \ xs -> cons x (f <$> xs)

repeat :: forall a. a -> List a
repeat = iterate id

uncons :: forall a. List a -> { head :: a, tail :: List a }
uncons xs = case step xs of
                 Cons x xs -> { head: x, tail: xs }

head :: forall a. List a -> a
head = _.head <<< uncons

tail :: forall a. List a -> List a
tail = _.tail <<< uncons

filter :: forall a. (a -> Boolean) -> List a -> List a
filter p xs = List $ go <$> runList xs where
  go (Cons y ys) | p y = Cons y $ filter p ys
                 | otherwise = go $ step xs

insertAt :: forall a. Int -> a -> List a -> List a
insertAt 0 x xs = x : xs
insertAt n x xs = List $ go <$> runList xs
  where
    go (Cons y ys) = Cons y (insertAt (n - 1) x ys)

deleteAt :: forall a. Int -> List a -> List a
deleteAt n xs = List $ go n <$> runList xs
  where
    go 0 (Cons y ys) = step ys
    go n (Cons y ys) = Cons y (deleteAt (n - 1) ys)

updateAt :: forall a. Int -> a -> List a -> List a
updateAt n x xs = List $ go n <$> runList xs
  where
    go 0 (Cons _ ys) = Cons x ys
    go n (Cons y ys) = Cons y $ updateAt (n - 1) x ys

alterAt :: forall a. Int -> (a -> Maybe a) -> List a -> List a
alterAt n f xs = List $ go n <$> runList xs
  where
    go 0 (Cons y ys) = case f y of
                            Just y' -> Cons y' ys
                            _ -> step ys
    go n (Cons y ys) = Cons y (alterAt (n - 1) f ys)

modifyAt :: forall a. Int -> (a -> a) -> List a -> List a
modifyAt n f = alterAt n (Just <<< f)

merge :: forall a. List a -> List a -> List a
merge xs ys = List $ go <$> runList xs <*> runList ys
  where
    go (Cons xh xt) (Cons yh yt) = Cons xh $ yh : merge xt yt

drop :: forall a. Int -> List a -> List a
drop n xs = List $ go n <$> runList xs
  where
    go 0 xs = xs
    go n (Cons x xs) = go (n - 1) (step xs)

dropWhile :: forall a. (a -> Boolean) -> List a -> List a
dropWhile p xs = go (step xs)
  where
    go (Cons x xs) | p x = go (step xs)
    go xs = fromStep xs

deleteBy :: forall a. (a -> a -> Boolean) -> a -> List a -> List a
deleteBy p x xs = List $ go <$> runList xs
  where
    go (Cons y ys) | p x y = step ys
                   | otherwise = Cons y (deleteBy p x ys)

delete :: forall a. Eq a => a -> List a -> List a
delete = deleteBy eq

zipWith :: forall a b c. (a -> b -> c) -> List a -> List b -> List c
zipWith f xs ys = List $ go <$> runList xs <*> runList ys
  where
    go (Cons a as) (Cons b bs) = Cons (f a b) (zipWith f as bs)

zip :: forall a b. List a -> List b -> List (Tuple a b)
zip = zipWith Tuple
