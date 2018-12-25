module Data.List.Infinite where

import Prelude

import Control.Comonad (class Comonad)
import Control.Extend (class Extend)
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
  defer f = List $ L.defer \_ -> step $ f unit

instance functorList :: Functor List where
  map :: forall a b. (a -> b) -> List a -> List b
  map f xs = List (go <$> runList xs)
    where
      go (Cons y ys) = Cons (f y) (f <$> ys)

instance semigroupList :: Semigroup (List a) where append = merge

instance applyList :: Apply List where
  apply :: forall a b. List (a -> b) -> List a -> List b
  apply fs as = List $ go <$> runList fs <*> runList as
    where
      go (Cons f ft) (Cons a at) = Cons (f a) (apply ft at)

instance applicativeList :: Applicative List where
  pure :: forall a. a -> List a
  pure = repeat

instance extendList :: Extend List where
  extend :: ∀ a b. (List a -> b) -> List a -> List b
  extend f = map f <<< duplicate

instance comonadList :: Comonad List where
  extract :: ∀ a. List a -> a
  extract = head

duplicate :: ∀ a. List a -> List (List a)
duplicate xs = case step xs of
  Cons _ ys -> List $ L.defer \_ -> Cons xs (duplicate ys)

-- traverse :: forall a b f. Applicative f => (a -> f b) -> List a -> f (List b)
-- traverse f = go <<< step
--   where
--     go (Cons x xs) = cons <$> f x <*> traverse f xs
--
-- sequence :: forall a f. Applicative f => List (f a) -> f (List a)
-- sequence = traverse identity

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
repeat = iterate identity

uncons :: forall a. List a -> { head :: a, tail :: List a }
uncons xs = case step xs of
                 Cons y ys -> { head: y, tail: ys }

head :: forall a. List a -> a
head = _.head <<< uncons

tail :: forall a. List a -> List a
tail = _.tail <<< uncons

filter :: forall a. (a -> Boolean) -> List a -> List a
filter p xs = List $ go <$> runList xs where
  go (Cons y ys) | p y = Cons y $ filter p ys
                 | otherwise = step $ filter p ys
                 
insertAt :: forall a. Int -> a -> List a -> List a
insertAt 0 x xs = x : xs
insertAt n x xs = List $ go <$> runList xs
  where
    go (Cons y ys) = Cons y (insertAt (n - 1) x ys)

deleteAt :: forall a. Int -> List a -> List a
deleteAt n xs = List $ go n <$> runList xs
  where
    go 0 (Cons y ys) = step ys
    go m (Cons y ys) = Cons y (deleteAt (m - 1) ys)

updateAt :: forall a. Int -> a -> List a -> List a
updateAt n x xs = List $ go n <$> runList xs
  where
    go 0 (Cons _ ys) = Cons x ys
    go m (Cons y ys) = Cons y $ updateAt (m - 1) x ys

alterAt :: forall a. Int -> (a -> Maybe a) -> List a -> List a
alterAt n f xs = List $ go n <$> runList xs
  where
    go 0 (Cons y ys) = case f y of
                            Just y' -> Cons y' ys
                            _ -> step ys
    go m (Cons y ys) = Cons y (alterAt (m - 1) f ys)

modifyAt :: forall a. Int -> (a -> a) -> List a -> List a
modifyAt n f = alterAt n (Just <<< f)

merge :: forall a. List a -> List a -> List a
merge xs ys = List $ go <$> runList xs <*> runList ys
  where
    go (Cons xh xt) (Cons yh yt) = Cons xh $ yh : merge xt yt

drop :: forall a. Int -> List a -> List a
drop n xs = List $ go n <$> runList xs
  where
    go 0 ys = ys
    go m (Cons y ys) = go (m - 1) (step ys)

dropWhile :: forall a. (a -> Boolean) -> List a -> List a
dropWhile p xs = go (step xs)
  where
    go (Cons y ys) | p y = go (step ys)
    go ys = fromStep ys

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

unfold :: ∀ s a. s -> (s -> Tuple a s) -> List a
unfold init next =
  List $ L.defer \_ ->
    let
      Tuple val state = next init
    in
      Cons val (unfold state next)
