{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , StandaloneDeriving
  , FlexibleInstances
  , FlexibleContexts
  , MultiParamTypeClasses
  , TupleSections
  , TypeOperators
  , OverloadedStrings
  #-}
module Pickler where

import Control.Category
import Control.Arrow (Arrow, arr)
import Control.Applicative hiding (optional, many, some)
import Control.Error (fmapLT)
import Control.Monad.Identity
import Control.Monad.State (StateT (StateT), runStateT, mapStateT)
import Control.Monad.Trans.Either
import Control.Monad.Trans (lift)
import Data.Label
import Data.Label.Maybe ((:~>))
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Prelude hiding (head, tail, (.), id)

import qualified Control.Monad.State as State
import qualified Data.Label.Maybe    as Partial
import qualified Data.Label.Abstract as Abstract

type Partial  i o = StateT i (EitherT [Text] Identity) o

data Point i j o = Point
  { _parser  :: i -> EitherT [Text] Identity (o, i)
  , _printer :: j -> EitherT [Text] Identity (i, j)
  }

instance Functor (Point i j) where
  fmap f p = Point (runStateT (f <$> StateT (_parser p)))
                   (_printer p)

instance Monoid i => Applicative (Point i j) where
  pure a = Point (runStateT (pure a)) (runStateT (pure mempty))
  a <*> b = Point (runStateT (StateT (_parser a) <*> StateT (_parser b)))
                  (runStateT ((<>) <$> StateT (_printer a) <*> StateT (_printer b)))

instance Monoid i => Alternative (Point i j) where
  empty = Point (runStateT empty) (runStateT empty)
  a <|> b = Point (runStateT (StateT (_parser  a) <|> StateT (_parser  b)))
                  (runStateT (StateT (_printer a) <|> StateT (_printer b)))

infix 5 >-

(>-) :: (j :~> o) -> Pickler i o -> Point i j o
(>-) f (Pickler (Point p q)) = Point p $ \i ->
  case Partial.get f i of
    Nothing -> left ["projection failed"]
    Just v  -> fmap (const i) <$> q v

newtype Pickler i o = Pickler { unPickler :: Point i o o }

pickler :: Partial i o -> Partial o i -> Pickler i o
pickler p q = Pickler (Point (runStateT p) (runStateT q))

parser :: Pickler i o -> Partial i o
parser = StateT . _parser  . unPickler

printer :: Pickler i o -> Partial o i
printer = StateT . _printer . unPickler

parse :: Pickler i o -> i -> Either [Text] (o, i)
parse p = runIdentity . runEitherT . _parser (unPickler p)

print :: Pickler i o -> o -> Either [Text] (i, o)
print p = runIdentity . runEitherT . _printer (unPickler p)

many :: Monoid i => Pickler i a -> Pickler i [a]
many p = Pickler $
  (:) <$> head >- p
      <*> tail >- many p
      <|> pure []

some :: Monoid i => Pickler i o -> Pickler i [o]
some p = Pickler $
  (:) <$> head >- p
      <*> tail >- many p

optional :: Monoid i => Pickler i a -> Pickler i (Maybe a)
optional v = Pickler $
  Just <$> just >- v
       <|> pure Nothing

tuple :: Monoid i => Pickler i a -> Pickler i b -> Pickler i (a, b)
tuple a b = Pickler $
  (,) <$> fstL >- a
      <*> sndL >- b

-------------------------------------------------------------------------------

infix  5 `parses`
infix  5 `prints`

parses :: Pickler i o -> Partial i o -> Pickler i o
parses p q = pickler q (printer p)

prints :: Pickler i o -> Partial o i -> Pickler i o
prints p q = pickler (parser p) q

reverted :: Pickler o i -> Pickler i o
reverted (Pickler (Point p q)) = Pickler (Point q p)

(?) :: Pickler i o -> Text -> Pickler i o
p ? e = pickler (err (parser p)) (err (printer p))
  where err = mapStateT (fmapLT (const [e]))

-------------------------------------------------------------------------------
-- Token parsers.

peek :: Partial [a] a
peek =
  do m <- State.gets listToMaybe
     case m of
       Nothing -> lift (left ["eof"])
       Just x  -> pure x

next :: Partial [i] ()
next = State.modify (drop 1)

any :: Pickler [o] o
any = pickler p q
  where p = peek <* next
        q = State.gets pure

satisfy :: (o -> Bool) -> Pickler [o] o
satisfy f = pickler p q
  where e = lift $ left ["satisfy"]
        p = do x <- peek
               if f x then x <$ next else e
        q = do x <- State.get
               if f x then pure [x] else e

token :: Text -> Pickler [Text] Text
token t = satisfy (== t) ? t

prefix :: Text -> Pickler [Text] o -> Pickler [Text] o
prefix t p = Pickler $ constL t >- token t *> id >- p

constL :: Arrow arr => a -> Lens arr f a
constL t = Abstract.lens (arr (const t)) (arr snd)

-------------------------------------------------------------------------------

-- Lenses for maybe.

just :: Maybe a :~> a
just = Partial.lens id (fmap . const . Just)

-- Lenses for tuples.

fstL :: Arrow arr => Lens arr (a, b) a
fstL = Abstract.lens (arr fst) (arr (\(a, (_, b)) -> (a, b)))

sndL :: Arrow arr => Lens arr (a, b) b
sndL = Abstract.lens (arr snd) (arr (\(b, (a, _)) -> (a, b)))

-- Lenses for lists.

head :: [a] :~> a
head = Partial.lens
  listToMaybe
  (\x -> list Nothing (\_ xs -> Just (x:xs)))

tail :: [a] :~> [a]
tail = Partial.lens
  (list Nothing (const Just))
  (\xs -> list Nothing (\x _ -> Just (x:xs)))

list :: b -> (a -> [a] -> b) -> [a] -> b
list d _ []     = d
list _ f (x:xs) = f x xs

