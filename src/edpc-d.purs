module Main where

import Prelude
import Control.Monad.State.Class
import Control.Monad.State.Trans
import Data.Array as A
import Data.Char as Char
import Data.CodePoint.Unicode as C
import Data.Foldable
import Data.Unfoldable
import Data.Int as Int
import Data.Int64 (Int64)
import Data.Int64 as Int64
import Data.Maybe
import Data.Newtype (unwrap)
import Data.Ord.Max
import Data.String as S
import Data.String.CodePoints as SC
import Data.Tuple
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Partial.Unsafe (unsafePartial)

readInt :: String -> Maybe (Tuple Int String)
readInt str =
  let str' = S.dropWhile C.isSpace str
      -- partition :: (a -> Bool) -> [a] -> ([a], [a]) が欲しい
      digits = S.takeWhile C.isDecDigit str'
      rest = S.drop (S.length digits) str'
   in (\n -> {- 部分適用できず -} Tuple n rest) <$> Int.fromString digits

readInt64 :: String -> Maybe (Tuple Int64 String)
readInt64 str =
  let str' = S.dropWhile C.isSpace str
      -- partition :: (a -> Bool) -> [a] -> ([a], [a]) が欲しい
      digits = S.takeWhile C.isDecDigit str'
      rest = S.drop (S.length digits) str'
   in (\n -> {- 部分適用できず -} Tuple n rest) <$> Int64.fromString digits

int :: forall m. Partial => MonadState String m => m Int
int = state (fromJust <$> readInt)

int64 :: forall m. Partial => MonadState String m => m Int64
int64 = state (fromJust <$> readInt64)

ints2 :: forall m. Partial => MonadState String m => m (Tuple Int Int)
ints2 = Tuple <$> int <*> int

ints64_2 :: forall m. Partial => MonadState String m => m (Tuple Int64 Int64)
ints64_2 = Tuple <$> int64 <*> int64

dropSpace' :: forall m. (MonadState String m) => m Unit
dropSpace' = modify_ $ \s ->
  let len = S.length $ S.takeWhile C.isSpace s
   in S.drop len s

line :: forall m. (MonadState String m) => m String
line = do
  dropSpace'
  s <- S.takeWhile (\c -> c /= {- これ面倒です -} SC.codePointFromChar '\n') <$> get
  modify_ $ S.drop $ 1 + S.length s
  pure s

ints :: forall m. Partial => MonadState String m => m (Array Int)
ints = unfoldr readInt <$> line

int64s :: forall m. Partial => MonadState String m => m (Array Int64)
int64s = unfoldr readInt64 <$> line

main' :: Partial => StateT String Effect Unit
main' = do
  Tuple n wMax <- ints2
  -- この型表記は簡単にしたい
  wvs :: Array (Tuple Int Int64) <- replicateA n (Tuple <$> int <*> int64)

  -- weight -> maximum value
  let arr0 = A.cons (Max (Int64.fromInt 0)) $ A.replicate wMax (mempty :: Max Int64)
  let res = foldl step arr0 wvs
        where
          step :: Array (Max Int64) -> Tuple Int Int64 -> Array (Max Int64)
          step arr (Tuple dw dv) = A.mapWithIndex f arr
            where
              f :: Int -> Max Int64 -> Max Int64
              f w v = case arr A.!! (w - dw) of
                Nothing -> v
                Just v' -> v <> Max (unwrap v' + dv) {- Max Int64 同士は加算できない (Semiring ではない) -}

  -- liftEffect $ log $ show res
  let ans = fromJust $ maximum res
  liftEffect $ log $ if ans == mempty then "-1" else (\s -> {- Int64 の接尾辞 l を消す -} S.take (S.length s - 1) s) (show (unwrap ans))

main :: Effect Unit
main = do
  input <- FS.readTextFile UTF8 "/dev/stdin"
  unsafePartial $ evalStateT main' input
