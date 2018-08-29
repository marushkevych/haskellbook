-- working through tutorial "How to think about Monads"
-- https://gist.github.com/cscalfani/b63552922a8deb2656ecd5ec8a1a77a8

module ThinkAboutMonads where

  -- lets say we have debuggable type like (Float, String):
  type Debuggable a = (a, String)

  addDebuggable :: Float -> Debuggable Float
  addDebuggable x = (x + 10, "added 10\n")

  mutliplyDebuggable :: Float -> Debuggable Float
  mutliplyDebuggable y = (y * 100, "multiplied 100\n")

  -- lets write a function which would compose the debuggable functions
  composeDebuggable :: (b -> Debuggable c) -> (a -> Debuggable b) -> (a -> Debuggable c)
  composeDebuggable f f' x =
    let 
      (r1, s1) = f' x
      (r2, s2) = f r1
    in
      (r2, s1 ++ s2)

  -- now how do we compose debuggable function with regular function?
  -- we need to convert regular funciton to debuggable:
  mkFuncDebuggable :: (a -> b) -> (a -> Debuggable b)
  mkFuncDebuggable f x = mkDebuggable(f x)

  mkDebuggable :: a -> Debuggable a
  mkDebuggable x = (x, "")

  composed :: Float -> Debuggable Float
  composed = composeDebuggable (mkFuncDebuggable (+3)) mutliplyDebuggable

  -- now how would we apply the output of one FuncDebuggable function to another FuncDebuggable?
  applyDebuggable :: Debuggable a -> (a -> Debuggable b) -> Debuggable b
  applyDebuggable (a, s) f =
    let (a', s') = f a
    in (a', s ++ s')

  res = mkDebuggable 12 `applyDebuggable` addDebuggable `applyDebuggable` mutliplyDebuggable

  -- we can now rewrite composeDebuggable in terms of applyDebuggable
  composeDebuggable' :: (b -> Debuggable c) -> (a -> Debuggable b) -> (a -> Debuggable c)
  composeDebuggable' f f' x = f' x `applyDebuggable` f

-- So we are left with 3 very useful things that let us work easily and consistently with Debuggables:
-- 1. Our special side-effect type (Debuggable) - Monad
-- 2. A way to apply our special side-effect functions (applyDebuggable) - type class function (>>=) or "bind"
-- 3. A way to turn non-special types into our special type (mkDebuggable) - type class function "return"
--
-- class Applicative m => Monad m where
--   (>>=) :: m a -> (a -> m b) -> m b
--   return :: a -> m a

-- (mkFuncDebuggable and composeDebuggable have been left out since they can be written in terms of mkDebuggable and applyDebuggable, respectively)
--
-- In general terms, Debuggable is a computation with side-effects.
-- Functions that operate on Debuggables will operate on the computation and optionally produce a side-effect. 
-- The previous computation value is input and optionally modified by the function to produce a new computation value along with a side-effect.
-- The previous side-effect value is modified in a similar way EXCEPT it isn't modified by the function, but instead is modified by applyDebuggable.
-- So side-effects are "managed" by the code that applies them. This is a nice feature since we don't have to keep writing code to manage the side-effects over and over again.
