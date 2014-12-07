{-# LANGUAGE GADTs #-}
module Data.Machine.Group
  ( groupingOn
  , taggedBy
  , partitioning
  , starve
  , awaitUntil
  )where
import Data.Machine

isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)

groupingOn :: Monad m => (a -> a -> Bool) -> ProcessT m a b -> ProcessT m a b
groupingOn f m = taggedBy f ~> partitioning m

taggedBy :: Monad m => (a -> a -> Bool) -> ProcessT m a (Either () a)
taggedBy f = construct $ await >>= go
  where go x = do
          yield (Right x)
          y <- await
          if not (f x y) then (yield (Left ()) >> go y) else go y


-- | Run a machine multiple times over partitions of the input stream specified by 
-- Left () values.
partitioning :: Monad m => ProcessT m a b -> ProcessT m (Either () a) b
partitioning s = go s where
  go m = MachineT $ runMachineT m >>= \v -> case v of
    -- Machine stops (possibly before inputs)
    Stop            -> runMachineT $ awaitUntil isLeft (const $ go s)

    -- Machine yields a value
    Yield o r       -> return $ Yield o (go r)

    -- Machine waits for a value
    Await f Refl r  -> return $ Await g Refl (starve r $ encased Stop)
      where
        -- No change: unwrap input and give to underlying machine.
        g (Right a) = go (f a)
        -- New group: starve r, then wait for more input (restarting machine)
        -- NOTE: if Left () happens with no more input, this will be wrong-ish(?)
        -- Meaning of "Left ()" is kind of "stop old machine and _immediately_ start new one."
        g (Left  ()) = starve r $ go s

-- | Run a machine with no input until it's stopped.
starve :: Monad m => ProcessT m a b -> MachineT m k b -> MachineT m k b
starve m cont = MachineT $ runMachineT m >>= \v -> case v of
  Stop            -> runMachineT cont -- Continue with cont instead of stopping
  Yield o r       -> return $ Yield o (starve r cont)
  Await _ Refl r  -> runMachineT (starve r cont)

-- Read inputs until a condition is met, then behave as cont with
-- input matching condition as first input of cont.
-- If await fails, stop.
awaitUntil :: Monad m => (a -> Bool) -> (a -> ProcessT m a b) -> ProcessT m a b
awaitUntil f cont = encased $ Await g Refl (encased Stop)
  where g a = if f a then cont a else awaitUntil f cont
