{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Reflex.Dom.Tables.Internal
-- Description : Internal utilities for Reflex.Dom.Tables
-- Copyright   : (c) Yuri Meister
-- License     : BSD3
-- Stability   : experimental
--
-- Internal utilities for the Reflex.Dom.Tables module.
-- These functions handle higher-kinded data transformations and
-- dynamic value folding with triggers.
--
-- __Warning:__ This is an internal module. Functions may change
-- without notice. Use "Reflex.Dom.Tables" for the stable API.

module Reflex.Dom.Tables.Internal where

import Control.Monad.Fix
import Data.Functor.Identity
import Data.Map (Map)
import HigherKinded
import Reflex.Dom hiding (Attrs, El)



-- | Type alias for DOM elements in the Reflex context.
-- Represents an element with event results in the DOM builder space.
--
-- @t@ - Reflex timeline type
-- @m@ - Monad stack with DOM building capability
type El t m = Element EventResult (DomBuilderSpace m) t


-- | Apply a transformation to rows using higher-kinded data representation.
--
-- Converts regular row data to its HKD representation, applies the given
-- transformation, then converts back to the original type. This allows
-- type-safe operations on row data while maintaining the original structure.
--
-- @hkt@ - Higher-kinded type tag
-- @f@ - Functor containing rows
-- @row@ - Original row type
-- @rowHKD@ - Higher-kinded representation of row
--
-- ==== __Example__
--
-- > -- Apply a filter that works on HKD representation
-- > filteredRows = withHKDRows @MyHKT myHKDFilter originalRows
withHKDRows
  :: forall hkt f row rowHKD.
     ( Functor f
     , ConstructHKD rowHKD row hkt Identity
     )
  => (f (rowHKD Identity) -> f (rowHKD Identity))
  -> f row
  -> f row
withHKDRows f rows = resultRows
  where
    resultRows :: f row
    resultRows = runIdentity . fromHKD @rowHKD @row @hkt <$> resultRows'
    --
    resultRows' :: f (rowHKD Identity)
    resultRows' = f rows'
    --
    rows' :: f (rowHKD Identity)
    rows' = toHKD @rowHKD @row @hkt . Identity <$> rows


-- | Apply a transformation to indexed rows using higher-kinded data representation.
--
-- Similar to 'withHKDRows' but specifically for transformations that change
-- from key-indexed maps to integer-indexed maps with key-row pairs.
-- Commonly used for sorting operations where the result needs positional indices.
--
-- @hkt@ - Higher-kinded type tag
-- @rowHKD@ - Higher-kinded representation of row
-- @key@ - Original key type for row identification
-- @row@ - Original row type
--
-- ==== __Example__
--
-- > -- Sort rows and get them with positional indices
-- > sortedIndexedRows = withHKDIndexedRows @MyHKT mySortFunction keyedRows
withHKDIndexedRows
  :: forall hkt rowHKD key row.
     ( ConstructHKD rowHKD row hkt Identity
     )
  => (Map key (rowHKD Identity) -> Map Int (key, (rowHKD Identity)))
  -> Map key row
  -> Map Int (key, row)
withHKDIndexedRows f rows = resultRows
  where
    resultRows :: Map Int (key, row)
    resultRows = (\(key, row) -> (key, runIdentity $ fromHKD @rowHKD @row @hkt row)) <$> resultRows'
    --
    resultRows' :: Map Int (key, rowHKD Identity)
    resultRows' = f rows'
    --
    rows' :: Map key (rowHKD Identity)
    rows' = toHKD @rowHKD @row @hkt . Identity <$> rows


-- | Create a dynamic value that depends on both input and accumulated state,
-- with manual trigger control.
--
-- This function combines 'foldDyn' with external trigger capability.
-- It maintains an internal state @y@ that can be updated via the trigger,
-- and produces an output @x'@ that depends on both the state and input @x@.
--
-- @x@ - Input dynamic value type
-- @y@ - State/configuration type that can be updated via trigger
-- @x'@ - Output type produced by combining state and input
-- @t@ - Reflex timeline type
-- @m@ - Monad stack with required Reflex capabilities
--
-- Returns:
--
-- 1. The output dynamic value
-- 2. A tuple of:
--    * The current state as a dynamic
--    * A trigger function to update the state
--
-- ==== __Example__
--
-- > -- Create a filtered list with updateable filter config
-- > (filtered, (config, updateConfig)) <- foldDynWithTrigger
-- >   applyFilter
-- >   initialConfig
-- >   inputData
-- >
-- > -- Later: update the filter
-- > liftIO $ updateConfig (\cfg -> cfg { filterThreshold = 10 })
foldDynWithTrigger
  :: forall x y x' t m.
     ( MonadHold t m
     , MonadFix m
     , TriggerEvent t m
     , PerformEvent t m
     )
  => (Dynamic t y -> Dynamic t x -> Dynamic t x')
  -> y
  -> Dynamic t x
  -> m
      ( Dynamic t x'
      , ( Dynamic t y
        , (y -> y) -> IO ()
        )
      )
foldDynWithTrigger f xInit xDyn = do
  (triggerEv, trigger)
    :: ( Event t (y -> y)
       , (y -> y) -> IO ()
       )
    <- newTriggerEvent

  yDyn
    :: Dynamic t y
    <- foldDyn ($) xInit triggerEv

  let xDyn' :: Dynamic t x'
      xDyn' = f yDyn xDyn

  pure
    ( xDyn'
    , ( yDyn
      , trigger
      )
    )
