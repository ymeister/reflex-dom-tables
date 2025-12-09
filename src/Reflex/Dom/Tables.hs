{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
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
-- Module      : Reflex.Dom.Tables
-- Description : Dynamic tables for Reflex-DOM with sorting and filtering
-- Copyright   : (c) Yuri Meister
-- License     : BSD3
-- Stability   : experimental
--
-- Dynamic tables for Reflex-DOM with sorting and filtering capabilities.
--
-- This module provides a flexible table widget that supports:
--
-- * Dynamic row data updates
-- * Customizable column configuration with nested headers
-- * Sorting by columns using custom comparison functions
-- * Filtering rows based on column predicates
-- * Per-element attribute customization
--
-- = Basic Usage
--
-- > import Reflex.Dom.Tables
-- >
-- > -- Simple table with default configuration
-- > table <- elTable myData def { tableConfig_columns = myColumns }
--
-- = Advanced Features
--
-- The module uses higher-kinded data types for type-safe sorting and filtering
-- configurations. Sorting no longer requires @Ord@ instances - you provide
-- comparison functions directly, allowing for custom sort logic and
-- non-sortable columns.

module Reflex.Dom.Tables where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Writer.Strict (MonadWriter(..), execWriter)
import Data.Default
import Data.Functor.Compose
import Data.Functor.Contravariant
import Data.Functor.Identity
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, isNothing)
import Data.Semigroup (Any(..), All(..))
import Data.Traversable
import GHC.Generics ((:*:)(..))
import HigherKinded
import Reflex.Dom hiding (Attrs, El)
import Reflex.Dom.Attrs

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
#endif

import Reflex.Dom.Tables.Internal

-- | Configuration for building dynamic tables.
--
-- The configuration specifies column structure, attributes for various
-- table elements, and how to render cells.
--
-- @key@ - Type used to uniquely identify rows
-- @row@ - Type of row data (often @Dynamic t@ for dynamic rows)
-- @th@ - Type produced by header cells
-- @td@ - Type produced by data cells
-- @t@ - Reflex timeline type
-- @m@ - Monad stack
data TableConfig key row th td t m = TableConfig
  { tableConfig_columns :: [TableColumn key row (m th) (m td)]
    -- ^ Column definitions including headers and cell renderers
  , tableConfig_tableAttrs :: [Attrs t m]
    -- ^ Attributes for the @\<table\>@ element
  , tableConfig_theadAttrs :: [Attrs t m]
    -- ^ Attributes for the @\<thead\>@ element
  , tableConfig_tbodyAttrs :: [Attrs t m]
    -- ^ Attributes for the @\<tbody\>@ element
  , tableConfig_thAttrs :: Maybe th -> [Attrs t m]
    -- ^ Attributes for @\<th\>@ elements, can depend on header value
  , tableConfig_trAttrs :: key -> row -> [Attrs t m]
    -- ^ Attributes for @\<tr\>@ elements, can depend on row key and data
  , tableConfig_tdAttrs :: td -> key -> row -> [Attrs t m]
    -- ^ Attributes for @\<td\>@ elements, can depend on cell value, row key and data
  }

instance Reflex t => Default (TableConfig key row th td t m) where
  def = TableConfig
    { tableConfig_columns = def
    , tableConfig_tableAttrs = def
    , tableConfig_theadAttrs = def
    , tableConfig_tbodyAttrs = def
    , tableConfig_thAttrs = \_ -> def
    , tableConfig_trAttrs = \_ _ -> def
    , tableConfig_tdAttrs = \_ _ _ -> def
    }

-- | Column configuration for tables.
--
-- Columns can be simple data columns ('TD'), columns with headers ('TH'),
-- or hierarchical columns with nested sub-columns ('THs').
--
-- @key@ - Row identifier type
-- @row@ - Row data type
-- @th@ - Header cell result type
-- @td@ - Data cell result type
data TableColumn key row th td
  = TD (key -> row -> td)
    -- ^ Data column without header
  | TH (th, key -> row -> td)
    -- ^ Column with header and data renderer
  | THs (th, [TableColumn key row th td])
    -- ^ Hierarchical column with header spanning multiple sub-columns

-- | Extract data cell renderers from column configuration.
-- Flattens nested column structures to get all leaf-level data renderers.
getTDs :: [TableColumn key row th td] -> [key -> row -> td]
getTDs tableCols = go tableCols []
  where
    go [] !tds = tds
    go (TD td : cols) !tds = td : go cols tds
    go (TH (_, td) : cols) !tds = td : go cols tds
    go (THs (_, thcols) : cols) !tds = go thcols $ go cols tds

-- | Extract header rows from column configuration.
-- Returns a list of header rows, where each cell contains the header value
-- and its colspan. Handles hierarchical headers by returning multiple rows.
getTHRows :: [TableColumn key row th td] -> [[Maybe (th, Int)]]
getTHRows tableCols = takeWhile (\ths -> (not $ null ths) && (not $ all (isNothing) ths)) $ fmap (go tableCols) [0 :: Int ..]
  where
    go [] _ = []
    go (TD _ : cols) 0 = go cols 0
    go (TH (h, _) : cols) 0 = Just (h, 1) : go cols 0
    go (THs (h, thcols) : cols) 0 = Just (h, length thcols) : go cols 0
    go (TD _ : cols) i = Nothing : go cols i
    go (TH (_, _) : cols) i = Nothing : go cols i
    go (THs (_, thcols) : cols) i = go thcols (i - 1) <> go cols i

-- | Transform a table column by applying functions to headers and data renderers.
--
-- The first function transforms header values, the second transforms data cell renderers.
-- Recursively applies to nested columns in 'THs'.
mapTableColumn
  :: ( (th -> th')
     , ((key -> row -> td) -> (key' -> row' -> td'))
     )
  -> TableColumn key row th td
  -> TableColumn key' row' th' td'
mapTableColumn (thF, tdF) tableColumn = go tableColumn
  where
    go (TD td) = TD $ tdF td
    go (TH (th, td)) = TH (thF th, tdF td)
    go (THs (th, thcols)) = THs (thF th, go <$> thcols)

-- | Flipped version of 'mapTableColumn' for more convenient usage with operators.
forTableColumn
  :: TableColumn key row th td
  -> ( (th -> th')
     , ((key -> row -> td) -> (key' -> row' -> td'))
     )
  -> TableColumn key' row' th' td'
forTableColumn = flip mapTableColumn

-- | Result of rendering a table.
--
-- Contains references to all created DOM elements and their associated values,
-- allowing for further manipulation or event handling after table creation.
--
-- @key@ - Row identifier type
-- @row@ - Row data type
-- @th@ - Header cell value type
-- @td@ - Data cell value type
-- @t@ - Reflex timeline type
-- @m@ - Monad stack
data Table key row th td t m = Table
  { table_tableEl :: El t m
    -- ^ The @\<table\>@ element
  , table_theadEl :: El t m
    -- ^ The @\<thead\>@ element
  , table_tbodyEl :: El t m
    -- ^ The @\<tbody\>@ element
  , table_thEls :: [[El t m]]
    -- ^ Header cell elements organized by row
  , table_trEls :: Map key (El t m)
    -- ^ Row elements mapped by row key
  , table_tdEls :: Map key [El t m]
    -- ^ Data cell elements mapped by row key
  , table_thVals :: [[th]]
    -- ^ Header values organized by row
  , table_trVals :: Map key [td]
    -- ^ Data cell values mapped by row key
  , table_ths :: [[(El t m, th)]]
    -- ^ Header elements paired with their values
  , table_trs :: Map key (El t m, [(El t m, td)])
    -- ^ Row elements paired with their cell elements and values
  }

-- | Build a dynamic table widget.
--
-- Creates a table that automatically updates when the input data changes.
-- Supports hierarchical headers, custom attributes, and produces references
-- to all created elements.
--
-- ==== __Example__
--
-- > data Person = Person { name :: Text, age :: Int }
-- >
-- > let columns = [ TH (text "Name", \_ p -> text (name p))
-- >               , TH (text "Age", \_ p -> text (T.pack $ show $ age p))
-- >               ]
-- > table <- elTable people def { tableConfig_columns = columns }
elTable
  :: forall key row th td t m.
     ( DomBuilder t m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , Ord key
     , Eq row
     )
  => Dynamic t (Map key row)
  -> TableConfig key (Dynamic t row) th td t m
  -> m (Dynamic t (Table key row th td t m))
elTable rows cfg = do
  let columns = tableConfig_columns cfg
      headerRows = getTHRows columns
      cols = getTDs columns
      tableAttrs = tableConfig_tableAttrs cfg
      theadAttrs = tableConfig_theadAttrs cfg
      tbodyAttrs = tableConfig_tbodyAttrs cfg
      thAttrs = tableConfig_thAttrs cfg
      trAttrs = tableConfig_trAttrs cfg
      tdAttrs = tableConfig_tdAttrs cfg

  (tableEl, ((theadEl, ths), (tbodyEl, trsDyn))) <- elAttrs' "table" tableAttrs $ do

    thead :: (El t m, [[(El t m, th)]]) <-
      elAttrs' "thead" theadAttrs $
        for headerRows $ \headers ->
          el "tr" $
            fmap catMaybes $ for headers $ \case
              Nothing -> do
                void $ elAttrs' "th" (thAttrs Nothing) $ blank
                pure Nothing
              Just (header, colspan) -> mdo
                th@(_, thVal) <- elAttrs' "th"
                  ( thAttrs (Just thVal)
                  <> case colspan of
                      1 -> []
                      _ -> ["colspan" ~: show colspan]
                  ) header
                pure $ Just th

    tbody :: (El t m, Dynamic t (Map key (El t m, [(El t m, td)]))) <-
      elAttrs' "tbody" tbodyAttrs $
        listWithKey rows $ \k row ->
          elAttrs' "tr" (trAttrs k row) $
            for cols $ \col -> mdo
              td@(_, tdVal) <- elAttrs' "td" (tdAttrs tdVal k row) $ col k row
              pure td

    pure (thead, tbody)

  pure $ ffor trsDyn $ \trs ->
    Table
      { table_tableEl = tableEl
      , table_theadEl = theadEl
      , table_tbodyEl = tbodyEl
      , table_thEls = fmap fst <$> ths
      , table_trEls = fst <$> trs
      , table_tdEls = ffor trs $ fmap fst . snd
      , table_thVals = fmap snd <$> ths
      , table_trVals = ffor trs $ fmap snd . snd
      , table_ths = ths
      , table_trs = trs
      }

-- | Table sorting configuration using higher-kinded data.
--
-- Maps each column to an optional comparison function.
-- Columns with 'Nothing' are not sortable.
type TableSortConfig columnsHKD = columnsHKD ColumnSortConfig
-- | Per-column sorting configuration.
-- 'Nothing' means the column is not sortable.
-- 'Just' contains a comparison function for sorting.
type ColumnSortConfig = Maybe `Compose` Comparison

-- | Create sorted table rows with dynamic sort configuration.
--
-- Returns sorted rows as a map indexed by display position, along with
-- the current sort configuration and a trigger to update it.
--
-- The sorting uses comparison functions instead of requiring @Ord@ instances,
-- allowing for custom sort logic and non-sortable columns.
--
-- ==== __Example__
--
-- > (sortedRows, (sortConfig, updateSort)) <- tableSortedRows rows
-- > -- Use sortedRows in your table
-- > -- Call updateSort to change sorting
tableSortedRows
  :: forall hkt rowHKD key row t m.
     ( MonadHold t m
     , MonadFix m
     , TriggerEvent t m
     , PerformEvent t m
     --
     , ConstructHKD rowHKD row hkt Identity
     , IsHKD rowHKD hkt ColumnSortConfig
     , IsHKD rowHKD hkt Identity
     , IsHKD rowHKD hkt (Identity :*: Identity)
     )
  => Dynamic t (Map key row)
  -> m
      ( Dynamic t (Map Int (key, row))
      , ( Dynamic t (TableSortConfig rowHKD)
        , (TableSortConfig rowHKD -> TableSortConfig rowHKD) -> IO ()
        )
      )
tableSortedRows = foldDynWithTrigger go initSortConfig
  where
    initSortConfig = pureHKD @rowHKD @hkt $ Compose Nothing
    --
    go = liftA2 (tableSortRows @hkt)

-- | Sort table rows using the provided configuration.
--
-- Pure function that sorts a map of rows according to the sort configuration.
-- Returns a map indexed by display position.
tableSortRows
  :: forall hkt rowHKD key row.
     ( ConstructHKD rowHKD row hkt Identity
     , IsHKD rowHKD hkt ColumnSortConfig
     , IsHKD rowHKD hkt Identity
     , IsHKD rowHKD hkt (Identity :*: Identity)
     )
  => TableSortConfig rowHKD
  -> Map key row
  -> Map Int (key, row)
tableSortRows sortConfig = withHKDIndexedRows @hkt (tableSortRows' @hkt sortConfig)

--

-- | Create sorted table rows with dynamic sort configuration (HKD variant).
--
-- Like 'tableSortedRows' but works directly with higher-kinded row data.
tableSortedRows'
  :: forall hkt key rowHKD t m.
     ( MonadHold t m
     , MonadFix m
     , TriggerEvent t m
     , PerformEvent t m
     --
     , IsHKD rowHKD hkt ColumnSortConfig
     , IsHKD rowHKD hkt Identity
     , IsHKD rowHKD hkt (Identity :*: Identity)
     )
  => Dynamic t (Map key (rowHKD Identity))
  -> m
      ( Dynamic t (Map Int (key, rowHKD Identity))
      , ( Dynamic t (TableSortConfig rowHKD)
        , (TableSortConfig rowHKD -> TableSortConfig rowHKD) -> IO ()
        )
      )
tableSortedRows' = foldDynWithTrigger go initSortConfig
  where
    initSortConfig = pureHKD @rowHKD @hkt $ Compose Nothing
    --
    go = liftA2 (tableSortRows' @hkt)

-- | Sort table rows using the provided configuration (HKD variant).
--
-- Like 'tableSortRows' but works directly with higher-kinded row data.
tableSortRows'
  :: forall hkt key rowHKD.
     ( IsHKD rowHKD hkt ColumnSortConfig
     , IsHKD rowHKD hkt Identity
     , IsHKD rowHKD hkt (Identity :*: Identity)
     )
  => TableSortConfig rowHKD
  -> Map key (rowHKD Identity)
  -> Map Int (key, (rowHKD Identity))
tableSortRows' sortConfig rowsMap = Map.fromAscList $ zip [0..] sortedRowsList
  where
    sortedRowsList :: [(key, rowHKD Identity)]
    sortedRowsList = case isSortActive of
      True -> List.sortBy sorter rowsList
      False -> rowsList
    --
    isSortActive :: Bool
    isSortActive =
      or $ execWriter $ traverseHKD @rowHKD @hkt
        ( \columnConfig -> do
            case columnConfig of
              Compose (Just _) -> tell [True]
              _ -> pure ()
            pure columnConfig
        )
        sortConfig
    --
    sorter :: (key, rowHKD Identity) -> (key, rowHKD Identity) -> Ordering
    sorter (_, a) (_, b) = mconcat orderings
      where
        orderings :: [Ordering]
        orderings =
          execWriter $ bitraverseHKD @rowHKD @hkt
            ( \columnConfig (Identity colA :*: Identity colB) -> do
                case columnConfig of
                  Compose (Just (Comparison comparison)) -> tell $ [comparison colA colB]
                  _ -> pure ()
                pure columnConfig
            )
            sortConfig
            zippedAB
        --
        zippedAB :: rowHKD (Identity :*: Identity)
        zippedAB = zipHKD @rowHKD @hkt (:*:) a b
    --
    rowsList :: [(key, rowHKD Identity)]
    rowsList = Map.toList rowsMap

-- | Table filtering configuration using higher-kinded data.
--
-- Maps each column to an optional filter predicate.
-- Columns with 'Nothing' have no filtering applied.
type TableFilterConfig key columnsHKD = columnsHKD (ColumnFilterConfig key (columnsHKD Identity))
-- | Per-column filtering configuration.
-- 'Nothing' means no filtering for this column.
-- 'Just' contains a filter predicate.
type ColumnFilterConfig key columns = Maybe `Compose` ColumnFilterPredicate key columns

-- | Filter predicate for a column.
--
-- @key@ - Row key type
-- @columns@ - Full row data type
-- @a@ - Individual column value type
data ColumnFilterPredicate key columns a
  = ColumnFilterPredicate_Some (key -> columns -> a -> Bool)
    -- ^ At least one 'Some' predicate must match (OR logic)
  | ColumnFilterPredicate_Many (key -> columns -> a -> Bool)
    -- ^ All 'Many' predicates must match (AND logic)

-- | Create filtered table rows with dynamic filter configuration.
--
-- Returns filtered rows along with the current filter configuration
-- and a trigger to update it.
--
-- Filters are combined using:
-- - OR logic for 'ColumnFilterPredicate_Some' predicates
-- - AND logic for 'ColumnFilterPredicate_Many' predicates
--
-- ==== __Example__
--
-- > (filteredRows, (filterConfig, updateFilter)) <- tableFilteredRows rows
-- > -- Use filteredRows in your table
-- > -- Call updateFilter to change filtering
tableFilteredRows
  :: forall hkt rowHKD key row t m.
     ( MonadHold t m
     , MonadFix m
     , TriggerEvent t m
     , PerformEvent t m
     --
     , ConstructHKD rowHKD row hkt Identity
     , IsHKD rowHKD hkt Identity
     , IsHKD rowHKD hkt (ColumnFilterConfig key (rowHKD Identity))
     )
  => Dynamic t (Map key row)
  -> m
      ( Dynamic t (Map key row)
      , ( Dynamic t (TableFilterConfig key rowHKD)
        , (TableFilterConfig key rowHKD -> TableFilterConfig key rowHKD) -> IO ()
        )
      )
tableFilteredRows = foldDynWithTrigger go initFilterConfig
  where
    initFilterConfig = pureHKD @rowHKD @hkt $ Compose Nothing
    --
    go = liftA2 (tableFilterRows @hkt)

-- | Filter table rows using the provided configuration.
--
-- Pure function that filters a map of rows according to the filter configuration.
tableFilterRows
  :: forall hkt rowHKD key row.
     ( ConstructHKD rowHKD row hkt Identity
     , IsHKD rowHKD hkt Identity
     , IsHKD rowHKD hkt (ColumnFilterConfig key (rowHKD Identity))
     )
  => TableFilterConfig key rowHKD
  -> Map key row
  -> Map key row
tableFilterRows filterConfig = withHKDRows @hkt (tableFilterRows' @hkt filterConfig)

--

-- | Create filtered table rows with dynamic filter configuration (HKD variant).
--
-- Like 'tableFilteredRows' but works directly with higher-kinded row data.
tableFilteredRows'
  :: forall hkt key rowHKD t m.
     ( MonadHold t m
     , MonadFix m
     , TriggerEvent t m
     , PerformEvent t m
     --
     , IsHKD rowHKD hkt Identity
     , IsHKD rowHKD hkt (ColumnFilterConfig key (rowHKD Identity))
     )
  => Dynamic t (Map key (rowHKD Identity))
  -> m
      ( Dynamic t (Map key (rowHKD Identity))
      , ( Dynamic t (TableFilterConfig key rowHKD)
        , (TableFilterConfig key rowHKD -> TableFilterConfig key rowHKD) -> IO ()
        )
      )
tableFilteredRows' = foldDynWithTrigger go initFilterConfig
  where
    initFilterConfig = pureHKD @rowHKD @hkt $ Compose Nothing
    --
    go = liftA2 (tableFilterRows' @hkt)

-- | Filter table rows using the provided configuration (HKD variant).
--
-- Like 'tableFilterRows' but works directly with higher-kinded row data.
tableFilterRows'
  :: forall hkt key rowHKD.
     ( IsHKD rowHKD hkt Identity
     , IsHKD rowHKD hkt (ColumnFilterConfig key (rowHKD Identity))
     )
  => TableFilterConfig key rowHKD
  -> Map key (rowHKD Identity)
  -> Map key (rowHKD Identity)
tableFilterRows' filterConfig rowsMap = filteredRowsMap
  where
    filteredRowsMap = case isFilterActive of
      True -> Map.filterWithKey filterer rowsMap
      False -> rowsMap
    --
    isFilterActive =
      getAny $ execWriter $ traverseHKD @rowHKD @hkt
        ( \columnConfig -> do
            case columnConfig of
              Compose (Just _) -> tell $ Any True
              _ -> pure ()
            pure columnConfig
        )
        filterConfig
    --
    filterer :: key -> rowHKD Identity -> Bool
    filterer key x =
        case (getAny <$> someM, getAll <$> manyM) of
          (Just somes, Just manys) -> somes && manys
          (Just somes, _) -> somes
          (_, Just manys) -> manys
          _ -> True
      where
        (someM :: Maybe Any, manyM :: Maybe All) =
          execWriter $ bitraverseHKD @rowHKD @hkt
            ( \columnConfig (Identity colX) -> do
                case columnConfig of
                  Compose (Just (ColumnFilterPredicate_Some f)) -> tell (Just $ Any $ f key x colX, Nothing)
                  Compose (Just (ColumnFilterPredicate_Many f)) -> tell (Nothing, Just $ All $ f key x colX)
                  _ -> pure ()
                pure (Identity colX)
            )
            filterConfig
            x
