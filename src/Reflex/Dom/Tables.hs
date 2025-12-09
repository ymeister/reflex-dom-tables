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

--
-- | Table configuration
--

data TableConfig key row th td t m = TableConfig
  { tableConfig_columns :: [TableColumn key row (m th) (m td)] -- ^ columns configuration
  , tableConfig_tableAttrs :: [Attrs t m] -- ^ <table> attributes
  , tableConfig_theadAttrs :: [Attrs t m] -- ^ <thead> attributes
  , tableConfig_tbodyAttrs :: [Attrs t m] -- ^ <tbody> attributes
  , tableConfig_thAttrs :: Maybe th -> [Attrs t m] -- ^ <th> attributes
  , tableConfig_trAttrs :: key -> row -> [Attrs t m] -- ^ <tr> attributes
  , tableConfig_tdAttrs :: td -> key -> row -> [Attrs t m] -- ^ <td> attributes
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

--
-- | Column configuration
--

data TableColumn key row th td
  = TD (key -> row -> td)
  | TH (th, key -> row -> td)
  | THs (th, [TableColumn key row th td])

getTDs :: [TableColumn key row th td] -> [key -> row -> td]
getTDs tableCols = go tableCols []
  where
    go [] !tds = tds
    go (TD td : cols) !tds = td : go cols tds
    go (TH (_, td) : cols) !tds = td : go cols tds
    go (THs (_, thcols) : cols) !tds = go thcols $ go cols tds

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

forTableColumn
  :: TableColumn key row th td
  -> ( (th -> th')
     , ((key -> row -> td) -> (key' -> row' -> td'))
     )
  -> TableColumn key' row' th' td'
forTableColumn = flip mapTableColumn

--
-- | Table output
--

data Table key row th td t m = Table
  { table_tableEl :: El t m
  , table_theadEl :: El t m
  , table_tbodyEl :: El t m
  , table_thEls :: [[El t m]]
  , table_trEls :: Map key (El t m)
  , table_tdEls :: Map key [El t m]
  , table_thVals :: [[th]]
  , table_trVals :: Map key [td]
  , table_ths :: [[(El t m, th)]]
  , table_trs :: Map key (El t m, [(El t m, td)])
  }

--
-- | Table widget
--

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

--
-- | Table sorting
--

type TableSortConfig columnsHKD = columnsHKD ColumnSortConfig
type ColumnSortConfig = Maybe `Compose` Comparison

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

--
-- | Table filtering
--

type TableFilterConfig key columnsHKD = columnsHKD (ColumnFilterConfig key (columnsHKD Identity))
type ColumnFilterConfig key columns = Maybe `Compose` ColumnFilterPredicate key columns

data ColumnFilterPredicate key columns a
  = ColumnFilterPredicate_Some (key -> columns -> a -> Bool)
  | ColumnFilterPredicate_Many (key -> columns -> a -> Bool)

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
