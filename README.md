# reflex-dom-tables

Dynamic table widgets for Reflex-DOM with built-in sorting and filtering capabilities.

## Features

- **Dynamic Data**: Tables automatically update when the underlying data changes
- **Flexible Column Configuration**: Support for simple columns, columns with headers, and hierarchical multi-level headers
- **Customizable Styling**: Fine-grained control over attributes for all table elements
- **Higher-Kinded Data**: Type-safe configuration using HKD patterns for sorting and filtering

## Installation

Add `reflex-dom-tables` to your project dependencies.

### With Cabal

Add to your `.cabal` file:

```cabal
build-depends:
    reflex-dom-tables
```

## Quick Start

### Basic Table

```haskell
import Reflex.Dom
import Reflex.Dom.Tables
import Data.Default

data Person = Person
  { personName :: Text
  , personAge :: Int
  , personCity :: Text
  }

basicTable
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     )
  => Dynamic t (Map Int Person)
  -> m ()
basicTable people = do
  let columns =
        [ TH []
            ( text "Name"
            , \_ p -> text (personName p)
            )
        , TH []
            ( text "Age"
            , \_ p -> text (T.pack $ show $ personAge p)
            )
        , TH []
            ( text "City"
            , \_ p -> text (personCity p)
            )
        ]

      config = def { tableConfig_columns = columns }

  _ <- elTable people config
  return ()
```

### Table with Sorting

```haskell
import HigherKinded.Applied

sortableTable :: (...) => Dynamic t (Map Int Person) -> m ()
sortableTable people = do
  -- Create sorted rows with sort configuration
  (sortedRows, (sortConfig, updateSort)) <- tableSortedRows @Applied @(F Person) people

  -- Define columns with sort triggers
  let columns =
        [ TH []
            ( sortHeader "Name" updateSort nameComparison
            , \_ p -> text (personName p)
            )
        , TH []
            ( sortHeader "Age" updateSort ageComparison
            , \_ p -> text (T.pack $ show $ personAge p)
            )
        , TH []
            ( text "City"  -- Not sortable
            , \_ p -> text (personCity p)
            )
        ]

  _ <- elTable sortedRows (def { tableConfig_columns = columns })
  return ()
  where
    -- Custom comparison functions - no Ord constraint needed!
    nameComparison = Comparison $ \a b -> compare a b
    ageComparison = Comparison $ \a b -> compare a b

    sortHeader label updateSort comparison = do
      (e, _) <- el' "span" $ text label
      performEvent_ $ updateSort (setSortForColumn comparison) <$ domEvent Click e
```

### Table with Filtering

```haskell
filterableTable :: (...) => Dynamic t (Map Int Person) -> m ()
filterableTable people = do
  -- Create filter input
  searchText <- value <$> inputElement def

  -- Create filtered rows with filter configuration
  (filteredRows, (filterConfig, updateFilter)) <- tableFilteredRows @Applied @(F Person) people

  -- Update filter when search text changes
  performEvent_ $ updated searchText <&> \search ->
    updateFilter $ \cfg ->
      cfg { personName = ColumnFilterPredicate_Some $ \_ _ name -> T.isInfixOf search name
          }

  -- Display filtered table
  _ <- elTable filteredRows (def { tableConfig_columns = columns })
  return ()
```

### Hierarchical Headers

```haskell
hierarchicalTable :: (...) => Dynamic t (Map Int SalesData) -> m ()
hierarchicalTable sales = do
  let columns =
        [ TH []
            ( text "Product"
            , \_ s -> text (productName s)
            )
        , THs ["class" ~: "quarter-data"]  -- Attributes cascade to subcolumns
            ( text "Q1"
            , [ TH []
                  ( text "Jan"
                  , \_ s -> text $ showSales (janSales s)
                  )
              , TH []
                  ( text "Feb"
                  , \_ s -> text $ showSales (febSales s)
                  )
              , TH []
                  ( text "Mar"
                  , \_ s -> text $ showSales (marSales s)
                  )
              ]
            )
        , THs ["class" ~: "quarter-data"]
            ( text "Q2"
            , [ TH []
                  ( text "Apr"
                  , \_ s -> text $ showSales (aprSales s)
                  )
              , TH []
                  ( text "May"
                  , \_ s -> text $ showSales (maySales s)
                  )
              , TH []
                  ( text "Jun"
                  , \_ s -> text $ showSales (junSales s)
                  )
              ]
            )
        ]

  _ <- elTable sales (def { tableConfig_columns = columns })
  return ()
```

## Advanced Features

### Column-Level Attributes

You can set attributes directly on columns, and `THs` attributes cascade to their subcolumns:

```haskell
columnsWithStyles :: (...) => [TableColumn Int Person (m ()) (m ()) t m]
columnsWithStyles =
  [ TH ["class" ~: "name-column"]
      ( text "Name"
      , \_ p -> text (personName p)
      )
  , THs ["class" ~: "numeric-columns"]  -- Cascades to Age and Score
      ( text "Stats"
      , [ TH ["class" ~: "age"]  -- Combined with parent: "numeric-columns age"
          ( text "Age"
          , \_ p -> text (T.pack $ show $ personAge p)
          )
        , TH ["class" ~: "score"]  -- Combined with parent: "numeric-columns score"
          ( text "Score"
          , \_ p -> text (T.pack $ show $ personScore p)
          )
        ]
      )
  ]
```

Column attributes are combined with dynamic attributes from `tableConfig_tdAttrs` and `tableConfig_thAttrs`, giving you fine-grained control over styling.

### Custom Attributes

Control the appearance of your table with custom attributes:

```haskell
styledTable :: (...) => Dynamic t (Map Int Person) -> m ()
styledTable people = do
  let config = def
        { tableConfig_columns = columns
        , tableConfig_tableAttrs = ["class" ~: "table table-striped"]
        , tableConfig_trAttrs = \key row ->
            ["class" ~: if even key then "even-row" else "odd-row"]
        , tableConfig_tdAttrs = \cellValue key row ->
            ["data-key" ~: T.pack (show key)]
        }

  _ <- elTable people config
  return ()
```

### Accessing Table Elements

The `elTable` function returns a `Table` structure with references to all created elements:

```haskell
interactiveTable :: (...) => Dynamic t (Map Int Person) -> m ()
interactiveTable people = do
  tableResult <- elTable people (def { tableConfig_columns = columns })

  -- Access specific elements
  let tableEl = table_tableEl <$> tableResult  -- The <table> element
      rowEls = table_trEls <$> tableResult      -- Map of row elements
      cellEls = table_tdEls <$> tableResult     -- Map of cell elements

  -- Add interactions
  performEvent_ $ switchDyn $ ffor rowEls $ \rows ->
    leftmost $ Map.elems $ Map.mapWithKey (\k el ->
      traceEvent ("Row " <> show k <> " clicked") $
        domEvent Click el
    ) rows

  return ()
```

## API Documentation

### Core Types

- `TableConfig`: Configuration for table rendering
- `TableColumn`: Column definitions with attributes (TD, TH, THs)
  - Each constructor takes `[Attrs t m]` for column-specific styling
  - `THs` attributes cascade to subcolumns
- `Table`: Result structure containing element references
- `TableSortConfig`: Sorting configuration using comparison functions
- `TableFilterConfig`: Filtering configuration with predicates

### Main Functions

- `elTable`: Create a dynamic table
- `tableSortedRows`: Enable sorting with comparison functions
- `tableFilteredRows`: Enable filtering with predicates
- `mapTableColumn`: Transform column definitions
- `getTDs`: Extract data renderers from columns
- `getTHRows`: Extract header structure from columns

## Why No Ord Constraint?

Traditional sorting requires an `Ord` instance for the entire row type or complex type-level machinery to select fields. This library takes a different approach:

1. **Direct Comparison Functions**: Provide comparison functions for each sortable column
2. **Partial Sorting**: Some columns can be non-sortable (no comparison function)
3. **Custom Logic**: Implement domain-specific sorting (e.g., case-insensitive, locale-aware)
4. **Complex Types**: Sort by computed values without creating wrapper types

## Contributing

Contributions are welcome! Please feel free to submit issues and pull requests.

## License

BSD3 - See LICENSE file for details.

## Author

Yuri Meister
