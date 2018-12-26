module Squeal.PostgreSQL.Extended
  ( module Squeal.PostgreSQL
  , module Squeal.PostgreSQL.Render
  , arrayAgg
  , arrayOverlap
  , arrayCardinality
  , unnest
  ) where

import Squeal.PostgreSQL
import Squeal.PostgreSQL.Render

arrayAgg ::
     Expression db from 'Ungrouped params (nullity ty)
  -> Expression db from ('Grouped bys) params ('NotNull ('PGvararray ('NotNull ty)))
arrayAgg = unsafeAggregate "array_agg"

arrayOverlap ::
  Expression db from 'Ungrouped params (nullity ('PGvararray ty)) ->
  Expression db from 'Ungrouped params (nullity ('PGvararray ty)) ->
  Expression db from 'Ungrouped params (nullity 'PGbool)
arrayOverlap = unsafeBinaryOp "&&"

arrayCardinality ::
  Expression db from 'Ungrouped params (nullity ('PGvararray ty)) ->
  Expression db from 'Ungrouped params (nullity 'PGint8)
arrayCardinality = unsafeFunction "cardinality"

unnest ::
     Expression db '[] 'Ungrouped params (nullity ('PGvararray ty))
  -> Query db params '[ "unnest" ::: ty ]
unnest q = UnsafeQuery $ "SELECT * FROM unnest" <> parenthesized (renderExpression q)
