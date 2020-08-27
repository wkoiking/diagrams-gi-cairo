-----------------------------------------------------------------------------
-- |
-- Module      :  Geometry.TwoD.Points
-- Copyright   :  (c) 2014-2017 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Special functions for points in R2.
--
-----------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}

module Geometry.TwoD.Points where

import           Data.List
import           Linear.Affine

import           Geometry.Space
import           Geometry.TwoD.Types  (P2)
import           Geometry.TwoD.Vector

-- | Find the convex hull of a list of points using Andrew's monotone chain
--   algorithm O(n log n).
--
--   Returns clockwise list of points starting from the left-most point.
convexHull2D :: OrderedField n => [P2 n] -> [P2 n]
convexHull2D ps = init upper ++ reverse (tail lower)
  where
    (upper, lower) = sortedConvexHull (sort ps)

-- | Find the convex hull of a set of points already sorted in the x direction.
--   The first list of the tuple is the upper hull going clockwise from
--   left-most to right-most point. The second is the lower hull from
--   right-most to left-most in the anti-clockwise direction.
sortedConvexHull :: OrderedField n => [P2 n] -> ([P2 n], [P2 n])
sortedConvexHull ps = (chain True ps, chain False ps)
 where
   chain upper (p1_:p2_:rest_) =
     case go (p2_ .-. p1_) p2_ rest_ of
       Right l -> p1_:l
       Left l  -> chain upper (p1_:l)
     where
       test = if upper then (>0) else (<0)
       -- find the convex hull by comparing the angles of the vectors with
       -- the cross product and backtracking if necessary
       go d p1 l@(p2:rest)
         -- backtrack if the direction is outward
         | test $ d `crossZ` d' = Left l
         | otherwise                =
             case go d' p2 rest of
               Left m  -> go d p1 m
               Right m -> Right (p1:m)
         where
           d' = p2 .-. p1
       go _ p1 p = Right (p1:p)

   chain _ l = l
