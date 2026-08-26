{-# LANGUAGE OverloadedStrings #-}

-- | Сравнение двух снапшотов одной категории.
module Vprok.Diff
  ( DiffResult(..)
  , PriceMove(..)
  , diffProducts
  , renderDiff
  ) where

import qualified Data.Map.Strict as M
import           Data.Maybe      (mapMaybe)
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Vprok.Report    (formatDouble)
import           Vprok.Types

data PriceMove = PriceMove
  { moveProduct :: Product
  , moveFrom    :: Double
  , moveTo      :: Double
  } deriving (Eq, Show)

data DiffResult = DiffResult
  { drAppeared     :: [Product]
  , drDisappeared  :: [Product]
  , drPriceDown    :: [PriceMove]
  , drPriceUp      :: [PriceMove]
  , drBackInStock  :: [Product]
  , drOutOfStock   :: [Product]
  , drUnchanged    :: Int
  } deriving (Eq, Show)

diffProducts :: [Product] -> [Product] -> DiffResult
diffProducts old new = DiffResult
  { drAppeared    = [ p | (k, p) <- M.toList newMap, not (M.member k oldMap) ]
  , drDisappeared = [ p | (k, p) <- M.toList oldMap, not (M.member k newMap) ]
  , drPriceDown   = [ m | m <- moves, moveTo m < moveFrom m ]
  , drPriceUp     = [ m | m <- moves, moveTo m > moveFrom m ]
  , drBackInStock = [ p | (before, p) <- paired, not (inStock before), inStock p ]
  , drOutOfStock  = [ p | (before, p) <- paired, inStock before, not (inStock p) ]
  , drUnchanged   = length [ () | (before, p) <- paired
                                , prodPrice before == prodPrice p
                                , inStock before == inStock p ]
  }
  where
    oldMap = M.fromList [ (prodUrl p, p) | p <- old ]
    newMap = M.fromList [ (prodUrl p, p) | p <- new ]
    paired = [ (before, p) | (k, p) <- M.toList newMap
                           , Just before <- [M.lookup k oldMap] ]
    moves  = mapMaybe toMove paired
    toMove (before, p) = do
      from <- prodPrice before
      to   <- prodPrice p
      if from == to then Nothing else Just (PriceMove p from to)

renderDiff :: DiffResult -> Text
renderDiff d = T.unlines $ concat
  [ section "Подешевело" (map moveLine (drPriceDown d))
  , section "Подорожало" (map moveLine (drPriceUp d))
  , section "Появилось в наличии" (map nameLine (drBackInStock d))
  , section "Пропало из наличия" (map nameLine (drOutOfStock d))
  , section "Новые товары" (map nameLine (drAppeared d))
  , section "Исчезли из выдачи" (map nameLine (drDisappeared d))
  , [ "Без изменений: " <> T.pack (show (drUnchanged d)) ]
  ]
  where
    section title rows
      | null rows = [title <> ": —", ""]
      | otherwise = (title <> ":") : rows ++ [""]
    moveLine m = T.concat
      [ "  ", prodName (moveProduct m)
      , ": ", formatDouble (moveFrom m)
      , " -> ", formatDouble (moveTo m)
      , " (", diffSign (moveTo m - moveFrom m), ")"
      ]
    nameLine p = "  " <> prodName p
    diffSign x
      | x > 0     = "+" <> formatDouble x
      | otherwise = formatDouble x
