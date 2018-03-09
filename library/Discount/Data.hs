{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Discount.Data where

import           Data.Decimal              (DecimalRaw (Decimal))
import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc (Pretty, pretty, vsep, (<+>), (<>))
import           Discount.Util             (prettyTxt)

type OrderProblem = String
type ArgProblem = String

newtype DiscountCode = DiscountCode Text  deriving (Show, Eq)
newtype ProductId = ProductId Int deriving (Show, Eq)
newtype ProductQuantity = ProductQuantity Integer deriving (Show, Eq)
newtype PriceCents a = PriceCents a deriving (Show, Eq, Num, Functor)
newtype TotalPriceCents a = Total (PriceCents a) deriving (Show, Eq, Num)
newtype OriginalPriceCents a = Original (PriceCents a) deriving (Show, Eq)
newtype ProductName = ProductName Text deriving (Show, Eq)
newtype DiscountPercentage = DiscountPercentage Integer deriving (Show, Eq)

data Order = Order [LineItem] (Maybe Discount) deriving (Show, Eq)
data LineItem = LineItem Product ProductQuantity deriving (Show, Eq)
data Product = Product ProductName (PriceCents Integer) ProductId deriving (Show, Eq)
data Discount = DiscountAll DiscountCode DiscountPercentage
              | DiscountItems DiscountCode DiscountPercentage [ProductId] deriving (Show, Eq)
data RequestOrder = RequestOrder [RequestLineItem] (Maybe DiscountCode)
data RequestLineItem = RequestLineItem ProductId ProductQuantity
data DisplayOrder = DisplayOrder (TotalPriceCents Integer) [DisplayLineItem] deriving (Show, Eq)
data DisplayLineItem = DisplayLineItem (PriceCents Integer)
                                       (Maybe (OriginalPriceCents Integer))
                                       ProductQuantity
                                       ProductName deriving (Show, Eq)
data Command = Command [(ProductId, ProductQuantity)] (Maybe DiscountCode) deriving (Show, Eq)

instance Pretty ProductId where
  pretty (ProductId a) = pretty a

instance Pretty DiscountCode where
  pretty (DiscountCode code) = pretty code

instance (Integral a, Show a) => Pretty (OriginalPriceCents a) where
  pretty (Original a) = prettyTxt "(" <> pretty a <+> prettyTxt "before discount)"

instance (Integral a, Show a) => Pretty (TotalPriceCents a) where
  pretty (Total a) = prettyTxt "Total" <+> pretty a

instance (Integral a, Show a) => Pretty (PriceCents a) where
  pretty (PriceCents a) = pretty ("$" :: String) <> (pretty $ show $ Decimal 2 a)

instance Pretty ProductName where
  pretty (ProductName name) = pretty '"' <> pretty name <> pretty '"'

instance Pretty DisplayLineItem where
  pretty (DisplayLineItem price orig (ProductQuantity quant) name) = pretty price
                                                                    <+> pretty orig
                                                                    <+> prettyTxt "for"
                                                                    <+> pretty quant
                                                                    <+> copyPlural
                                                                    <+> prettyTxt "of"
                                                                    <+> pretty name
    where copyPlural | quant > 1 = prettyTxt "copies"
                     | otherwise = prettyTxt "copy"

instance Pretty DisplayOrder where
  pretty (DisplayOrder total items) = vsep $ ["Your cart:"
                                              , ""
                                              , vsep $ fmap pretty items
                                              , "--"
                                              , pretty total
                                              , ""
                                              ]
