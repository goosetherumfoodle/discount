-- | An example module.
module Example where

import Data.ByteString.Char8 (ByteString)

-- | An example function.
main :: IO ()
main = return ()

newtype DiscountCode = DiscountCode ByteString  deriving (Show, Eq)
newtype ProductId = ProductId Int deriving (Show, Eq)
newtype ProductQuantity = ProductQuantity Int deriving (Show, Eq)
newtype PriceCents = PriceCents Int deriving (Show, Eq)
newtype TotalPriceCents = TotalPriceCents PriceCents
newtype OriginalPriceCents = OriginalPriceCents PriceCents
newtype ProductName = ProductName ByteString deriving (Show, Eq)
newtype DiscountPercentage = DiscountPercentage Int deriving (Show, Eq)

data Order = Order [LineItem] Discount deriving (Show, Eq)
data LineItem = LineItem Product ProductQuantity deriving (Show, Eq)
data Product = Product ProductName PriceCents ProductId deriving (Show, Eq)
data Discount = DiscountAll DiscountCode DiscountPercentage
              | DiscountItems DiscountCode DiscountPercentage [ProductId] deriving (Show, Eq)

-- input types
data RequestOrder = RequestOrder [RequestLineItem] (Maybe DiscountCode)
data RequestLineItem = RequestLineItem ProductId ProductQuantity

-- output types
data DisplayOrder = DisplayOrder TotalPriceCents [DisplayLineItem]
data DisplayLineItem = DisplayLineItem PriceCents OriginalPriceCents ProductQuantity ProductName

processOrderRequest :: [Product] -> [Discount] -> RequestOrder -> Order
processOrderRequest = undefined

processOrder :: Order -> DisplayOrder
processOrder = undefined
