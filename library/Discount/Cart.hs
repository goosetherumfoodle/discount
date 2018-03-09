{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Discount.Cart where

import           Data.Decimal              (DecimalRaw (Decimal))
import           Data.Foldable             (fold)
import           Data.List                 (find)
import           Data.Monoid               (Sum (Sum), getSum, (<>))
import           Data.Text.Prettyprint.Doc (pretty)
import           Discount.Data
import           Discount.Util             (throwLeft)

processOrderRequest :: [Product] -> [Discount] -> RequestOrder -> IO Order
processOrderRequest prod disc reqOrd = throwLeft $ processOrderRequest' prod disc reqOrd

processOrderRequest' :: [Product] -> [Discount] -> RequestOrder -> Either OrderProblem Order
processOrderRequest' prods _ (RequestOrder reqItems Nothing) = Order <$> (getLineItem reqItems) <*> (Right Nothing)
  where getLineItem = processLineItemsRequest prods
processOrderRequest' prods discs (RequestOrder reqItems (Just reqDisc)) = Order <$> (getLineItem reqItems) <*> (Just <$> (getDiscount reqDisc))
  where getLineItem = processLineItemsRequest prods
        getDiscount = processDiscountRequest discs

processLineItemsRequest :: [Product] -> [RequestLineItem] -> Either OrderProblem [LineItem]
processLineItemsRequest prods = traverse $ processLineItemRequest prods


processLineItemRequest :: [Product] -> RequestLineItem -> Either OrderProblem LineItem
processLineItemRequest prods (RequestLineItem rId quant) | Nothing      <- find (hasId rId) prods = Left $ show problem
                                                         | (Just found) <- find (hasId rId) prods = Right $ LineItem found quant
  where hasId queryId (Product _ _ id) = queryId == id
        problem = "Unable to find a product with the requested id: " <> pretty rId

processDiscountRequest :: [Discount] -> DiscountCode -> Either OrderProblem Discount
processDiscountRequest discs queryCode | Nothing      <- find hasCode discs = Left problem
                                       | (Just found) <- find hasCode discs = Right found
  where hasCode (DiscountAll code _)     = code == queryCode
        hasCode (DiscountItems code _ _) = code == queryCode
        problem = "Unable to find a discount with the requested code: " <> (show . pretty $ queryCode)

processOrder :: Order -> DisplayOrder
processOrder = DisplayOrder <$> getTotalPrice . getDisplayItems <*> getDisplayItems

getTotalPrice :: [DisplayLineItem] -> TotalPriceCents Integer
getTotalPrice = getSum . fold . fmap totalPerItem

totalPerItem :: DisplayLineItem -> Sum (TotalPriceCents Integer)
totalPerItem (DisplayLineItem price _ _ _) = Sum $ Total price

getDisplayItems :: Order -> [DisplayLineItem]
getDisplayItems (Order items disc) = fmap (processLineItem disc) items

processLineItem :: Maybe Discount -> LineItem -> DisplayLineItem
processLineItem Nothing item = nonDiscountedItem item
processLineItem (Just (DiscountAll _ discPer)) item = discountedItem discPer item
processLineItem (Just (DiscountItems _ discPer discIds)) item | inDiscountList item discIds = discountedItem discPer item
                                                              | otherwise = nonDiscountedItem item

inDiscountList :: Foldable t => LineItem -> t ProductId -> Bool
inDiscountList (LineItem (Product _ _ prodId) _) = elem prodId

nonDiscountedItem :: LineItem -> DisplayLineItem
nonDiscountedItem (LineItem (Product name price _) quant) = DisplayLineItem (nonDiscountTotal price quant) Nothing quant name

discountedItem :: DiscountPercentage -> LineItem -> DisplayLineItem
discountedItem percentage (LineItem (Product name price _) quant) = DisplayLineItem (discountTotal price quant percentage)
                                                                                    (Just $ originalTotal price quant)
                                                                                    quant
                                                                                    name

nonDiscountTotal :: PriceCents Integer -> ProductQuantity -> PriceCents Integer
nonDiscountTotal (PriceCents cents) (ProductQuantity quant) = PriceCents $ cents * quant

originalTotal :: PriceCents Integer -> ProductQuantity -> OriginalPriceCents Integer
originalTotal price quant = Original $ nonDiscountTotal price quant

discountTotal :: PriceCents Integer -> ProductQuantity -> DiscountPercentage -> PriceCents Integer
discountTotal (PriceCents cents) (ProductQuantity quant) (DiscountPercentage percent) = PriceCents integralTotal
  where integralTotal = round decimalTotal
        decimalTotal = (Decimal 0 quant) * (Decimal 0 cents) * percentComplement
        percentComplement = (Decimal 2 (100 - percent))
