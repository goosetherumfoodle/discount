{-# LANGUAGE OverloadedStrings #-}

module Database where

import           Discount.Data

productLibrary :: [Product]
productLibrary = [ Product (ProductName "Black Jacobins") (PriceCents 2000) (ProductId 1)
                 , Product (ProductName "Freedom Is a Constant Struggle") (PriceCents 1500) (ProductId 2)
                 ]

discountLibrary :: [Discount]
discountLibrary = [ DiscountAll (DiscountCode "WELCOME") (DiscountPercentage 50)
                  , DiscountItems (DiscountCode "JAC75") (DiscountPercentage 75) [ProductId 1]
                  ]
