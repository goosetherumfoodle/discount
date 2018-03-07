{-# LANGUAGE OverloadedStrings #-}

import qualified Test.Tasty
import Test.Tasty.Hspec
import Example hiding (main)

main :: IO ()
main = do
    test <- testSpec "discounted" spec
    Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ do
  describe "processOrderRequest" $ do
    context "with valid discount code and productId's" $ do
      it "creates correct Order" $ do
        let products = [
              Product (ProductName "cat") (PriceCents 2000) (ProductId 1)
              , Product (ProductName "dog") (PriceCents 2500) (ProductId 2)
              ]
        let discounts = [
              DiscountAll (DiscountCode "first") (DiscountPercentage 10)
              , DiscountItems (DiscountCode "second") (DiscountPercentage 20) [ProductId 1]
              ]
        let request = RequestOrder [RequestLineItem (ProductId 1) (ProductQuantity 1)
                                   , RequestLineItem (ProductId 2) (ProductQuantity 2)]
                                   (Just (DiscountCode "second"))
        let expected = Order [
              LineItem (Product (ProductName "cat") (PriceCents 2000) (ProductId 1)) (ProductQuantity 1)
              , LineItem (Product (ProductName "dog") (PriceCents 2500) (ProductId 2)) (ProductQuantity 2)
              ]
                       (DiscountItems (DiscountCode "second") (DiscountPercentage 20) [ProductId 1])

        let results = processOrderRequest products discounts request

        results `shouldBe` expected
