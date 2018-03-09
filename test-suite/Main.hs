{-# LANGUAGE OverloadedStrings #-}

import qualified Test.Tasty
import Test.Tasty.Hspec
import Discount.UserInput (processArgs')
import Discount.Cart (getTotalPrice
                     , processOrder
                     , processOrderRequest'
                     , processDiscountRequest
                     , processLineItemsRequest
                     , getDisplayItems)
import Discount.Data
import Data.Text.Prettyprint.Doc (pretty)

main :: IO ()
main = do
    test <- testSpec "discounted" spec
    Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ do
  describe "processDiscountRequest" $ do
    context "with valid discount code" $ do
      it "creates a LineItem" $ do
        let discounts = [
              DiscountAll (DiscountCode "first") (DiscountPercentage 10)
              , DiscountItems (DiscountCode "second") (DiscountPercentage 20) [ProductId 1]
              ]
            requestCode = DiscountCode "second"
            expected = Right $ DiscountItems (DiscountCode "second") (DiscountPercentage 20) [ProductId 1]

        processDiscountRequest discounts requestCode `shouldBe` expected

    context "with an invalid discount code" $ do
      it "creates a problem string" $ do
        let discounts = [
              DiscountAll (DiscountCode "first") (DiscountPercentage 10)
              , DiscountItems (DiscountCode "second") (DiscountPercentage 20) [ProductId 1]
              ]
            requestCode = DiscountCode "badCode"
            expected = Left $ "Unable to find a discount with the requested code: badCode"

        processDiscountRequest discounts requestCode `shouldBe` expected

  describe "processLineItemRequest" $ do
    context "with valid product ids" $ do
      it "creates a LineItem" $ do
        let products = [
              Product (ProductName "cat") (PriceCents 2000) (ProductId 1)
              , Product (ProductName "dog") (PriceCents 2500) (ProductId 2)
              , Product (ProductName "lizard") (PriceCents 1000) (ProductId 3)
              ]
            requestItems = [
              RequestLineItem (ProductId 1) (ProductQuantity 1)
              , RequestLineItem (ProductId 2) (ProductQuantity 2)
              ]
            expected = Right $ [
              LineItem (Product (ProductName "cat") (PriceCents 2000) (ProductId 1)) (ProductQuantity 1)
              , LineItem (Product (ProductName "dog") (PriceCents 2500) (ProductId 2)) (ProductQuantity 2)
              ]

        processLineItemsRequest products requestItems `shouldBe` expected

    context "with an invalid product id" $ do
      it "creates an OrderProblem" $ do
        let products = [
              Product (ProductName "cat") (PriceCents 2000) (ProductId 1)
              , Product (ProductName "dog") (PriceCents 2500) (ProductId 2)
              ]
            requestItems = [
              RequestLineItem (ProductId 1) (ProductQuantity 1)
              , RequestLineItem (ProductId 5) (ProductQuantity 2)
              ]
            expected = Left $ "Unable to find a product with the requested id: 5"

        processLineItemsRequest products requestItems `shouldBe` expected

  describe "processOrderRequest" $ do
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
      let expected = Right $ Order [
              LineItem (Product (ProductName "cat") (PriceCents 2000) (ProductId 1)) (ProductQuantity 1)
            , LineItem (Product (ProductName "dog") (PriceCents 2500) (ProductId 2)) (ProductQuantity 2)
            ]
                     $ Just $ DiscountItems (DiscountCode "second") (DiscountPercentage 20) [ProductId 1]

      processOrderRequest' products discounts request `shouldBe` expected

  describe "processOrder" $ do
    it "generates correct display order" $ do
      let order = Order lineItems (Just discount)
          lineItems = [
              LineItem (Product (ProductName "cat") (PriceCents 2000) (ProductId 1)) (ProductQuantity 2)
            , LineItem (Product (ProductName "dog") (PriceCents 2500) (ProductId 2)) (ProductQuantity 2)
            ]
          discount = DiscountItems (DiscountCode "") (DiscountPercentage 20) [ProductId 1]
          originalPrice = Just $ Original $ PriceCents $ 4000
          displayedLineItems = [
              DisplayLineItem (PriceCents 3200) originalPrice (ProductQuantity 2) (ProductName "cat")
            , DisplayLineItem (PriceCents 5000) Nothing (ProductQuantity 2) (ProductName "dog")
            ]
          expected = DisplayOrder (Total (PriceCents 8200)) displayedLineItems

      processOrder order `shouldBe` expected

  describe "getTotalPrice" $ do
    it "sums prices" $ do
      let items = [
              DisplayLineItem (PriceCents 2000) Nothing (ProductQuantity 1) (ProductName "cat")
            , DisplayLineItem (PriceCents 250) (Just $ Original $ PriceCents 2500) (ProductQuantity 2) (ProductName "dog")
            ]
          expected = Total $ PriceCents 2250

      getTotalPrice items `shouldBe` expected

  describe "getDisplayitems" $ do
    context "with no discount" $ do
      it "generates correct line times for display" $ do
        let order = Order lineItems Nothing
            lineItems = [
                LineItem (Product (ProductName "cat") (PriceCents 2000) (ProductId 1)) (ProductQuantity 1)
              , LineItem (Product (ProductName "dog") (PriceCents 2500) (ProductId 2)) (ProductQuantity 2)
              ]
            expected = [
                DisplayLineItem (PriceCents 2000) Nothing (ProductQuantity 1) (ProductName "cat")
              , DisplayLineItem (PriceCents 5000) Nothing (ProductQuantity 2) (ProductName "dog")
              ]

        getDisplayItems order `shouldBe` expected

    context "with an 'all' discount" $ do
      it "applies discount to all items" $ do
        let order = Order lineItems discount
            discount = Just $ DiscountAll (DiscountCode "") (DiscountPercentage 90)
            lineItems = [
                LineItem (Product (ProductName "cat") (PriceCents 2000) (ProductId 1)) (ProductQuantity 1)
              , LineItem (Product (ProductName "dog") (PriceCents 2500) (ProductId 2)) (ProductQuantity 2)
              ]
            expected = [
                DisplayLineItem (PriceCents 200) (Just $ Original $ PriceCents 2000) (ProductQuantity 1) (ProductName "cat")
              , DisplayLineItem (PriceCents 500) (Just $ Original $ PriceCents 5000) (ProductQuantity 2) (ProductName "dog")
              ]

        getDisplayItems order `shouldBe` expected

    context "with a 'product_list' discount" $ do
      it "discounts only applicable items" $ do
        let order = Order lineItems discount
            discount = Just $ DiscountItems (DiscountCode "") (DiscountPercentage 90) [ProductId 2]
            lineItems = [
                LineItem (Product (ProductName "cat") (PriceCents 2000) (ProductId 1)) (ProductQuantity 1)
              , LineItem (Product (ProductName "dog") (PriceCents 2500) (ProductId 2)) (ProductQuantity 2)
              ]
            expected = [
                DisplayLineItem (PriceCents 2000) Nothing (ProductQuantity 1) (ProductName "cat")
              , DisplayLineItem (PriceCents 500) (Just $ Original $ PriceCents 5000) (ProductQuantity 2) (ProductName "dog")
              ]

        getDisplayItems order `shouldBe` expected

  describe "prettyprinting display order" $ do
    it "should be in correct format" $ do
      let originalPrice = Just $ Original $ PriceCents $ 4000
          displayedLineItems = [
              DisplayLineItem (PriceCents 3200) originalPrice (ProductQuantity 2) (ProductName "cat")
            , DisplayLineItem (PriceCents 5000) Nothing (ProductQuantity 2) (ProductName "dog")
            ]
          dispOrder = DisplayOrder (Total (PriceCents 8200)) displayedLineItems
      (show . pretty $ dispOrder) `shouldBe` "Your cart:\n\n$32.00 ($40.00 before discount) for 2 copies of \"cat\"\n$50.00  for 2 copies of \"dog\"\n--\nTotal $82.00\n"

  describe "processArgs'" $ do
    context "with no discount" $ do
      it "captures product ids and quantities" $ do
        processArgs' ["1", "2", "10", "3"] `shouldBe` Right (Command [(ProductId 10, ProductQuantity 3), (ProductId 1, ProductQuantity 2)] Nothing)

    context "with a long discount flag" $ do
      it "captures discount code, product ids and quantities" $ do
        processArgs' ["--discount", "WELCOME", "1", "2"] `shouldBe` Right (Command [(ProductId 1, ProductQuantity 2)] (Just $ DiscountCode "WELCOME"))

    context "with a short discount flag" $ do
      it "captures discount code, product ids and quantities" $ do
        processArgs' ["-d", "WELCOME", "1", "2"] `shouldBe` Right (Command [(ProductId 1, ProductQuantity 2)] (Just $ DiscountCode "WELCOME"))

    context "with no args" $ do
      it "generates error message" $ do
        processArgs' [] `shouldBe` Left "Please call with product ids to process"

    context "with discount but no product ids" $ do
      it "generates error message" $ do
        processArgs' ["-d", "WELCOME"] `shouldBe` Left "Please call with product ids to process"

    context "with an odd number of product ids/quantities" $ do
      it "generates error message" $ do
        processArgs' ["-d", "WELCOME", "1", "2", "3"] `shouldBe` Left "Product id 3 did not have an associated quantity"
