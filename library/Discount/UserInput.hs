{-# LANGUAGE OverloadedStrings #-}

module Discount.UserInput where

import           Data.Text                 (pack)
import           Data.Text.Prettyprint.Doc (pretty, (<+>))
import           Discount.Data
import           Discount.Util             (prettyTxt, throwLeft)

processCommand :: Command -> RequestOrder
processCommand (Command prods disc) = RequestOrder (fmap (\prod -> RequestLineItem (fst prod) (snd prod)) prods) disc

processArgs' :: [String] -> Either ArgProblem Command
processArgs' args = processArgsIter (Right $ Command [] Nothing) args

processArgs :: [String] -> IO Command
processArgs = throwLeft . processArgs'

isDiscountFlag :: String -> Bool
isDiscountFlag = (||) <$> (== "--discount") <*> (== "-d")

processArgsIter :: Either ArgProblem  Command -> [String] -> Either ArgProblem Command
processArgsIter left@(Left _) _ = left
processArgsIter right@(Right cmd) [] | (emptyCmd cmd)  = Left "Please call with product ids to process"
                                     | otherwise = right
  where emptyCmd (Command prods _) = length prods == 0
processArgsIter _ (a:[]) | isDiscountFlag a = Left "Please call with a discount code"
                         | otherwise = Left $ show $ prettyTxt "Product id" <+> pretty a <+> "did not have an associated quantity"
processArgsIter (Right (Command prods disc)) (a:b:cs) | isDiscountFlag a = processArgsIter (Right $ Command prods $ Just $ DiscountCode $ pack b) cs
                                                      | otherwise = processArgsIter (Right $ Command newProds disc) cs
  where newProds = [(ProductId $ read a, ProductQuantity $ read b)] ++ prods
