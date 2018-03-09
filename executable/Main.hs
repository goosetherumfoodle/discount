import           Data.Text.Prettyprint.Doc (pretty)
import           Database                  (discountLibrary, productLibrary)
import           Discount.Cart             (processOrder, processOrderRequest)
import           Discount.UserInput        (processArgs, processCommand)
import           System.Environment        (getArgs)

main :: IO ()
main = do args <- getArgs
          cmd  <- processArgs args
          let orderRequest = processCommand cmd
          order <- processOrderRequest productLibrary discountLibrary orderRequest
          let displayOrder = processOrder order
          putStr . show . pretty $ displayOrder
