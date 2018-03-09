# [discounted][]

# Database
The database of products and discounts is in `executable/Database.hs`

# Compile
```
stack install --local-bin-path .
```
# Running app
```
./discounted <product-id> <product-quantity>
```
## Arguments
* Arguments are pairs of product ids and quantities.
```
./discounted <id> <quantity> <id> <quantity> ...
```
example (5 copies of the product 2, and 3 copies of the product 1):
```
./discounted 2 5 1 3
```

* Discounts are applied by adding the flag `--discount` (or `-d`)
```
./discounted --discount WELCOME 2 5 1 3
```

# Run the test suite.
```
stack test
```

[discounted]: https://github.com/goosetherumfoodle/discounted
