##------------------------------------------------------------------------------
## Vector //
##------------------------------------------------------------------------------

x <- letters
first(x)
first(x, 3)
first(x, 3, keep = TRUE)

## Conditions //
first(x, 30)
first(x, 30, strict = 1)
first(x, 30, strict = 2)
try(first(x, 30, strict = 3))

##------------------------------------------------------------------------------
## List //
##------------------------------------------------------------------------------

x <- as.list(letters)
first(x)
first(x, 3)
res <- first(x, 3, keep = TRUE)
attributes(res)

## Conditions //
first(x, 30)
first(x, 30, strict = 1)
first(x, 30, strict = 2)
try(first(x, 30, strict = 3))

##------------------------------------------------------------------------------
## Data frame //
##------------------------------------------------------------------------------

x <- data.frame(a= 1:5, b = letters[1:5], stringsAsFactors = FALSE)

## Rows:
first(x)
first(x, 3)
first(x, 3, keep = TRUE)

## Columns:
first(x, margin = 2)
first(x, 2, margin = 2)
first(x, 3, margin = 2)

first(x, margin = 2, drop = TRUE)
first(x, 2, margin = 2, drop = TRUE)

## Conditions:
first(x, 10)
first(x, 10, strict = 1)
first(x, 10, strict = 2)
try(first(x, 10, strict = 3))

first(x, 3, margin = 2)
first(x, 3, margin = 2, strict = 1)
first(x, 3, margin = 2, strict = 2)
try(first(x, 3, margin = 2, strict = 3))


