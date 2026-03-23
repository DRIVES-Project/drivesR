# Print a character vector as a list of quoted strings separated by commas.

Usually this is something I would use to trim excess names off a data
frame.

## Usage

``` r
print_stringvec_as_commasep(mystringvec)
```

## Arguments

- mystringvec:

  A character vector.

## Value

The character vector printed in the console in a way that can be copied
into a vector in the code.

## Examples

``` r
df1 <- as.data.frame(matrix(1:26, nrow = 1, ncol = 26, dimnames = list(NULL,LETTERS)));
print_stringvec_as_commasep(LETTERS);
#> 'A' ,'B' ,'C' ,'D' ,'E' ,'F' ,'G' ,'H' ,'I' ,'J' ,'K' ,'L' ,'M' ,'N' ,'O' ,'P' ,'Q' ,'R' ,'S' ,'T' ,'U' ,'V' ,'W' ,'X' ,'Y' ,'Z'
# Copy the output into the script, remove a handful of values I don't want
# for whatever arbitrary, difficult-to-code reason.
colsIWant <- c("A" ,"B" ,"C" ,"D" ,"M"  ,"S" ,"T" ,"U" ,"V" ,"W" ,"X" ,"Y" ,"Z");
df2 <- df1[,colsIWant]
```
