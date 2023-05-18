# Exception Notes

## All operators

In R, there are several types of operators used for various operations, including arithmetic, comparison, assignment, logical, and special operators. Here's a list of common operators in R, including some operators that may not be part of the official R language definition but are used in popular packages or extensions:

1. Arithmetic Operators:
   - `+`: Addition
   - `-`: Subtraction
   - `*`: Multiplication
   - `/`: Division
   - `^` or `**`: Exponentiation
   - `%%`: Modulus (remainder after division)
   - `%/%`: Integer Division

2. Comparison Operators:
   - `==`: Equal to
   - `!=`: Not equal to
   - `<`: Less than
   - `>`: Greater than
   - `<=`: Less than or equal to
   - `>=`: Greater than or equal to

3. Assignment Operators:
   - `<-` or `<<-`: Assignment (right to left)
   - `->` or `->>`: Assignment (left to right)
   - `=`: Assignment (used mostly within function arguments)

4. Logical Operators:
   - `&`: Element-wise AND
   - `|`: Element-wise OR
   - `&&`: AND (returns the first non-TRUE value or the last value)
   - `||`: OR (returns the first non-FALSE value or the last value)
   - `!`: NOT

5. Special Operators:
   - `:=`: Assignment operator used in the data.table package
   - `%>%`: Pipe operator, often used in the dplyr and magrittr packages to chain operations
   - `%<>%`: Compound assignment pipe operator, used in magrittr to update a variable by applying a function
   - `%*%`: Matrix multiplication
   - `%in%`: Matching operator, checks if an element belongs to a given set
   - `%o%`: Outer product
   - `%x%`: Kronecker product
   - `%/%`: Integer division
   - `%%`: Modulus (remainder after division)
   - `$`: List or data frame element extraction
   - `@`: S4 object slot extraction

6. Other Operators:
   - `|>`: Pipe operator introduced in R 4.1.0, which allows for more readable code by chaining operations

Please note that some of these operators, like `:=` and `%>%`, are not part of the base R language but are introduced by packages to extend R's functionality. The pipe operator `|>` was introduced in R 4.1.0 as a native alternative to the popular `%>%` operator from the magrittr package.


### NOTES
 - `|>` has the same precedence as %xyz% according to code definition. I must have missed this.
 
## check equality with a data table

> ## checking for a data table change
> dt <- data.table(letter = c("a","b","c"),done=rep(FALSE,3))
> dt2 <- dt
> df <- as.data.frame(dt)
> isTRUE(all.equal(df,dt,check.attributes=FALSE))
[1] TRUE
> dt[1:2,done:=TRUE]
> ## dt2 changes with dt
> identical(dt,dt2)
[1] TRUE
> ## we can check the values this way
> isTRUE(all.equal(df,dt,check.attributes=FALSE))
[1] FALSE

##note - we might want to separately compare the attributes 