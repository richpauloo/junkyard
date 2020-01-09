library(lobstr)


# make a new column named 3 that is the sum of columns 1 and 2
df <- data.frame(runif(3), runif(3))
df
names(df) <- c(1, 2)
df
df$`3` <- df$`1` + df$`2`
df

# how much memory does y occupy
x <- runif(1e6)
y <- list(x, x, x)
object.size(y) 
# 3 times x. list() copies x 3 times

# On which line does a get copied in the following example?
a <- c(1, 5, 3, 2)
b <- a
b[[1]] <- 10
# a is copied when b is modified in line 3, `b[[1]] <- 10`


# names v values

# x is a name, and 1:3 is a value
x <- 1:3

# y <- x creates another name that binds to 1:3
y <- x

# confirm this by checking the "memory address" of an object
lobstr::obj_addr(x)
obj_addr(y)

# Explain the relationship between a, b, c and d in the following code:
a <- 1:10
b <- a
c <- b
d <- 1:10

obj_addr(a)
obj_addr(b)
obj_addr(c)
obj_addr(d)

# a, b, c are names pointing to an object 1:10 at `obj_addr(a)`, 
# whereas d is a name pointing to a different object d, at `obj_addr(d)`


# The following code accesses the mean function in multiple ways. Do they all point to the same underlying function object? Verify this with lobstr::obj_addr().
mean
base::mean
get("mean")
evalq(mean)
match.fun("mean")

all.equal(obj_addr(mean), 
          obj_addr(base::mean), 
          obj_addr(get("mean")),
          obj_addr(evalq(mean)),
          obj_addr(match.fun("mean"))
          )

# By default, base R data import functions, like read.csv(), will automatically convert non-syntactic names to syntactic ones. Why might this be problematic? What option allows you to suppress this behaviour?

?read.csv # check.names = FALSE


# What rules does make.names() use to convert non-syntactic names into syntactic ones?

?make.names()
# The character "X" is prepended if necessary. All invalid characters are translated to ".". A missing value is translated to "NA". Names which match R keywords have a dot appended to them. Duplicated values are altered by make.unique.


# I slightly simplified the rules that govern syntactic names. Why is .123e1 not a syntactic name? Read ?make.names for the full details.

?make.names()
# A syntactically valid name consists of letters, numbers and the dot or underline characters and starts with a letter or the dot not followed by a number. 


# copy-on-modify
x <- c(1, 2, 3)
y <- x

obj_addr(x)
obj_addr(y)

y[[3]] <- 4

obj_addr(x)
obj_addr(y)

# the shared biniding of x and y to 1:3 was changed when 
# we modified y. R created a new object, at a new memory address, 
# and re-bound y to this new object. This is called "copy-on-modify"
# because R copied a new object when we modified it.

# When exploring copy-on-modify behaviour interactively, be aware that you’ll get different results inside of RStudio. That’s because the environment pane must make a reference to each object in order to display information about it. This distorts your interactive exploration but doesn’t affect code inside of functions, and so doesn’t affect performance during data analysis. For experimentation, I recommend either running R directly from the terminal, or using RMarkdown (like this book).



