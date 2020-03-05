# k-nearest neighbours

# first generate a line with normally distributed noise
x = scale(seq(0, 10, 0.01), scale = 10)
y = 2 + 3 * x + rnorm(length(x))
