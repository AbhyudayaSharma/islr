print('Hello world')
data = read.csv('Advertising.csv')

tv_model = lm(sales~data$TV, data = data)
radio_model = lm(sales~radio, data = data)
newspaper_model = lm(sales~newspaper, data = data)

plot(data$TV, data$sales)
abline(tv_model, col = 'blue')

plot(data$radio, data$sales)
abline(radio_model, col = 'red')

plot(data$newspaper, data$TV)
abline(newspaper_model, col = 'purple')

# Force R to ignore beta-0 and only predict beta-1
tv_model_2 = lm(sales~0+TV, data = data)
radio_model_2 = lm(sales~0+radio, data = data)
newspaper_model_2 = lm(sales~0+newspaper, data = data)

# predict(radio_model, 23)

plot(data$TV, data$sales)
abline(tv_model_2, col = 'blue')

plot(data$radio, data$sales)
abline(radio_model_2, col = 'red')

plot(data$newspaper, data$sales)
abline(newspaper_model_2, col = 'purple')

# For loops are weird
for (i in 1:10) {
  if (i %% 2 == 0) {
    print(i)
  }
}

for (i in 0:10) {
  j <- i + 2
  print(j)
}
  
for (i in seq(1, 10, 3)) {
  print(i)
}

print(T == TRUE)
print(F == FALSE)

x = 5
print('x is ' + x)
x <- 5

print(x + 1)

# wow, there's even a prefix notation for binary operators
`<-`(x, 10)
print(x)

# string concatenation, space separator is added automatically
print(paste('hello', 'world'))

# length of strings
nchar('wow')
wow = 'wow'
wow[1] # string indexing doesn't work as expected
toupper(wow)
paste(wow, 3923.23i) # concating imaginary numbers to a str
.wow = 'wOw' # hidden variables
.wow == wow
