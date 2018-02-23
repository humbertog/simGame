





x <- seq(0,10, .01)
y <- rnorm( length(x), 3 + 2.5 *x, 4)

plot(x,y)

m <- lm(y ~ x)
summary(m)
plot(m)




xn <- (2*x + 5) 
plot(xn,y)


mn <- lm(y ~ xn)
summary(mn)
