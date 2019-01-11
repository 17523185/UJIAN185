
library(polynom)
dat = read.csv("nomor12.csv")
dat
dat2 = read.csv("nomor35.csv")
dat2

#NOMOR 1
model <-lm(y ~ x, dat)
summary(model)

#NOMOR 2
predict(model, data.frame(x = 55))

#NOMOR 3
x3 <- c(0,1,2,3,4)
y3 <- c(1.00,2.25,3.75,4.25,5.65)
poly.calc(x3,y3)

#NOMOR 4
f1 <- function(x){
    return (1 - 0.07916667*x + 2.19375*x^2 - 0.9958333*x^3 + 0.13125*x^4)
}
f1(2.75)

#NOMOR 5
plot(x3,y3)
curve(f1, add=TRUE)

library(pracma)

#NOMOR 11
fx1 <- function(x){
    return (x^2 - 6)
}
trapzfun(fx1,0,1)

#NOMOR 12
fx2 <- function(x){
    return (x^3 + 4*x^2 - 10)
}
trapzfun(fx2,1,2)

#NOMOR 14
h <- 0.1
x <- seq(0,1, by=h)
f <- function(x){
    return (x^2)
}
f0 <- f(x[1])
fi <- sapply(x[2:10], f)
fn <- f(x[length(x)])

trap <- function(f0, fi, fn, h){
    return (h/2 * (f0 + 2 * sum(fi) + fn))
}

trap(f0,fi,fn,h)

#NOMOR 15
h <- 0.2
x <- seq(0,1, by=h)
f <- function(x){
    return (x^2)
}
f0 <- f(x[1])
fi <- sapply(x[2:5], f)
fn <- f(x[length(x)])

trap <- function(f0, fi, fn, h){
    return (h/2 * (f0 + 2 * sum(fi) + fn))
}

trap(f0,fi,fn,h)
