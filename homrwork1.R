mat <- matrix(1:12, nrow=3, ncol=4)

mat <- rbind(mat, rep(8, ncol(mat)))

mat <- cbind(mat, rep(9, nrow(mat)))


x <- c(1:10,1)
class(x)



class(c(1:10, 1))

x <- c(1,3,5)
y <- c(3,2,10)
c <- rbind(x,y)
class(c)

x <- list(2, "a", "b", TRUE)
x[1]


x <- c(1,2,NA,4,5,NA)
good <- !is.na(x)
mean(x[good])

