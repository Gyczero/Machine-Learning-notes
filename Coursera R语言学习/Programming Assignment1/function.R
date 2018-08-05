add2 <- function(x,y) {
  x + y 
}

above10 <- function(x) {
  use <- x > 10
  x[use]
}

above <- function(x,n = 10) {
  use <- x > n
  x[use]
}


columnean <- function(y, removeNA = TRUE) {
  colum <- ncol(y)
  means <- numeric(colum)
  for(i in 1:colum) {
    means[i] <- mean(y[,i], na.rm = removeNA)
  }
  means  #返回最后一个表达式
}