factorize <- function(x){
  if(!is.matrix(x) && !is.data.frame(x)) x <- as.matrix(x)
x <- apply(x, 2, function(x){
  x <- as.factor(x)
    y <- data.frame(matrix(0, nrow = length(x), ncol = nlevels(x)))
      for (c in 1:dim(y)[2]){
        y[which(x == levels(x)[c]),c] <- 1
      }
    names(y) <- levels(x)
    x <- y
  })
z <- rep(NA,dim(x[[1]])[1])
for (i in 1:length(x)){
  z <- data.frame(z,x[[i]])
}
z <- z[,-1]
}