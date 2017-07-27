unit2perc <- function(x,y){
  
  ##  Arguments
  
  # x <- Dataframe from which variables contained in 'y' were originally drawn.
  # y <- Descriptive Statistics matrix to be converted to proportional values.
  
  ##  Dependencies
  
  source("namegetter.R")
  
  ##  Identify rows relating to single factor
  
  n <- namegetter(x)
  factornames <- c(n[[1]],n[[2]])
  for (f in 1:length(factornames)){
    fac_levels <- grep(factornames[f],colnames(y))
    if (length(fac_levels) != 0) {
      
      ##  Average levels
      
      totvec <- rep(0,dim(y)[1])
      for (r in 1:dim(y)[1]){
        tot <- sum(y[r,fac_levels], na.rm = TRUE)
        z <- x[,c(rownames(y)[r],grep(factornames[f],colnames(x),value = TRUE))]
        z <- z[!is.na(z[,1]),2:dim(z)[2]]
        totvec[r] <- tot/sum(z,na.rm = TRUE) * 100
        for (i in 1:length(fac_levels)){
          y[r,fac_levels[i]] <- (y[r,fac_levels[i]]/tot) * 100
          
          ##  Add total column
          
          if ((i == length(fac_levels)) && (r == dim(y)[1])) {
            if (fac_levels[i] < dim(y)[2]) {y <- data.frame(y[,1:fac_levels[i]],totvec,y[,(fac_levels[i]+1):dim(y)[2]])} else {
              y <- data.frame(y,totvec)
            }
            nm <- strsplit(names(y)[fac_levels[i]],"_")[[1]]
            colnames(y)[fac_levels[i]+1] <- paste0(nm[1]," Total")
          }
        }
      }
    }
  }
  y <- format(round(y,2),nsmall = 2)
  y <- apply(y,c(1,2), function(y) paste0(y,"%"))
  return(y)
}