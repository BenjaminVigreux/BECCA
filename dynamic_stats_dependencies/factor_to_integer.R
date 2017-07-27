##  Converts factor variables of class "factor" to a matrix of those variables as
##  binary values, in separate columns for each variable.

##  x <- A matrix or data.frame of factor variables, or vector of single variable
##  of class factor.

##  class <- Desired output class ("integer" or "numeric").

##  keepFactor <- include original class "factor" variable in output matrix. Logical.

##  integrate <- FALSE returns a new matrix, TRUE integrates output with "x".

##  ignore <- integer between 1 and 100. Threshold number of levels higher than
##  which the factor2int will ignore a variable. Designed to catch text entry variables.

##  Issues
# DQ5.Live and DQ8.LiveInRegion not converted.
# Class argument urrently doesn't do anything - possibly because attempts to
# coerce object of class "matrix"??

factor2int <- function(x,
                       class = c(NULL,"integer","numeric"),
                       keepFactor = c(FALSE,TRUE),
                       integrate = c(FALSE,TRUE),
                       ignore = 1:100){
  
  if (missing(class)) {class <- "none"}
  if (missing(keepFactor)) {keepFactor <- FALSE}
  if (missing(integrate)) {integrate <- FALSE}
  ##  missing(ignore) contained in loop.
  
  n <- 0
  
  ##  Single Variable Input
  
  if (is.factor(x)) {
    m <- matrix(NA, nrow = length(x), ncol = length(levels(x)))
    colnames(m) <- levels(x)
    for (l in 1:length(levels(x))){
      for (i in 1:length(x)){
        if (x[i] == levels(x)[l]) {m[i,l] <- 1}
      }
      if (class != "none") {
        if (class == "integer") {m[,l] <- as.integer(m[,l])}
        if (class == "numeric") {m[,l] <- as.numeric(m[,l])}}
    }
    if (keepFactor == TRUE) {m <- data.frame(x,m)}
    return(m)
  } else
    
    ## Dataframes and matrices
    
  {if ((is.data.frame(x)) || (is.matrix(x))) {
    m <- NA
    if (integrate == TRUE){
      for (c in 1:dim(x)[2]){
        if (missing(ignore)) {ignore <- length(levels(x[,c]))+1}
        if ((is.factor(x[,c])) && 
            ((length(grep("^Q[0-9]",names(x)[c])) > 0) || 
             (length(grep("^DQ[0-9]",names(x)[c])) > 0) ||
             (length(grep("^R[0-9]",names(x)[c])) > 0) ||
             (length(grep("^C[0-9]",names(x)[c])) > 0)) &&
            (length(grep("[Oo]ther$",names(x)[c])) == 0) &&
            (length(levels(x[,c])) < ignore)) {
          if (!exists("n")) {n <- length(levels(x[,c]))
          assign(paste0("n",c),length(levels(x[,c])))} else {n <- n+length(levels(x[,c]))
          assign(paste0("n",c),length(levels(x[,c])))}
        }}
      } else {n <- 0}
    for (c in 1:(dim(x)[2]+n)){
      if ((is.factor(x[,c])) && 
          ((length(grep("^Q[0-9]",names(x)[c])) > 0) || 
           (length(grep("^DQ[0-9]",names(x)[c])) > 0) ||
           (length(grep("^R[0-9]",names(x)[c])) > 0) ||
           (length(grep("^C[0-9]",names(x)[c])) > 0)) &&
          (length(grep("[Oo]ther$",names(x)[c])) == 0) &&
          (length(levels(x[,c])) < ignore)) {
        y <- matrix(NA, nrow = dim(x)[1], ncol = length(levels(x[,c])))
        names <- rep(NA, length(levels(x[,c])))
        for (i in 1:length(levels(x[,c]))){
          names[i] <- paste0(names(x)[c],"_",levels(x[,c])[i])
        }
        colnames(y) <- names
        for (l in 1:length(levels(x[,c]))){
          for (i in 1:dim(x)[1]){
            if (x[i,c] == levels(x[,c])[l]) {y[i,l] <- 1}
          }
          if (class != "none") {
            if (class == "integer") {y[,l] <- as.integer(y[,l])}
            if (class == "numeric") {y[,l] <- as.numeric(y[,l])}}
        }
        assign(paste0("m",c),y)
        if (integrate == TRUE) {
          if (keepFactor == FALSE) {
            x <- data.frame(x[,1:c],y,x[,(c+1):dim(x)[2]])
            if (!exists("f")) {f <- c} else {f <- c(f,c)}
          } else {
            x <- data.frame(x[,1:c],y,x[,(c+1):dim(x)[2]])
          }
        } else {
          if (keepFactor == FALSE){
            m <- data.frame(m,get(paste0("m",c)))
          } else {
            m <- data.frame(m,x[,c],get(paste0("m",c)))
            colnames(m)[dim(m)[2]-dim(y)[2]] <- names(x)[c]
          }}
      }}
    if (integrate == FALSE) {m <- m[,-1]}
    if ((integrate == TRUE) && (keepFactor == FALSE)) {x <- x[,-f]
    return(x)} else {return(m)}
  } else {print("Pass single factor variable only")}
  }}