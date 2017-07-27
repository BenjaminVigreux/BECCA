factor2bin <- function(data,
                       include,
                       exclude,
                       keepFactor = c(FALSE, TRUE),
                       integrate = c(FALSE, TRUE),
                       keepVarName = c(FALSE, TRUE),
                       maxLevels) {
  
  # SUMMARY
  
  # Function that converts the factor variables in a data frame or matrix 
  # into binomial variables with one column per factor level. If a single 
  # variable is supplied as character vector, a matrix is returned.
  
  # ARGUMENTS
  
  # data <- a dataframe or matrix containing at at least one variable of 
  # class 'factor', or a single factor variable as a character vector.
  
  # include <- Vector of regular expressions, specifying which variables 
  # to include. If NULL, all variables (subject to other ARGS) will be
  # evaluated.
  
  # exclude <- Vector of variable names not to evaluated. Overrides 
  # 'include'.
  
  # keepFactor <- If TRUE, original factor variables will be returned 
  # as part of the output. Default is FALSE.
  
  # integrate <- If TRUE, output variables will be integrated with the 
  # input frame, which will be returned as the output. If FALSE (default), 
  # a new matrix containing only the new variables (and the original 
  # converted variables if "keepFactor = TRUE") is returned.
  
  # keepVarName <- If TRUE, new variables will be named 
  # "<variable name>_<factor_level>". If FALSE (default), they are named 
  # <factor level>.
  
  # maxLevels <- A single integer providing an optional cap on the number 
  # of number of levels in a variable, above which the variable will be 
  # ignored.
  
  # DEFAULTS/REQUIRED
  
  library(data.table)
  
  stopifnot(!missing(data))
  if (missing(keepFactor))
    keepFactor <- FALSE
  if (missing(integrate))
    integrate <- FALSE
  if (missing(keepVarName))
    keepVarName <- FALSE
  if (missing(maxLevels))
    maxLevels <- max(as.numeric(lapply(data, nlevels)))
  if ((!is.numeric(maxLevels)) || (!length(maxLevels) == 1))
    stop("maxLevels must be a single integer")
  if (missing(exclude)) {
    exclude <- NULL
  } else {
    if (!is.character(exclude))
      stop("'exclude' must be a character vector.")
  }
  
  # FUNCTION
  
  # If character vector, convert to matrix (and factor variable if not).
  if ((!is.data.frame(data)) && (!is.matrix(data))) {
    if (!is.factor(data)) {
      data <- as.factor(data)
      warning("Variable was coerced to 'factor'.")
    }
    data <- as.matrix(data)
  }
  
  # Require at least one factor variable [Include ignore statement?]
  if (!any(as.logical(lapply(data, is.factor))))
    stop ("'data' must contain at least one variable of class 'factor'.")
  
  # Required objects
  added <- 0
  new <- NULL
  out <- data
  
  # Should variable be processed?
  for (v in 1:dim(data)[2]) {
    # Is variable a factor?
    if (is.factor(data[, v])) {
      # If include is missing, process all factor variables.
      if (missing(include)) {
        pass <- TRUE
      } else {
        # Is factor variable specified for processsing by include
        pattern <- rep(FALSE, length(include))
      for (e in 1:length(include)) {
        if (length(grep(include[e], colnames(data)[v]))) {
          pattern[e] <- TRUE
        }
      }
        ifelse(any(pattern), pass <- TRUE, pass <- FALSE)
      }
      # Is the number of levels higher than max threshold?
      if (nlevels(data[, v]) > maxLevels)
        pass <- FALSE
      # Is the variable specified for exclusion
      for (e in 1:length(exclude)) {
        if (length(grep(exclude[e], colnames(data)[v])))
          pass <- FALSE
      }
    } else {
      pass <- FALSE
    }
    
    # Create new variables
    if (pass) {
      var <- data[, v]
      new <- data.frame(matrix(0, ncol = nlevels(droplevels(var)), nrow = dim(data)[1]))
      for (i in 1:length(var)){
        l <- match(var[i], levels(droplevels(var)))
        new[i, l] <- 1
      }
      # Name new variables
      v.levels <- levels(droplevels(var))
      if (any(v.levels == ""))
        v.levels[v.levels == ""] <- "blank"
      if (keepVarName) {
        names(new) <- transpose(lapply(v.levels, function(v.levels) paste0(names(data)[v], "_", v.levels)))[[1]]
      } else {
        names(new) <- v.levels
      }
      # Keep factor?
      if (keepFactor)
        new <- data.frame(var, new)
      # Reintegrate with original data
      if (integrate) {
        out <- data.frame(out[, 1:(v+added-1)], new, out[, (v+added+1):(dim(out)[2])])
        added <- added + dim(new)[2] - 1
      } else {
        # New variables
        out <- data.frame(out, new)
      }
    }
  }
  return(out)
  }