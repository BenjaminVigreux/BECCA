data.breakdown <- function(data) {
  
  col.names <- c("Data type", "Min/n.levels", "1st Quartile", "Median", "Mean", "Variance", "IQR", "3rd Quartile", "Max", "NAs")
  special <- c("EntryDate", "positivity", "texts_org", "texts_eng", "titles_org", "titles_eng")
  breakdown <- matrix(NA, ncol = length(col.names), nrow = dim(data)[2],
                      dimnames = list(colnames(data), col.names))
  
  # Get datatypes
  classes <- lapply(data, class)
  
  for (c in 1:dim(data)[2]) {
    
    # Terms
    var <- data[, c]
    empties <- sum(is.na(var)) + sum(is.null(var)) + sum(var == "", na.rm = TRUE)
    idvar <- NULL
    flagged <- NULL
    obs <- dim(data)[1]
    
    # Special variables
    if (any(colnames(data)[c] == special)) {
      
      sp.name <- colnames(data)[c]
      breakdown[c, 1] <- switch(sp.name,
                                "EntryDate" = "Date",
                                "positivity" = "Continuous",
                                "texts_org" = "Micronarrative data",
                                "texts_eng" = "Micronarrative data",
                                "titles_org" = "Micronarrative data",
                                "titles_eng" = "Micronarrative data")
    } else {
      
      # Class == factor
      if (classes[c] == "factor") {
        
        #ID variables
        if ((nlevels(var) - empties) == obs) {
          idvar <- c(idvar, colnames(data)[c])
          breakdown[c, 1] <- "Identifier"
        } else {
          # Flag up possible non-data variables (nlevels > 10% of obs).
          if ((nlevels(var) - empties) >= 0.1 * obs) {
            flagged <- c(flagged, colnames(data)[c])
          } else {
            # Show most frequent levels
            breakdown[c, 1] <- "Factor"
            breakdown[c ,2] <- paste0(nlevels(var), " levels")
            level.names <- names(rev(sort(summary(var))))[1:(ncol(breakdown) - 2)]
            level.names[level.names == ""] <- "empty cell"
            toplevels <- paste(level.names, ":" ,rev(sort(summary(var)))[1:(ncol(breakdown) - 2)])
            breakdown[c, 3:ncol(breakdown)] <- toplevels
          }
        }
      }
      
      # Class == integer
      if (classes[c] == "integer") {
        #ID variables
        if ((length(table(var)) - empties) == obs) {
          idvar <- c(idvar, colnames(data)[c])
          breakdown[c, 1] <- "Identifier"
        } else {
          if (!length(table(var)) > 2) {
            # Binomial data
            breakdown[c, 1] <- "Binomial"
            breakdown[c, 2] <- paste("Count :", sum(var == 1, na.rm = TRUE))
            breakdown[c, 3] <- paste0("Prop: ", format(round((sum(var == 1, na.rm = TRUE)/obs)*100, 2), 2), "%")
          } else {
            breakdown[c, 1] <- "Continuous"
            if (length(table(var)) > ncol(breakdown)) {
              # Continuous data
              mn <- as.numeric(format(round(min(var, na.rm = TRUE), 3), 3))
              md <- as.numeric(format(round(median(var, na.rm = TRUE), 3), 3))
              xbar <- as.numeric(format(round(mean(var, na.rm = TRUE), 3), 3))
              sigma2 <- as.numeric(format(round(var(var, na.rm = TRUE), 3), 3))
              iqr <- as.numeric(format(round(IQR(var, na.rm = TRUE), 3), 3))
              mx <- as.numeric(format(round(max(var, na.rm = TRUE), 3), 3))
              breakdown[c, ] <- c("Continuous", mn, (md-mn)/2, md, xbar, sigma2, iqr, (mx-md)/2, mx, empties)
            } else {
              # Class == integer, but minimal unique values - possibly continuous data.
              values <- paste0("'", names(table(var)), "' : ", table(var))
              breakdown[c, 2:(length(values) + 1)] <- values
            }
          }
        }
      }
      # Class == numeric
      if (classes[c] == "numeric") {
        # Continuous data
        mn <- as.numeric(format(round(min(var, na.rm = TRUE), 3), 3))
        md <- as.numeric(format(round(median(var, na.rm = TRUE), 3), 3))
        xbar <- as.numeric(format(round(mean(var, na.rm = TRUE), 3), 3))
        sigma2 <- as.numeric(format(round(var(var, na.rm = TRUE), 3), 3))
        iqr <- as.numeric(format(round(IQR(var, na.rm = TRUE), 3), 3))
        mx <- as.numeric(format(round(max(var, na.rm = TRUE), 3), 3))
        breakdown[c, ] <- c("Continuous", mn, (md-mn)/2, md, xbar, sigma2, iqr, (mx-md)/2, mx, empties)
      }
      
      # Class == logical
      if (classes[c] == "logical") {
        if (sum(is.na(var), na.rm =  TRUE) == obs)
          breakdown[c, 1] <- "Empty variable"
      }
    }
  }
  out <- list(breakdown, idvar, flagged)
  return(out)
}