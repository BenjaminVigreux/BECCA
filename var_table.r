var.table <- function(table, cov, type, level, range) {
  
  # Function to tabluate input variables in descriptive stats.
  
  # table <- existing table (if not created, set to NULL).
  # cov <- Covariate name(s).
  # type <- Type of covariate (triad, dyad, stone {cont}/ question, descriptor {factor}).
  # level <- Level of factor variable.
  # range <- Range for continous variables (slider input).
  
  library(shiny)
  
  if (req(cov) != "Select") {
    
    # Continuous Variables
    if ((type == "Triads") || (type == "Dyads") || (type == "Stones")) {
      
      var <- cov
      min <- range[1]
      max <- range[2]
      col.name <- switch(substr(cov,1,1),
                         "T" = c("Triads", "T Min", "T Max"),
                         "D" = c("Dyads", "D Min", "D Max"),
                         "S" = c("Stones", "S Min", "S Max"))
      cn <- grep(col.name[1],colnames(table))
      # New sigtype
      if (length(cn) == 0){
        if (is.null(table)) {
          # First entry
          table <-  matrix(data = c(var, min, max), nrow = 1, dimnames = list(NULL,col.name))
        } else {
          # New column 
          table <- cbind(table, matrix(NA, ncol = 3, nrow = dim(table)[1]))
          table[1,(dim(table)[2]-2):dim(table)[2]] <- c(var, min, max)
          colnames(table)[(dim(table)[2]-2):dim(table)[2]] <- col.name
        }
      } else {
        if (dim(table)[1] == length(table[, cn][!is.na(table[, cn])])) {
          # New Row
          new <- rep(NA, dim(table)[2])
          new[cn:(cn+2)] <- c(var, min, max)
          table <- rbind(table,new)
        } else {
          # Blank row
          r <- length(table[!is.na(table[,cn]),cn])
          table[r+1,cn:(cn+2)] <- c(var, min, max)
        }
      }
    } else {
      
      # Factor Variables
      for (v in 1:length(level)) {
        var <- level[v]
        col.name <- switch(substr(var,1,1),
                           "Q" = "Questions",
                           "D" = "Descriptors")
        cn <- grep(col.name, colnames(table))
        if (is.null(table)) {
          # First entry
          table <- matrix(data = var, nrow = 1, dimnames = list(NULL, col.name))
        } else {
          if (length(cn) == 0) {
            # New column
            table <- cbind(table, c(var, rep(NA, dim(table)[1]-1)))
            colnames(table)[dim(table)[2]] <- col.name
          } else {
            if (dim(table)[1] == length(table[, cn][!is.na(table[, cn])])) {
              # New row
              new <- rep(NA, dim(table)[2])
              new[cn] <- var
              table <- rbind(table,new)
              rownames(table)[dim(table)[1]] <- ""
            } else {
              # Blank row
              r <- length(table[!is.na(table[, cn]), cn])
              table[r+1,cn] <- var
            }
          }
        }
      }
    }
  }
  return(table)
}