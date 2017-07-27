unit2perc <- function(x,y){
  
  ##  NB. CONTINUOUS VARIABLE percentages show the number of stories in that
  ##  CELL (stories that fall into ROW and COLUMN ranges) as a percentage of
  ##  all the stories in the variable range specified in the ROW.
  
  ##  NB. FACTOR VARIABLE percentages show the number of stories in that
  ##  CELL as a percentage of all the stories that derive from the factor variable
  ##  given by the COLUMN. Total percentage of stories shown in all COLUMNS
  ##  deriving from that factor is given by the "Total column" that succeeds
  ##  the final COLUMN for a given factor.
  
  ##  Arguments
  
  # x <- Dataframe from which variables contained in 'y' were originally drawn.
  # y <- Descriptive Statistics matrix to be converted to proportional values.
  
  ##  Dependencies
  
  source("namegetter.R")
  
  ##  Identify rows relating to single factor
  
  tds <- grep("^T[0-9]|^S[0-9]|^D[0-9]",colnames(y),value = TRUE)
  n <- namegetter(x)
  factornames <- c(n[[1]],n[[2]],tds)
  for (f in 1:length(factornames)){
    fac_levels <- grep(factornames[f],colnames(y))
    if (length(fac_levels) != 0) {
      
      ##  Average levels
      
      totvec <- rep(0,dim(y)[1])
      for (r in 1:dim(y)[1]){
        tot <- sum(y[r,fac_levels], na.rm = TRUE)
        z <- x[,c(rownames(y)[r],grep(factornames[f],colnames(x),value = TRUE))]
        if (max(z[,1], na.rm = TRUE) != 1) z[,1] <- desc_rows[,rownames(y)[r]]
        if (max(z[,2], na.rm = TRUE) != 1) z[,2] <- desc_cols[,grep(factornames[f],colnames(x),value = TRUE)]
        z <- z[!is.na(z[,1]),2:dim(z)[2]]
        totvec[r] <- tot/sum(z,na.rm = TRUE) * 100
        for (i in 1:length(fac_levels)){
          if (length(fac_levels) > 1) y[r,fac_levels[i]] <- (y[r,fac_levels[i]]/tot) * 100
          if (length(fac_levels) == 1) y[r,fac_levels[i]] <- (y[r,fac_levels[i]]/length(which(as.logical(desc_rows[,rownames(y)[r]])))) * 100
          
          ##  Add total column
          
          if ((i == length(fac_levels)) && (r == dim(y)[1]) && (match(factornames[f],c(n[[1]],n[[2]]), nomatch = FALSE))) {
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