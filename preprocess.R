preprocess <- function(x){

##  DEPENDENCIES

  source("factor_to_integer.R")
  source("namegetter.R")
  source("sort_signifiers.R")
  source("unit2perc.R")
  
  ##  PREPROCESSING
  
  x_bin <<- factor2int(x, integrate = TRUE, ignore = 20, keepFactor = FALSE)
  
  ##  Remove remaining factors ("Other", Q7.Score)
  
  a <- rep(NA,dim(x_bin)[2])
  for (i in 1:dim(x_bin)[2]){
    if (((is.factor(x_bin[,i])) && 
         ((length(grep("^Q[0-9]",names(x_bin)[i]))) ||
          (length(grep("^DQ[0-9]",names(x_bin)[i]))))) || 
        (length(grep("Score$",names(x_bin)[i])))){
      a[i] <- i
    }
  }
  a <- a[!is.na(a)]
  x_bin <<- x_bin[,-a]
  
  ##  Sort by signifier type
  
  types <- sigtypes(x_bin) # Sig types so stones, triads, and dyads dropdowns work.
  for (i in 1:length(types)){
    assign(names(types)[i],types[[i]], envir = .GlobalEnv)
  }
  rm(types)
  
  ##  Namegetter
  
  factor_names <- namegetter(x_bin)
  qnames <<- factor_names[[1]]
  dqnames <<- factor_names[[2]]
}