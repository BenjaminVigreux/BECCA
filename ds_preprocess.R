ds.process <- function(dataset) {
  
  # Load dependent functions
  source("factor2binomial.R")
  source("namegetter.R")
  source("sort_signifiers.R")
  
  # Convert factor levels to integer variables
  vars <- c("^Q[0-9]", "^DQ[0-9]", "^R[0-9]", "^C[0-9]")
  dataset <- factor2bin(dataset, 
                        include = vars, 
                        integrate = TRUE, 
                        keepFactor = FALSE, 
                        keepVarName = TRUE, 
                        maxLevels = 20, 
                        exclude = "[Oo]ther$")
  
  # Remove remaining factors ("Other", Q7.Score)
  dataset <- dataset[names(dataset) != "Q7.Score"]
  dataset <- dataset[,-grep("Other$", names(dataset))]
  
  # Sort variables by signifier type
  sigtypes(dataset)
  
  # Get names of factor variables
  factor_names <- namegetter(dataset)
  
  assign("qnames", factor_names[[1]], pos = parent.frame(n=1))
  assign("dqnames", factor_names[[2]], pos = parent.frame(n=1))
  
  assign("triads", triads, pos = parent.frame(n=1))
  assign("stones", stones, pos = parent.frame(n=1))
  assign("dyads", dyads, pos = parent.frame(n=1))
  
  return(dataset)
}