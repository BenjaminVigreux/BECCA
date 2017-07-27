clean.data <- function(data, translate = c(TRUE, FALSE), factorNames = c(TRUE, FALSE), textVar, titleVar, posVar, dateVar, dateFormat) {
  
  # Function to clean new datasets for BECCA.
  
  # ARGUMENTS
  # data <- dataset to be cleaned.
  # translate <- If TRUE, new variables will be created with translated texts & titles.
  # textVar <- name of the variable containing the stories.
  # titleVar <- name of the variable containing the story titles.
  # posVar <- name of variable containing positivity score.
  # dateVar <- name of the variable containg Entry Date.
  # dateFormat <- character string indicating the format of the date (eg. "DD/MM/YYYY").
  
  # DEFAULTS/REQUIRED
  
  stopifnot(!missing(data))
  if (missing(translate))
    translate <- FALSE
  if (missing(factorNames))
    factorNames <- FALSE
  
   positivity <- data[, posVar]
   positivity <- transpose(lapply(positivity, function(positivity) {
     positivity <- switch(positivity,
                          "strongly positive" = 2,
                          "positive" = 1,
                          "neutral" = 0,
                          "negative" = -1,
                          "strongly negative" = -2)
   }
   )
   )[[1]]
   
   data <- data.frame(data, positivity)
   
   # Global Variable names
   names(data)[names(data) == textVar] <- "texts_org"
   names(data)[names(data) == titleVar] <- "titles_org"
   names(data)[names(data) == dateVar] <- "EntryDate"
   
   if (length(dateVar))
     data$EntryDate <- as.Date(data$EntryDate, format = dateFormat)
   
   # Translator
   
   if (translate) {
     
     load("yandexAPI.txt")
     library(RYandexTranslate)
     
     # Stories
     texts_org <- as.character(data$texts_org)
     texts_eng <- rep(NA, length(texts_org))
     
     for (i in 1:length(texts_eng)) {
       tryCatch({
         texts_eng[i] <- translate(yandexAPI, texts_org[i], "en")$text
       }, function(e)
         cat("Error translating story",i,", skipped.\n"))
       if (i %% 10 == 0)
         print(paste0("Translated ",i," of ", length(texts_org)," stories."))
     }
     
     # Titles
     titles_org <- as.character(data$titles_org)
     titles_eng <- rep(NA, length(titles_org))
     
     for (i in 1:length(titles_eng)) {
       tryCatch({
         titles_eng[i] <- translate(yandexAPI, titles_org[i], "en")$text
       }, function(e)
         cat("Error translating title",i,", skipped.\n"))
       if (i %% 10 == 0)
         print(paste0("Translated ",i," of ", length(titles_org)," titles."))
     }
     data <- data.frame(data, texts_eng, titles_eng)
   }
   
   # Sort signifiers into subsets.
   sigtypes(data)
   
   # Remove examples
   data <- data[, -grep("^T0|^D0|^S0|^Q0|^DQ0", names(data))]
   
   # Namegetter
   if (factorNames) {
     source("namegetter.R")
     fac.names <- namegetter(data)
     qnames <- fac.names[[1]]
     dqnames <- fac.name[[2]]
     assign("qnames", qnames, parent.frame(n=1))
     assign("dqnames", dqnames, parent.frame(n=1))
   }
   
return(data)
}