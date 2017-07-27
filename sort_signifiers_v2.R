##  Sort signifers 

##  x <- Dataframe of signifiers, texts and story titles to be sorted.
##  storychar <- Optional threshold for the mean number of characters in each 
##  observation of a variable, below which the variable is NOT identified as
##  the variable containing the stories.

sigtypes <- function(x, storychar){
  
  triads <- NULL
  dyads <- NULL
  stones <- NULL
  questions <- NULL
  descriptors <- NULL
  respondent <- NULL
  collector <- NULL
  
  for (i in 1:dim(x)[2]){
    
    ##  Triads
    
    if ((length(grep("T[1-9]",colnames(x)[i])) == 1) && 
        (length(grep(".X$|.Y$",colnames(x)[i])) == 0)) 
    {triads <- data.frame(cbind(triads,x[,i]))
    colnames(triads)[dim(triads)[2]] <- colnames(x)[i]}
    
    ##  Dyads
    
    if ((length(grep("D[1-9]",colnames(x)[i])) == 1) && 
        (length(grep(".X$|.Y$",colnames(x)[i])) == 0)) 
    {dyads <- data.frame(cbind(dyads,x[,i]))
    colnames(dyads)[dim(dyads)[2]] <- colnames(x)[i]}
    
    ##  Stones
    
    if (length(grep("S[1-9]",colnames(x)[i])) == 1) 
    {stones <- data.frame(cbind(stones,x[,i]))
    colnames(stones)[dim(stones)[2]] <- colnames(x)[i]}
    
    ##  Questions
    
    if (length(grep("^Q[1-9]",colnames(x)[i])) == 1) 
    {questions <- data.frame(cbind(questions,x[,i]))
    colnames(questions)[dim(questions)[2]] <- colnames(x)[i]}
    
    ##  Descriptors
    
    if (length(grep("DQ[1-9]",colnames(x)[i])) == 1) 
    {descriptors <- data.frame(cbind(descriptors,x[,i]))
    colnames(descriptors)[dim(descriptors)[2]] <- colnames(x)[i]}
    
    ##  Respondent
    
    if (length(grep("^R[1-9]",colnames(x)[i])) == 1) 
    {respondent <- data.frame(cbind(respondent,x[,i]))
    colnames(respondent)[dim(respondent)[2]] <- colnames(x)[i]}
    
    ##  Collector
    
    if (length(grep("^C[1-9]",colnames(x)[i])) == 1) 
    {collector <- data.frame(cbind(collector,x[,i]))
    colnames(collector)[dim(collector)[2]] <- colnames(x)[i]}
    
    ##  Texts
      # Original Language
    
    if (sum(match(c("Your.experience","FragmentEntry","Describe.your.example.here"),colnames(data)[i]), na.rm = TRUE) >= 1){
      if (missing(storychar)) {storychar <- mean(nchar(as.character(x[,i])))-1}
      if (mean(nchar(as.character(x[,i]))) > storychar) {
        texts_org <<- as.character(x[,i])
      }}
    
      # English
    if (sum(match(c("English","FragEntryEng"),colnames(data)[i]), na.rm = TRUE) >= 1){
      if (missing(storychar)) {storychar <- mean(nchar(as.character(x[,i])))-1}
      if (mean(nchar(as.character(x[,i]))) > storychar) {
        texts_eng <<- as.character(x[,i])
      }}
    
    ##  Titles # Cannot handle pretranslated texts (see 'lang'). Scrap auto identification???
    
    if (sum(match(c("StoryTitle","Story.Title"),colnames(data)[i]), na.rm = TRUE) >= 1){
      titles_org <<- as.character(x[,i])
    }
  
    if (sum(match(c("EngTitle"),colnames(data)[i]), na.rm = TRUE) >= 1){
      titles_eng <<- as.character(x[,i])
    }
  }
  
  triads <<- triads
  dyads <<- dyads
  stones <<- stones
  questions <<- questions
  descriptors <<- descriptors
  respondent <<- respondent
  collector <<- collector
}