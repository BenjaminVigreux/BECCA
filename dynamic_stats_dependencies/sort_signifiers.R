##  Sort signifers 

##  x <- Dataframe of signifiers, texts and story titles to be sorted.
##  storychar <- Optional threshold for the mean number of characters in each 
##  observation of a variable, below which the variable is NOT identified as
##  the variable containing the stories.

sigtypes <- function(x, storychar){
  
  triads <- NA
  dyads <- NA
  stones <- NA
  questions <- NA
  descriptors <- NA
  respondent <- NA
  collector <- NA
  
  for (i in 1:dim(x)[2]){
    
    ##  Triads
    
    if ((length(grep("T[1-9]",colnames(x)[i])) == 1) && 
        (length(grep(".X$|.Y$",colnames(x)[i])) == 0)) 
    {triads <- data.frame(triads,x[,i])
    colnames(triads)[dim(triads)[2]] <- colnames(x)[i]}
    
    ##  Dyads
    
    if ((length(grep("D[1-9]",colnames(x)[i])) == 1) && 
        (length(grep(".X$|.Y$",colnames(x)[i])) == 0)) 
    {dyads <- data.frame(dyads,x[,i])
    colnames(dyads)[dim(dyads)[2]] <- colnames(x)[i]}
    
    ##  Stones
    
    if (length(grep("S[1-9]",colnames(x)[i])) == 1) 
    {stones <- data.frame(stones,x[,i])
    colnames(stones)[dim(stones)[2]] <- colnames(x)[i]}
    
    ##  Questions
    
    if (length(grep("^Q[1-9]",colnames(x)[i])) == 1) 
    {questions <- data.frame(questions,x[,i])
    colnames(questions)[dim(questions)[2]] <- colnames(x)[i]}
    
    ##  Descriptors
    
    if (length(grep("DQ[1-9]",colnames(x)[i])) == 1) 
    {descriptors <- data.frame(descriptors,x[,i])
    colnames(descriptors)[dim(descriptors)[2]] <- colnames(x)[i]}
    
    ##  Respondent
    
    if (length(grep("^R[1-9]",colnames(x)[i])) == 1) 
    {respondent <- data.frame(respondent,x[,i])
    colnames(respondent)[dim(respondent)[2]] <- colnames(x)[i]}
    
    ##  Collector
    
    if (length(grep("^C[1-9]",colnames(x)[i])) == 1) 
    {collector <- data.frame(collector,x[,i])
    colnames(collector)[dim(collector)[2]] <- colnames(x)[i]}
    
    ##  Texts
      # Original Language
    
    if (sum(match(c("Your.experience","FragmentEntry","Describe.your.example.here"),colnames(data)[i]), na.rm = TRUE) >= 1){
      if (missing(storychar)) {storychar <- mean(nchar(as.character(x[,i])))-1}
      if (mean(nchar(as.character(x[,i]))) > storychar) {
        texts_org <- as.character(x[,i])
      }}
    
      # English
    if (sum(match(c("English","FragEntryEng"),colnames(data)[i]), na.rm = TRUE) >= 1){
      if (missing(storychar)) {storychar <- mean(nchar(as.character(x[,i])))-1}
      if (mean(nchar(as.character(x[,i]))) > storychar) {
        texts_eng <- as.character(x[,i])
      }}
    
    ##  Titles # Cannot handle pretranslated texts (see 'lang'). Scrap auto identification???
    
    if (sum(match(c("StoryTitle","Story.Title"),colnames(data)[i]), na.rm = TRUE) >= 1){
      titles_org <- as.character(x[,i])
    }
  
    if (sum(match(c("EngTitle"),colnames(data)[i]), na.rm = TRUE) >= 1){
      titles_eng <- as.character(x[,i])
    }
  }
    
    if (is.data.frame(triads)) {triads <- triads[,-1]}
    if (is.data.frame(dyads)) {dyads <- dyads[,-1]}
    if (is.data.frame(stones)) {stones <- stones[,-1]}
    if (is.data.frame(questions)) {questions <- questions[,-1]}
    if (is.data.frame(descriptors)) {descriptors <- descriptors[,-1]}
    if (is.data.frame(respondent)) {respondent <- respondent[,-1]}
    if (is.data.frame(collector)) {collector <- collector[,-1]}
    if (!exists("texts_org")) {texts_org <- NA}
    if (!exists("texts_eng")) {texts_eng <- NA}
    if (!exists("titles_org")) {titles_org <- NA}
    if (!exists("titles_eng")) {titles_eng <- NA}
  
  x <- list(triads,dyads,stones,questions,descriptors,respondent,collector,texts_org,texts_eng,titles_org,titles_eng)
  names(x) <- c("triads","dyads","stones","questions","descriptors","respondent","collector","texts_org","texts_eng","titles_org","titles_eng")
  x <- x[as.logical(is.na(x)) == FALSE]
  
  return(x)
}