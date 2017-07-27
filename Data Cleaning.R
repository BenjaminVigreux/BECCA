##  DATA CLEANING

##  Load Data

# NB: If running this code on it's own, remove the # and set "dataset"" to desired country.
# Otherwise, if running from another script make sure that script sets "dataset" to desired
# country itself (Must be in global env on way or another).

# Choose dataset

#dataset <- "tajikistan"
#switch_c <- 1
#switch_t <- 1

#load("dataset.txt")
#load("switches.RData")

wd <- getwd()
if (dataset == "kyrgyzstan") {data <- read.csv("original_datasets/foikyrg2015_Standard.csv", sep = ",", encoding = "UTF-8")}
if (dataset == "unicef") {data <- read.csv("original_datasets/foiunicef2015_Standard.csv", sep = ",", encoding = "UTF-8")
data <- data[seq(from = 2, to = 2648, by = 2),]}
if (dataset == "yemen") {data <- read.csv("original_datasets/foiyemen2015_Standard.csv", sep = ",", encoding = "UTF-8")}
if (dataset == "serbia") {data <- read.csv("original_datasets/Serbia roma_Standard.csv", sep = ",", encoding = "UTF-8")}
if (dataset == "tajikistan") {data <- read.csv("original_datasets/tajikistan2015_Standard.csv", sep = ",", encoding = "UTF-8")}
if (dataset == "moldova") {data <- read.csv("original_datasets/foimoldova2015_Standard.csv", sep = ",", encoding = "UTF-8")}

search <- search()
if (!is.na(match("clean", search))) {detach(clean)}
if (!is.na(match("data", search))) {detach(data)}
search <- search()
if (is.na(match("data", search))) {attach(data)}

##  Convert Positivity to a score (1-5, 5 = strongly positive).
  # Should work for Serbia, but levels are different and haven't tested it using 'pmatch'.

feel <- c("negative","neutral","positive","strongly negative","strongly positive")
Qnum <- as.numeric(substr(grep(paste0("^Q[0-9]."),names(data), value = TRUE)[length(grep(paste0("^Q[0-9]."),names(data), value = TRUE))],2,2)) + 1

for (c in 1:dim(data)[2]){
  if (sum(as.numeric(as.logical(pmatch(feel,data[,c]))), na.rm = TRUE) > 2){
    x <- rep(NA, dim(data)[1])
    for (r in 1:dim(data)[1]){
      if (data[r,c] != "") {if (data[r,c] == "strongly negative") {x[r] <- -2}}
      if (data[r,c] != "") {if (data[r,c] == "negative") {x[r] <- -1}}
      if (data[r,c] != "") {if (data[r,c] == "neutral") {x[r] <- 0}}
      if (data[r,c] != "") {if (data[r,c] == "positive") {x[r] <- 1}}
      if (data[r,c] != "") {if (data[r,c] == "strongly positive") {x[r] <- 2}}
    }
  }
  #if (sum(x,na.rm = TRUE) > 0) {stop("Not an error, but if prevents knitting then remove this stop func, non-essential. Data Cleaning.R, Positivity>>Score")}
}

if (exists("x")) {assign(paste0("Q",Qnum,".Score"),x)
  data <- data.frame(get(paste0("Q",Qnum,".Score")),data)
  colnames(data)[1] <- paste0("Q",Qnum,".Score")}

##  Variable Selection

# This code should be able to identify the different signifers in each dataset we have, and
# hopefully in future ones assuming that variables are also prefixed with a letter signifying
# their type ("T" for Triad). The columns containing the narratives and titles have different
# names varying from dataset to dataset, with no reliable pattern for identification. If a new
# name used in future datasets, the code will not pick it up, so this aspect will rely on user
# training.
# NB: If there are two variables containing the texts (i.e. native lang & English), at present
# this code will use the last one in the loop.

# UPDATE 13/07

# All datasets should now work.
# Collector and Respondent variables from Tajikistan now included.
# Original and translated texts and titles now included:
  # FYI NEW VARIABLE NAMES: 'texts_eng','texts_org','titles_eng','titles_org'
# Allocates output files to subdirectories within working directory. Creates new folders if non-existent.

# REMAINING ISSUES 13/07

# Serbia dataset has three variables ("Student code","Image Prompter","Image Prompter"[ENG])
# that I can't identify because we have no FoI for Serbia.
# Need to make Positivity --> Score work for other datasets.

source("sort_signifiers.R", print.eval = TRUE)

## ISSUE (found in Tajikistan):
#Warning message:
#In if (translate(yandexAPI, y[1], lang = "en") != "Maximum daily translated text volume exceeded") { :
#    the condition has length > 1 and only the first element will be used

sigs <- sigtypes(data)

for (i in 1:length(sigs)){
  assign(names(sigs)[i],sigs[[i]])
  if (!exists("sig_frame")) {sig_frame <- sigs[[i]]} else {sig_frame <- data.frame(sig_frame,sigs[[i]])}
  if (names(sigs[i]) == "texts_org") {colnames(sig_frame)[dim(sig_frame)[2]] <- "texts_org"}
  if (names(sigs[i]) == "titles_org") {colnames(sig_frame)[dim(sig_frame)[2]] <- "titles_org"}
}

clean <- data.frame(FragmentID,NarrID,EntryDate,ServerEntryDate,sig_frame,Latitude,Longitude,Language)

##  Namegetter

if(exists("qnames")) {rm(qnames)}
if(exists("dqnames")) {rm(dqnames)}
for (c in 1:dim(clean)[2]){
  q <- names(clean)[grep(paste0("^Q",c,"."),names(clean))[1]]
  dq <- names(clean)[grep(paste0("^DQ",c,"."),names(clean))[1]]
  if(!exists("qnames")) 
  {if(!is.na(q)) {qnames <- q}} else
  {if(!is.na(q)) {qnames <- c(qnames,q)}}
  if(!exists("dqnames"))
  {if(!is.na(dq)) {dqnames <- dq}} else
  {if(!is.na(dq)) {dqnames <- c(dqnames,dq)}}
}

  ##  Trim levels off names

for(i in 1:length(qnames)){
  for (n in 1:nchar(qnames[i])){
    if(substr(qnames[i],n,n) == "_") {qnames[i] <- substr(qnames[i],1,(n-1))}
  }
}
for(i in 1:length(dqnames)){
  for (n in 1:nchar(dqnames[i])){
    if(substr(dqnames[i],n,n) == "_") {dqnames[i] <- substr(dqnames[i],1,(n-1))}
  }
}

##  Subset the descriptors

source("factor_to_integer.R")

subset_frame <- factor2int(clean, integrate = FALSE, ignore = 20)

for (i in 1:dim(subset_frame)[2]){
  assign(names(subset_frame)[i],subset(subset_frame,names(subset_frame) == names(subset_frame)[i]))
}

##  Translation

require(RYandexTranslate)
load("yandexAPI.txt")

wd <- getwd()

  ##  Narrative Translator

if (switch_t == 1){
  texts_eng <- rep(NA, length(texts_org))
  for (i in 1:length(texts_org)){tryCatch({
  x <- translate(yandexAPI,text=texts_org[i],lang="en")
  texts_eng[i] <- x$text},error=function(e){cat("Error translating story",i,"\n")})
  if (i%%10 == 0){print(paste0("Translated ",i," stories"))}
}

  ##  Title Translator
  
if (exists("titles_org")){
titles_eng <- rep(NA, length(titles_org))
for (i in 1:length(titles_org)){tryCatch({
  x <- translate(yandexAPI,text=titles_org[i],lang="en")
  titles_eng[i] <- x$text},error=function(e){cat("Error translating title",i,"\n")})
  if (i%%10 == 0){print(paste0("Translated ",i," titles"))}
}} else {print("No file 'titles_org', skipped!") }

##  Variable "Other" Translator (Not currently working)

#for (i in 1:dim(clean)[2]){
#  if ((grep("[Oo]ther",colnames(clean)[i])) && (is.factor(clean[,i]))){
#    x <- clean[,i]
#      for (j in 1:length(x)){
#        if (x[j] != ""){clean[j,i] <- translate(yandexAPI,text=x[j],lang="en")}
#      }
#  }
#}

if (!dir.exists(paste0(wd,"/clean_data/texts"))) {dir.create(paste0(wd,"/clean_data/texts"), recursive = TRUE)}
save(list = c("texts_org","titles_org","texts_eng","titles_eng"), file = paste0(wd,"/clean_data/texts/",dataset,"_translated_texts.RData"))
if ((!exists("titles_org")) && (!exists("titles_eng"))) {save(list = c("texts_org","texts_eng"), file = paste0(wd,"/clean_data/texts/",dataset,"_translated_texts.RData"))}
} else {
  if (file.exists(paste0(wd,"/clean_data/texts/",dataset,"_translated_texts.RData"))){
    load(paste0(wd,"/clean_data/texts/",dataset,"_translated_texts.RData"))}
}

##  Corpus Builder

require(quanteda)
if (switch_c == 1){
  corpus <- data.frame(texts_eng,clean)
  corpus[,1] <- as.character(corpus$texts_eng)
  corpus <- corpus(corpus, textField = "texts_eng") #, docnames = FragmentID
  #names(docvars(corpus))
  dfm <- dfm(corpus, ignoredFeatures = stopwords("english"))
  if (!dir.exists(paste0(wd,"/clean_data/corpus"))) {dir.create(paste0(wd,"/clean_data/corpus"), recursive = TRUE)}
  save(list = c("corpus","dfm"),file = paste0(wd,"/clean_data/corpus/",dataset,"_corpus.RData"))
} else {
  if (file.exists(paste0(wd,"/clean_data/",dataset,"_corpus.RData"))){
    load(paste0(wd,"/clean_data/",dataset,"_corpus.RData"))}
}

##  Translated texts and titles into 'clean'.

if (exists("texts_eng")) {clean <- data.frame(clean,texts_eng)}
if (exists("titles_eng")) {clean <- data.frame(clean,titles_eng)}

##  Output

detach(data)

if (!dir.exists(paste0(wd,"/clean_data"))) {dir.create(paste0(wd,"/clean_data"), recursive = TRUE)}
if (!dir.exists(paste0(wd,"/clean_data/subsets"))) {dir.create(paste0(wd,"/clean_data/subsets"), recursive = TRUE)}

save(list = c(grep("^DQ[0-9]",ls(), value = TRUE),
              grep("^Q[0-9]",ls(), value = TRUE),
              grep("^C[0-9]",ls(), value = TRUE),
              grep("^R[0-9]",ls(), value = TRUE)), file = paste0(wd,"/clean_data/subsets/",dataset,"_subsets.RData"))
save(list = c("clean","corpus","dfm"),file = paste0(wd,"/clean_data/",dataset,"_clean.RData"))
save("dataset",file = "dataset.txt")
rm(list = ls())
wd <- getwd()
load("dataset.txt")
load(paste0(wd,"/clean_data/",dataset,"_clean.RData"))
load(paste0(wd,"/clean_data/texts/",dataset,"_translated_texts.RData"))

##  Unpack Signifiers

sigs <- sigtypes(clean)
for (i in 1:length(sigs)){
  assign(names(sigs)[i],sigs[[i]])
}
rm(sigs)
