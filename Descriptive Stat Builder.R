##  Descriptive Stat builder

#dataset <- "moldova"
load("dataset.txt")

if (!exists("clean",where = .GlobalEnv)) 
{source("Data Cleaning.R", print.eval = TRUE)}

search <- search()
if (!is.na(match("clean", search))) {detach(clean)}
if (!is.na(match("data", search))) {detach(data)}
search <- search()
if (is.na(match("clean", search))) {attach(clean)}

storyvar <- NA # matrix containing number of stories per level for each descriptor.
qnames <- NA # Vector of column names
for (i in 1:dim(clean)[2]){
  if (length(grep("^Q[0-9]",colnames(clean)[i])) == 1) {
    if (is.factor(clean[,i])){
      if (length(grep("[Oo]ther",colnames(clean)[i])) == 0) {
    f <- data.frame(matrix(NA, nrow = dim(clean)[1], ncol = length(levels(clean[,i]))))
    # "f" is a matrix of the absolute no. of stories per level for factor variable i. 
    for (k in 1:dim(f)[2]){
      colnames(f)[k] <- paste0(names(clean)[i],".",levels(clean[,i])[k])
      if (colnames(f)[k] == "") {colnames(f)[k] <- paste0(colnames(clean)[i],".NA")}
      for (l in 1:dim(f)[1]){
        if (levels(clean[,i])[k] == clean[l,i]) {f[l,k] <- as.integer(1)}
      }
    }
    storyvar <- data.frame(storyvar,f)
    qnames <- c(qnames,paste0(colnames(f)))
  }} else {storyvar <- data.frame(storyvar,as.numeric(clean[,i]))
    qnames <- c(qnames,colnames(clean)[i])}
  }
}
qnames <- qnames[-1]
storyvar <- storyvar[,-1]
colnames(storyvar) <- qnames
#desvar <- data.frame(data$DQ1.Age,data$DQ2.Gender,data$DQ5.Live,data$DQ6.Where.do.you.live.,data$DQ7.Language) #Matrix of desired descriptive variables

##  Subset the descriptors

# Left the binomial integers as they were because respondents can answer with more than one, but I thought it might be useful to have subsets based on the other descriptors.

#Stats <- matrix(NA, nrow = dim(desvar)[2], ncol = dim(storyvar)[2])
#Stats <- data.frame(matrix(NA, nrow = 40, ncol = dim(storyvar)[2]))

##  Age

dm <- data.frame(matrix(NA, nrow=length(levels(clean$DQ1.Age)), ncol = dim(storyvar)[2]))
for (i in 1:length(levels(clean$DQ1.Age))){
  x <- assign(paste0("DQ1.",levels(clean$DQ1.Age)[i]),subset(storyvar, DQ1.Age == levels(clean$DQ1.Age)[i]))
  for (j in 1:dim(x)[2]){
    if (class(x[,j]) == "integer") {dm[i,j] <- sum(x[,j],na.rm = TRUE)}
    if (class(x[,j]) == "numeric") {dm[i,j] <- sum(x[,j],na.rm = TRUE)}
  }
  if (levels(clean$DQ1.Age)[i] == "") 
    {rownames(dm)[i] <- "DQ1.unspecified"} else 
    {rownames(dm)[i] <- paste0("DQ1.",levels(clean$DQ1.Age)[i])}
}
colnames(dm) <- qnames
Stats <- dm

Statsp <- NA
pall <- NA
ptot <- rep(0,dim(dm)[1])
for (l in 1:dim(dm)[2]){
  if (length(grep(paste0("Q",l,"."),names(dm))) >= 1)
  {p <- as.matrix(dm[,grep(paste0("Q",l,"."),names(dm))]) # "p" is a matrix of the proportion of stories per level for factor variable i.
  for (r in 1:dim(p)[1]){
    pval <- rep(0, dim(p)[2])
    p[r,] <- as.numeric(p[r,])
    ptot[r] <- sum(p[r,], na.rm = TRUE)
    for (c in 1:dim(p)[2]){
      if (!is.na(p[r,c])) {pval[c] <- ((p[r,c]/ptot[r]) * 100)}
    }
    p[r,] <- pval
  }
  pall <- cbind(pall,p)}
}
Statsp <- pall[,-1]

# Argument of length 0 caused by "Q7.Score". Single variable so if function assigns p as vector not matrix. (Fixed but no column name?)

##  Gender

dm <- data.frame(matrix(NA, nrow=length(levels(clean$DQ2.Gender)), ncol = dim(storyvar)[2]))
for (i in 1:length(levels(clean$DQ2.Gender))){
  x <- assign(paste0("DQ2.",levels(clean$DQ2.Gender)[i]),subset(storyvar, DQ2.Gender == levels(clean$DQ2.Gender)[i]))
  for (j in 1:dim(x)[2]){
    if (class(x[,j]) == "integer") {dm[i,j] <- sum(x[,j],na.rm = TRUE)}
    if (class(x[,j]) == "numeric") {dm[i,j] <- mean(x[,j],na.rm = TRUE)}
  }
  if (levels(clean$DQ2.Gender)[i] == "") 
    {rownames(dm)[i] <- "DQ2.unspecified"} else 
    {rownames(dm)[i] <- paste0("DQ2.",levels(clean$DQ2.Gender)[i])}
}
colnames(dm) <- qnames
Stats <- rbind(Stats,dm)

pall <- NA
ptot <- rep(0,dim(dm)[1])
for (l in 1:dim(dm)[2]){
  if (length(grep(paste0("Q",l,"."),names(dm))) >= 1)
  {p <- as.matrix(dm[,grep(paste0("Q",l,"."),names(dm))]) # "p" is a matrix of the proportion of stories per level for factor variable i.
  for (r in 1:dim(p)[1]){
    pval <- rep(0, dim(p)[2])
    p[r,] <- as.numeric(p[r,])
    ptot[r] <- sum(p[r,], na.rm = TRUE)
    for (c in 1:dim(p)[2]){
      if (!is.na(p[r,c])) {pval[c] <- ((p[r,c]/ptot[r]) * 100)}
    }
    p[r,] <- pval
  }
  pall <- cbind(pall,p)}
}
pall <- pall[,-1]
Statsp <- rbind(Statsp,pall)

##  Education

dm <- data.frame(matrix(NA, nrow=length(levels(clean$DQ3.Education)), ncol = dim(storyvar)[2]))
for (i in 1:length(levels(clean$DQ3.Education))){
  x <- assign(paste0("DQ3.",levels(clean$DQ3.Education)[i]),subset(storyvar, DQ3.Education == levels(clean$DQ3.Education)[i]))
  for (j in 1:dim(x)[2]){
    if (class(x[,j]) == "integer") {dm[i,j] <- sum(x[,j],na.rm = TRUE)}
    if (class(x[,j]) == "numeric") {dm[i,j] <- mean(x[,j],na.rm = TRUE)}
  }
  if (levels(clean$DQ3.Education)[i] == "") 
    {rownames(dm)[i] <- "DQ3.unspecified"} else 
    {rownames(dm)[i] <- paste0("DQ3.",levels(clean$DQ3.Education)[i])}
}
colnames(dm) <- qnames
Stats <- rbind(Stats,dm)

pall <- NA
ptot <- rep(0,dim(dm)[1])
for (l in 1:dim(dm)[2]){
  if (length(grep(paste0("Q",l,"."),names(dm))) >= 1)
  {p <- as.matrix(dm[,grep(paste0("Q",l,"."),names(dm))]) # "p" is a matrix of the proportion of stories per level for factor variable i.
  for (r in 1:dim(p)[1]){
    pval <- rep(0, dim(p)[2])
    p[r,] <- as.numeric(p[r,])
    ptot[r] <- sum(p[r,], na.rm = TRUE)
    for (c in 1:dim(p)[2]){
      if (!is.na(p[r,c])) {pval[c] <- ((p[r,c]/ptot[r]) * 100)}
    }
    p[r,] <- pval
  }
  pall <- cbind(pall,p)}
}
pall <- pall[,-1]
Statsp <- rbind(Statsp,pall)

if(dataset == "kyrgyzstan"){

##  Urban/Rural

dm <- data.frame(matrix(NA, nrow=length(levels(clean$DQ5.Live)), ncol = dim(storyvar)[2]))
for (i in 1:length(levels(clean$DQ5.Live))){
  x <- assign(paste0("DQ5.",levels(clean$DQ5.Live)[i]),subset(storyvar, DQ5.Live == levels(clean$DQ5.Live)[i]))
  for (j in 1:dim(x)[2]){
    if (class(x[,j]) == "integer") {dm[i,j] <- sum(x[,j],na.rm = TRUE)}
    if (class(x[,j]) == "numeric") {dm[i,j] <- mean(x[,j],na.rm = TRUE)}
  }
if (levels(clean$DQ5.Live)[i] == "") 
    {rownames(dm)[i] <- "DQ5.unspecified"} else 
    {rownames(dm)[i] <- paste0("DQ5.",levels(clean$DQ5.Live)[i])}
}
colnames(dm) <- qnames
Stats <- rbind(Stats,dm)

pall <- NA
ptot <- rep(0,dim(dm)[1])
for (l in 1:dim(dm)[2]){
  if (length(grep(paste0("Q",l,"."),names(dm))) >= 1)
  {p <- as.matrix(dm[,grep(paste0("Q",l,"."),names(dm))]) # "p" is a matrix of the proportion of stories per level for factor variable i.
  for (r in 1:dim(p)[1]){
    pval <- rep(0, dim(p)[2])
    p[r,] <- as.numeric(p[r,])
    ptot[r] <- sum(p[r,], na.rm = TRUE)
    for (c in 1:dim(p)[2]){
      if (!is.na(p[r,c])) {pval[c] <- ((p[r,c]/ptot[r]) * 100)}
    }
    p[r,] <- pval
  }
  pall <- cbind(pall,p)}
}
pall <- pall[,-1]
Statsp <- rbind(Statsp,pall)

##  Region (Urban)

dm <- data.frame(matrix(NA, nrow=length(levels(clean$DQ6.Where.do.you.live.)), ncol = dim(storyvar)[2]))
for (i in 1:length(levels(clean$DQ6.Where.do.you.live.))){
  x <- assign(paste0("DQ6.urban.",levels(clean$DQ6.Where.do.you.live.)[i]),subset(`DQ5.urban area`, DQ6.Where.do.you.live. == levels(clean$DQ6.Where.do.you.live.)[i]))
  for (j in 1:dim(x)[2]){
    if (class(x[,j]) == "integer") {dm[i,j] <- sum(x[,j],na.rm = TRUE)}
    if (class(x[,j]) == "numeric") {dm[i,j] <- mean(x[,j],na.rm = TRUE)}
  }
if (levels(clean$DQ6.Where.do.you.live.)[i] == "") 
    {rownames(dm)[i] <- "DQ6.urban.unspecified"} else 
    {rownames(dm)[i] <- paste0("DQ6.urban.",levels(clean$DQ6.Where.do.you.live.)[i])}
}
colnames(dm) <- qnames
Stats <- rbind(Stats,dm)

pall <- NA
ptot <- rep(0,dim(dm)[1])
for (l in 1:dim(dm)[2]){
  if (length(grep(paste0("Q",l,"."),names(dm))) >= 1)
  {p <- as.matrix(dm[,grep(paste0("Q",l,"."),names(dm))]) # "p" is a matrix of the proportion of stories per level for factor variable i.
  for (r in 1:dim(p)[1]){
    pval <- rep(0, dim(p)[2])
    p[r,] <- as.numeric(p[r,])
    ptot[r] <- sum(p[r,], na.rm = TRUE)
    for (c in 1:dim(p)[2]){
      if (!is.na(p[r,c])) {pval[c] <- ((p[r,c]/ptot[r]) * 100)}
    }
    p[r,] <- pval
  }
  pall <- cbind(pall,p)}
}
pall <- pall[,-1]
Statsp <- rbind(Statsp,pall)

##  Region (Rural)

dm <- data.frame(matrix(NA, nrow=length(levels(clean$DQ6.Where.do.you.live.)), ncol = dim(storyvar)[2]))
for (i in 1:length(levels(clean$DQ6.Where.do.you.live.))){
  x <- assign(paste0("DQ6.rural.",levels(clean$DQ6.Where.do.you.live.)[i]),subset(`DQ5.rural area`, DQ6.Where.do.you.live. == levels(clean$DQ6.Where.do.you.live.)[i]))
  for (j in 1:dim(x)[2]){
    if (class(x[,j]) == "integer") {dm[i,j] <- sum(x[,j],na.rm = TRUE)}
    if (class(x[,j]) == "numeric") {dm[i,j] <- mean(x[,j],na.rm = TRUE)}
  }
if (levels(clean$DQ6.Where.do.you.live.)[i] == "") 
    {rownames(dm)[i] <- "DQ6.rural.unspecified"} else 
    {rownames(dm)[i] <- paste0("DQ6.rural.",levels(clean$DQ6.Where.do.you.live.)[i])}
}
colnames(dm) <- qnames
Stats <- rbind(Stats,dm)

pall <- NA
ptot <- rep(0,dim(dm)[1])
for (l in 1:dim(dm)[2]){
  if (length(grep(paste0("Q",l,"."),names(dm))) >= 1)
  {p <- as.matrix(dm[,grep(paste0("Q",l,"."),names(dm))]) # "p" is a matrix of the proportion of stories per level for factor variable i.
  for (r in 1:dim(p)[1]){
    pval <- rep(0, dim(p)[2])
    p[r,] <- as.numeric(p[r,])
    ptot[r] <- sum(p[r,], na.rm = TRUE)
    for (c in 1:dim(p)[2]){
      if (!is.na(p[r,c])) {pval[c] <- ((p[r,c]/ptot[r]) * 100)}
    }
    p[r,] <- pval
  }
  pall <- cbind(pall,p)}
}
pall <- pall[,-1]
Statsp <- rbind(Statsp,pall)

##  Language/Ethnicity

dm <- data.frame(matrix(NA, nrow=length(levels(clean$DQ7.Language)), ncol = dim(storyvar)[2]))
for (i in 1:length(levels(clean$DQ7.Language))){
  x <- assign(paste0("DQ7.",levels(clean$DQ7.Language)[i]),subset(storyvar, DQ7.Language == levels(clean$DQ7.Language)[i]))
  for (j in 1:dim(x)[2]){
    if (class(x[,j]) == "integer") {dm[i,j] <- sum(x[,j],na.rm = TRUE)}
    if (class(x[,j]) == "numeric") {dm[i,j] <- mean(x[,j],na.rm = TRUE)}
  }
if (levels(clean$DQ7.Language)[i] == "") 
    {rownames(dm)[i] <- "DQ6.rural.unspecified"} else 
    {rownames(dm)[i] <- paste0("DQ7.",levels(clean$DQ7.Language)[i])}
}
colnames(dm) <- qnames
Stats <- rbind(Stats,dm)

pall <- NA
ptot <- rep(0,dim(dm)[1])
for (l in 1:dim(dm)[2]){
  if (length(grep(paste0("Q",l,"."),names(dm))) >= 1)
  {p <- as.matrix(dm[,grep(paste0("Q",l,"."),names(dm))]) # "p" is a matrix of the proportion of stories per level for factor variable i.
  for (r in 1:dim(p)[1]){
    pval <- rep(0, dim(p)[2])
    p[r,] <- as.numeric(p[r,])
    ptot[r] <- sum(p[r,], na.rm = TRUE)
    for (c in 1:dim(p)[2]){
      if (!is.na(p[r,c])) {pval[c] <- ((p[r,c]/ptot[r]) * 100)}
    }
    p[r,] <- pval
  }
  pall <- cbind(pall,p)}
}
pall <- pall[,-1]
Statsp <- rbind(Statsp,pall)
}

if(dataset == "moldova"){

##  Centre/Transinistria

dm <- data.frame(matrix(NA, nrow=length(levels(clean$DQ5.Live)), ncol = dim(storyvar)[2]))
for (i in 1:length(levels(clean$DQ5.Live))){
  x <- assign(paste0("DQ5.",levels(clean$DQ5.Live)[i]),subset(storyvar, DQ5.Live == levels(clean$DQ5.Live)[i]))
  for (j in 1:dim(x)[2]){
    if (class(x[,j]) == "integer") {dm[i,j] <- sum(x[,j],na.rm = TRUE)}
    if (class(x[,j]) == "numeric") {dm[i,j] <- mean(x[,j],na.rm = TRUE)}
  }
  if (levels(clean$DQ5.Live)[i] == "") 
  {rownames(dm)[i] <- "DQ5.unspecified"} else 
  {rownames(dm)[i] <- paste0("DQ5.",levels(clean$DQ5.Live)[i])}
}
colnames(dm) <- qnames
Stats <- rbind(Stats,dm)

pall <- NA
ptot <- rep(0,dim(dm)[1])
for (l in 1:dim(dm)[2]){
  if (length(grep(paste0("Q",l,"."),names(dm))) >= 1)
  {p <- as.matrix(dm[,grep(paste0("Q",l,"."),names(dm))]) # "p" is a matrix of the proportion of stories per level for factor variable i.
  for (r in 1:dim(p)[1]){
    pval <- rep(0, dim(p)[2])
    p[r,] <- as.numeric(p[r,])
    ptot[r] <- sum(p[r,], na.rm = TRUE)
    for (c in 1:dim(p)[2]){
      if (!is.na(p[r,c])) {pval[c] <- ((p[r,c]/ptot[r]) * 100)}
    }
    p[r,] <- pval
  }
  pall <- cbind(pall,p)}
}
pall <- pall[,-1]
Statsp <- rbind(Statsp,pall)

##  Region

dm <- data.frame(matrix(NA, nrow=length(levels(clean$DQ8.LiveInRegion)), ncol = dim(storyvar)[2]))
for (i in 1:length(levels(clean$DQ8.LiveInRegion))){
  x <- assign(paste0("DQ8.",levels(clean$DQ8.LiveInRegion)[i]),subset(storyvar, DQ8.LiveInRegion == levels(clean$DQ8.LiveInRegion)[i]))
  for (j in 1:dim(x)[2]){
    if (class(x[,j]) == "integer") {dm[i,j] <- sum(x[,j],na.rm = TRUE)}
    if (class(x[,j]) == "numeric") {dm[i,j] <- mean(x[,j],na.rm = TRUE)}
  }
  if (levels(clean$DQ8.LiveInRegion)[i] == "") 
  {rownames(dm)[i] <- "DQ8.unspecified"} else 
  {rownames(dm)[i] <- paste0("DQ8.",levels(clean$DQ8.LiveInRegion)[i])}
}
colnames(dm) <- qnames
Stats <- rbind(Stats,dm)

pall <- NA
ptot <- rep(0,dim(dm)[1])
for (l in 1:dim(dm)[2]){
  if (length(grep(paste0("Q",l,"."),names(dm))) >= 1)
  {p <- as.matrix(dm[,grep(paste0("Q",l,"."),names(dm))]) # "p" is a matrix of the proportion of stories per level for factor variable i.
  for (r in 1:dim(p)[1]){
    pval <- rep(0, dim(p)[2])
    p[r,] <- as.numeric(p[r,])
    ptot[r] <- sum(p[r,], na.rm = TRUE)
    for (c in 1:dim(p)[2]){
      if (!is.na(p[r,c])) {pval[c] <- ((p[r,c]/ptot[r]) * 100)}
    }
    p[r,] <- pval
  }
  pall <- cbind(pall,p)}
}
pall <- pall[,-1]
Statsp <- rbind(Statsp,pall)
}

Stats <- data.frame(Stats)
Statsp <- data.frame(Statsp)
colnames(Stats) <- names(storyvar)

Statsp <- format(round(Statsp,2),nsmall = 2)

##  Output

detach(clean)
save(list = c("Stats","Statsp"),file = paste0(dataset,"_stats.RData"))
save("dataset",file = "dataset.txt")
rm(list = ls())
load("dataset.txt")
load(paste0(dataset,"_stats.RData"))
load(paste0(dataset,"_clean.RData"))
