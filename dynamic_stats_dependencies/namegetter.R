namegetter <- function(x){
  
  ##  Namegetter
  
  for (c in 1:dim(x)[2]){
    q <- names(x)[grep(paste0("^Q",c,"."),names(x))[1]]
    dq <- names(x)[grep(paste0("^DQ",c,"."),names(x))[1]]
    if(!exists("qnames")) 
    {if(length(!is.na(q)) == 1) {qnames <- q}} else
    {if(length(!is.na(q)) == 1) {qnames <- c(qnames,q)}}
    if(!exists("dqnames"))
    {if(length(!is.na(dq)) == 1) {dqnames <- dq}} else
    {if(length(!is.na(dq)) == 1) {dqnames <- c(dqnames,dq)}}
  }
  
  qnames <- qnames[!is.na(qnames)]
  dqnames <- dqnames[!is.na(dqnames)]
  
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
  
  y <- list(qnames,dqnames)
  return(y)
}