mytimeseries <- function(data,Country,ncountry,Cut_days) {
  
  # Duplicated because not saved from the previous chunk
  Feeling_values <- as.numeric(levels(as.factor(data[[ncountry]]$Feeling_num)))
  
  #
  # Time series of average Feeling
  #
  mindate <- min(data[[ncountry]]$timedate_num)
  maxdate <- max(data[[ncountry]]$timedate_num)
  Ncut <- ceiling((maxdate-mindate)/Cut_days/24/60/60)
  print(paste("#Bins for the time span:",Ncut,"(bin width in days:",Cut_days,")"))
  
  # Cut time interval into Ncut bins
  # timedate_num_cut includes the reference bin of each data
  # Increase the number of digits to 10 because default 5 digits only gives days
  timedate_num_cut <- cut(data[[ncountry]]$timedate_num,Ncut,dig.lab=10)
  # The levels contain the boundaries of the bins
  labs <- levels(timedate_num_cut)
  # Convert from character to numeric removing parentheses (from the official documentation of cut)
  # dum is a matrix with left and right boundaries as columns
  dum <- cbind(as.numeric( sub("\\((.+),.*", "\\1", labs) ),
               as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", labs) ))
  # Compute the bin centers
  timedate_num_cutcenter <- rowMeans(dum)
  #timedate_cutcenters <- as.POSIXct((timedate_num_cutcenter-25569)*86400, tz="GMT", origin="1970-01-01")
  timedate_cutcenters <- as.POSIXct(timedate_num_cutcenter, tz="GMT", origin="1970-01-01")
  
  timedate_cut <- timedate_num_cut
  for (nlev in 1:length(levels(timedate_cut))){
    dum <- as.character(timedate_cutcenters[nlev])
    # Discard time and take only the day
    dum <- substr(dum,1,10)
    # Convert data. Take account that year is with 4 digits (20...)
    dum <- as.Date(dum,"20%y-%m-%d")
    # Convert date to Month day (add Year?)
    levels(timedate_cut)[nlev] <- format(dum,"%b %d")
  }
  
  tab <- table(data[[ncountry]]$Feeling,timedate_num_cut)
  
  # Compute Weighted mean and standard deviation inside bins
  Feeling_mean <- rep(0,Ncut)
  Feeling_std <- rep(0,Ncut)
  for (ncut in 1:Ncut){
    Feeling_mean[ncut] <- weighted.mean(Feeling_values,tab[,ncut])
    Feeling_std[ncut] <- sqrt(sum(tab[,ncut] * (Feeling_values - Feeling_mean[ncut])^2)/(sum(tab[,ncut])*(sum(tab[,ncut])-1)))
  }
  
  mywidth <- (as.numeric(timedate_cutcenters[2])-as.numeric(timedate_cutcenters[1]))/5
  
  p <- qplot(timedate_cutcenters,Feeling_mean)
  p <- p + geom_line(color = "orange")
  p <- p + geom_errorbar(aes(x=timedate_cutcenters, ymin=Feeling_mean-Feeling_std, ymax=Feeling_mean+Feeling_std),
                         width = mywidth)
  p <- p + ggtitle(Country[[ncountry]]$country)
  p <- p + labs(x="Time",y="Feeling")
  print(p) # Mandatory if run with source, otherwise it does not print
  
  # Copy data[[ncountry]] into datadum because error in lm otherwise
  datadum <- data[[ncountry]]
  # Test by ANOVA whether the means in the bins are all equal
  fit <- lm(formula = datadum$Feeling_num ~ timedate_num_cut)
  res <- anova(fit)
  
  print(paste("p-value of Test equal means (ANOVA):",res$`Pr(>F)`[1]))
  
}