mytimeseries2 <- function(ncountry,Cut_days,checkNeg,checkPos,checkNeu) {
  
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
  timedate_cutcenters <- as.POSIXct(timedate_num_cutcenter, tz="GMT", origin="1970-01-01")
  #timedate_cutcenters <- as.POSIXct((timedate_num_cutcenter-25569)*86400, tz="GMT", origin="1970-01-01")
  
  timedate_cut <- timedate_num_cut
  for (nlev in 1:length(levels(timedate_cut))){
    dum <- as.character(timedate_cutcenters[nlev])
    # Discard time and take only the day
    dum <- substr(dum,1,10)
    # Convert data. Take account that year is with 4 digits (20...)
    dum <- as.Date(dum,"20%y-%m-%d")
    # Convert date to Month day (add Year?)
    levels(timedate_cut)[nlev] <- format(dum,format ="%Y-%b %d")
  }
  
  tab <- table(data[[ncountry]]$Feeling,timedate_num_cut)
  
  # Compute Sums and Percentage Positive/Neutral/Negative
  
  Feeling_sumsPos <- rep(0, Ncut)
  Feeling_sumsNeg <- rep(0, Ncut)
  Feeling_sumsNeu <- rep(0, Ncut)
  
  # sumsPos
  for (ncut in 1:Ncut) {
    Feeling_sumsPos[ncut] <- tab[1, ncut]
    Feeling_sumsNeu[ncut] <- tab[2, ncut]
    Feeling_sumsNeg[ncut] <- tab[3, ncut]
  }
  
  #sumsPos
  #sumsNeg
  #sumsNeu
  
  Feeling_sumsTotal = Feeling_sumsPos + Feeling_sumsNeg + Feeling_sumsNeu
  
  Feeling_percentPos = Feeling_sumsPos / Feeling_sumsTotal * 100
  Feeling_percentNeg = Feeling_sumsNeg / Feeling_sumsTotal * 100
  Feeling_percentNeu = Feeling_sumsNeu / Feeling_sumsTotal * 100
  
  ### Plot
  
  dat <- data.frame(timedate_cutcenters, Feeling_percentPos, Feeling_percentNeu, Feeling_percentNeg)
  p <- ggplot(dat[!is.na(dat$Feeling_percentPos),], aes(x = timedate_cutcenters, y = Feeling_percentPos)) + geom_blank() + ylim(0, 100) 
  p <- p + labs(x="Date",y="Percentage of Stories")
  p <- p + ggtitle(Country[[ncountry]]$country)
  
  if (checkNeg  == T) {
    p <- p + geom_line(data = dat[!is.na(dat$Feeling_percentPos),], mapping = aes(x = timedate_cutcenters, y = Feeling_percentPos), 
                       colour = "sienna2", size = 1.5)
                       # colour = "#D8B365", size = 1.5)
    p <- p + geom_point(data = dat[!is.na(dat$Feeling_percentPos),], mapping = aes(x = timedate_cutcenters, y = Feeling_percentPos))
  }
  
  if (checkPos == T) {
    p <- p + geom_line(data = dat[!is.na(dat$Feeling_percentPos),], mapping = aes(x = timedate_cutcenters, y = Feeling_percentNeg), 
                       colour = "aquamarine4", size = 1.5)
                       # colour = "grey90", size = 1.5)
    p <- p + geom_point(data = dat[!is.na(dat$Feeling_percentPos),], mapping = aes(x = timedate_cutcenters, y = Feeling_percentNeg))
  } 
  
  if (checkNeu == T) {
    p <- p + geom_line(data = dat[!is.na(dat$Feeling_percentPos),], mapping = aes(x = timedate_cutcenters, y = Feeling_percentNeu), 
                       colour = "lightgoldenrod", size = 1.5)
                       # colour = "#5AB4AC", size = 1.5)
    p <- p + geom_point(data = dat[!is.na(dat$Feeling_percentPos),], mapping = aes(x = timedate_cutcenters, y = Feeling_percentNeu))
  }
  print(p)
}