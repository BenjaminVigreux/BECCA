mylikert <- function(data,Country,ncountry,Cut_days) {
  
  # Duplicated because not saved from the previous chunk
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
    levels(timedate_cut)[nlev] <- format(dum,"%b %d")
  }
  # End duplication
  
  # Likert plots
  
  dflik <- data.frame(data[[ncountry]]$Feeling)
  # Change name of the variable because printed in the plot
  names(dflik) <- Country[[ncountry]]$country
  lik <- likert(dflik,grouping=timedate_cut)
  # Using include.histrogram = TRUE is bugging with ggtitle(title) and ordering of the histogram
  # Note that + ggtitle(Country[[ncountry]]$country) does not work when including histogram
  plot(lik,TYPE="bar",include.histogram = TRUE,panel.arrange = "v",
       # low.color = "#D8B365",
       low.color = "sienna2",
       # high.color = "#5AB4AC",
       high.color = "aquamarine4",
       # neutral.color = "grey90",
       neutral.color = "lightgoldenrod",
       group.order=rev(levels(timedate_cut)),
       label.completed = "Stories", label.missing = "NA",
       xlab="# Stories")
  
}
