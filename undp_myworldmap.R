myworldmap <- function(data,Country) {
  require(plotly)
  mycountries <- character(Ncountry)
  mycodes <- character(Ncountry)
  avg_Feeling <- numeric(Ncountry)
  pos_Feeling <- numeric(Ncountry)
  neg_Feeling <- numeric(Ncountry)
  neu_Feeling <- numeric(Ncountry)
  N_Feeling <- numeric(Ncountry)
  minlat=100
  maxlat=-100
  minlon=200
  maxlon=-200
  for (ncountry in 1:Ncountry){
    mycountries[ncountry] <- Country[[ncountry]]$country
    mycodes[ncountry] <- Country[[ncountry]]$countrycode
    avg_Feeling[ncountry] <- mean(data[[ncountry]]$Feeling_num,na.rm=TRUE)
    pos_Feeling[ncountry] <- sum(data[[ncountry]]$Feeling_num>0,na.rm=TRUE)
    neg_Feeling[ncountry] <- sum(data[[ncountry]]$Feeling_num<0,na.rm=TRUE)
    neu_Feeling[ncountry] <- sum(data[[ncountry]]$Feeling_num==0,na.rm=TRUE)
    N_Feeling[ncountry] <- length(data[[ncountry]]$Feeling_num)
    minlat <- min(minlat,Country[[ncountry]]$latmin)
    minlon <- min(minlon,Country[[ncountry]]$lonmin)
    maxlat <- max(maxlat,Country[[ncountry]]$latmax)
    maxlon <- max(maxlon,Country[[ncountry]]$lonmax)
  }
  
  df <- data.frame(country=mycountries,
                   code=mycodes,
                   Feeling=avg_Feeling,
                   positive=pos_Feeling,
                   negative=neg_Feeling,
                   neutral=neu_Feeling,
                   N=N_Feeling)
  
  df$hover <- with(df,paste(toupper(country)," ",
                            "<br>Average Feeling: ",format(Feeling,digits=2),
                            "<br>Negative: ",format(100*negative/N,digits=0),"%",
                            "<br>Neutral: ",format(100*neutral/N,digits=0),"%",
                            "<br>Positive: ",format(100*positive/N,digits=0),"%",
                            sep=""))
  # light grey boundaries
  l <- list(color = toRGB("grey"), width = 0.5)
  
  # specify map projection/options
  g <- list(
    showframe = FALSE,
    showcoastlines = TRUE,
    projection = list(type = 'Mercator'),
    showland = TRUE,
    landcolor = toRGB("grey83"),
    #subunitcolor = toRGB("white"),
    countrycolor = toRGB("white"),
    showlakes = TRUE,
    lakecolor = toRGB("white"),
    #showsubunits = TRUE,
    showcountries = TRUE,
    resolution = 50,
    countrywidth = 0.5,
    subunitwidth = 0.5,
    lonaxis=list(showgrid=TRUE,range=c(minlon,maxlon)),
    lataxis=list(showgrid=TRUE,range=c(minlat,maxlat)),
    showlakes=TRUE,
    lakecolor = toRGB("white")
    #  showcountry=TRUE
  )
  
  plot_ly(df,
          z = Feeling,
          locations = code,
          type = 'choropleth',
          color = Feeling,
          colors = c("red","yellow","blue","green"), # 'Blues',
          marker = list(line = l),
          colorbar = list(title = 'Feeling', 
                          #yanchor="bottom",xanchor="left",
                          # x=0, y=0,
                          len=1),
          hoverinfo = "text", #  "a",..., "text", "name" with a "+" OR "all" or "none".
          text=hover
  ) %>%
    layout(title = '',
           geo = g) %>%
    add_trace(type="scattergeo",
              locations = code, text = country, mode="text")
  
}