# Analyze triads

require(xlsx)
require(RYandexTranslate)
require(ggtern)
require(RColorBrewer) # for the Brewer color palette
# 

# Source initial parameters
source("undp_setup.R") # Initial parameters

# Source reading and translation of the data base
source("undp_read.R") # Read data bases and translate foreign words of new records

# Source preprocessing
source("undp_preprocess.R")

for (ncountry in (1:Ncountry)){
  
  N <- length(data[[ncountry]]$FragmentID)
  Ntriads <- length(Country[[ncountry]]$var_triads)
  feeling_Nlev <- length(levels(data[[ncountry]]$feeling))
  if (feeling_Nlev == 3) {
    clev = clev3
    Lev <- Lev3
  } else {
    clev <- clev5
    Lev <- Lev5
  }
  
  # triad_2_all contains the data of all triads in X-Y cartesian coordinates
  triad_2_all <- matrix(0,nrow=N,ncol=2*Ntriads)
  triad_3_all <- matrix(0,nrow=N,ncol=3*Ntriads)
  
  for (ntriads in 1:Ntriads) {
    whatT <- paste("T",ntriads,sep="")
    print(paste(Country[[ncountry]]$country,"Triad",whatT))
    ind <- grep(whatT,names(data[[ncountry]]))
    ind <- ind[1]
    triad_3 <- matrix(0,nrow=N,ncol=3)
    triad_2 <- matrix(0,nrow=N,ncol=2)
    # Coerce triad_2 and triad_3 (which are lists of factors) to numeric matrices
    for (i in 1:3) {
      dum <- data[[ncountry]][,(ind+i-1):(ind+i-1)]
      triad_3[,i] <- as.numeric((levels(dum))[dum])/100
    }
    for (i in 1:2) {
      dum <- data[[ncountry]][,(ind+i+2):(ind+i+2)]
      triad_2[,i] <- as.numeric(levels(dum))[dum]
    }
    # Fill the relevant columns of triad_2_all
    triad_2_all [,(2*ntriads-1):(2*ntriads)] <- triad_2
    triad_3_all [,(3*ntriads-2):(3*ntriads)] <- triad_3
    
    mycols <- brewer.pal(feeling_Nlev, "Spectral")
    mycols[(feeling_Nlev-1)/2+1] <- "#FFFF00" # Set neutral Yellow
    mapping <- mycols
    names(mapping) <- as.character(clev)
    
    p <- ggtern(data=data.frame(x=triad_3[,1],y=triad_3[,2],z=triad_3[,3]),
                aes(x=x,y=y,z=z)) + 
      geom_point(aes(color=factor(data[[ncountry]]$feeling_num)),
                 size=1,shape=16,alpha=0.5,na.rm=TRUE) +
      scale_color_manual(values=mapping,labels=Lev)+
      labs(title = paste(Country[[ncountry]]$country,"-",Country[[ncountry]]$var_triads[[ntriads]]$T),
           color ="Feeling",
           x     = Country[[ncountry]]$var_triads[[ntriads]]$TT[1],
           y     = Country[[ncountry]]$var_triads[[ntriads]]$TT[2],
           z     = Country[[ncountry]]$var_triads[[ntriads]]$TT[3]) +
      theme_gray(base_size = 9) +
      theme(legend.position      = c(0, 1),
            legend.justification = c(0, 1),
            legend.box.just      = 'left',
            tern.axis.title.T=element_text(hjust=0.5),
            tern.axis.title.R=element_text(hjust=1),
            tern.axis.title.L=element_text(hjust=0)
      )
    
    print(p)
  }
  
}

