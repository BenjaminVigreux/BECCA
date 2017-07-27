mytriad <- function(data,Country,ncountry,ntriads) {
  
  require(ggplot2) # Explicitly needed because overlap of annotate with package NLP
  require(mixtools) # for computing ellipsoids from clustering
  
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
  
  whatT <- paste("T",ntriads,sep="")
  #print(paste(Country[[ncountry]]$country,"Triad",whatT))
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
  mycols <- brewer.pal(feeling_Nlev, "Spectral")
  mycols[(feeling_Nlev-1)/2+1] <- "#FFFF00" # Set neutral Yellow
  mapping <- mycols
  names(mapping) <- as.character(clev)
  
  ind <- which(!is.na(rowSums(triad_2)))
  df <- data.frame(triad_2[ind,],factor(data[[ncountry]]$feeling_num[ind]))
  names(df)[1:2] <- c("x","y")
  names(df)[3] <- "Feeling"
  
  topy <- sqrt(3)/2
  
  datatriangle=data.frame(x=c(0,1,0.5),y=c(0,0,topy))
  
  N1 <- 10
  # delta1 <- 1/(N1)
  # delta2 <- 0.5*delta1
  # delta3 <- topy*delta1
  myseq1 <- seq(0,N1)/N1
  myseq2 <- myseq1*0.5
  myseq3 <- myseq1*topy
  zeroseq <- vector("numeric",N1+1)
  
  # dflines <- data.frame(x=rbind(myseq,0.5+myseq2),y=rbind(zeroseq,rev(myseq3)))
  linecol <- "white"
  linesize <- 0.15
  linetype <- "solid"
  triangle_bg <- "gray90"
  extralim=0.15
  
  p <- ggplot(data=df,aes(x=x,y=y))
  # The underlying triangle
  p <- p + geom_polygon(data=datatriangle,colour="white",fill=triangle_bg)
  # Remove all original axes information
  p <- p + theme(axis.line=element_blank(),
                 axis.text.x=element_blank(),
                 axis.text.y=element_blank(),
                 axis.ticks=element_blank(),
                 axis.title.x=element_blank(),
                 axis.title.y=element_blank(),
                 legend.position="none",
                 panel.background=element_blank(),
                 # panel.border=element_rect(colour = "black", fill=NA, size=1),
                 panel.border=element_blank(),
                 panel.grid.major=element_blank(),
                 panel.grid.minor=element_blank(),
                 plot.background=element_blank()
                 # plot.margin = unit(c(1, 1, 1, 1), "cm")
                 # plot.margin = margin(t=1,b=1,r=1,l=1,unit="cm")
  )
  # equal aspect ratio
  p <- p + coord_fixed()
  # widen limits so that labels can fit the areap
  p <- p + xlim(-extralim,1+extralim)
  p <- p + ylim(-extralim,topy+extralim)
  # The gridlines
  xx1 <- c(myseq1[2],0.5+myseq2[2],0.5+myseq2[4],myseq1[4],myseq1[6],0.5+myseq2[6],0.5+myseq2[8],
           myseq1[8],myseq1[10],0.5+myseq2[10])
  yy1 <- c(0,topy-myseq3[2],topy-myseq3[4],0,0,topy-myseq3[6],topy-myseq3[8],0,0,
           topy-myseq3[10])
  xx2 <- c(myseq2[2],1-myseq2[2],1-myseq2[4],myseq2[4],myseq2[6],1-myseq2[6],1-myseq2[8],
           myseq2[8],myseq2[10],1-myseq2[10])
  yy2 <- c(myseq3[2],myseq3[2],myseq3[4],myseq3[4],myseq3[6],myseq3[6],myseq3[8],myseq3[8],
           myseq3[10],myseq3[10])
  xx3 <- c(myseq1[10],myseq2[10],myseq2[8],myseq1[8],myseq1[6],myseq2[6],myseq2[4],myseq1[4],
           myseq1[2],myseq2[2])
  yy3 <- c(0,myseq3[10],myseq3[8],0,0,myseq3[6],myseq3[4],0,0,myseq3[2])
  p <- p + geom_path(data=data.frame(x=xx1,y=yy1),
                     color=linecol,size=linesize,linetype=linetype)
  p <- p + geom_path(data=data.frame(x=xx2,y=yy2),
                     color=linecol,size=linesize,linetype=linetype)
  p <- p + geom_path(data=data.frame(x=xx3,y=yy3),
                     color=linecol,size=linesize,linetype=linetype)
  xx1 <- c(myseq1[3],0.5+myseq2[3],0.5+myseq2[5],myseq1[5],myseq1[7],0.5+myseq2[7],0.5+myseq2[9],myseq1[9])
  yy1 <- c(0,topy-myseq3[3],topy-myseq3[5],0,0,topy-myseq3[7],topy-myseq3[9],0)
  xx2 <- c(myseq2[3],1-myseq2[3],1-myseq2[5],myseq2[5],myseq2[7],1-myseq2[7],1-myseq2[9],myseq2[9])
  yy2 <- c(myseq3[3],myseq3[3],myseq3[5],myseq3[5],myseq3[7],myseq3[7],myseq3[9],myseq3[9])
  xx3 <- c(myseq1[9],myseq2[9],myseq2[7],myseq1[7],myseq1[5],myseq2[5],myseq2[3],myseq1[3])
  yy3 <- c(0,myseq3[9],myseq3[7],0,0,myseq3[5],myseq3[3],0)
  p <- p + geom_path(data=data.frame(x=xx1,y=yy1),
                     color=linecol,size=4*linesize,linetype=linetype)
  p <- p + geom_path(data=data.frame(x=xx2,y=yy2),
                     color=linecol,size=4*linesize,linetype=linetype)
  p <- p + geom_path(data=data.frame(x=xx3,y=yy3),
                     color=linecol,size=4*linesize,linetype=linetype)
  # Axes ticks
  p <- p + geom_text(data=data.frame(x=myseq1[seq(3,N1-1,by=2)],y=zeroseq[seq(3,N1-1,by=2)]),
                     size=3,angle=60,aes(
                       #x=myseq,y=zeroseq,
                       label=as.character(myseq1[seq(3,N1-1,by=2)]*100)
                     ),
                     hjust = 1, vjust = 0.55)
  p <- p + geom_text(data=data.frame(x=myseq2[seq(3,N1-1,by=2)],y=myseq3[seq(3,N1-1,by=2)]),
                     size=3,angle=300,aes(
                       #x=myseq2,y=myseq3,
                       label=as.character(myseq1[seq(3,N1-1,by=2)]*100)),
                     hjust = 1, vjust = 0.55)
  p <- p + geom_text(data=data.frame(x=1-myseq2[seq(3,N1-1,by=2)],y=myseq3[seq(3,N1-1,by=2)]),
                     size=3,angle=0,aes(
                       #x=1-myseq2,y=myseq3,
                       label=as.character(myseq1[seq(3,N1-1,by=2)]*100)),
                     hjust = 0, vjust = 0.55)
  # Axes labels
  # Left
  p <- p + ggplot2::annotate("text",
                    x=0,y=0,label=Country[[ncountry]]$var_triads[[ntriads]]$TT[1], 
                    alpha=1, size=3, hjust=0.5, vjust=1.5 
  )
  # Top
  p <- p + ggplot2::annotate("text",
                    x=0.5,y=topy,label=Country[[ncountry]]$var_triads[[ntriads]]$TT[2],
                    alpha=1, size=3, hjust=0.5, vjust=-0.75
  )
  # Right
  p <- p + ggplot2::annotate("text",
                    x=1,y=0,label=Country[[ncountry]]$var_triads[[ntriads]]$TT[3],
                    alpha=1, size=3, hjust=0.5, vjust=1.5
  )
  # The points
  p <- p + geom_point(aes(color=Feeling), shape=16, size=2, alpha = 0.4)
  # Colormap of the points according to Feeling
  # Put legend and position it
  p <-  p + scale_color_manual(name = "Feeling",
                               labels = Lev,
                               values = mapping)
  p <- p + theme(legend.position = c(0.9,0.75)) 
  # Add title
  p <- p + ggtitle(paste(Country[[ncountry]]$var_triads[[ntriads]]$T))
  p <- p + theme(plot.title = element_text(vjust = 0.5, hjust = 0.5))
  
  # Cluster Analysis
  if (Nclust >0) {
    ind.naomit <- which(!is.na(rowSums(triad_2)))
    triad_2_naomit <- triad_2[ind.naomit,]
    
    # K-means
    # cl_triad <- kmeans(triad_2_naomit,Nclust,iter.max = 1000, nstart = 100)
    # clusters <- cl_triad$cluster
    
    # Hierarchical OK Nclust 5 euclidean
    d <- dist(triad_2_naomit, method = "manhattan") # distance matrix
    fit <- hclust(d, method="ward.D2") 
    clusters <- cutree(fit, k=Nclust) # cut tree into Nclust clusters
    
    # Find the largest Nclustplot clusters
    sizecluster <- rep(0,ntimes=Nclust)
    for (nclust in 1:Nclust) {
      sizecluster[nclust] <- length(which(clusters==nclust))
    }
    sizeclusterord <- sort(sizecluster,index.return=TRUE,decreasing = TRUE)
    centerclusters <- aggregate(triad_2_naomit,by=list(clusters),FUN=mean)[,2:3]
    names(centerclusters) <- c("x","y")
    
    # Find average Feeling over clusters
    feelingclusters <- aggregate(data[[ncountry]]$feeling_num[ind.naomit],by=list(clusters),FUN=mean)[,2]
    
    # Plot ellipsoids of clusters
    
    for (nclust in 1:Nclustplot) {
      # Select data of cluster nclust
      mycluster <- sizeclusterord$ix[nclust]
      ind <- which(clusters==mycluster)
      dataclus <- triad_2_naomit[ind,]
      # Compute ellipsoid
      ell <- ellipse(mu=colMeans(dataclus), sigma=cov(dataclus), alpha = alphaclust, npoints = 50, draw = FALSE)
      ell[ell[,2]<0,] <- NA # clip ellipse below B
      ell[ell[,2]>ell[,1]*sqrt(3),] <- NA # clip ellipse to the left lf L
      ell[ell[,2]>(1-ell[,1])*sqrt(3),] <- NA # clip ellipse to the left lf L
      p <- p + geom_path(data=data.frame(x=ell[,1],y=ell[,2]),na.rm=TRUE)
      p <- p + geom_point(data=data.frame(x=centerclusters[mycluster,1],
                                          y=centerclusters[mycluster,2]),
                          size=2,shape=17)
      p <- p + geom_label(data=data.frame(x=centerclusters[mycluster,1],
                                          y=centerclusters[mycluster,2]),
                          label=format(feelingclusters[mycluster],digits=2),
                          size = 3, # size of label numbers
                          hjust = 0.5, vjust = "outward",alpha=0.5)
    } # End of for (nclust in 1:Nclustplot) {
  } # End of if (Nclust >0) {

return(p)
}
