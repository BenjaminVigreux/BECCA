mytriad <- function(data,Country,ncountry,varname,ntriads) {
  
  require(ggplot2) # Explicitly needed because overlap of annotate with package NLP
  require(mixtools) # for computing ellipsoids from clustering
  
  N <- length(data[[ncountry]]$FragmentID)
  Ntriads <- length(Country[[ncountry]]$var_triads)
  
  # Exact match
  indvar <- grep(paste0("^",varname,"$"),names(data[[ncountry]]))

  var_Nlev <- length(levels(data[[ncountry]][,indvar]))
  clev <- levels(data[[ncountry]][,indvar])
  Lev <- clev

  whatT <- paste("T",ntriads,sep="")
  ind <- grep(whatT,names(data[[ncountry]]))
  ind <- ind[1]
  triad_3 <- matrix(0,nrow=N,ncol=3)
  triad_2 <- matrix(0,nrow=N,ncol=2)
  # Coerce triad_2 and triad_3 (which are lists of factors) to numeric matrices
  for (i in 1:3) {
    dum <- data[[ncountry]][,(ind+i-1):(ind+i-1)]
    triad_3[,i] <- dum/100
  }
  
  triad_2[,1] <- triad_3[,2]+ triad_3[,3]/2
  triad_2[,2] <- triad_3[,3]*sqrt(3)/2
  
  
  #for (i in 1:2) {
  # dum <- data[[ncountry]][,(ind+i+2):(ind+i+2)]
  #triad_2[,i] <- dum
  #}
  mycols <- colorRampPalette(c("red","cyan","yellow","gray","green","blue"))(var_Nlev)
  if (var_Nlev == 2) mycols <- c("red","blue")
  if (var_Nlev == 3) mycols <- c("red","yellow","blue")
  if (var_Nlev == 4) mycols <- c("red","cyan","yellow","blue")
  if (var_Nlev == 5) mycols <- c("red","cyan","yellow","green","blue")
  if (var_Nlev == 6) mycols <- c("red","cyan","yellow","green","magenta","blue")
  mapping <- mycols
  names(mapping) <- as.character(clev)
  
  ind <- which(!is.na(rowSums(triad_2)))
  df <- data.frame(triad_2[ind,],factor(data[[ncountry]][ind,indvar]))
  names(df)[1:2] <- c("x","y")
  names(df)[3] <- "variable"
  
  topy <- sqrt(3)/2
  
  datatriangle=data.frame(x=c(0,1,0.5),y=c(0,0,topy))
  
  N1 <- 10
  myseq1 <- seq(0,N1)/N1
  myseq2 <- myseq1*0.5
  myseq3 <- myseq1*topy
  zeroseq <- vector("numeric",N1+1)
  
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
  p <- p + geom_point(aes(color=variable), shape=16, size=2, alpha = 0.4)
  # Colormap of the points according to the variable
  # Put legend and position it
  p <-  p + scale_color_manual(name = varname,
                               labels = Lev,
                               values = mapping)
  # p <- p + theme(legend.position = c(0.9,0.75))
  if (ntriads == 1 & length(levels(df$variable)) > 1) {
    p <- p + theme(legend.position = c(1.0,0.60))
  } else {
    p <- p + theme(legend.position = "none")
  }
  p <- p + theme(legend.text= element_text(size=8))
  p <- p + theme(legend.key.size = unit(0.5, "cm"))
  # Add title
  p <- p + ggtitle(paste(Country[[ncountry]]$var_triads[[ntriads]]$T))
  p <- p + theme(plot.title = element_text(vjust = 0.5, hjust = 0.5))
  
 
return(p)
}
