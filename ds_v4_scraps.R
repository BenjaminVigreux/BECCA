rows <- matrix("",
               nrow = max(length(input$x_t1),length(input$x_d1),length(input$x_s1),length(input$x_q1_level),length(input$x_dq1_level)),
               ncol = length(c(input$x_t1,input$x_d1,input$x_s1,input$x_q1_level,input$x_dq1_level)))



observe({
  if(!exists("rows")) {rows <- NULL}
  xinputs <- list(input$x_t1,input$x_d1,input$x_s1,input$x_q1_level,input$x_dq1_level)
  if (!is.null(c(input$x_t1,input$x_d1,input$x_s1,input$x_q1_level,input$x_dq1_level))) {
    
    for (c in 1:dim(rows)[2]) {
      blanks <- rep("",dim(rows)[1] - length(xinputs[c]))
      rows[,c] <- c(xinputs[[c]],blanks)
    }
    
    xtabnames <- strsplit(rows[1,], c("[DQSTRC][0-9].","_"))
    for (i in 1:length(xtabnames)){
      x <- xtabnames[[i]]
      colnames(rows)[i] <- x[2]
    }
    
    output$x_chosen <- renderTable({
      rows
    })
  }
})

observe({
  if (!is.null(c(input$y_t1,input$y_d1,input$y_s1,input$y_q1_level,input$y_dq1_level))) {
    cols <- matrix(data = c(input$y_t1,input$y_d1,input$y_s1,input$y_q1_level,input$y_dq1_level), byrow = TRUE)
    ytabnames <- strsplit(cols[1,], c("[DQSTRC][0-9].","_"))
    for (i in 1:length(ytabnames)){
      y <- ytabnames[[i]]
      colnames(cols)[i] <- y[2]
    }
    
    output$y_chosen <- renderTable({
      cols
    })
  }
})