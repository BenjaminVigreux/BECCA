server <- shinyServer(function(input,output,session){
  
  wd <<- getwd()
  
  ##  DEPENDENCIES
  
  source("factor_to_integer.R")
  source("namegetter.R")
  source("sort_signifiers.R")
  source("unit2perc.R")
  
  ##  CHOOSE DATASET
  
  observeEvent(input$dataset,{
    dataset <- reactive({switch(input$dataset,
                                "Moldova" = "moldova",
                                "Kyrgyzstan UNDP" = "kyrgyzstan",
                                "Kyrgyzstan Unicef" = "unicef",
                                "Serbia" = "serbia",
                                "Yemen" = "yemen",
                                "Tajikistan" = "tajikistan")})
    
    if (exists("clean_bin", where = .GlobalEnv)) rm(clean_bin, envir = .GlobalEnv)
    load(paste0(wd,"/clean_data/",dataset(),"_clean.RData"))
    
    
    ##  PREPROCESSING
    
    clean_bin <- factor2int(clean, integrate = TRUE, ignore = 20, keepFactor = FALSE)
    
    ##  Remove remaining factors ("Other", Q7.Score)
    
    a <- rep(NA,dim(clean_bin)[2])
    for (i in 1:dim(clean_bin)[2]){
      if (((is.factor(clean_bin[,i])) && 
           ((length(grep("^Q[0-9]",names(clean_bin)[i]))) ||
            (length(grep("^DQ[0-9]",names(clean_bin)[i]))))) || 
          (length(grep("Score$",names(clean_bin)[i])))){
        a[i] <- i
      }
    }
    a <- a[!is.na(a)]
    clean_bin <- clean_bin[,-a]
    
    ##  Sort by signifier type
    
    
    sigtypes(clean_bin) # Sig types so stones, triads, and dyads dropdowns work.
    # for (i in 1:length(types)){
    # assign(names(types)[i],types[[i]], envir = parent.frame(n = 1))
    # }
    # rm(types)
    
    ##  Namegetter
    
    factor_names <- namegetter(clean_bin)
    qnames <- factor_names[[1]]
    dqnames <- factor_names[[2]]
    
    ##  TIER 2 OPTIONS - SELECT CONTINOUS/FACTOR VARIABLES
    
    output$x1out <<- renderUI({switch(input$x1,
                                      "Triads" = sidebarPanel(width = 12,
                                                              selectInput(inputId = "x_t1",
                                                                          label = "X Triad 1",
                                                                          choices = c("Select",names(triads))),
                                                              sliderInput(inputId = "xt_sld",
                                                                          label = "Choose Range",
                                                                          min = 0, max = 10, value = c(0,6), step = 0.5,
                                                                          dragRange = TRUE)),
                                      "Dyads" = sidebarPanel(width = 12,
                                                             selectInput(inputId = "x_d1",
                                                                         label = "X Dyad 1",
                                                                         choices = c("Select",names(dyads))),
                                                             sliderInput(inputId = "xd_sld",
                                                                         label = "Choose Range",
                                                                         min = 0, max = 10, value = c(0,6), step = 0.5,
                                                                         dragRange = TRUE)),
                                      "Stones" = sidebarPanel(width = 12,
                                                              selectInput(inputId = "x_s1",
                                                                          label = "X Stone 1",
                                                                          choices = c("Select",names(stones))),
                                                              sliderInput(inputId = "xs_sld",
                                                                          label = "Choose Range",
                                                                          min = 0, max = 10, value = c(0,6), step = 0.5,
                                                                          dragRange = TRUE)),
                                      "Questions" = selectInput(inputId = "x_q1",
                                                                label = "Questions",
                                                                choices = c("Select",qnames),
                                                                selected = "Select"),
                                      "Descriptors" = selectInput(inputId = "x_dq1",
                                                                  label = "Descriptors",
                                                                  choices = c("Select",dqnames),
                                                                  selected = "Select")
    )
    })
    
    output$y1out <<- renderUI({switch(input$y1,
                                      "Triads" = sidebarPanel(width = 12,
                                                              selectInput(inputId = "y_t1",
                                                                          label = "Y Triad 1",
                                                                          choices = c("Select",names(triads))),
                                                              sliderInput(inputId = "yt_sld",
                                                                          label = "Choose Range",
                                                                          min = 0, max = 10, value = c(0,6), step = 0.5,
                                                                          dragRange = TRUE)),
                                      "Dyads" = sidebarPanel(width = 12,
                                                             selectInput(inputId = "y_d1",
                                                                         label = "Y Dyad 1",
                                                                         choices = c("Select",names(dyads))),
                                                             sliderInput(inputId = "yd_sld",
                                                                         label = "Choose Range",
                                                                         min = 0, max = 10, value = c(0,6), step = 0.5,
                                                                         dragRange = TRUE)),
                                      "Stones" = sidebarPanel(width = 12,
                                                              selectInput(inputId = "y_s1",
                                                                          label = "Y Stone 1",
                                                                          choices = c("Select",names(stones))),
                                                              sliderInput(inputId = "ys_sld",
                                                                          label = "Choose Range",
                                                                          min = 0, max = 10, value = c(0,6), step = 0.5,
                                                                          dragRange = TRUE)),
                                      "Questions" = selectInput(inputId = "y_q1",
                                                                label = "Questions",
                                                                choices = c("Select",qnames),
                                                                selected = "Select"),
                                      "Descriptors" = selectInput(inputId = "y_dq1",
                                                                  label = "Descriptors",
                                                                  choices = c("Select",dqnames),
                                                                  selected = "Select")
    )
    })
    
    ##  TIER 3 OPTIONS - SELECT FACTOR LEVELS
    
    if (exists("list_qnames")) {rm(list_qnames)}
    list_qnames <- "Select"
    for (i in 1:length(qnames)){
      list_qnames <- c(list_qnames,assign(qnames[i],paste0(i)))
    }
    names(list_qnames) <- c("Select",qnames)
    
    if (exists("list_dqnames")) {rm(list_dqnames)}
    list_dqnames <- "Select"
    for (i in 1:length(dqnames)){
      list_dqnames <- c(list_dqnames,assign(dqnames[i],paste0(i)))
    }
    names(list_dqnames) <- c("Select",dqnames)
    
    ##  Rows
    
    x_q1 <- renderText({list_qnames[match.arg(input$x_q1,c("Select",names(list_qnames)))]})
    x_dq1 <- renderText({list_dqnames[match.arg(input$x_dq1,c("Select",names(list_dqnames)))]})
    
    output$x_q1_factor <- renderUI({sidebarPanel(
      checkboxInput(inputId = "xq_all","All/None"),
      checkboxGroupInput(inputId = "x_q1_level",
                         label = "Factor Level",
                         choices = names(clean_bin[,grep(paste0("^Q",x_q1(),"."),names(clean_bin),value = TRUE)])))
    })
    output$x_dq1_factor <- renderUI({sidebarPanel(
      checkboxInput(inputId = "xdq_all","All/None"),
      checkboxGroupInput(inputId = "x_dq1_level",
                         label = "Factor Level",
                         choices = names(clean_bin[,grep(paste0("^DQ",x_dq1(),"."),names(clean_bin),value = TRUE)])))
    })
    
    ##  Columns
    
    y_q1 <- renderText({list_qnames[match.arg(input$y_q1,c("Select",names(list_qnames)))]})
    y_dq1 <- renderText({list_dqnames[match.arg(input$y_dq1,c("Select",names(list_dqnames)))]})
    
    output$y_q1_factor <- renderUI({sidebarPanel(
      checkboxInput(inputId = "yq_all","All/None"),
      checkboxGroupInput(inputId = "y_q1_level",
                         label = "Factor Level",
                         choices = names(clean_bin[,grep(paste0("^Q",y_q1(),"."),names(clean_bin),value = TRUE)])))
    })
    output$y_dq1_factor <- renderUI({sidebarPanel(
      checkboxInput(inputId = "ydq_all","All/None"),
      checkboxGroupInput(inputId = "y_dq1_level",
                         label = "Factor Level",
                         choices = names(clean_bin[,grep(paste0("^DQ",y_dq1(),"."),names(clean_bin),value = TRUE)])))
    })
    
    ##  CHECKBOXES
    
    ##  Select All
    
    observe({
      req(input$xq_all)
      updateCheckboxGroupInput(
        session, "x_q1_level", choices = names(clean_bin[,grep(paste0("^Q",x_q1(),"."),names(clean_bin),value = TRUE)]),
        selected = if (input$xq_all == TRUE) names(clean_bin[,grep(paste0("^Q",x_q1(),"."),names(clean_bin),value = TRUE)]))
    },label = "xq_obs")
    observe({
      req(input$xdq_all)
      updateCheckboxGroupInput(
        session, "x_dq1_level", choices = names(clean_bin[,grep(paste0("^DQ",x_dq1(),"."),names(clean_bin),value = TRUE)]),
        selected = if (input$xdq_all == TRUE) names(clean_bin[,grep(paste0("^DQ",x_dq1(),"."),names(clean_bin),value = TRUE)]))
    },label = "xdq_obs")
    observe({
      req(input$yq_all)
      updateCheckboxGroupInput(
        session, "y_q1_level", choices = names(clean_bin[,grep(paste0("^Q",y_q1(),"."),names(clean_bin),value = TRUE)]),
        selected = if (input$yq_all == TRUE) names(clean_bin[,grep(paste0("^Q",y_q1(),"."),names(clean_bin),value = TRUE)]))
    },label = "yq_obs")
    observe({
      req(input$ydq_all)
      updateCheckboxGroupInput(
        session, "y_dq1_level", choices = names(clean_bin[,grep(paste0("^DQ",y_dq1(),"."),names(clean_bin),value = TRUE)]),
        selected = if (input$ydq_all == TRUE) names(clean_bin[,grep(paste0("^DQ",y_dq1(),"."),names(clean_bin),value = TRUE)]))
    },label = "ydq_obs")
    
    clean <<- clean
    clean_bin <<- clean_bin
    cat(dataset()," ")
  }, label = "Selection Frame")
  
  ##  SELECTED VARIABLE SUMMARY
  
  output$x_sel <- renderText({
    req(length(c(input$x_t1,input$x_d1,input$x_s1)) > 0)
    var <- c(input$x_t1,input$x_d1,input$x_s1)
    var <- paste0("Summary of ",var[var != "Select"])})
  
  output$x_sum <- renderTable({
    req(length(c(input$x_t1,input$x_d1,input$x_s1)) > 0)
    var <- renderText(c(input$x_t1,input$x_d1,input$x_s1))
    if (var() != "Select") matrix(data = c(names(summary(clean[,var()])),summary(clean[,var()])), ncol = length(summary(clean[,var()])), nrow = 2, byrow = TRUE)
  }, include.rownames = FALSE, include.colnames = FALSE)
  
  output$y_sel <- renderText({
    req(length(c(input$y_t1,input$y_d1,input$y_s1)) > 0)
    var <- c(input$y_t1,input$y_d1,input$y_s1)
    var <- paste0("Summary of "var[var != "Select"])})
  
  output$y_sum <- renderTable({
    req(length(c(input$y_t1,input$y_d1,input$y_s1)) > 0)
    var <- renderText(c(input$y_t1,input$y_d1,input$y_s1))
    if (var() != "Select") matrix(data = c(names(summary(clean[,var()])),summary(clean[,var()])), ncol = length(summary(clean[,var()])), nrow = 2, byrow = TRUE)
  }, include.rownames = FALSE, include.colnames = FALSE)
  
  ##  Deselect when unused
  
  observe({
    req(input$x_t1,input$x_d1,input$x_s1,input$y_t1,input$y_d1,input$y_s1)
    
    if (input$x1 != "Triads") updateSelectInput(session, "x_t1", choices = c("Select",names(triads)), selected = "Select")
    if (input$x1 != "Dyads") updateSelectInput(session, "x_d1", choices = c("Select",names(dyads)), selected = "Select")
    if (input$x1 != "Stones") updateSelectInput(session, "x_s1", choices = c("Select",names(stones)), selected = "Select")
    
    if (input$y1 != "Triads") updateSelectInput(session, "y_t1", choices = c("Select",names(triads)), selected = "Select")
    if (input$y1 != "Dyads") updateSelectInput(session, "y_t1", choices = c("Select",names(dyads)), selected = "Select")
    if (input$y1 != "Stones") updateSelectInput(session, "y_t1", choices = c("Select",names(stones)), selected = "Select")
  })
  
  ##  BUTTONS
  
  observe({
    if (input$go == 0) # tells action button to do nothing when not clicked ..
      return()
  }, label = "go_sub")
  observe({
    req(input$x_sub)
    if (input$x_sub == 0) # tells action button to do nothing when not clicked ..
      return()
  }, label = "x_sub")
  observe({
    req(input$y_sub)
    if (input$y_sub == 0) # tells action button to do nothing when not clicked ..
      return()
  }, label = "y_sub")
  observe({
    req(input$reset)
    if (input$reset == 0) # tells action button to do nothing when not clicked ..
      return()
  }, label = "reset")
  
  ##  DISPLAY SELECTED ROWS AND COLUMN VARIABLES
  
  ##  Rows
  
  rows <<- NULL
  x_inv <<- NULL
  
  observeEvent(input$x_sub,{
    x_name <- switch(input$x1,
                     "Triads" = input$x_t1,
                     "Dyads" = input$x_d1,
                     "Stones" = input$x_s1,
                     "Questions" = input$x_q1_level,
                     "Descriptors" = input$x_dq1_level)
    x_name <- x_name[x_name != "Select"]
    
    ##  First Entry
    
    if (is.null(rows)) {
      rows <- matrix(data = x_name, byrow = TRUE)
      if (dim(rows)[1] == 1) {
        rows <- switch(substr(rows[1,1],1,1),
                       "T" = cbind(rows, paste0(input$xt_sld[1]," : ",input$xt_sld[2])),
                       "S" = cbind(rows, paste0(input$xs_sld[1]," : ",input$xs_sld[2])),
                       "D" = cbind(rows, paste0(input$xd_sld[1]," : ",input$xd_sld[2])))
        colnames(rows)[1:2] <- switch(substr(rows[1,1],1,1),
                                      "T" = c("Triads", "Triad Range"),
                                      "S" = c("Stones", "Stone Range"),
                                      "D" = c("Dyads", "Dyad Range"))
      } else {
        
        x_inv <<- x_name
        
        x <- tstrsplit(rows[1,dim(rows)[2]], c("[DQSTRC][0-9].|_"))
        colnames(rows)[dim(rows)[2]] <- x[[2]]
        ##  Remove Factor Name
        for(r in 1:length(x_name)){
          x <- tstrsplit(rows[r,dim(rows)[2]], c("[DQSTRC][0-9].|_"))
          rows[r,dim(rows)[2]] <- x[[3]]
        }
      }
    } else {
      
      ##  Factor Variables
      
      if (length(x_name) > 1) {
        
        x_inv <<- c(x_inv,x_name)
        
        if (dim(rows)[1] == length(x_name)) rows <- cbind(rows,x_name) else {
          if (dim(rows)[1] > length(x_name)) {
            rows <- cbind(rows,c(x_name,rep("",dim(rows)[1]-length(x_name))))
          } else {
            rows <- rbind(rows,matrix("",nrow = length(x_name)-dim(rows)[1],ncol = dim(rows)[2]))
            rows <- cbind(rows,x_name)
          }
        }
        
        ##  Colnames
        
        x <- tstrsplit(rows[1,dim(rows)[2]], c("[DQSTRC][0-9].|_"))
        colnames(rows)[dim(rows)[2]] <- x[[2]]
        ##  Remove Factor Name
        for(r in 1:length(x_name)){
          x <- tstrsplit(rows[r,dim(rows)[2]], c("[DQSTRC][0-9].|_"))
          rows[r,dim(rows)[2]] <- x[[3]]
        }
        
      } else {
        
        ##  Continuous Variables
        
        c <- match("Triads", colnames(rows), nomatch = FALSE)
        if (substr(x_name,1,1) == "T") {
          if (c) {if (match("",rows[,c],nomatch = FALSE)) {
            rows[match("",rows[,c])[1],c:(c+1)] <- c(x_name,paste0(input$xt_sld[1]," : ",input$xt_sld[2]))} else {
              rows <- rbind(rows,rep("",dim(rows)[1]))
              rows[match("",rows[,c])[1],c:(c+1)] <- c(x_name,paste0(input$xt_sld[1]," : ",input$xt_sld[2]))}} else {
                rows <- cbind(rows,c(x_name,rep("",dim(rows)[1]-1)),c(paste0(input$xt_sld[1]," : ",input$xt_sld[2]),rep("",dim(rows)[1]-1)))
                colnames(rows)[(dim(rows)[2]-1):dim(rows)[2]] <- c("Triads","Triad Range")}
        }
        c <- match("Dyads", colnames(rows), nomatch = FALSE)
        if (substr(x_name,1,1) == "D") {
          if (c) {if (match("",rows[,c],nomatch = FALSE)) {
            rows[match("",rows[,c])[1],c:(c+1)] <- c(x_name,paste0(input$xd_sld[1]," : ",input$xd_sld[2]))} else {
              rows <- rbind(rows,rep("",dim(rows)[1]))
              rows[match("",rows[,c])[1],c:(c+1)] <- c(x_name,paste0(input$xd_sld[1]," : ",input$xd_sld[2]))}} else {
                rows <- cbind(rows,c(x_name,rep("",dim(rows)[1]-1)),c(paste0(input$xd_sld[1]," : ",input$xd_sld[2]),rep("",dim(rows)[1]-1)))
                colnames(rows)[(dim(rows)[2]-1):dim(rows)[2]] <- c("Dyads","Dyad Range")}
        }
        c <- match("Stones", colnames(rows), nomatch = FALSE)
        if (substr(x_name,1,1) == "S") {
          if (c) {if (match("",rows[,c],nomatch = FALSE)) {
            rows[match("",rows[,c])[1],c:(c+1)] <- c(x_name,paste0(input$xs_sld[1]," : ",input$xs_sld[2]))} else {
              rows <- rbind(rows,rep("",dim(rows)[1]))
              rows[match("",rows[,c])[1],c:(c+1)] <- c(x_name,paste0(input$xs_sld[1]," : ",input$xs_sld[2]))}} else {
                rows <- cbind(rows,c(x_name,rep("",dim(rows)[1]-1)),c(paste0(input$xs_sld[1]," : ",input$xs_sld[2]),rep("",dim(rows)[1]-1)))
                colnames(rows)[(dim(rows)[2]-1):dim(rows)[2]] <- c("Stones","Stone Range")}
        }
      }
    }
    
    rows <<- rows
    
    output$x_chosen <- renderUI({
      html <- paste0("$$",print(xtable(rows, align=c("l",rep("c", ncol(rows))), caption = "Selected Rows"), 
                                caption.placement = "top", floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE),"$$")
      list(
        withMathJax(HTML(html))
      )
    })
  }, label = "x submit")
  
  ##  Cols
  
  cols <<- NULL
  y_inv <<- NULL
  
  observeEvent(input$y_sub,{
    y_name <- switch(input$y1,
                     "Triads" = input$y_t1,
                     "Dyads" = input$y_d1,
                     "Stones" = input$y_s1,
                     "Questions" = input$y_q1_level,
                     "Descriptors" = input$y_dq1_level)
    y_name <- y_name[y_name != "Select"]
    
    ##  First Entry
    
    if (is.null(cols)) {
      cols <- matrix(data = y_name, byrow = TRUE)
      if (dim(cols)[1] == 1) {
        cols <- switch(substr(cols[1,1],1,1),
                       "T" = cbind(cols, paste0(input$yt_sld[1]," : ",input$yt_sld[2])),
                       "S" = cbind(cols, paste0(input$ys_sld[1]," : ",input$ys_sld[2])),
                       "D" = cbind(cols, paste0(input$yd_sld[1]," : ",input$yd_sld[2])))
        colnames(cols)[1:2] <- switch(substr(cols[1,1],1,1),
                                      "T" = c("Triads", "Triad Range"),
                                      "S" = c("Stones", "Stone Range"),
                                      "D" = c("Dyads", "Dyad Range"))
      } else {
        
        y_inv <<- y_name
        
        y <- tstrsplit(cols[1,dim(cols)[2]], c("[DQSTRC][0-9].|_"))
        colnames(cols)[dim(cols)[2]] <- y[[2]]
        ##  Remove Factor Name
        for(r in 1:length(y_name)){
          y <- tstrsplit(cols[r,dim(cols)[2]], c("[DQSTRC][0-9].|_"))
          cols[r,dim(cols)[2]] <- y[[3]]
        }
      }
    } else {
      
      ##  Factor Variables
      
      if (length(y_name) > 1) {
        
        y_inv <<- c(y_inv,y_name)
        
        if (dim(cols)[1] == length(y_name)) cols <- cbind(cols,y_name) else {
          if (dim(cols)[1] > length(y_name)) {
            cols <- cbind(cols,c(y_name,rep("",dim(cols)[1]-length(y_name))))
          } else {
            cols <- rbind(cols,matrix("",nrow = length(y_name)-dim(cols)[1],ncol = dim(cols)[2]))
            cols <- cbind(cols,y_name)
          }
        }
        ##  Colnames
        y <- tstrsplit(cols[1,dim(cols)[2]], c("[DQSTRC][0-9].|_"))
        colnames(cols)[dim(cols)[2]] <- y[[2]]
        ##  Remove Factor Name
        for(r in 1:length(y_name)){
          y <- tstrsplit(cols[r,dim(cols)[2]], c("[DQSTRC][0-9].|_"))
          cols[r,dim(cols)[2]] <- y[[3]]
        }
        
      } else {
        
        ##  Continuous Variables
        
        c <- match("Triads", colnames(cols), nomatch = FALSE)
        if (substr(y_name,1,1) == "T") {
          if (c) {if (match("",cols[,c],nomatch = FALSE)) {
            cols[match("",cols[,c])[1],c:(c+1)] <- c(y_name,paste0(input$yt_sld[1]," : ",input$yt_sld[2]))} else {
              cols <- rbind(cols,rep("",dim(cols)[1]))
              cols[match("",cols[,c])[1],c:(c+1)] <- c(y_name,paste0(input$yt_sld[1]," : ",input$yt_sld[2]))}} else {
                cols <- cbind(cols,c(y_name,rep("",dim(cols)[1]-1)),c(paste0(input$yt_sld[1]," : ",input$yt_sld[2]),rep("",dim(cols)[1]-1)))
                colnames(cols)[(dim(cols)[2]-1):dim(cols)[2]] <- c("Triads","Triad Range")}
        }
        c <- match("Dyads", colnames(cols), nomatch = FALSE)
        if (substr(y_name,1,1) == "D") {
          if (c) {if (match("",cols[,c],nomatch = FALSE)) {
            cols[match("",cols[,c])[1],c:(c+1)] <- c(y_name,paste0(input$yd_sld[1]," : ",input$yd_sld[2]))} else {
              cols <- rbind(cols,rep("",dim(cols)[1]))
              cols[match("",cols[,c])[1],c:(c+1)] <- c(y_name,paste0(input$yd_sld[1]," : ",input$yd_sld[2]))}} else {
                cols <- cbind(cols,c(y_name,rep("",dim(cols)[1]-1)),c(paste0(input$yd_sld[1]," : ",input$yd_sld[2]),rep("",dim(cols)[1]-1)))
                colnames(cols)[(dim(cols)[2]-1):dim(cols)[2]] <- c("Dyads","Dyad Range")}
        }
        c <- match("Stones", colnames(cols), nomatch = FALSE)
        if (substr(y_name,1,1) == "S") {
          if (c) {if (match("",cols[,c],nomatch = FALSE)) {
            cols[match("",cols[,c])[1],c:(c+1)] <- c(y_name,paste0(input$ys_sld[1]," : ",input$ys_sld[2]))} else {
              cols <- rbind(cols,rep("",dim(cols)[1]))
              cols[match("",cols[,c])[1],c:(c+1)] <- c(y_name,paste0(input$ys_sld[1]," : ",input$ys_sld[2]))}} else {
                cols <- cbind(cols,c(y_name,rep("",dim(cols)[1]-1)),c(paste0(input$ys_sld[1]," : ",input$ys_sld[2]),rep("",dim(cols)[1]-1)))
                colnames(cols)[(dim(cols)[2]-1):dim(cols)[2]] <- c("Stones","Stone Range")}
        }
      }
    }
    
    cols <<- cols
    
    output$y_chosen <- renderUI({
      html <- paste0("$$",print(xtable(cols, align=c("l",rep("c", ncol(cols))), caption = "Selected Columns"), 
                                caption.placement = "top", floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE),"$$")
      list(
        withMathJax(HTML(html))
      )
    })
  }, label = "y submit")
  
  ##  DATAFRAME CONSTRUCTION
  
  observe({
    isolate({
      
      values <- reactiveValues(dynamic_frame = clean_bin)
      
      observeEvent(input$go,{
        
        ##  Convert continuous variables to binary based on ranges.
        
        row_sig <- colnames(rows)[match(c("Triads","Dyads","Stones"),colnames(rows))]
        row_sig <- row_sig[!is.na(row_sig)]
        col_sig <- colnames(cols)[match(c("Triads","Dyads","Stones"),colnames(cols))]
        col_sig <- col_sig[!is.na(col_sig)]
        
        row_range <- colnames(rows)[match(c("Triad Range","Dyad Range","Stone Range"),colnames(rows))]
        row_range <- row_range[!is.na(row_range)]
        col_range <- colnames(cols)[match(c("Triad Range","Dyad Range","Stone Range"),colnames(cols))]
        col_range <- col_range[!is.na(col_range)]
        
        cont_rows <- as.character(rows[,row_sig])
        cont_rows <- cont_rows[cont_rows != ""]
        cont_rows <- values$dynamic_frame[,cont_rows]
        cont_rows_range <- as.character(rows[,row_range])[as.character(rows[,row_range]) != ""]
        cont_rows_range <- tstrsplit(cont_rows_range," : ")
        
        cont_cols <- as.character(cols[,col_sig])
        cont_cols <- cont_cols[cont_cols != ""]
        cont_cols <- values$dynamic_frame[,cont_cols]
        cont_cols_range <- as.character(cols[,col_range])[as.character(cols[,col_range]) != ""]
        cont_cols_range <- tstrsplit(cont_cols_range," : ")
        
        if (length(cont_rows) > 0) {
          for (c in 1:dim(cont_rows)[2]){
            if (substr(colnames(cont_rows)[c],1,1) == "T"){
              cont_rows[,c] <- as.integer(((as.numeric(cont_rows_range[[1]][c]) * 10) < cont_rows[,c]) & ((as.numeric(cont_rows_range[[2]][c]) * 10) > cont_rows[,c]))
            } else {
              cont_rows[,c] <- as.integer(((as.numeric(cont_rows_range[[1]][c]) / 10) < cont_rows[,c]) & ((as.numeric(cont_rows_range[[2]][c]) / 10) > cont_rows[,c]))
            }
          }
          cont_rows <- as.data.frame(ifelse(cont_rows == 1,1,NA))
        }
        
        if (length(cont_cols) > 0) {
          for (c in 1:dim(cont_cols)[2]){
            if (substr(colnames(cont_cols)[c],1,1) == "T"){
              cont_cols[,c] <- as.integer(((as.numeric(cont_cols_range[[1]][c]) * 10) < cont_cols[,c]) & ((as.numeric(cont_cols_range[[2]][c]) * 10) > cont_cols[,c]))
            } else {
              cont_cols[,c] <- as.integer(((as.numeric(cont_cols_range[[1]][c]) / 10) < cont_cols[,c]) & ((as.numeric(cont_cols_range[[2]][c]) / 10) > cont_cols[,c]))
            }
          }
          cont_cols <- as.data.frame(ifelse(cont_cols == 1,1,NA))
        }
        
        ##  Factor Variables
        
        if (length(cont_rows) == 0) desc_rows <<- values$dynamic_frame[,x_inv]
        if (length(x_inv) == 0) desc_rows <<- cont_rows
        if ((length(x_inv) > 0) && (length(cont_rows) > 0)) desc_rows <<- cbind(cont_rows,values$dynamic_frame[,x_inv])
        
        if (length(cont_cols) == 0) desc_cols <<- values$dynamic_frame[,y_inv]
        if (length(y_inv) == 0) desc_cols <<- cont_cols
        if ((length(y_inv) > 0) && (length(cont_cols) > 0)) desc_cols <<- cbind(cont_cols,values$dynamic_frame[,y_inv])
        
        desc_stats <<- matrix(NA, nrow = dim(desc_rows)[2], ncol = dim(desc_cols)[2])
        
        for (r in 1:dim(desc_rows)[2]){
          for (c in 1:dim(desc_cols)[2]){
            desc_stats[r,c] <- table(desc_rows[,r],desc_cols[,c])
          }
        }
        
        rownames(desc_stats) <- names(desc_rows)
        colnames(desc_stats) <- names(desc_cols)
        
        desc_props <- unit2perc(clean_bin,desc_stats)
        
        values$stats <<- desc_stats
        values$props <<- desc_props
        
        
        
        output$stats <- renderUI({
          html <- paste0("$$",print(xtable(values$props, align=c("l",rep("c", ncol(values$stats)))), 
                                    floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE),"$$")
          list(
            withMathJax(HTML(html))
          )
        })
        
        output$props <- renderUI({
          html <- paste0("$$",print(xtable(values$stats, align=c("l",rep("c", ncol(values$props)))), 
                                    floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE),"$$")
          list(
            withMathJax(HTML(html))
          )
        })
        
      }, label = "df_builder")
    })}, label = "main_wrapper")
  
  observeEvent(input$reset,{
    rows <<- NULL
    cols <<- NULL
    desc_stats <<- NULL
    desc_props <<- NULL
    output$x_chosen <<- renderUI({rows})
    output$y_chosen <<- renderUI({cols})
    output$stats <<- renderUI({desc_stats})
    output$props <<- renderUI({desc_props})
    updateSelectInput(session, "x1", choices = c("Select","Triads","Dyads","Stones","Questions","Descriptors"), selected = "Select")
    updateSelectInput(session, "y1", choices = c("Select","Triads","Dyads","Stones","Questions","Descriptors"), selected = "Select")
  }, label = "reset")
  
  
})