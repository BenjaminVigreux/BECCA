require(shiny)
require(shinyBS)
require(data.table)
require(xtable)
require(ggplot2)
options(shiny.reactlog = TRUE)


  ##  TO DO

##  Account for Triad/Stone/Dyad NA columns
##  Convent factors stored as integer to factor??

  ##  ISSUES

##  Cannot deselect checkboxes, which crashes app when dataset is changed.

  ##  USER INTERFACE

ui <- shinyUI(fluidPage(
  tags$head(tags$style(".leftAlign{float:left;}")),
  
  headerPanel("Model Builder"),
  
  ##  SIDEBAR
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput(inputId = "dataset",
                  label = "Select Dataset",
                  choices = c("Moldova","Kyrgyzstan UNDP","Kyrgyzstan Unicef","Serbia","Yemen","Tajikistan"),
                  selected = "Moldova"),
      hr(),
      
      ##  Choose Dependent Variable
      
      selectInput(inputId = "y1",
                  label = "Choose Dependent Variable",
                  choices = c("Select","Triads","Dyads","Stones","Questions","Descriptors")),
      conditionalPanel(condition = "input.y1 != 'Select'",
                       uiOutput("y2")),
      hr(),br(),
      
      ##  Choose Independent Variables
      
      selectInput(inputId = "x1",
                  label = "Choose Independent Variables",
                  choices = c("Select","Triads","Dyads","Stones","Questions","Descriptors")),
      conditionalPanel(condition = "input.x1 != 'Select'",
                       uiOutput("x2")),
      hr(),br(),
      
      ##  Interaction Effects
      
      selectInput(inputId = "i1",
                  label = "Interaction Term 1",
                  choices = c("Select","Triads","Dyads","Stones","Questions","Descriptors")),
      conditionalPanel(condition = "input.i1 != 'Select'",
                       uiOutput("i1out")),
      selectInput(inputId = "i2",
                  label = "Interaction Term 2",
                  choices = c("Select","Triads","Dyads","Stones","Questions","Descriptors")),
      conditionalPanel(condition = "input.i2 != 'Select'",
                       uiOutput("i2out"),
                       actionButton("go_int", "Add Interaction")),
      
      fluidRow(actionButton("go","Build Model"),actionButton("reset","Reset"))),
      
    mainPanel(tabsetPanel(type = "tabs", ## Remove toptable??
                                   tabPanel("Variable Selection",
                                            fluidRow(p(textOutput("y_sel")),tableOutput("y_sum")),hr(),
                                            fluidRow(p(textOutput("x_sel"),tableOutput("x_sum"))),hr(),
                                            fluidRow(p(textOutput("i_sel")),column(width = 8,tableOutput("i_sum")),column(width = 4,tableOutput("i_chosen"))),class = "leftAlign"),
      tabPanel("Model",fluidRow(p("Model Summary"),dataTableOutput("model_sum")),class = "leftAlign")))
  )
))

##################################################  SERVER  ##################################################
  
  server <- shinyServer(function(input,output,session){
    
    wd <<- getwd()
    
    ##  DEPENDENCIES
    
    source("factor_to_integer.R")
    source("namegetter.R")
    source("sort_signifiers.R")
    source("unit2perc.R")
    source("factor_matrix.R")
    
    ##  CHOOSE DATASET
    
    observeEvent(input$dataset,{
      dataset <- reactive({switch(input$dataset,
                                  "Moldova" = "moldova",
                                  "Kyrgyzstan UNDP" = "kyrgyzstan",
                                  "Kyrgyzstan Unicef" = "unicef",
                                  "Serbia" = "serbia",
                                  "Yemen" = "yemen",
                                  "Tajikistan" = "tajikistan")})
      
      if (exists("int1", where = .GlobalEnv)) rm(int1, envir = .GlobalEnv)
      if (exists("int2", where = .GlobalEnv)) rm(int2, envir = .GlobalEnv)
      if (exists("int", where = .GlobalEnv)) rm(int, envir = .GlobalEnv)
      if (exists("i_chosen", where = .GlobalEnv)) rm(i_chosen, envir = .GlobalEnv)
      if (exists("clean_bin", where = .GlobalEnv)) rm(clean_bin, envir = .GlobalEnv) ## Keep clean_bin??
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
    
    sigtypes(clean)
    
      ##  Namegetter
    
    factor_names <- namegetter(clean)
    qnames <- factor_names[[1]]
    dqnames <- factor_names[[2]]
    
    ##  TIER 2 OPTIONS - SELECT DEPENDENT VARIABLE
    
    output$y2 <<- renderUI({switch(input$y1,
                                   "Triads" = sidebarPanel(width = 12,
                                                           selectInput(inputId = "ytout",
                                                                       label = "Triads",
                                                                       choices = c("Select",names(triads)))),
                                   "Dyads" = sidebarPanel(width = 12,
                                                          selectInput(inputId = "ydout",
                                                                      label = "Dyads",
                                                                      choices = c("Select",names(dyads)))),
                                   "Stones" = sidebarPanel(width = 12,
                                                           selectInput(inputId = "ysout",
                                                                       label = "Stones",
                                                                       choices = c("Select",names(stones)))),
                                   "Questions" = selectInput(inputId = "yqout",
                                                             label = "Questions",
                                                             choices = c("Select",qnames),
                                                             selected = "Select"),
                                   "Descriptors" = selectInput(inputId = "ydqout",
                                                               label = "Descriptors",
                                                               choices = c("Select",dqnames),
                                                               selected = "Select")
    )
    })
    
    output$x2 <<- renderUI({switch(input$x1,
                                   "Triads" = sidebarPanel(width = 12,
                                                           checkboxGroupInput(inputId = "xtout",
                                                                       label = "Triads",
                                                                       choices = names(triads))),
                                   "Dyads" = sidebarPanel(width = 12,
                                                          checkboxGroupInput(inputId = "xdout",
                                                                      label = "Dyads",
                                                                      choices = names(dyads))),
                                   "Stones" = sidebarPanel(width = 12,
                                                           checkboxGroupInput(inputId = "xsout",
                                                                       label = "Stones",
                                                                       choices = names(stones))),
                                   "Questions" = checkboxGroupInput(inputId = "xqout",
                                                             label = "Questions",
                                                             choices = names(questions)),
                                   "Descriptors" = checkboxGroupInput(inputId = "xdqout",
                                                               label = "Descriptors",
                                                               choices = names(descriptors))
    )
    })
    
    ##  INTERACTION EFFECTS
    
    output$i1out <<- renderUI({switch(input$i1,
                                      "Triads" = sidebarPanel(width = 12,
                                                              selectInput(inputId = "i1tout",
                                                                          label = "Triads",
                                                                          choices = names(triads))),
                                      "Dyads" = sidebarPanel(width = 12,
                                                             selectInput(inputId = "i1dout",
                                                                         label = "Dyads",
                                                                         choices = names(dyads))),
                                      "Stones" = sidebarPanel(width = 12,
                                                              selectInput(inputId = "i1sout",
                                                                          label = "Stones",
                                                                          choices = names(stones))),
                                      "Questions" = selectInput(inputId = "i1qout",
                                                                label = "Questions",
                                                                choices = names(questions)),
                                      "Descriptors" = selectInput(inputId = "i1dqout",
                                                                  label = "Descriptors",
                                                                  choices = names(descriptors))
    )
    })
    
    output$i2out <<- renderUI({switch(input$i2,
                                      "Triads" = sidebarPanel(width = 12,
                                                              selectInput(inputId = "i2tout",
                                                                          label = "Triads",
                                                                          choices = names(triads))),
                                      "Dyads" = sidebarPanel(width = 12,
                                                             selectInput(inputId = "i2dout",
                                                                         label = "Dyads",
                                                                         choices = names(dyads))),
                                      "Stones" = sidebarPanel(width = 12,
                                                              selectInput(inputId = "i2sout",
                                                                          label = "Stones",
                                                                          choices = names(stones))),
                                      "Questions" = selectInput(inputId = "i2qout",
                                                                label = "Questions",
                                                                choices = names(questions)),
                                      "Descriptors" = selectInput(inputId = "i2dqout",
                                                                  label = "Descriptors",
                                                                  choices = names(descriptors))
    )
    })
    
    clean <<- clean
    clean_bin <<- clean_bin
    }, label = "Selection Frame")
    
    ##  REACTIVE VALUES
    
    values <- reactiveValues()
    
    observe({
      req(length(c(input$i1tout,input$i1dout,input$i1sout,input$i1qout,input$i1dqout)) > 0)
      req(length(c(input$i2tout,input$i2dout,input$i2sout,input$i2qout,input$i2dqout)) > 0)
      
      values$int1 <- c(input$i1tout,input$i1dout,input$i1sout,input$i1qout,input$i1dqout)
      values$int2 <- c(input$i2tout,input$i2dout,input$i2sout,input$i2qout,input$i2dqout)
      values$int <- paste(values$int1,"*",values$int2)
    })
    
    ##  SELECTED VARIABLE SUMMARY
    
    output$y_sel <- renderText({
      req(length(c(input$ytout,input$ydout,input$ysout)) > 0)
      res <<- c(input$ytout,input$ydout,input$ysout)
      paste0("Summary of Dependent Variable: ",res[res != "Select"])})
    
    ## Do this for factor variables???
    
    output$y_sum <- renderTable({
      req(length(c(input$ytout,input$ydout,input$ysout)) > 0)
      var <- c(input$ytout,input$ydout,input$ysout,input$yqout,input$ydqout)
      if (var != "Select") matrix(data = c(names(summary(clean[,var])),summary(clean[,var])), ncol = length(summary(clean[,var])), nrow = 2, byrow = TRUE)
    }, include.rownames = FALSE, include.colnames = FALSE)
    
    output$x_sel <- renderText("Summary of Independent Variables")
        
    output$x_sum <- renderTable({
      req(length(c(input$xtout,input$xdout,input$xsout,input$xqout,input$xdqout)) > 0)
      var <- c(input$xtout,input$xdout,input$xsout,input$xqout,input$xdqout)
      if (var != "Select") as.matrix(summary(clean[,var]))
    }, include.rownames = FALSE)
    
    ##  Create interaction term
    
    output$i_sel <- renderText({
      req(length(c(input$i1tout,input$i1dout,input$i1sout,input$i1qout,input$i1dqout)) > 0)
      req(length(c(input$i2tout,input$i2dout,input$i2sout,input$i2qout,input$i2dqout)) > 0)
      
      paste("Interaction of ",values$int1,"and",values$int2)
      })
    
    output$i_sum <- renderTable({
      req(length(c(input$i1tout,input$i1dout,input$i1sout,input$i1qout,input$i1dqout)) > 0)
      req(length(c(input$i2tout,input$i2dout,input$i2sout,input$i2qout,input$i2dqout)) > 0)
      
      int1 <- values$int1
      int2 <- values$int2
      
      ##  2 factor variables
      
      if (is.factor(clean[,int1]) && is.factor(clean[,int1])){
      
      i1mat <- factorize(clean[,int1])
      i2mat <- factorize(clean[,int2])
      
      imat <- matrix(NA, nrow = dim(i1mat)[2], ncol = dim(i2mat)[2])
      for(r in 1:dim(i1mat)[2]){
        for(c in 1:dim(i2mat)[2]){
          tab <- table(i1mat[,r],i2mat[,c])
          imat[r,c] <- tab[rownames(tab) == 1,colnames(tab) == 1]
        }
      }
      rownames(imat) <- levels(clean[,int1])
      colnames(imat) <- levels(clean[,int2])
      }
      
      ##  1 factor, 1 continuous
      
      if (((is.factor(clean[,int1])) && (is.numeric(clean[,int2]))) ||
          ((is.factor(clean[,int2])) && (is.numeric(clean[,int1])))){
        ifelse(is.factor(clean[,int1]),fac <- clean[,int1], con <- clean[,int1])
        ifelse(is.factor(clean[,int2]),fac <- clean[,int2], con <- clean[,int2])
        
        names(con) <- fac
        imat <- matrix(NA, nrow = nlevels(fac), ncol = 7, dimnames = list(levels(fac),names(summary(con[names(con) == levels(fac)[1]]))))
        
        for(r in 1:dim(imat)[1]){
          level_summary <- summary(con[names(con) == levels(fac)[r]])
          filler <- rep("-",7 - length(level_summary))
            imat[r,] <- c(level_summary,filler)
        }
      }
      
      ##  2 continous variables
      
      if ((is.numeric(clean[,int1])) && (is.numeric(clean[,int2]))){
        imat <- rbind(summary(clean[,int1]),summary(clean[,int2]))
        rownames(imat) <- c(int1,int2)
        colnames(imat) <- names(summary(clean[,int2]))
      }
      imat <<- imat
      imat
    })
    
    ##  Deselect when unused
    
    observe({
      req(input$x_t1,input$x_d1,input$x_s1,input$y_t1,input$y_d1,input$y_s1)
      
      if (input$x1 != "Triads") updateSelectInput(session, "ytout", choices = c("Select",names(triads)), selected = "Select")
      if (input$x1 != "Dyads") updateSelectInput(session, "ydout", choices = c("Select",names(dyads)), selected = "Select")
      if (input$x1 != "Stones") updateSelectInput(session, "ysout", choices = c("Select",names(stones)), selected = "Select")
      if (input$x1 != "Questions") updateSelectInput(session, "yqout", choices = c("Select",names(qnames)), selected = "Select")
      if (input$x1 != "Descriptors") updateSelectInput(session, "ydqout", choices = c("Select",names(dqnames)), selected = "Select")
      
      if (input$y1 != "Triads") updateSelectInput(session, "xtout", choices = c("Select",names(triads)), selected = "Select")
      if (input$y1 != "Dyads") updateSelectInput(session, "xdout", choices = c("Select",names(dyads)), selected = "Select")
      if (input$y1 != "Stones") updateSelectInput(session, "xsout", choices = c("Select",names(stones)), selected = "Select")
      if (input$x1 != "Questions") updateSelectInput(session, "xqout", choices = c("Select",names(qnames)), selected = "Select")
      if (input$x1 != "Descriptors") updateSelectInput(session, "xdqout", choices = c("Select",names(dqnames)), selected = "Select")
    })
    
      ##  BUTTONS
    
    observe({
      if (input$go == 0) # tells action button to do nothing when not clicked ..
        return()
    }, label = "go_sub")
    observe({
      req(input$go_int)
      if (input$go_int == 0) # tells action button to do nothing when not clicked ..
        return()
    }, label = "go_int")
    observe({
      req(input$reset)
      if (input$reset == 0) # tells action button to do nothing when not clicked ..
        return()
    }, label = "reset")
      
    ##  MODEL BUILDER
    
    observe({
      isolate({
        
          ##  Create interaction term
        
        observeEvent(input$go_int,{
          req(length(c(input$i1tout,input$i1dout,input$i1sout,input$i1qout,input$i1dqout)) > 0)
          req(length(c(input$i2tout,input$i2dout,input$i2sout,input$i2qout,input$i2dqout)) > 0)
          
          output$i_chosen <- renderTable({ ## Why does this run whenever inputs changes after first AB click?
            browser()
            if (!exists("i_chosen", where = .GlobalEnv)){
              i_chosen <<- matrix(data = values$int, nrow = 1, ncol = 1)} else {
              i_chosen <<- rbind(i_chosen, values$int)}
            colnames(i_chosen) <<- "Chosen Interactions"
            i_chosen
          }, include.rownames = FALSE)
        })
        
        observeEvent(input$go,{
          
          ##  Generate Formula
          
            ##  Covariates
          
          if (exists("xform",where = .GlobalEnv)) rm(xform, envir = .GlobalEnv)
          for (i in 1:length(var)){
            ifelse(exists("xform",frame = sys.nframe()),xform <- paste(xform,"+",var[i]), xform <- var[1])
          }
          
            ##  Interactions
          
          if (exists("i_chosen", where = .GlobalEnv)) {
            for (i in 1:dim(i_chosen)[1]){
              xform <- paste(xform,"+",i_chosen[i,1])
            }
          }
          
          
          form <- as.formula(paste(res,"~",xform))
          
          ##  Model
          
          model <- lm(form, data = clean)
          
          output$model_sum <- renderDataTable({
            M <- format(round(coef(summary(model)),3),3)
            colnames(M)[4] <- "p value"
            M
          })
        })
      })
    })
    
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
  
shinyApp(ui = ui, server = server)