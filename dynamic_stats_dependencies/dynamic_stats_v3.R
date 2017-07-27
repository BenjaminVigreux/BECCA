require(shiny)
require(shinyBS)
options(shiny.reactlog = TRUE)

##  Fix Tier 3 Collapse Panels
##  What to do about triads, stones and dyads
##  Make Proportional
##  Make Pretty!

ui <- shinyUI(fluidPage(
  
  headerPanel("Descriptive Statistics"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput(inputId = "unit",
                  label = "Number/Percentage",
                  choices = c("Number of Stories","Percentage")),
      hr(),
      
      ##  Rows
      
      h4("Rows"),
      
      selectInput(inputId = "x1",
                  label = "Choose Rows",
                  choices = c("Select","Triads","Dyads","Stones","Questions","Descriptors")),
      conditionalPanel(condition = "input.x1 !='Select'",
                       uiOutput("x1out")),
      conditionalPanel(condition = "input.x1 =='Questions'",
                       bsCollapse(
                         bsCollapsePanel(title = "Level",
                                         uiOutput("x_q1_factor")),
                         id = "x1_q",
                         multiple = FALSE,
                         open = NULL)),
      conditionalPanel(condition = "input.x1 == 'Descriptors'",
                       bsCollapse(
                         bsCollapsePanel(title = "Level",
                                         uiOutput("x_dq1_factor")),
                         id = "x1_dq",
                         multiple = FALSE,
                         open = NULL)),
      hr(),
      
      ##  Columns
      
      h4("Columns"),
      selectInput(inputId = "y1",
                  label = "Choose Columns",
                  choices = c("Select","Triads","Dyads","Stones","Questions","Descriptors")),
      conditionalPanel(condition = "input.y1 !='Select'",
                       uiOutput("y1out")),
      conditionalPanel(condition = "input.y1 =='Questions'",
                       bsCollapse(
                         bsCollapsePanel(title = "Level",
                                         uiOutput("y_q1_factor")),
                         id = "y1_q",
                         multiple = FALSE,
                         open = NULL)),
      conditionalPanel(condition = "input.y1 == 'Descriptors'",
                       bsCollapse(
                         bsCollapsePanel(title = "Level",
                                         uiOutput("y_dq1_factor")),
                         id = "y1_dq",
                         multiple = FALSE,
                         open = NULL)),
      actionButton("go","Build Statistics")),
      
    mainPanel(textOutput("x_chosen"),
              textOutput("y_chosen"),
              tableOutput("stats")
    )
  )))
  
  server <- shinyServer(function(input,output,session){
    
    dataset <- "moldova"
    wd <- getwd()
    
    load(paste0(wd,"/clean_data/",dataset,"_clean.RData"))
    
    #source("Data Cleaning.R")
    source("factor_to_integer.R")
    source("namegetter.R")
    source("sort_signifiers.R")
    
    factornames <- namegetter(clean)
    qnames <- factornames[[1]]
    dqnames <- factornames[[2]]
    
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
    
    types <- sigtypes(clean_bin) # Sig types so stones, triads, and dyads dropdowns work.
    
    for (i in 1:length(types)){
      assign(names(types)[i],types[[i]])
    }
    
    ##  Tier 2 Variable Selection
    
    output$unit <- renderText({input$unit})

    output$x1out <- renderUI({switch(input$x1,
                                     "Triads" = sidebarPanel(
                                       checkboxInput(inputId = "xt_all","All/None", value = FALSE),
                                       checkboxGroupInput(inputId = "x_t1",
                                                          label = "X Triad 1",
                                                          choices = names(triads))),
                                     "Dyads" = sidebarPanel(
                                       checkboxInput(inputId = "xd_all","All/None"),
                                       checkboxGroupInput(inputId = "x_d1",
                                                                  label = "X Dyad 1",
                                                                  choices = names(dyads))),
                                     "Stones" = sidebarPanel(
                                       checkboxInput(inputId = "xs_all","All/None"),
                                       checkboxGroupInput(inputId = "x_s1",
                                                          label = "X Stone 1",
                                                          choices = names(stones))),
                                     "Questions" = selectInput(inputId = "x_q1",
                                                               label = "X Question 1",
                                                               choices = c("Select",qnames),
                                                               selected = "Select"),
                                     "Descriptors" = selectInput(inputId = "x_dq1",
                                                                 label = "X Descriptor 1",
                                                                 choices = c("Select",dqnames),
                                                                 selected = "Select")
    )
    })

output$y1out <- renderUI({switch(input$y1,
                                 "Triads" = sidebarPanel(
                                   checkboxInput(inputId = "yt_all","All/None"),
                                   checkboxGroupInput(inputId = "y_t1",
                                                      label = "Y Triad 1",
                                                      choices = names(triads))),
                                 "Dyads" = sidebarPanel(
                                   checkboxInput(inputId = "yd_all","All/None"),
                                   checkboxGroupInput(inputId = "y_d1",
                                                      label = "Y Dyad 1",
                                                      choices = names(dyads))),
                                 "Stones" = sidebarPanel(
                                   checkboxInput(inputId = "ys_all","All/None"),
                                   checkboxGroupInput(inputId = "y_s1",
                                                      label = "Y Stone 1",
                                                      choices = names(stones))),
                                 "Questions" = selectInput(inputId = "y_q1",
                                                           label = "Y Question 1",
                                                           choices = c("Select",qnames),
                                                           selected = "Select"),
                                 "Descriptors" = selectInput(inputId = "y_dq1",
                                                             label = "Y Descriptor 1",
                                                             choices = c("Select", dqnames),
                                                             selected = "Select")
)
})
    
    ##  Tier 3 Level Selection
    
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
    
    
    ##  X Variables (rows)
    
    ##  X1
    
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
    
    ##  Y Variables (columns)
    
    ##  Y1
    
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
    
    ##  Observer
    
#    observe({
      ##  X Vars
      # x1
#      if (input$x1 != "Select") {req(input$x_q1, input$x_dq1) 
#        if (input$x_q1 != "Select") {updateCollapse(session,"x1_q",open = "Level")}
#        if (input$x_dq1 != "Select") {updateCollapse(session,"x1_dq",open = c("Level"))}}
#      
#      ##  Y Vars
#      # y1
#      if (input$y1 != "Select")
#      {if (exists("input$y_q1")) {if (input$y_q1 != "Select") {updateCollapse(session,"y1_q",open = c("Level"))}}
#        if (exists("input$y_dq1")) {if (input$y_dq1 != "Select") {updateCollapse(session,"y1_dq",open = c("Level"))}}}

#    })
        
    ##  Dataframe Construction
    
    output$x_chosen <- renderText({c(input$x_t1,input$x_d1,input$x_s1,input$x_q1_level,input$x_dq1_level)})
    output$y_chosen <- renderText({c(input$y_t1,input$y_d1,input$y_s1,input$y_q1_level,input$y_dq1_level)})
    
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
    
    observe({
      if (input$go == 0) # tells action button to do nothing when not clicked ..
        return()
    
    rnames <- reactive({c(input$x_t1,input$x_d1,input$x_s1,input$x_q1_level,input$x_dq1_level)})
    cnames <- reactive({c(input$y_t1,input$y_d1,input$y_s1,input$y_q1_level,input$y_dq1_level)})
    
#    xt <- reactive({input$x_t1})
#    xd <- reactive({input$x_d1})
#    xs <- reactive({input$x_s1})
#    xq <- reactive({input$x_q1_level})
#    xdq <- reactive({input$x_dq1_level})
    
#    xt <- input$x_t1
#    xd <- input$x_d1
#    xs <- input$x_s1
#    xq <- input$x_q1_level
#    xdq <- input$x_dq1_level
    
#    yt <- input$y_t1
#    yd <- input$y_d1
#    ys <- input$y_s1
#    yq <- input$y_q1_level
#    ydq <- input$y_dq1_level
    
#    dsrows <- reactive({c(input$x_t1,input$x_d1,input$x_s1,input$x_q1_level,input$x_dq1_level)})
#    dscols <- reactive({c(input$y_t1,input$y_d1,input$y_s1,input$y_q1_level,input$y_dq1_level)})
    
    isolate({
    
    values <- reactiveValues(dynamic_frame = clean_bin)
    
#    desc_stats <- read.csv("clean_bin.csv")
    
    observeEvent(input$go,{
      desc_rows <- values$dynamic_frame[,c(input$x_t1,input$x_d1,input$x_s1,input$x_q1_level,input$x_dq1_level)]
      desc_cols <- values$dynamic_frame[,c(input$y_t1,input$y_d1,input$y_s1,input$y_q1_level,input$y_dq1_level)]
      desc_stats <- matrix(NA, nrow = dim(desc_rows)[2], ncol = dim(desc_cols)[2])
      
      for (r in 1:dim(desc_rows)[2]){
        for (c in 1:dim(desc_cols)[2]){
          desc_stats[r,c] <- table(desc_rows[,r],desc_cols[,c])
        }
      }
      
      rownames(desc_stats) <- names(desc_rows)
      colnames(desc_stats) <- names(desc_cols)
      
      values$stats <- desc_stats
    })
    
    output$stats <- renderTable({values$stats})
    
    })})
    
    
  })
  
  shinyApp(ui = ui, server = server)