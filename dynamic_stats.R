require(shiny)
require(shinyBS)

##  Fix triad, stones and dyads drop downs
##  Stop the drop downs from closing, and opening x1 factor when option is selected.
##  Make Descriptor levels display.
##  Make Proportional

ui <- shinyUI(fluidPage(
  
  headerPanel("Descriptive Statistics"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput(inputId = "unit",
                  label = "Number/Percentage",
                  choices = c("Number of Stories","Percentage")),
      hr(),
      
      ##  X Variables (rows)
      
      h4("X Variables (rows)"),
      
        ##  X1
      
      selectInput(inputId = "x1",
                  label = "X Variable 1",
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
      
        ##  X2
      
      conditionalPanel(condition = "input.x1 != 'Select'",
                       selectInput(inputId = "x2",
                                   label = "X Variable 2",
                                   choices = c("Select","Triads","Dyads","Stones","Questions","Descriptors"))),
      conditionalPanel(condition = "input.x2 !='Select'",
                       uiOutput("x2out")),
      conditionalPanel(condition = "input.x2 =='Questions'",
                       bsCollapse(
                         bsCollapsePanel(title = "Level",
                                         uiOutput("x_q2_factor")),
                         id = "x2_q",
                         multiple = FALSE,
                         open = NULL)),
      conditionalPanel(condition = "input.x2 == 'Descriptors'",
                       bsCollapse(
                         bsCollapsePanel(title = "Level",
                                         uiOutput("x_dq2_factor")),
                         id = "x2_dq",
                         multiple = FALSE,
                         open = NULL)),
      
        ##  X3
      
      conditionalPanel(condition = "input.x2 != 'Select'",
                       selectInput(inputId = "x3",
                                   label = "X Variable 3",
                                   choices = c("Select","Triads","Dyads","Stones","Questions","Descriptors"))),
      conditionalPanel(condition = "input.x3 !='Select'",
                       uiOutput("x3out")),
      conditionalPanel(condition = "input.x3 =='Questions'",
                       bsCollapse(
                         bsCollapsePanel(title = "Level",
                                         uiOutput("x_q3_factor")),
                         id = "x3_q",
                         multiple = FALSE,
                         open = NULL)),
      conditionalPanel(condition = "input.x3 == 'Descriptors'",
                       bsCollapse(
                         bsCollapsePanel(title = "Level",
                                         uiOutput("x_dq3_factor")),
                         id = "x3_dq",
                         multiple = FALSE,
                         open = NULL)),
      
        ##  X4
      
      conditionalPanel(condition = "input.x3 != 'Select'",
                       selectInput(inputId = "x4",
                                   label = "X Variable 4",
                                   choices = c("Select","Triads","Dyads","Stones","Questions","Descriptors"))),
      conditionalPanel(condition = "input.x4 !='Select'",
                       uiOutput("x4out")),
      conditionalPanel(condition = "input.x4 =='Questions'",
                       bsCollapse(
                         bsCollapsePanel(title = "Level",
                                         uiOutput("x_q4_factor")),
                         id = "x4_q",
                         multiple = FALSE,
                         open = NULL)),
      conditionalPanel(condition = "input.x4 == 'Descriptors'",
                       bsCollapse(
                         bsCollapsePanel(title = "Level",
                                         uiOutput("x_dq4_factor")),
                         id = "x4_dq",
                         multiple = FALSE,
                         open = NULL)),
      hr(),
      
      ##  Y Variables (columns)
      
        ##  Y1
      
      h4("Y Variables (columns)"),
      selectInput(inputId = "y1",
                  label = "Y Variable 1",
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
      
        ##  Y2
      
      conditionalPanel(condition = "input.y1 != 'Select'",
                       selectInput(inputId = "y2",
                                   label = "Y Variable 2",
                                   choices = c("Select","Triads","Dyads","Stones","Questions","Descriptors"))),
      conditionalPanel(condition = "input.y2 !='Select'",
                       uiOutput("y2out")),
      conditionalPanel(condition = "input.y2 =='Questions'",
                       bsCollapse(
                         bsCollapsePanel(title = "Level",
                                         uiOutput("y_q2_factor")),
                         id = "y2_q",
                         multiple = FALSE,
                         open = NULL)),
      conditionalPanel(condition = "input.y2 == 'Descriptors'",
                       bsCollapse(
                         bsCollapsePanel(title = "Level",
                                         uiOutput("y_dq2_factor")),
                         id = "y2_dq",
                         multiple = FALSE,
                         open = NULL)),
      ##  Y3
      
      conditionalPanel(condition = "input.y2 != 'Select'",
                       selectInput(inputId = "y3",
                                   label = "Y Variable 3",
                                   choices = c("Select","Triads","Dyads","Stones","Questions","Descriptors"))),
      conditionalPanel(condition = "input.y3 !='Select'",
                       uiOutput("y3out")),
      conditionalPanel(condition = "input.y3 =='Questions'",
                       bsCollapse(
                         bsCollapsePanel(title = "Level",
                                         uiOutput("y_q3_factor")),
                         id = "y3_q",
                         multiple = FALSE,
                         open = NULL)),
      conditionalPanel(condition = "input.y3 == 'Descriptors'",
                       bsCollapse(
                         bsCollapsePanel(title = "Level",
                                         uiOutput("y_dq3_factor")),
                         id = "y3_dq",
                         multiple = FALSE,
                         open = NULL)),
      
      ##  Y4
      
      conditionalPanel(condition = "input.y3 != 'Select'",
                       selectInput(inputId = "y4",
                                   label = "Y Variable 4",
                                   choices = c("Select","Triads","Dyads","Stones","Questions","Descriptors"))),
      conditionalPanel(condition = "input.y4 !='Select'",
                       uiOutput("y4out")),
      conditionalPanel(condition = "input.y4 =='Questions'",
                       bsCollapse(
                         bsCollapsePanel(title = "Level",
                                         uiOutput("y_q4_factor")),
                         id = "y4_q",
                         multiple = FALSE,
                         open = NULL)),
      conditionalPanel(condition = "input.y4 == 'Descriptors'",
                       bsCollapse(
                         bsCollapsePanel(title = "Level",
                                         uiOutput("y_dq4_factor")),
                         id = "y4_dq",
                         multiple = FALSE,
                         open = NULL))),
    
    mainPanel(textOutput("x1_chosen")
              #tableOutput("rows"),
              #tableOutput("cols"),
              #tableOutput("stats")
    )
  )))
  
  server <- shinyServer(function(input,output,session){
    
    #source("Data Cleaning.R")
    source("factor_to_integer.R")
    source("namegetter.R")
    source("sort_signifiers.R")
    load(paste0(dataset,"_factornames.RData"))
    load(paste0(dataset,"_clean.RData"))
    
    clean_bin <- factor2int(clean, integrate = TRUE, ignore = 20, keepFactor = FALSE)
    types <- sigtypes(clean_bin) # Sig types so stones, triads, and dyads dropdowns work.
    
    ##  Tier 2 Variable Selection
    
    output$unit <- renderText({input$unit})

    output$x1out <- renderUI({switch(input$x1,
                                     "Triads" = checkboxGroupInput(inputId = "x_t1",
                                                                   label = "X Triad 1",
                                                                   choices = c("Select",names(triads)),
                                                                   selected = "Select"),
                                     "Dyads" = checkboxGroupInput(inputId = "x_d1",
                                                                  label = "X Dyad 1",
                                                                  choices = c("Select",names(dyads)),
                                                                  selected = "Select"),
                                     "Stones" = checkboxGroupInput(inputId = "x_s1",
                                                                   label = "X Stone 1",
                                                                   choices = c("Select",names(stones)),
                                                                   selected = "Select"),
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
  
  output$x2out <- renderUI({switch(input$x2,
                                   "Triads" = checkboxGroupInput(inputId = "x_t2",
                                                                 label = "X Triad 2",
                                                                 choices = c("Select",names(triads)),
                                                                 selected = "Select"),
                                   "Dyads" = checkboxGroupInput(inputId = "x_d2",
                                                                label = "X Dyad 2",
                                                                choices = c("Select",names(dyads)),
                                                                selected = "Select"),
                                   "Stones" = checkboxGroupInput(inputId = "x_s2",
                                                                 label = "X Stone 2",
                                                                 choices = c("Select",names(stones)),
                                                                 selected = "Select"),
                                   "Questions" = selectInput(inputId = "x_q2",
                                                             label = "X Question 2",
                                                             choices = c("Select",qnames),
                                                             selected = "Select"),
                                   "Descriptors" = selectInput(inputId = "x_dq2",
                                                               label = "X Descriptor 2",
                                                               choices = c("Select",dqnames),
                                                               selected = "Select")
                                   )
  })
output$x2_chosen <- renderText({input$x2})

output$x3out <- renderUI({switch(input$x3,
                                 "Triads" = checkboxGroupInput(inputId = "x_t3",
                                                               label = "X Triad 3",
                                                               choices = c("Select",names(triads)),
                                                               selected = "Select"),
                                 "Dyads" = checkboxGroupInput(inputId = "x_d3",
                                                              label = "X Dyad 3",
                                                              choices = c("Select",names(dyads)),
                                                              selected = "Select"),
                                 "Stones" = checkboxGroupInput(inputId = "x_s3",
                                                               label = "X Stone 3",
                                                               choices = c("Select",names(stones)),
                                                               selected = "Select"),
                                 "Questions" = selectInput(inputId = "x_q3",
                                                           label = "X Question 3",
                                                           choices = c("Select", qnames),
                                                           selected = "Select"),
                                 "Descriptors" = selectInput(inputId = "x_dq3",
                                                             label = "X Descriptor 3",
                                                             choices = c("Select", dqnames),
                                                             selected = "Select")
                                 )

})
output$x3_chosen <- renderText({input$x3})

output$x4out <- renderUI({switch(input$x4,
                                 "Triads" = checkboxGroupInput(inputId = "x_t4",
                                                               label = "X Triad 4",
                                                               choices = c("Select",names(triads)),
                                                               selected = "Select"),
                                 "Dyads" = checkboxGroupInput(inputId = "x_d4",
                                                              label = "X Dyad 4",
                                                              choices = c("Select",names(dyads)),
                                                              selected = "Select"),
                                 "Stones" = checkboxGroupInput(inputId = "x_s4",
                                                               label = "X Stone 4",
                                                               choices = c("Select",names(stones)),
                                                               selected = "Select"),
                                 "Questions" = selectInput(inputId = "x_q4",
                                                           label = "X Question 4",
                                                           choices = c("Select", qnames),
                                                           selected = "Select"),
                                 "Descriptors" = selectInput(inputId = "x_dq4",
                                                             label = "X Descriptor 4",
                                                             choices = c("Select", dqnames),
                                                             selected = "Select")
)
})
output$x4_chosen <- renderText({input$x4})

output$y1out <- renderUI({switch(input$y1,
                                 "Triads" = checkboxGroupInput(inputId = "y_t1",
                                                               label = "Y Triad 1",
                                                               choices = c("Select",names(triads)),
                                                               selected = "Select"),
                                 "Dyads" = checkboxGroupInput(inputId = "y_d1",
                                                              label = "Y Dyad 1",
                                                              choices = c("Select",names(dyads)),
                                                              selected = "Select"),
                                 "Stones" = checkboxGroupInput(inputId = "y_s1",
                                                               label = "Y Stone 1",
                                                               choices = c("Select",names(stones)),
                                                               selected = "Select"),
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

output$y2out <- renderUI({switch(input$y2,
                                 "Triads" = checkboxGroupInput(inputId = "y_t2",
                                                               label = "Y Triad 2",
                                                               choices = c("Select",names(triads)),
                                                               selected = "Select"),
                                 "Dyads" = checkboxGroupInput(inputId = "y_d2",
                                                              label = "Y Dyad 2",
                                                              choices = c("Select",names(dyads)),
                                                              selected = "Select"),
                                 "Stones" = checkboxGroupInput(inputId = "y_s2",
                                                               label = "Y Stone 2",
                                                               choices = c("Select",names(stones)),
                                                               selected = "Select"),
                                 "Questions" = selectInput(inputId = "y_q2",
                                                           label = "X Question 2",
                                                           choices = c("Select", qnames),
                                                           selected = "Select"),
                                 "Descriptors" = selectInput(inputId = "y_dq2",
                                                             label = "Y Descriptor 2",
                                                             choices = c("Select", dqnames),
                                                             selected = "Select")
)
})
output$y2_chosen <- renderText({input$y2})

output$y3out <- renderUI({switch(input$y3,
                                 "Triads" = checkboxGroupInput(inputId = "y_t3",
                                                               label = "Y Triad 3",
                                                               choices = c("Select",names(triads)),
                                                               selected = "Select"),
                                 "Dyads" = checkboxGroupInput(inputId = "y_d3",
                                                              label = "Y Dyad 3",
                                                              choices = c("Select",names(dyads)),
                                                              selected = "Select"),
                                 "Stones" = checkboxGroupInput(inputId = "y_s3",
                                                               label = "Y Stone 3",
                                                               choices = c("Select",names(stones)),
                                                               selected = "Select"),
                                 "Questions" = selectInput(inputId = "y_q3",
                                                           label = "Y Question 3",
                                                           choices = c("Select", qnames),
                                                           selected = "Select"),
                                 "Descriptors" = selectInput(inputId = "y_dq3",
                                                             label = "Y Descriptor 3",
                                                             choices = c("Select", dqnames),
                                                             sselected = "Select")
)
})
output$y3_chosen <- renderText({input$y3})

output$y4out <- renderUI({switch(input$y4,
                                 "Triads" = checkboxGroupInput(inputId = "y_t4",
                                                               label = "Y Triad 4",
                                                               choices = c("Select",names(triads)),
                                                               selected = "Select"),
                                 "Dyads" = checkboxGroupInput(inputId = "y_d4",
                                                              label = "Y Dyad 4",
                                                              choices = c("Select",names(dyads)),
                                                              selected = "Select"),
                                 "Stones" = checkboxGroupInput(inputId = "y_s4",
                                                               label = "Y Stone 4",
                                                               choices = c("Select",names(stones)),
                                                               selected = "Select"),
                                 "Questions" = selectInput(inputId = "y_q4",
                                                           label = "Y Question 4",
                                                           choices = c("Select", qnames),
                                                           selected = "Select"),
                                 "Descriptors" = selectInput(inputId = "y_dq4",
                                                             label = "Y Descriptor 4",
                                                             choices = c("Select", dqnames),
                                                             selected = "Select")
                                 )
                                 
})
output$y4_chosen <- renderText({input$y4})
    
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
    
    output$x_q1_factor <- renderUI({
      checkboxGroupInput(inputId = "x_q1_level",
                         label = "X Factor Level",
                         choices = c("Select",names(clean_bin[,grep(paste0("^Q",x_q1(),"."),names(clean_bin),value = TRUE)])),
                         selected = "Select")
    })
    output$x_dq1_factor <- renderUI({
      checkboxGroupInput(inputId = "x_dq1_level",
                         label = "X Factor Level",
                         choices = c("Select",names(clean_bin[,grep(paste0("^DQ",x_dq1(),"."),names(clean_bin),value = TRUE)])),
                         selected = "Select")
    })
    
    ##  X2
    
    x_q2 <- renderText({list_qnames[match.arg(input$x_q2,c("Select",names(list_qnames)))]})
    x_dq2 <- renderText({list_dqnames[match.arg(input$x_dq2,c("Select",names(list_dqnames)))]})
    
    output$x_q2_factor <- renderUI({
      checkboxGroupInput(inputId = "x_q2_level",
                         label = "X Factor Level",
                         choices = c("Select",names(clean_bin[,grep(paste0("^Q",x_q2(),"."),names(clean_bin),value = TRUE)])),
                         selected = "Select")
    })
    output$x_dq2_factor <- renderUI({
      checkboxGroupInput(inputId = "x_dq2_level",
                         label = "X Factor Level",
                         choices = c("Select",names(clean_bin[,grep(paste0("^DQ",x_dq2(),"."),names(clean_bin),value = TRUE)])),
                         selected = "Select")
    })
    
    ##  X3
    
    x_q3 <- renderText({list_qnames[match.arg(input$x_q3,c("Select",names(list_qnames)))]})
    x_dq3 <- renderText({list_dqnames[match.arg(input$x_dq3,c("Select",names(list_dqnames)))]})
    
    output$x_q3_factor <- renderUI({
      checkboxGroupInput(inputId = "x_q3_level",
                         label = "X Factor Level",
                         choices = c("Select",names(clean_bin[,grep(paste0("^Q",x_q3(),"."),names(clean_bin),value = TRUE)])),
                         selected = "Select")
    })
    output$x_dq3_factor <- renderUI({
      checkboxGroupInput(inputId = "x_dq3_level",
                         label = "X Factor Level",
                         choices = c("Select",names(clean_bin[,grep(paste0("^DQ",x_dq3(),"."),names(clean_bin),value = TRUE)])),
                         selected = "Select")
    })
    
    ##  X4
    
    x_q4 <- renderText({list_qnames[match.arg(input$x_q4,c("Select",names(list_qnames)))]})
    x_dq4 <- renderText({list_dqnames[match.arg(input$x_dq4,c("Select",names(list_dqnames)))]})
    
    output$x_q4_factor <- renderUI({
      checkboxGroupInput(inputId = "x_q4_level",
                         label = "X Factor Level",
                         choices = c("Select",names(clean_bin[,grep(paste0("^Q",x_q4(),"."),names(clean_bin),value = TRUE)])),
                         selected = "Select")
    })
    output$x_dq4_factor <- renderUI({
      checkboxGroupInput(inputId = "x_dq4_level",
                         label = "X Factor Level",
                         choices = c("Select",names(clean_bin[,grep(paste0("^DQ",x_dq4(),"."),names(clean_bin),value = TRUE)])),
                         selected = "Select")
    })
    
    ##  Y Variables (columns)
    
    ##  Y1
    
    y_q1 <- renderText({list_qnames[match.arg(input$y_q1,c("Select",names(list_qnames)))]})
    y_dq1 <- renderText({list_dqnames[match.arg(input$y_dq1,c("Select",names(list_dqnames)))]})
    
    output$y_q1_factor <- renderUI({
      checkboxGroupInput(inputId = "y_q1_level",
                         label = "Y Factor Level",
                         choices = c("Select",names(clean_bin[,grep(paste0("^Q",y_q1(),"."),names(clean_bin),value = TRUE)])),
                         selected = "Select")
    })
    output$y_dq1_factor <- renderUI({
      checkboxGroupInput(inputId = "y_dq1_level",
                         label = "Y Factor Level",
                         choices = c("Select",names(clean_bin[,grep(paste0("^DQ",y_dq1(),"."),names(clean_bin),value = TRUE)])),
                         selected = "Select")
    })
    
    
    ##  Y2
    
    y_q2 <- renderText({list_qnames[match.arg(input$y_q2,c("Select",names(list_qnames)))]})
    y_dq2 <- renderText({list_dqnames[match.arg(input$y_dq2,c("Select",names(list_dqnames)))]})
    
    output$y_q2_factor <- renderUI({
      checkboxGroupInput(inputId = "y_q2_level",
                         label = "Y Factor Level",
                         choices = c("Select",names(clean_bin[,grep(paste0("^Q",y_q2(),"."),names(clean_bin),value = TRUE)])),
                         selected = "Select")
    })
    output$y_dq2_factor <- renderUI({
      checkboxGroupInput(inputId = "y_dq2_level",
                         label = "Y Factor Level",
                         choices = c("Select",names(clean_bin[,grep(paste0("^DQ",y_dq2(),"."),names(clean_bin),value = TRUE)])),
                         selected = "Select")
    })
    
    ##  Y3
    
    y_q3 <- renderText({list_qnames[match.arg(input$y_q3,c("Select",names(list_qnames)))]})
    y_dq3 <- renderText({list_dqnames[match.arg(input$y_dq3c,c("Select",names(list_dqnames)))]})
    
    output$y_q3_factor <- renderUI({
      checkboxGroupInput(inputId = "y_q3_level",
                         label = "Y Factor Level",
                         choices = c("Select",names(clean_bin[,grep(paste0("^Q",y_q3(),"."),names(clean_bin),value = TRUE)])),
                         selected = "Select")
    })
    output$y_dq3_factor <- renderUI({
      checkboxGroupInput(inputId = "y_dq3_level",
                         label = "Y Factor Level",
                         choices = c("Select",names(clean_bin[,grep(paste0("^DQ",y_dq3(),"."),names(clean_bin),value = TRUE)])),
                         selected = "Select")
    })
    
    ##  Y4
    
    y_q4 <- renderText({list_qnames[match.arg(input$y_q4,c("Select",names(list_qnames)))]})
    y_dq4 <- renderText({list_dqnames[match.arg(input$y_dq4,c("Select",names(list_dqnames)))]})
    
    output$y_q4_factor <- renderUI({
      checkboxGroupInput(inputId = "y_q4_level",
                         label = "Y Factor Level",
                         choices = c("Select",names(clean_bin[,grep(paste0("^Q",y_q4(),"."),names(clean_bin),value = TRUE)])),
                         selected = "Select")
    })
    output$y_dq4_factor <- renderUI({
      checkboxGroupInput(inputId = "y_dq4_level",
                         label = "Y Factor Level",
                         choices = c("Select",names(clean_bin[,grep(paste0("^DQ",y_dq4(),"."),names(clean_bin),value = TRUE)])),
                         selected = "Select")
    })
                                    
                                    
    ##  Observer
    
    observe({
      ##  X Vars
      # x1
      if (input$x1 != "Select")
      {if (exists("input$x_q1")) {if (input$x_q1 != "Select") {updateCollapse(session,"x1_q",open = c("Level"))}}
        if (exists("input$x_dq1")) {if (input$x_dq1 != "Select") {updateCollapse(session,"x1_dq",open = c("Level"))}}}
      # x2
      if (input$x2 != "Select")
      {if (exists("input$x_q2")) {if (input$x_q2 != "Select") {updateCollapse(session,"x2_q",open = c("Level"))}}
        if (exists("input$x_dq2")) {if (input$x_dq2 != "Select") {updateCollapse(session,"x2_dq",open = c("Level"))}}}
      # x3
      if (input$x3 != "Select")
      {if (exists("input$x_q3")) {if (input$x_q3 != "Select") {updateCollapse(session,"x3_q",open = c("Level"))}}
        if (exists("input$x_dq3")) {if (input$x_dq3 != "Select") {updateCollapse(session,"x3_dq",open = c("Level"))}}}
      # x4
      if (input$x4 != "Select")
      {if (exists("input$x_q4")) {if (input$x_q4 != "Select") {updateCollapse(session,"x4_q",open = c("Level"))}}
        if (exists("input$x_dq4")) {if (input$x_dq4 != "Select") {updateCollapse(session,"x4_dq",open = c("Level"))}}}
      
      ##  Y Vars
      # y1
      if (input$y1 != "Select")
      {if (exists("input$y_q1")) {if (input$y_q1 != "Select") {updateCollapse(session,"y1_q",open = c("Level"))}}
        if (exists("input$y_dq1")) {if (input$y_dq1 != "Select") {updateCollapse(session,"y1_dq",open = c("Level"))}}}
      # y2
      if (input$y2 != "Select")
      {if (exists("input$y_q2")) {if (input$y_q2 != "Select") {updateCollapse(session,"y2_q",open = c("Level"))}}
        if (exists("input$y_dq2")) {if (input$y_dq2 != "Select") {updateCollapse(session,"y2_dq",open = c("Level"))}}}
      # y3
      if (input$y3 != "Select")
      {if (exists("input$y_q3")) {if (input$y_q3 != "Select") {updateCollapse(session,"y3_q",open = c("Level"))}}
        if (exists("input$y_dq3")) {if (input$y_dq3 != "Select") {updateCollapse(session,"y3_dq",open = c("Level"))}}}
      # y4
      if (input$y4 != "Select")
      {if (exists("input$y_q4")) {if (input$y_q4 != "Select") {updateCollapse(session,"y4_q",open = c("Level"))}}
        if (exists("input$y_dq4")) {if (input$y_dq4 != "Select") {updateCollapse(session,"y4_dq",open = c("Level"))}}}
    })
        
    ##  Dataframe Construction
    
    output$x1_chosen <- renderText({c(input$x_t1,input$x_d1,input$x_s1,input$x_q1_level,input$x_dq1_level)})
    output$x1_chosen <- renderText({c(input$x_t1,input$x_d1,input$x_s1,input$x_q1_level,input$x_dq1_level)})

  })
  
  shinyApp(ui = ui, server = server)
  