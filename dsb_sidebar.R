sidebarPanel(
  selectInput(inputId = "dataset",
              label = "Select Dataset",
              choices = c("Moldova","Kyrgyzstan UNDP","Kyrgyzstan Unicef","Serbia","Yemen","Tajikistan"),
              selected = "Moldova"),
  hr(),
  
  ##  Rows
  
  tags$div(
    
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
    conditionalPanel(condition = "input.x1 !='Select'",
                     actionButton("x_sub", "Submit"))),
  hr(),br(),
  
  ##  Columns
  
  tags$div(
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
    conditionalPanel(condition = "input.y1 !='Select'",
                     actionButton("y_sub", "Submit")),
    hr(),br()),
  fluidRow(actionButton("go","Build Statistics"),actionButton("reset","Reset")))