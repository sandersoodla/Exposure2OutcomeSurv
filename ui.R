library(shiny)
library(bslib)
library(networkD3)
library(DT)

# Define UI for application 
ui <- fluidPage(
  theme = bs_theme(bootswatch = "cerulean"),
  
  fluidRow(
    column(width = 11,
           # Application title
           titlePanel("Start condition to target condition overview")
    ),
    column(width = 1,
           textOutput("dbName"),
           textOutput("personCount")
    )
  ),
  
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        "startConditionId",
        "Start Condition concept",
        choices = NULL,
        selected = character(0),
        multiple = TRUE,
        options = list(
          plugins = list("remove_button"),
          placeholder = "Type to search conditions")
      ),
      textOutput("startConditionText"),
      hr(),
      selectizeInput(
        "targetConditionId",
        "Target Condition concept",
        choices = NULL,
        selected = character(0),
        multiple = TRUE,
        options = list(
          plugins = list("remove_button"),
          placeholder = "Type to search conditions")
      ),
      textOutput("targetConditionText"),
      actionButton("getData", "Get stats"),
      hr(),
      selectizeInput(
        "selectedPatient",
        "Select a Patient ID:",
        choices = NULL  # Initialize with no choices
      ),
      width = 3
    ),
    
    mainPanel(
      uiOutput("percentageOutputs"),
      
      tabsetPanel(
        tabPanel("Start to Target KM", uiOutput("kmPlotsUI")),
        tabPanel("Conditions Sankey",
                 sankeyNetworkOutput("sankeyPlotCondition"),
                 div(style = "min-height: 300px",
                     selectizeInput(
                       "hiddenConditions",
                       "Conditions to hide in Sankey Plot:",
                       choices = NULL,
                       multiple = TRUE,
                       selected = NULL,
                       options = list(
                         plugins = list("remove_button"),
                         placeholder = "Select conditions to hide")
                     )
                 )
        ),
        tabPanel("Procedures Sankey",
                 sankeyNetworkOutput("sankeyPlotProcedure"),
                 div(style = "min-height: 300px",
                     selectizeInput(
                       "hiddenProcedures",
                       "Procedures to hide in Sankey Plot:",
                       choices = NULL,
                       multiple = TRUE,
                       selected = NULL,
                       options = list(
                         plugins = list("remove_button"),
                         placeholder = "Select procedures to hide")
                     )
                 )
        ),
        tabPanel("Demographic overview", 
                 fluidRow(
                   column(6, uiOutput("startPyramidsUI")),
                   column(6, uiOutput("targetPyramidsUI"))
                 ),
        ),
        tabPanel("Patient Timeline", plotOutput("patientTimeline"))
      ),
      width = 9
    )
  )
)