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
           titlePanel("Exposure to outcome condition overview")
    ),
    column(width = 1,
           textOutput("dbName"),
           textOutput("personCount")
    )
  ),
  
  
  sidebarLayout(
    sidebarPanel(
      fileInput("exposureConditionFile", "Upload Exposure Conditions CSV", accept = ".csv"),
      
      selectizeInput(
        "exposureConditionIds",
        "or select exposure condition concepts:",
        choices = NULL,
        selected = character(0),
        multiple = TRUE,
        options = list(
          plugins = list("remove_button"),
          placeholder = "Type to search conditions")
      ),
      hr(),
      
      fileInput("outcomeConditionFile", "Upload Outcome Conditions CSV", accept = ".csv"),
      
      selectizeInput(
        "outcomeConditionIds",
        "or select outcome condition concepts:",
        choices = NULL,
        selected = character(0),
        multiple = TRUE,
        options = list(
          plugins = list("remove_button"),
          placeholder = "Type to search conditions")
      ),
      hr(),
      
      # --- Compute and Save section
      h5("Compute & Save Results"),
      # Input for naming the result set before saving
      textInput("saveFileName", "Enter name for this result set:", 
                # Provide a default name, based on the date
                value = paste0("km_results_", Sys.Date())), 
      
      # Button to trigger the KM calculation and saving process
      actionButton("runAnalysis", "Run Analysis", icon = icon("calculator"), class = "btn-primary"),
      hr(),
      
      # --- Load Section ---
      h5("Load Saved Results"),
      # Dropdown to select a previously saved result file
      # Choices are populated dynamically by the server
      selectInput("selectLoadFile", "Select saved result set:", choices = NULL), 
      
      # Button to trigger loading the selected file
      actionButton("loadButton", "Load Selected Results", icon = icon("folder-open")),
      
      br(),
      br(),
      
      # Text output to display the name of the currently loaded file
      h6("Status:"),
      verbatimTextOutput("currentResultSet"),
      
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Exposure to Outcome KM",
                 h4("KM Analysis Summary"),
                 p("Select one or more rows below to view plots."),
                 
                 # Output for the interactive summary table
                 DT::dataTableOutput("kmSummaryTable"),
                 
                 hr(),
                 
                 h4("Selected KM Plots"),
                 
                 # UI Output container where the server will dynamically generate the grid of plots
                 uiOutput("kmPlotsUI")
        ),
        tabPanel("Demographic overview", 
                 fluidRow(
                   column(6, uiOutput("startPyramidsUI")),
                   column(6, uiOutput("targetPyramidsUI"))
                 ),
        ),
        tabPanel("Patient Timeline",
                 selectizeInput(
                   "selectedPatient",
                   "Select a Patient ID:",
                   choices = NULL  # Initialize with no choices
                 ),
                 hr(),
                 plotOutput("patientTimeline")
        )
      ),
      width = 9
    )
  )
)