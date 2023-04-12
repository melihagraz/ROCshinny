library(shiny)
library(cutpointr)
library(pROC)
library(readxl)
library(dplyr)
library(ggplot2)

# Define UI
ui <- fluidPage(
  
  # App titleft
  titlePanel(""),
  
  # Sidebar panel
  sidebarLayout(
    sidebarPanel(
      
      # Select data file
      fileInput("datafile", "Choose a CSV file",
                accept = c("text/csv", "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Select predictor variable
      selectInput("xvar", "Select predictor variable", choices = NULL),
      
      # Select outcome variable
      selectInput("yvar", "Select outcome variable", choices = NULL),
      
      # Select method
      selectInput("method", "Select method", 
                  choices = c("youden_topleft", "cutoff", "maximized")),
      
      # Select index (only visible if method is "best")
      conditionalPanel(condition = "input.method == 'youden_topleft'",
                       selectInput("index", "Select index", 
                                   choices = c("youden", "closest.topleft"))
      ),
      
      # Select cutoff value (only visible if method is "cutoff")
      conditionalPanel(condition = "input.method == 'cutoff'",
                       numericInput("cutoff", "Enter cutoff value", value = 0.5)
      ),
      
      # Select constrain metric (only visible if method is "maximized")
      conditionalPanel(condition = "input.method == 'maximized'",
                       selectInput("constrain_metric", "Select constrain metric", 
                                   choices = c("specificity", "sensitivity")),
                       numericInput("min_cons", "Enter minimum constraint value", value = 0.5)
      ),
      
      # Run analysis button
      actionButton("run_analysis", "Run analysis"),
      
      downloadButton("download_table", "Download results table")
      
    ),
    
    # Main panel
    mainPanel(
      
      tabsetPanel(
        tabPanel("ROC Plot", plotOutput("roc_plot")),
        tabPanel("Table", tableOutput("roc_table")),
        tabPanel("Analysis", verbatimTextOutput("sales_text")),
        tabPanel("Data Preview", tableOutput("data_preview")),
        tabPanel("Summary", tableOutput("var_summary_table")),
        tabPanel("Distribution", plotOutput("dist_plot"))
      )
      
    )
  )
)

# Define server
server <- function(input, output, session) {
  source("/Users/melihagraz/Desktop/Research_Work/BapProje/gitshinny/ROCconv3.R")
  
  # Load data file
  data <- reactive({
    req(input$datafile)
    if(tools::file_ext(input$datafile$name) == "csv") {
      read.csv(input$datafile$datapath)
    } else if(tools::file_ext(input$datafile$name) %in% c("xls", "xlsx")) {
      read_excel(input$datafile$datapath, sheet = 1)
    }
  })
  
  # Display data preview
  output$data_preview <- renderTable({
    req(data())  
    head(data(), n = 4)  
  })
  
  output$dist_plot <- renderPlot({
    req(input$xvar, data())
    covariate <- sym(input$xvar)
    ggplot(data(), aes(x = !!covariate)) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "steelblue", alpha = 0.8) +
      geom_density(color = "red", size = 1.2) +
      labs(title = paste("Distribution of", input$xvar),
           x = input$xvar,
           y = "Density") +
      theme_minimal()
  })
  
  # Compute summary statistics for selected variable
  var_summary <- reactive({
    req(input$xvar)
    var <- sym(input$xvar)
    data() %>%
      summarise(across({{var}}, ~ list(Mean = mean(., na.rm = TRUE), SD = sd(., na.rm = TRUE), Median = median(., na.rm = TRUE))))
  })
  
  # Update predictor variable options based on selected data file
  observeEvent(input$datafile, {
    choices <- colnames(data())
    updateSelectInput(session, "xvar", choices = choices)
  })
  
  # Update outcome variable options based on selected data file
  observeEvent(input$datafile, {
    choices <- colnames(data())
    updateSelectInput(session, "yvar", choices = choices)
  })
  
  # Run analysis when "Run analysis" button is clicked
  analysis <- eventReactive(input$run_analysis, {
    xvar <- input$xvar
    yvar <- input$yvar
    method <- input$method
    index <- NULL
    if (method == "youden_topleft") {
      index <- input$index
    }
    cutoff <- NULL
    if (method == "cutoff") {
      cutoff <- input$cutoff
    }
    
    constrain_metric <- NULL
    min_cons <- NULL
    if (method == "maximized") {
      constrain_metric <- input$constrain_metric
      min_cons <- as.numeric(input$min_cons)
    }
    
    res <- ROC_fin(data(), xvar, yvar, method = method, index = index, cutoff = cutoff,
                   constrain_metric = constrain_metric, min_cons = min_cons)
    return(res)
  })
  
  # Display ROC plot
  output$roc_plot <- renderPlot({
    res <- analysis()
    if (is.list(res$best_res)) {
      plot(res$pl)
    } else if (is.list(res$specific_cutoff)){
        plot(res$pl)
      } else {
        plot(res$pl)
      }
  })
  
  output$roc_table <- renderTable({
    if (is.null(analysis())) {
      return(NULL)
    }
    
    res <- analysis()
    if (is.list(res$best_res)) {
      return(res$best_res)
    } else if (is.list(res$specific_cutoff)){
      return(res$specific_cutoff)
    } else {
      return(list(a=res$maximum_sensitivity, b=res$maximum_specificity))
    }
  })
  
  output$var_summary_table <- renderTable({
    summary_df <- var_summary()
    formatted_summary <- data.frame(
      Stat = c("Mean", "SD", "Median"),
      Result = c(summary_df[[1]]$Mean, summary_df[[1]]$SD, summary_df[[1]]$Median),
      stringsAsFactors = FALSE
    )
    formatted_summary
  })
  
}

shinyApp(ui = ui, server = server)
                 