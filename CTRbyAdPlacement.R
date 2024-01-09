# Load packages
library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(rsconnect)

# Function to read data from uploaded file
read_data <- function(file_path) {
  read.csv(file_path, sep = ";")
}

# Default dataset
example_data <- data.frame(
  AdPlacement = rep(c("Left Sidebar", "Center Page", "Right Sidebar"), each = 10),
  CTR = c(2.5, 2.7, 2.8, 2.6, 3, 2.4, 2.9, 2.5, 2.6, 2.7,
          3.8, 3.5, 4, 3.7, 3.9, 3.6, 4.1, 3.4, 3.8, 3.9,
          3.1, 2.9, 3, 3.2, 3.3, 2.8, 3.4, 3.1, 3.2, 3.5)
)

# UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f7f7f7;
        background-image: url('background_image.jpg');  # Ganti dengan path ke gambar latar belakang Anda
        background-size: cover;
      }
    "))
  ),
  titlePanel("CTR Performance by Ad Placement Location", windowTitle = "Ad Placement Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Data (Excel or CSV format)", accept = c('.csv')),
      radioButtons("dataset_choice", "Choose Dataset", choices = c("Uploaded File", "Enter Data", "Example"), selected = "Uploaded File"),
      conditionalPanel(
        condition = "input.dataset_choice == 'Enter Data'",
        textInput("left_sidebar_ctr", "Left Sidebar CTR"),
        textInput("center_page_ctr", "Center Page CTR"),
        textInput("right_sidebar_ctr", "Right Sidebar CTR"),
        actionButton("analyze_entered_data", "Analyze Entered Data", icon = icon("play"))
      ),
      actionButton("analyze", "Analyze Data", icon = icon("play")),
      p("Upload your data, use example dataset, or enter data."),
      p("Click 'Analyze Data' to see the results.")
    ),
    
    mainPanel(
      plotOutput("ctrPlot"),
      verbatimTextOutput("summary")
    )
  ),
  theme = shinytheme("cosmo"),  # Ganti tema sesuai preferensi Anda
  tags$head(
    tags$style(
      HTML(
        "
        .btn-primary {
          background-color: #FF69B4;
          border-color: #FF69B4;
        }
        .btn-primary:hover {
          background-color: #4B0082;
          border-color: #4B0082;
        }
        .navbar {
          background-color: #FF69B4;
        }
        "
      )
    )
  )
)

server <- function(input, output) {
  selected_data <- reactive({
    if (input$dataset_choice == "Uploaded File") {
      req(input$file)
      read_data(input$file$datapath)
    } else if (input$dataset_choice == "Enter Data") {
      entered_data <- data.frame(
        AdPlacement = rep(c("Left Sidebar", "Center Page", "Right Sidebar"), each = 10),
        CTR = c(as.numeric(input$left_sidebar_ctr), as.numeric(input$center_page_ctr), as.numeric(input$right_sidebar_ctr))
      )
      return(entered_data)
    } else {
      example_data
    }
  })
  
  output$ctrPlot <- renderPlot({
    ggplot(selected_data(), aes(x = AdPlacement, y = CTR)) +
      geom_boxplot(fill = "#FF69B4", color = "#4B0082") +
      labs(title = "CTR Performance by Ad Placement",
           x = "Ad Placement",
           y = "Click-Through Rate (CTR)")
  })
  
  output$summary <- renderPrint({
    result <- aov(CTR ~ AdPlacement, data = selected_data())
    summary_result <- summary(result)
    
    p_value <- as.numeric(summary_result[[1]][["Pr(>F)"]][1])
    significance <- ifelse(p_value < 0.05, "Statistically Significant", "Not Statistically Significant")
    
    cat(capture.output(print(summary_result), type = "message"))
    cat(paste("P-Value:", p_value, "\n"))
    cat(paste("Significance:", significance, "\n"))
  })
}

# Run the app
shinyApp(ui, server)
