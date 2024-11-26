library(shiny)
library(plotly)
library(CRISPRAid)

# Define UI for the app
ui <- fluidPage(

  # App title
  titlePanel(tags$h1(tags$b("CRISPRAid:"),
                     "Comprehensive CRISPR-Cas9 Data Analysis Tool")),

  # Sidebar layout with input and outpout definitions
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload gRNA Data (.csv):",
                accept = c("text/csv", "text/comma-separated-values. text/plain", ".csv")),
      selectInput("analysis", "Select Analysis Type:",
                  choices = c("Visualize gRNA Base Composition" = "base_composition",
                              "Visualize gRNA Efficiency" = "efficiency",
                              "Analyze gRNA Efficiency" = "analyze_efficiency",
                              "Compute Indels" = "compute_indels",
                              "Evaluate gRNA Specificity" = "evaluate_specificity",
                              "Simulate Off-Target Effects" = "simulate_off_target",
                              "Plot Off-Target Effects" = "plot_off_target")),
      actionButton("analyze", "Analyze Data"),
      downloadButton("download_demo", "Download Demo Data")
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Instructions",
                           h4("Instructions: Upload data and select an analysis type to visualize."),
                           br(),
                           h5("Supported analyses:"),
                           tags$ul(
                             tags$li("Base Composition Visualization: Displays the nucleotide composition of gRNA sequences."),
                             tags$li("Efficiency Heatmap: Shows a heatmap of gRNA efficiency values."),
                             tags$li("Analyze gRNA Efficiency: Computes the efficiency of a gRNA sequence."),
                             tags$li("Compute Indels: Calculates the frequency of indels in sequencing data."),
                             tags$li("Evaluate gRNA Specificity: Evaluates similarity to potential off-target sequences."),
                             tags$li("Simulate Off-Target Effects: Simulates potential off-target binding sites."),
                             tags$li("Plot Off-Target Effects: Visualizes off-target effects.")
                           )
                  ),
                  tabPanel("Input Summary",
                           h4("Summary of Uploaded Data:"),
                           verbatimTextOutput("textOut")),
                  tabPanel("Base Composition",
                           h4("Bar Plot of Base Composition:"),
                           plotOutput("baseCompositionPlot")),
                  tabPanel("Efficiency Heatmap",
                           h4("Heatmap of gRNA Efficiency:"),
                           plotOutput("efficiencyHeatmap")),
                  tabPanel("Analyze Efficiency",
                           h4("gRNA Efficiency Result:"),
                           verbatimTextOutput("efficiencyResult")),
                  tabPanel("Compute Indels",
                           h4("Indel Count:"),
                           tableOutput("indelResult")),
                  tabPanel("Evaluate Specificity",
                           h4("Specificity Scores:"),
                           tableOutput("specificityResult")),
                  tabPanel("Simulate Off-Target Effects",
                           h4("Simulated Off-Target Effects:"),
                           tableOutput("simulationResult")),
                  tabPanel("Plot Off-Target Effects",
                           h4("Off-Target Effects Plot:"),
                           plotlyOutput("offTargetPlot"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {

  # Step 1: Save the uploaded input CSV as a reactive object
  uploadedData <- reactive({
    req(input$file)
    read.csv(input$file$datapath, sep = ",", header = TRUE)
  })

  # Step 2: Define the selected analysis as a reactive object
  analysisResult <- reactive({
    req(uploadedData())
    data <- uploadedData()

    switch(input$analysis,
           base_composition = {
             gRNA_seqs <- data$gRNA
             visualize_gRNA_base_composition(gRNA_seqs)
           },
           efficiency = {
             visualize_gRNA_efficiency(data)
           },
           analyze_efficiency = {
             gRNA_seq <- data$gRNA[1]
             target_seq <- data$target[1]
             analyze_gRNA_efficiency(gRNA_seq, target_seq)
           },
           compute_indels = {
             compute_indels(data, indel_thres = 0.1)
           },
           evaluate_specificity = {
             gRNA_seq <- data$gRNA[1]
             off_target_seqs <- unlist(strsplit(data$off_target_seqs[1], ","))
             evaluate_gRNA_specificity(gRNA_seq, off_target_seqs)
           },
           simulate_off_target = {
             gRNA_seq <- data$gRNA[1]
             simulate_off_target_effects(gRNA_seq, genome_length = 1000, num_sites = 10)
           },
           plot_off_target = {
             plot_off_target_map(data)
           },
           stop("Invalid analysis type selected.")
    )
  })

  # Step 3: Render outputs for each feature
  output$baseCompositionPlot <- renderPlot({
    req(input$analysis == "base_composition", analysisResult())
    analysisResult()
  })

  output$efficiencyHeatmap <- renderPlot({
    req(input$analysis == "efficiency", analysisResult())
    analysisResult()
  })

  output$efficiencyResult <- renderPrint({
    req(input$analysis == "analyze_efficiency", analysisResult())
    analysisResult()
  })

  output$indelResult <- renderTable({
    req(input$analysis == "compute_indels", analysisResult())
    analysisResult()
  })

  output$specificityResult <- renderTable({
    req(input$analysis == "evaluate_specificity", analysisResult())
    analysisResult()
  })

  output$simulationResult <- renderTable({
    req(input$analysis == "simulate_off_target", analysisResult())
    analysisResult()
  })

  output$offTargetPlot <- renderPlotly({
    req(input$analysis == "plot_off_target", analysisResult())
    analysisResult()
  })

  # Step 4: Summary of uploaded data
  output$textOut <- renderPrint({
    req(uploadedData())
    summary(uploadedData())
  })

  # Step 5: Render the summary of uploaded data
  output$textOut <- renderPrint({
    req(uploadedData())
    summary(uploadedData())
  })

  # Step 6: Demo data download
  output$download_demo <- downloadHandler(
    filename = function() { "demo_data.csv" },
    content = function(file) {
      demo_path <- system.file("extdata", "demo_data.csv", package = "CRISPRAid")
      file.copy(demo_path, file)
    }
  )
}

# Create Shiny app
shinyApp(ui, server)
