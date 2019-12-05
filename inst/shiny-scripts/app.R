library(shiny)

# See above for the definitions of ui and server
ui <- fluidPage(

  # App title ----
  titlePanel("Let's build some circular phylogenetic tree with ring annotation "),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      numericInput("inputNum",
                   label = "Enter the number",
                   value = 20,
                   min = 10,
                   max = 250),

      # Horizontal line ----
      tags$hr(),

      # Input: Select a file ----
      fileInput("input1",
                label = "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),

      # Horizontal line ----
      tags$hr(),

      # Input: Select a file ----
      fileInput("input2",
                label = "Choose Newick File",
                multiple = FALSE)

    ),


    # Main panel for displaying outputs ----
    mainPanel(
      fluidRow(
        textOutput("selected_var")
      ),
      fluidRow(
        plotOutput("plot1", height = 800)
      )

    )
  )
)


server <- function(input, output) {

  output$selected_var <- renderText({
    paste("Number of input:", input$inputNum)
  })

  output$plot1 <- renderPlot({

    req(input$inputNum)
    req(input$input1)
    req(input$input2)

    table <- read.csv(input$input1$datapath)
    tree <- ape::read.tree(input$input2$datapath)

    plot1 <- VFtree::combinedPlot(inputTable = table,
                         inputTree = tree,
                         inputNum = input$inputNum)

    return(plot1)
  })

}


shinyApp(ui = ui, server = server)
