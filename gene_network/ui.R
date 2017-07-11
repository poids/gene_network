library(shiny)
library(igraph)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Gene Co-Expression Network"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       sliderInput("sam_size",
                   "Sample Size (# of Genes):",
                   min = 50,
                   max = 1000,
                   value = 500,
                   step = 50),
       
       sliderInput("cutoff",
                   "Correlation Threshold",
                   min = 0.50,
                   max = 1.00,
                   value = 0.85,
                   step = 0.05),
       
       
       selectInput("gene1",
                   label = h3("Select Gene 1:"),
                   names(distMatTEST[distMatTEST[,1]!=0,]),
                   selected = names(distMatTEST[distMatTEST[,1]!=0,])[1]
                   )#,
       
       # selectInput("gene2",
       #             label = h3("Select Gene 2:"),
       #             names(which(distMatrix_genes[input$gene1,]!=Inf & distMatrix_genes[input$gene1,]!=0)),
       #             selected = names(which(distMatrix_genes[input$gene1,]!=Inf & distMatrix_genes[input$gene1,]!=0))[1]
       #             )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("network"),
       br(),
       tableOutput("genepair")
    )
  )
))
