library(shiny)
library(igraph)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Gene Co-Expression Network"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      radioButtons("sort", label = h4("Sort by Statistics:"),
                   choices = list("Top logFC" = 1, "Bottom logFC" = 2,
                                  "Random" = 3),selected = 3),
  
      #Delete and use sliderinput below when done optimizing    
       sliderInput("sam_size",
                   "Sample Size (# of Genes):",
                   min = 25,
                   max = 250,
                   value = 100,
                   step = 25),
      
#Using different parameters above for debugging
      # sliderInput("sam_size",
      #             "Sample Size (# of Genes):",
      #             min = 50,
      #             max = 1000,
      #             value = 500,
      #             step = 50),
      
       sliderInput("cutoff",
                   "Correlation Threshold",
                   min = 0.50,
                   max = 1.00,
                   value = 0.85,
                   step = 0.05),
       
       
        selectInput("gene1",
                     label = h4("Select Gene 1:"),
                     ""
                     ),
       
        selectInput("gene2",
                    label = h4("Select Gene 2:"),
                    ""
                    )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("network"),
       br(),
       tableOutput("genepair")
    )
  )
))
