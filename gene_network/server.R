library(shiny)
library(igraph)

expr_data <- read.csv("./expr_data.csv", row.names = 1)
# colnames(expr_data)[1]<-"gene.name"
# expr_data <- as.data.frame(expr_data, row.names = expr_data[,1])



shinyServer(function(input, output) {
   
  output$network <- renderPlot({
   
    #Sample Size
    genes_sam <- expr_data[sample(nrow(expr_data), input$sam_size, F),]
    
    #correlation
    genes_cor <- cor(t(genes_sam)) # calculate the correlation between all gene pairs
    
    #Correlation Threshold
    genes_adj <- abs(genes_cor) > input$cutoff # leave original matrix intact
    diag(genes_adj) <- 0
    
    #Visualiztion
    gene_graph <- graph.adjacency(genes_adj, mode = "undirected") #convert adjacency to graph
    comps <- clusters(gene_graph)$membership                        #define gene cluster membership
    colbar <- rainbow(max(comps)+1)                                   #define colors
   
    #Aesthetics
    V(gene_graph)$color <- colbar[comps+1]                          #assign colors to nodes
    E(gene_graph)$color <- paste("grey")                  #assign colors to edges
  
    #Gene-pair distnaces in network
    distMatrix_genes <- shortest.paths(gene_graph, v = V(gene_graph), to = V(gene_graph))
   
    #Find genes with netowrks
    df1 <- distMatrix_genes
    df1[is.infinite(df1)]<-0
    df1 <- apply(df1, 1, sum)
    df1 <- names(df1[df1!=0])
   
    #Path between genes (VARIABLE= Separate input box for each gene in path; need to pull gene_pairs from distMatrix_genes in next line)
    #pl <- get.shortest.paths(gene_graph, "Tcta", "Robo1")$vpath[[1]]
    
    #Path aesthetics
    # V(gene_graph)[pl]$color <- paste("green")
    # V(gene_graph)[pl]
    # E(gene_graph, path = pl)$color <- paste("red")    # define edge color
    # E(gene_graph, path = pl)$width <- 8               # define edge width
    
    #Plot
    plot(gene_graph, layout = layout.fruchterman.reingold, vertex.size = 6, vertex.label = NA)
  
    })
    
    
    
    
  #output$genepair <- renderTable({
  # 
  # })
  
})
