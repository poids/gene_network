library(shiny)
library(igraph)

#Expression data
#expr_data <- read.csv("./expr_data.csv", row.names = 1)
# colnames(expr_data)[1]<-"gene.name"
# expr_data <- as.data.frame(expr_data, row.names = expr_data[,1])

print("start1")
ptm <- proc.time()

#Gene Correlation
genes_cor <- read.csv("./genes_cor.csv", row.names = 1)

print(proc.time() - ptm)

#import logFC stats
stat_results <- read.csv("./stat_results.csv")
#isolate geneID and logFC
DT <- data.table::data.table(results["unique_id"], results["logFC"])
#sum logFC for each gene
logFC<-as.data.frame(DT[, lapply(.SD, sum), by = "unique_id"])

#####Subset and reorder LogFC (ONLY REQUIRED FOR TESTING w/ SUBSET)
logFC <- subset(logFC, unique_id %in% rownames(genes_cor))
rownames(logFC) <- 1:nrow(logFC)

shinyServer(function(input, output, session) {

  x=reactiveValues(gene_graph=graph.adjacency(as.matrix(data.frame(c(0,0), c(0,0))), mode = "undirected"), df1="", df2="")
  
observeEvent(c(input$sort, input$sam_size, input$cutoff, input$gene1, input$gene2),{
 
    #TopFC
    if(input$sort==1) {
      TopFC <- as.numeric(rownames(logFC[order(logFC$logFC), ]))
      genes_cor <- genes_cor[TopFC, TopFC]

    #BottomFC
  } else if (input$sort==2) {
      BottomFC <- as.numeric(rownames(logFC[order(-logFC$logFC), ]))
      genes_cor <- genes_cor[BottomFC, BottomFC]
      
    #Random
  } else if (input$sort==3) {
      set.seed(42)
      rand <- sample(nrow(genes_cor))
      genes_cor <- genes_cor[rand,rand]
      
  }  
  
  #Subset number of genes
  genes_sam <- genes_cor[1:input$sam_size, 1:input$sam_size]
  
  
   #Correlation Threshold
  genes_adj <- abs(genes_sam) > input$cutoff # leave original matrix intact
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

  #Find genes with networks
  df1 <- distMatrix_genes
  df1[is.infinite(df1)]<-0
  df1 <- apply(df1, 1, sum)
  df1 <- names(df1[df1!=0])
  
x$df1 = df1  
  
  #Find genes within the network of the above gene ["gene1"]
if (!(input$gene1=="")) {
  df2 <- distMatrix_genes
  df2 <- df2[input$gene1, df1]
  df2[is.infinite(df2)]<-0
  df2 <- names(df2[df2!=0])
  
    x$df2 = df2
  }  

  
if (!(input$gene2=="")) {
  #Path between genes (VARIABLE= Separate input box for each gene in path; need to pull gene_pairs from distMatrix_genes in next line)
  pl <- get.shortest.paths(gene_graph, input$gene1, input$gene2)$vpath[[1]]
  
  #Path aesthetics
   V(gene_graph)[pl]$color <- paste("green")
   V(gene_graph)[pl]
   E(gene_graph, path = pl)$color <- paste("red")    # define edge color
   E(gene_graph, path = pl)$width <- 8               # define edge width
  }

x$gene_graph = gene_graph
})
  
observe({
  updateSelectInput(
    session, "gene1", choices=x$df1, selected = ""
  )
})

observe({
  updateSelectInput(
    session, "gene2", choices=x$df2, selected = ""
  )
})
   
  output$network <- renderPlot({
    #Plot
    plot(x$gene_graph, layout = layout.fruchterman.reingold, vertex.size = 6, vertex.label = NA)
    })

      
    
    
    
  #output$genepair <- renderTable({
  # 
  # })
  
})
