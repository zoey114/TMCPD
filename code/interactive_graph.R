# Load necessary libraries
library(shiny)
library(visNetwork)
library(igraph)

setwd("/Users/zhenwang/Documents/new_IR/")
load("./data/nonzero.dca.RData")
graph <- graph_from_adjacency_matrix(nonzero.dca[[21]], mode = "undirected")
nodes <- get.data.frame(graph, what = "vertices")
library(countrycode)
name = countrycode(nodes$name, "cown", "cowc")
nodes = cbind(nodes, name)
colnames(nodes) = c("id", "label")
edges <- get.data.frame(graph, what = "edges")
colnames(edges) <- c("from", "to")
nodes$value = degree(graph)


server <- function(input, output) {
  # Sample network data
  nodes <- nodes
  edges <- edges
  
  # Create an observer for the network
  observe({
    visNetwork(nodes, edges, width = "100%") %>%
      visIgraphLayout() %>%
      visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE)) %>%
      visInteraction(navigationButtons = TRUE)
  })
  
  # Save button click handler
  output$save_button <- downloadHandler(
    filename = function() {
      paste("node_positions.txt")
    },
    content = function(file) {
      visNetworkProxy("network") %>%
        visGetPositions() %>%
        write.table(file = file, quote = FALSE, sep = "\t", col.names = FALSE)
    }
  )
}

# Run the Shiny app
shinyApp(ui, server)

load("/Users/zhenwang/Library/CloudStorage/GoogleDrive-zwang9898@gmail.com/My\ Drive/project/dca_net/Data/new.layout.RData")

visNetwork(nodes, edges, width = "100%") %>%
  visIgraphLayout(layoutMatrix = newl) %>%
  visNodes()
  visInteraction(navigationButtons = TRUE)



