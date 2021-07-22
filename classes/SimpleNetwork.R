#### Define validator of the Network class (check for valid Nodes and Edges) ####
check_network <- function(object)
{
  errors <- character()
  
  for (i in 1:length(object@node_list))
  {
    if (class(object@node_list[[i]]) != "Node")
    {
      msg <- paste("Network contains an invalid node at index:", i)
      errors <- c(errors, msg)
    }
  }
  
  for (i in 1:length(object@edge_list))
  {
    if (class(object@edge_list[[i]]) != "Edge")
    {
      msg <- paste("Network contains an invalid edge at index: ", i)
      errors <- c(errors, msg)
    }
  }
  
  if (length(errors) == 0) TRUE else errors
}

#### Define Network class ####
SimpleNetwork <- setClass("SimpleNetwork",
                          slots = list(
                            node_list = "list",
                            edge_list = "list"
                          ),
                          validity = check_network
)

#### Getters ####
setGeneric("get_node_index", function(network, node) standardGeneric("get_node_index"))
setGeneric("get_edge_index", function(network, edge) standardGeneric("get_edge_index"))

setMethod("get_node_index", signature = c("SimpleNetwork", "Node"), function(network, node)
{
  node_test_vect <- rep(NA, length(network@node_list))
  for (i in 1:length(network@node_list))
  {
    node_test_vect[i] <- all.equal(network@node_list[[i]], node)
  }
  
  return(which(node_test_vect))
})

setMethod("get_edge_index", signature = c("SimpleNetwork", "Edge"), function(network, edge)
{
  edge_test_vect <- rep(NA, length(network@edge_list))
  for (i in 1:length(network@edge_list))
  {
    edge_test_vect[i] <- all.equal(network@edge_list[[i]], edge)
  }
  
  return(which(edge_test_vect))
})

#### Mutators: add/remove nodes ####
setGeneric("add_node", function(network, node) standardGeneric("add_node"))
setGeneric("remove_node", function(network, node) standardGeneric("remove_node"))

setMethod("add_node", signature = c("SimpleNetwork", "Node"), function(network, node)
{
  network@node_list <- append(network@node_list, node)
  return(network)
})

setMethod("remove_node", signature = c("SimpleNetwork", "Node"), function(network, node)
{
  # Identify the location of the node
  node_index <- get_node_index(network, node)
  
  # Remove the bad node
  network@node_list <- network@node_list[-node_index]
  
  # Identify any edges that contain the node
  edge_test_vect <- rep(NA, length(network@edge_list))
  for (i in 1:length(network@edge_list))
  {
    edge_test_vect[i] <- contains_node(network@edge_list[[i]], node)
  }
  network@edge_list <- network@edge_list[-which(edge_test_vect)]
  
  return(network)
})

#### Mutators: add/remove edges ####
setGeneric("add_edge", function(network, edge) standardGeneric("add_edge"))
setGeneric("remove_edge", function(network, edge) standardGeneric("remove_edge"))

setMethod("add_edge", signature = c("SimpleNetwork", "Edge"), function(network, edge)
{
  # Test if either node is present
  for (node in list(source_node(edge), target_node(edge)))
  {
    node_index <- get_node_index(network, node)
    
    if (length(node_index) == 0)
    {
      network <- add_node(network, node)
    }
  }
  
  # Test if the edge is present
  edge_index <- get_edge_index(network, edge)
  
  if (length(edge_index) == 0)
  {
    network@edge_list <- append(network@edge_list, edge)
  }
  
  return(network)
})

setMethod("remove_edge", signature = c("SimpleNetwork", "Edge"), function(network, edge) 
{
  # Test if the edge is present
  edge_index <- get_edge_index(network, edge)
  
  network@edge_list <- network@edge_list[-edge_index]
  
  return(network)
})