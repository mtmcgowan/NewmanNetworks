Edge <- setClass("Edge",
                 representation(
                   source_node = "Node",
                   target_node = "Node"
                 ))

# Generics
setGeneric("source_node", function(x) standardGeneric("source_node"))
setGeneric("source_node<-", function(x, value) standardGeneric("source_node<-"))

setGeneric("target_node", function(x) standardGeneric("target_node"))
setGeneric("target_node<-", function(x, value) standardGeneric("target_node<-"))

setGeneric("contains_node", function(edge, node) standardGeneric("contains_node"))

# Getters
setMethod("source_node", "Edge", function(x) x@source_node)
setMethod("target_node", "Edge", function(x) x@target_node)

# Setters
setMethod("source_node<-", "Edge", function(x, value) {
  x@source_node <- value
  x
})
setMethod("target_node<-", "Edge", function(x, value) {
  x@target_node <- value
  x
})

# Tests
setMethod("all.equal", signature = c("Edge", "Edge"), function(target, current)
{
  forward_test <- all.equal(target@source_node, current@source_node) & all.equal(target@target_node, current@target_node)
  reverse_test <- all.equal(target@source_node, current@target_node) & all.equal(target@target_node, current@source_node)
  
  return(forward_test | reverse_test)
})

setMethod("contains_node", signature = c("Edge", "Node"), function(edge, node)
{
  all.equal(edge@target_node, node) | all.equal(edge@source_node, node)
})