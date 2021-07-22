#### Definition ####
Node <- setClass("Node",
                 representation(
                   name = "character"
                 ),
                 prototype(
                   name = "default_node_name"
                 )
)

#### Generics ####
setGeneric("name", function(x) standardGeneric("name"))
setGeneric("name<-", function(x, value) standardGeneric("name<-"))

#### Getters ####
setMethod("name", "Node", function(x) x@name)

#### Setters ####
setMethod("name<-", "Node", function(x, value) 
{
  x@name <- value
  x
})

#### Equality ####
setMethod("all.equal", signature = c("Node", "Node"), function(target, current)
{
  target@name == current@name
}
)