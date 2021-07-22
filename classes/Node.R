#### Definition ####
Node <- setClass("Node",
                 representation(
                   name = "character"
                 ),
                 prototype(
                   name = NA_character_
                 )
)

#### Constructor ####
Node <- function(name = NA_character_) {
  new("Node", name = name)
}

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