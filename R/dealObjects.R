## dealObjects.R --- 
## Author          : Claus Dethlefsen
## Created On      : Wed Dec 10 09:29:14 2003
## Last Modified By: Claus Dethlefsen
## Last Modified On: Wed Aug 04 10:51:19 2004
## Update Count    : 102
## Status          : Unknown, Use with caution!
###############################################################################

#require(methods)
#require(dynamicGraph)


dealVariableDescription <- function(object) {

      Types <- rep("Continuous",object@n)
      Types[object@discrete] <- "Discrete"

      return(list(
                  names = names(object@nodes),
                  labels = NULL,
                  types = Types,
                  levels = NULL
                  )
             )
  }

dealEdges <- function(object)
  {
      From <- c()
      To   <- c()
      
      for (i in 1:object@n) {
          from <- object@nodes[[i]]$parents
          From <- c(From,from)
          To   <- c(To,rep(i,length(from)))
      }
      return(cbind(From,To))
  }

.load.dynamicgraph <- function() {
  require(methods)

  if (!isGeneric("dynamic.Graph")) {
  if (is.function("dynamic.Graph"))
    fun <- dynamic.Graph
  else
    fun <- function(object, ...)
  standardGeneric("dynamic.Graph")
  setGeneric("dynamic.Graph", fun)
}

setMethod("dynamic.Graph", signature(object = "networkclass"), 
          function(object, ...) {
#setMethod("dynamic.Graph", signature(object = "networkclass"),
#          function(object, factors = NULL, blocks = NULL,
#                   title = "dynamicDealGraph", 
#                   drawblocks = TRUE, right.to.left = FALSE,
#                   nested.blocks = FALSE, background = "white", 
#                   edgecolor = "black",  # Color set here!!
#                   width = 400, height = 400, ...)
#  {
            .load.deal.dynamic()
            
    VariableDescription <- dealVariableDescription(object = object)
    Edges <- dealEdges(object = object)

    Z <- DynamicGraph(names = VariableDescription$names,
                      types = VariableDescription$types,
                      from = Edges[,1], to = Edges[,2],
                      oriented = TRUE, 
                      object = object,
                      UserMenus = Menus)
                      
})
}
.load.deal.dynamic <- function() {
  require(methods)
  require(dynamicGraph)
  
  setClass("DealTestClass", 
           representation(bayesfactor = "numeric"))
  
#newDealTestObject <- function(test) {
#    result <- new("DealTestClass",
#                  bayesfactor = test)
#    return(result)
#}

  if (!isGeneric("label") && !isGeneric("label", where = 4)) {
    if (is.function("label"))
      fun <- label
    else
      fun <- function(object) standardGeneric("label")
    setGeneric("label", fun)
  }
  
  setMethod("label", "DealTestClass", function(object)
            format(object@bayesfactor, digits = 4))
  
  if (!isGeneric("width") && !isGeneric("width", where = 4)) {
    if (is.function("width"))
      fun <- width
    else
      fun <- function(object) standardGeneric("width")
    setGeneric("width", fun)
  }
  
  setMethod("width", "DealTestClass", function(object)
            (10-round(2 + 5 * (1 - min(1,object@bayesfactor)))))

  
  if (!isGeneric("testEdge")) {
    if (is.function("testEdge"))
      fun <- testEdge
    else
      fun <- function(object, action, name.1, name.2, ...)
        standardGeneric("testEdge")
    setGeneric("testEdge", fun)
  }
  
  
  setMethod("testEdge", signature(object = "networkclass"),
            function(object, action, name.1, name.2, ...)
            {
#    message(paste("SHOULD return a test object with the edge from",
#                  name.1, "to", name.2, "deleted from the argument object"))
              args <- list(...)
              env <- args$Arguments
              result <- NULL
              
              modelCurrent <- object
              nw <- recovernetwork(object)
              
              currentscore <- modelCurrent@score
              
              i <- (1:nw$n)[names(nw$nodes)==name.1]
              j <- (1:nw$n)[names(nw$nodes)==name.2]
              
              ##    cat("Trying to remove","i=",i,"(",name.1,"),j=",j,"(",name.2,")\n")
              
              nw2 <- remover(nw,i,j,object@data,object@prior)$nw
              
              if (!is.null(nw2)) 
                test <- exp(nw$score - nw2$score)
              else {
                cat("Failed.\n")
                cat("Trying to remove","i=",i,"(",name.1,"),j=",j,"(",name.2,")\n")
                test <- 0
              }
#    cat("BayesFactor: ",test,"\n")
#    return(newDealTestObject(test))
              return(new("DealTestClass", bayesfactor=test))
            })


  if (!isGeneric("modifyModel")) {
    if (is.function("modifyModel"))
      fun <- modifyModel
    else
      fun <- function(object, action, name, name.1, name.2, ...)
        standardGeneric("modifyModel")
    setGeneric("modifyModel", fun)
  }
  
  setMethod("modifyModel", signature(object = "networkclass"),
            function(object, action, name, name.1, name.2, ...)
            {
              args <- list(...)
              env <- args$Arguments
              result <- NULL

              two.to.pairs <- function(from, to) {
                edge.list <- vector("list", length(to))
                for (j in seq(along = to)) edge.list[[j]] <- c(from[j], 
                                to[j])
                return(edge.list)
              }
    
              
              FactorVertices <- NULL
              FactorEdges <- NULL
              
              nw <- recovernetwork(object)
              
              i <- (1:object@n)[names(object@nodes)==name.1]
              j <- (1:object@n)[names(object@nodes)==name.2]
              
              if (action == "dropEdge") {
                ##       message(paste("Should return an object with the edge from",
#                     name.1, "to", name.2, "deleted from the argument object"))
                nw2 <- remover(nw,i,j,object@data,object@prior)$nw
                if (!is.null(nw2)) {
                  new.object <- newnetwork(nw2,object@data,object@prior)
                  result <- list(object = new.object)
                }
                else {
                  cat("Failure:")
                  cat("Tried to drop edge ",i,",",j,".\n")
#                  new.object <- newnetwork(nw,object@data,object@prior)
#                    Edges <- dealEdges(object = new.object)
#                    ArgEdges <-   
#                      returnEdgeList(edge.list=two.to.pairs(Edges[,1],Edges[,2]),
#                                     vertices=env$vertexList,
#                                     oriented=TRUE
#                                     )
#                    env$edgeList <- ArgEdges
                    ## virker ej:
#                  env$redrawGraphWindow(graphWindow=env$graphWindow,object=new.object,edgeList=ArgEdges,Arguments=env)
                  ##  env$redrawGraphWindow(graphWindow=NULL,object=new.object,edgeList=ArgEdges,Arguments=env)                    
                }
                
              } else if (action == "addEdge") {
#       message(paste("Should return an object with the edge from",
#                     name.1, "to", name.2, "added to the argument object"))
                nw2 <- insert(nw,i,j,object@data,object@prior)$nw
                if (!is.null(nw2)) {
                  if (!cycletest(nw2)) {
                    new.object <-
    newnetwork(nw2,object@data,object@prior)
                  result <- list(object = new.object)
                  }
                  else {
                    cat("Failure:")
                    cat("Cycle created.\n")
#                    new.object <-
#                      newnetwork(nw,object@data,object@prior)
##                    Edges <- dealEdges(object = new.object)
##                    ArgEdges <-   
##                      returnEdgeList(edge.list=two.to.pairs(Edges[,1],Edges[,2]),
##                                     vertices=env$vertexList,
##                                     oriented=TRUE
##                                    )
#                    env$edgeList <- ArgEdges
                    ## virker ej:
##                     env$redrawGraphWindow(graphWindow=env$graphWindow,object=new.object,edgeList=ArgEdges,Arguments=env)
                    ## env$redrawGraphWindow(graphWindow=NULL,object=new.object,edgeList=ArgEdges,Arguments=env)                    
                  }
                }
                else {
                  cat("Failure:")
                  cat("Tried to add edge i=",i,",j=",j,".\n")
#                  new.object <- newnetwork(nw,object@data,object@prior)
#                    Edges <- dealEdges(object = new.object)
#                    ArgEdges <-   
#                      returnEdgeList(edge.list=two.to.pairs(Edges[,1],Edges[,2]),
#                                     vertices=env$vertexList,
#                                     oriented=TRUE
#                                     )
#                    env$edgeList <- ArgEdges
                    ## virker ej:
                  ## env$redrawGraphWindow(graphWindow=env$graphWindow,object=new.object,edgeList=ArgEdges,Arguments=env)
#                    env$redrawGraphWindow(graphWindow=NULL,object=new.object,edgeList=ArgEdges,Arguments=env)                    
                }
              } else if (action == "dropVertex")  {
                message(paste("Should return an object with the vertex", name,
                              "deleted from the argument object"))

                cat("Sorry..",action,"not implemented\n")
                new.object <- object
              } else if (action == "addVertex") {
                message(paste("Should return an object with the vertex", name, 
                              args$index, "added to the argument object"))
                cat("Sorry..",action,"not implemented\n")
                new.object <- object
              }
              
#              result <- list(object = new.object,
#                             FactorVertices = FactorVertices,
#                             FactorEdges = FactorEdges)

              return(result)
            }
            )
}  
  

DealLabelAllEdges <- function(object, slave = FALSE, 
                ...) {
                args <- list(...)
                Args <- args$Arguments
                getNodeName <- function(index, type) if (type == 
                  "Vertex") 
                  name(Args$vertexList[[index]])
                else if (type == "Factor") 
                  name(Args$factorVertexList[[abs(index)]])
                else if (type == "Block") 
                  label(Args$blockList[[abs(index)]])
                else NULL
                visitEdges <- function(edges) {
                  for (i in seq(along = edges)) {
                    vertices <- nodeIndicesOfEdge(edges[[i]])
                    types <- nodeTypesOfEdge(edges[[i]])
                    name.f <- getNodeName(vertices[1], types[1])
                    name.t <- getNodeName(vertices[2], types[2])
                    R <- testEdge(object, action = "remove", 
                      name.1 = name.f, name.2 = name.t, from = vertices[1], 
                      to = vertices[2], from.type = types[1], 
                      to.type = types[2], edge.index = i, force = force, 
                      Arguments = Args)
                    if (!is.null(R)) {
                      if (TRUE || (hasMethod("label", class(R)))) 
                        label(edges[[i]]) <- label(R)
                      if (TRUE || (hasMethod("width", class(R)))) 
                        width(edges[[i]]) <- width(R)
                    }
                  }
                  return(edges)
                }
                edgeList <- visitEdges(Args$edgeList)
                factorEdgeList <- visitEdges(Args$factorEdgeList)
                blockEdgeList <- visitEdges(Args$blockEdgeList)
                if (slave) 
                  Args$redrawGraphWindow(graphWindow = NULL, 
                    edgeList = edgeList, factorEdgeList = factorEdgeList, 
                    blockEdgeList = blockEdgeList, title = "A slave window", 
                    ...)
                else Args$redrawGraphWindow(graphWindow = Args$graphWindow, 
                  edgeList = edgeList, factorEdgeList = factorEdgeList, 
                  blockEdgeList = blockEdgeList, title = "Not used!", 
                  width = NULL, height = NULL, Arguments = Args)
            }
                
