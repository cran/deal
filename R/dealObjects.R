## dealObjects.R --- 
## Author          : Claus Dethlefsen
## Created On      : Wed Dec 10 09:29:14 2003
## Last Modified By: Claus Dethlefsen
## Last Modified On: Wed Nov 10 14:18:31 2004
## Update Count    : 123
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


if (!isGeneric("graphComponents")) {
  if (is.function("graphComponents"))
    fun <- graphComponents
  else
    fun <- function(object, viewType = NULL, ...)
    standardGeneric("graphComponents")
  setGeneric("graphComponents", fun)
}

setMethod("graphComponents", "networkclass",
          function(object, viewType = NULL, ...)
          { # print(viewType); print ("graphComponents")
            args <- list(...)
            Args <- args$Arguments
            Edges <- object@vertexEdges
            Vertices <- Args$vertexList
            VisibleVertices <- object@visibleVertices
            if (viewType == "Factor") {
              # require(ggm)
              # e <- NodeIndices(Edges)
              # if (length(e) > 0) {
              #   e <- lapply(e, function(egde) if (sum(abs(egde)) > 0) egde)
              #   e <- .removeNull(e)
              # } else
              #   e <- NULL
              # factors <- NULL
              # if (length(e) < 2) {
              #   if (length(e) == 1)
              #     factors <- append(e, as.list(VisibleVertices))
              #   else if (length(VisibleVertices) > 0)
              #     factors <- as.list(VisibleVertices)
              # } else {
              #   n <- Names(Vertices)
              #   X <- matrix(rep(0, length(n)^2), ncol = length(n))
              #   lapply(e, function(i) { X[i[1], i[2]] <<- 1 ;
              #             X[i[2], i[1]] <<- 1 } )
              #   dimnames(X) <- list(n, n)
              #   X <- X[VisibleVertices, VisibleVertices]
              #   # print ("graphComponents: ")
              #   # print(X)
              #   factors <- cliques(X)
              # }
              factors <- .cliquesFromEdges(Edges, Vertices, VisibleVertices)
              # print(factors)
              if (is.null(factors) || (length(factors) == 0)) {
                FactorVertices  <- .emptyDgList("dg.FactorVertexList")
                FactorEdges     <- .emptyDgList("dg.FactorEdgeList")
              } else {
                result <- returnFactorVerticesAndEdges(Vertices, factors)
                FactorVertices  <- result$FactorVertices
                FactorEdges     <- result$FactorEdges
              }
              list(vertexEdges     = object@vertexEdges, 
                   blockEdges      = object@blockEdges, 
                   factorVertices  = FactorVertices,
                   factorEdges     = FactorEdges,
                   visibleVertices = object@visibleVertices, 
                   visibleBlocks   = object@visibleBlocks, 
                   extraVertices   = object@extraVertices, 
                   extraEdges      = object@extraEdges)
            } else if (viewType == "Moral") {
              message("Moral view not implemented; ")
              list(vertexEdges     = object@vertexEdges, 
                   blockEdges      = .emptyDgList("dg.BlockEdgeList"),
                   factorVertices  = .emptyDgList("dg.FactorVertexList"),
                   factorEdges     = .emptyDgList("dg.FactorEdgeList"),
                   visibleVertices = object@visibleVertices, 
                   visibleBlocks   = numeric(), 
                   extraVertices   = object@extraVertices, 
                   extraEdges      = object@extraEdges)
            } else if (viewType == "Essential") {
              message("Essential view not implemented; ")
              list(vertexEdges      = object@vertexEdges, 
                   blockEdges      = .emptyDgList("dg.BlockEdgeList"),
                   factorVertices  = .emptyDgList("dg.FactorVertexList"),
                   factorEdges     = .emptyDgList("dg.FactorEdgeList"),
                   visibleVertices = object@visibleVertices, 
                   visibleBlocks   = numeric(), 
                   extraVertices   = object@extraVertices, 
                   extraEdges      = object@extraEdges)
            } else if (viewType == "Simple") {
              list(vertexEdges     = object@vertexEdges, 
                   blockEdges      = object@blockEdges, 
                   factorVertices  = .emptyDgList("dg.FactorVertexList"),
                   factorEdges     = .emptyDgList("dg.FactorEdgeList"),
                   visibleVertices = object@visibleVertices, 
                   visibleBlocks   = object@visibleBlocks, 
                   extraVertices   = object@extraVertices, 
                   extraEdges      = object@extraEdges)
            } else 
              message("View type not implemented; ")
          })

if (!isGeneric("setGraphComponents")) {
  if (is.function("setGraphComponents"))
    fun <- setGraphComponents
  else
    fun <- function(object, viewType = NULL,
                    visibleVertices = NULL,
                    extraVertices   = NULL,
                    vertexEdges     = NULL,
                    blockEdges      = NULL,
                    factorVertices  = NULL,
                    factorEdges     = NULL,
                    extraEdges      = NULL, ...)
      standardGeneric("setGraphComponents")
  setGeneric("setGraphComponents", fun)
}

setMethod("setGraphComponents", signature(object = "networkclass"),
          function(object, viewType = NULL,
                   visibleVertices = NULL,
                   visibleBlocks   = NULL,
                   extraVertices   = NULL,
                   vertexEdges     = NULL,
                   blockEdges      = NULL,
                   factorVertices  = NULL,
                   factorEdges     = NULL,
                   extraEdges      = NULL, ...)
 {
    if (!is.null(visibleVertices)) object@visibleVertices <- visibleVertices
    if (!(viewType == "Moral"))
      if (!is.null(visibleBlocks  )) object@visibleBlocks   <- visibleBlocks
    if (!is.null(extraVertices  )) object@extraVertices   <- extraVertices
    if (!is.null(vertexEdges    )) object@vertexEdges     <- vertexEdges
    if (!is.null(blockEdges     )) object@blockEdges      <- blockEdges
    if ((viewType == "Factor")) {
      if (!is.null(factorVertices )) object@factorVertices  <- factorVertices
      if (!is.null(factorEdges    )) object@factorEdges     <- factorEdges
    }
    return(object)
 })



#.load.dynamicgraph <- function() {
#  require(methods)

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
#            .load.deal.dynamic()
            
    VariableDescription <- dealVariableDescription(object = object)
    Edges <- dealEdges(object = object)

    Z <- DynamicGraph(names = VariableDescription$names,
                      types = VariableDescription$types,
                      from = Edges[,1], to = Edges[,2],
                      oriented = TRUE, 
                      object = object,
                      UserMenus = Menus)
                      
})
  
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
              from.type <- args$from.type
              to.type <- args$to.type
              f <- function(type) if(is.null(type)) "" else paste("(",
                                                                  type, ")")
              
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

#                  message(paste("Should return an object with the edge from",
#                  name.1, f(from.type), "to", name.2, f(to.type),
#                  "deleted from the argument object"))

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
              Args <- args$Arguments
              Edges <- args$newEdges$vertexEdges
              Vertices <- Args$vertexList
              env <- args$Arguments
              result <- NULL

              DoFactors <- FALSE
              if (!is.null(args$Arguments)
                  && !is.null(args$Arguments$factorVertexList)
                  && (length(args$Arguments$factorVertexList) > 0)
                  && !is.null(args$Arguments$vertexList))
                DoFactors <- TRUE

              FactorVertices  <- NULL
              FactorEdges     <- NULL
              BlockEdges      <- NULL
              VisibleVertices <- Args$visibleVertices
              VisibleBlocks   <- Args$visibleBlocks
              ExtraVertices   <- NULL
              ExtraEdges      <- NULL

              f <- function(type) if (is.null(type)) "" else paste("(", type, ")")
              g <- function(type) if (is.null(type)) "" else type

              
              two.to.pairs <- function(from, to) {
                edge.list <- vector("list", length(to))
                for (j in seq(along = to)) edge.list[[j]] <- c(from[j], 
                                to[j])
                return(edge.list)
              }
    
              
              nw <- recovernetwork(object)
              
              i <- (1:object@n)[names(object@nodes)==name.1]
              j <- (1:object@n)[names(object@nodes)==name.2]
              
              if (action == "dropEdge") {
                ##       message(paste("Should return an object with the edge from",
#                     name.1, "to", name.2, "deleted from the argument object"))
                nw2 <- remover(nw,i,j,object@data,object@prior)$nw
                if (!is.null(nw2)) {
                  new.object <- newnetwork(nw2,object@data,object@prior)
                }
                else {
                  cat("Failure:")
                  cat("Tried to drop edge ",i,",",j,".\n")
                  new.object <- object
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
                  }
                  else {
                    cat("Failure:")
                    cat("Cycle created.\n")
                  new.object <- object
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
                  new.object <- object
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
              
    return(list(object          = new.object,
                BlockEdges      = BlockEdges, 
                FactorVertices  = FactorVertices,
                FactorEdges     = FactorEdges,
                VisibleVertices = VisibleVertices, 
                VisibleBlocks   = VisibleBlocks, 
                ExtraVertices   = ExtraVertices,
                ExtraEdges      = ExtraEdges))

#              result <- list(object = new.object,
#                             FactorVertices = FactorVertices,
#                             FactorEdges = FactorEdges)

#              return(result)
            }
            )
#}  

if (!isGeneric("Str")) {
  if (is.function("Str"))
    fun <- Str
  else
    fun <- function(object, setRowLabels = FALSE, title = "", ...)
    standardGeneric("Str")
  setGeneric("Str", fun)
}

setMethod("Str", "networkclass",
          function(object, setRowLabels = FALSE, title = "", ...) {
            message(object@name) })


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
#                  DynamicGraph(overwrite=FALSE, addModel = FALSE,
#                frameModels = Args$frameModels,  
#                               frameViews = Args$frameViews, graphWindow = Args$graphWindow, edgeList = edgeList, oriented=TRUE,
#                               object = object, factorVertexList = Args$factorVertexList, 
#                               factorEdgeList = Args$FactorEdgeList, blockEdgeList = Args$BlockEdgeList, 
#                               title = "Result from Label Edges", Arguments = Args)

                  ##                  Args$redrawView(graphWindow = NULL, 
                  ##                   edgeList = edgeList, factorEdgeList = factorEdgeList, 
                  ##                    blockEdgeList = blockEdgeList, title = "A slave window", 
                  ##                    ...)
#                else
                  Args$redrawView(graphWindow = NULL, edgeList = edgeList, 
                    factorEdgeList = factorEdgeList, blockEdgeList = blockEdgeList, 
                    title = "A slave window", ...)
                else Args$redrawView(graphWindow = Args$graphWindow, 
                  edgeList = edgeList, factorEdgeList = factorEdgeList, 
                  blockEdgeList = blockEdgeList, title = "Not used!", 
                  width = NULL, height = NULL, Arguments = Args)
#                  DynamicGraph(overwrite=FALSE, addModel = FALSE,
#                frameModels = Args$frameModels,  
#                               frameViews = Args$frameViews, graphWindow = Args$graphWindow, edgeList = edgeList, oriented=TRUE,
#                               object = object, factorVertexList = Args$factorVertexList, 
#                               factorEdgeList = Args$FactorEdgeList, blockEdgeList = Args$BlockEdgeList, 
#                               title = "Result from Label Edges", Arguments = Args)

##                  Args$redrawView(graphWindow = Args$graphWindow, 
##                  edgeList = edgeList, factorEdgeList = factorEdgeList, 
##                  blockEdgeList = blockEdgeList, title = "Not used!", 
##                  width = NULL, height = NULL, Arguments = Args)
            }
                
