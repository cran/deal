## dealnetworkclass: wrap an S3 network class into an S4 network class
##
##

require(methods)

setClass("networkclass", representation(
                                        n = "integer",
                                        nodes = "list",
                                        continuous = "integer",
                                        discrete = "integer",
                                        nd = "integer",
                                        nc = "integer",
                                        banlist = "matrix",
                                        score = "numeric",
                                        data = "data.frame",
                                        prior = "list"
                                        )
         )

newnetwork <- function(nw,data,prior) {

if(!is.null(nw$banlist))
    ban <- nw$banlist
else
    ban <- matrix(nrow=0,ncol=2)

result <- new("networkclass",
                  n = nw$n,
                  nodes = nw$nodes,
                  continuous = nw$continuous,
                  discrete = nw$discrete,
                  nd = nw$nd,
                  nc = nw$nc,
                  banlist = ban,
                  score = nw$score,
                  data = data,
                  prior = prior
                  )
    return(result)
}

recovernetwork <- function(object) {
    nw <- list(
               n = object@n,
               nodes = object@nodes,
               continuous = object@continuous,
               discrete = object@discrete,
               nd = object@nd,
               nc = object@nc,
               banlist = object@banlist,
               score = object@score
               )
    class(nw) <- "network"
    nw
}

Autosearch <- function(object,...) {
    args <- list(...)
    env <- args$Arguments
    nw <- recovernetwork(object)
    prior <- object@prior
    data  <- object@data

    nw.new <- autosearch(nw,data,prior,trace=FALSE)$nw

    object.new <- newnetwork(nw.new,data,prior)
    two.to.pairs <- function(from, to) {
        edge.list <- vector("list", length(to))
        for (j in seq(along = to)) edge.list[[j]] <- c(from[j], 
            to[j])
        return(edge.list)
    }
    
    Edges <- dealEdges(object = object.new)

    ArgEdges <-   
          returnEdgeList(edge.list=two.to.pairs(Edges[,1],Edges[,2]),
                         vertices=env$vertexList,
                         oriented=TRUE
                         )
    env$redrawGraphWindow(
                       env$graphLattice,
                          edgeList=ArgEdges,
                       factorVertexList = env$factorVertexList,
                       factorEdgeList = env$factorEdgeList,
                       visibleVertices = env$visibleVertices,
                       object = object.new,
                       title = "Result from Search",
                       transformation = NULL,  
                       background = "white",
                       vertexColor = "black", w = 10, width = 400,  
                                    height = 400)                                     
    result <- list(object=object.new)
    return(result)
}
