## dealnetworkclass: wrap an S3 network class into an S4 network class
##
##

#.load.deal.networkclass <- function() {

#  require(methods)
  setClassUnion("integerOrNULL", c("NULL","integer"))
  
  setClass("networkclass", representation(
                                          n = "integer",
                                          nodes = "list",
                                          continuous = "integerOrNULL",
                                          discrete = "integerOrNULL",
                                          nd = "integer",
                                          nc = "integer",
                                          banlist = "matrix",
                                          score = "numeric",
                                          data = "data.frame",
                                          prior = "list",
                                          name             =     "character",
                                          visibleVertices  = "numeric",
                                          visibleBlocks    = "numeric",
                                          extraVertices    = "dg.VertexList",
                                          vertexEdges      = "dg.VertexEdgeList",
                                          blockEdges       = "dg.BlockEdgeList",
                                          factorVertices   = "dg.FactorVertexList",
                                          factorEdges      = "dg.FactorEdgeList",
                                          extraEdges       = "dg.ExtraEdgeList")
           )
#}
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
                prior = prior,
                name=modelstring(nw),
                  extraVertices  = .emptyDgList("dg.VertexList"),
                  vertexEdges    = .emptyDgList("dg.VertexEdgeList"),
                  blockEdges     = .emptyDgList("dg.BlockEdgeList"),
                  factorVertices = .emptyDgList("dg.FactorVertexList"),
                  factorEdges    = .emptyDgList("dg.FactorEdgeList"),
                  extraEdges     = .emptyDgList("dg.ExtraEdgeList")
                
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
    DynamicGraph(addModel = TRUE, frameModels = env$frameModels, 
                 frameViews = env$frameViews, graphWindow = env$graphWindow, edgeList = ArgEdges, oriented=TRUE,
                 object = object.new, factorVertexList = env$factorVertexList, 
                 factorEdgeList = env$FactorEdgeList, blockEdgeList = env$BlockEdgeList, 
                 title = "Result from Autosearch", Arguments = env)
    
#    env$redrawView(graphWindow = env$graphWindow,
#                   edgeList=ArgEdges,
#                   factorEdgeList = env$factorEdgeList,
#                   blockEdgeList = env$blockEdgeList,
#                   object = object.new,
#                   title = "Result from Search",
#                   Arguments = env
#                   )
                   
    result <- list(object=object.new)
    return(result)
}
