
 Menus <- 
    list(
         MainUser =
         list(label = "Autosearch",
              command = function(object, ...)
          {
             Autosearch(object,...)
          },
              update.vertices = TRUE,
              update.edges = TRUE
              ),
         MainUser = list(label = "Label all edges, in this window", 
           command = function(object, ...) DealLabelAllEdges(object, 
                    slave = FALSE, ...)),
         Vertex = list(label = "Edit distribution",
         command = function(object, name, ...)
     {
         args <- list(...)
         nw <- recovernetwork(object)
         edit(as.matrix(nodes(nw)[[args$index]]$prob))
         
     },
           update.vertices = TRUE,
           update.edges = TRUE
     ),
         MainUser =
         list(label = "Savenet",
              command = function(object, ...)
          {
            Args <- list(...)$Arguments
            ReturnVal <- modalDialog("Filename dialog", 
                                     "Enter filename", "default.net", graphWindow = Args$graphWindow)
                    print(ReturnVal)
            
            savenet(recovernetwork(object),file(ReturnVal))
          },
              update.vertices = TRUE,
              update.edges = TRUE
              )
         )
