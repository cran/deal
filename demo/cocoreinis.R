library(CoCo)  

data(Reinis)
CoCoObject <- makeCoCo()
enterTable(Reinis, object = CoCoObject)
fullModel <- makeModel(enterModel("*", object = CoCoObject))
library(CoCoGraph)
fullGraph <- CoCo::dynamic.Graph(fullModel, title = "CoCo")
backward(recursive = TRUE, object = CoCoObject,follow=TRUE)
lastModel <- makeModel("last", object = CoCoObject)
#backwardGraph <- CoCo::dynamic.Graph(lastModel, title = "Last",fullGraph)

