library(CoCoCg)  

data(mathmark,package="gRbase")

CoCoObject <- makeCoCoCg()
enterDataFrame(mathmark, object = CoCoObject)
fullModel <- makeModel(enterModel("*", object = CoCoObject))
library(CoCoGraph)
fullGraph <- CoCo::dynamic.Graph(fullModel, title = "CoCo")
optionsCoCo("exact.test"="on")
backward(recursive = TRUE, object = CoCoObject,follow=TRUE)
lastModel <- makeModel("last", object = CoCoObject)
#backwardGraph <- CoCo::dynamic.Graph(lastModel, title = "Last",fullGraph)



