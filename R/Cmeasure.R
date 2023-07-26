#Cmeasure <- function(x,y,method = "pathlength",p=1,distance="euclidean"){
# # res <- Cmeasure(x,y)
# # Calculate the C-Measure subtypes MinimalPathlength and MinimWiring
# # INPUT
# # x    Vektor der Punkte in Eingaberaum
# # y    Vektor der Punkte in Ausgaberaum
# # k    Anzahl der Nachbarn in der Naehe
# # method   es wird nur Minimal Pathlength und Minimal Wiring implementiert
# # p     fuer Minimal Wiring 'wiring', Ausgaberaum, default=1
# # distance   Distanzmass in Nachbarnschaftberechnung
# # OUTPUT
# # MinimalPathlength    the calculated value
#    MinimWiring 			 the calculated value
# author MT

Cmeasure <- function(x, y, k = 1) {
  KNNGraph= function (DistanceMatrix, k, Data) 
  {
    requireNamespace("cccd")
    requireNamespace("igraph")
    KNNGraphAdjMatrix = NULL
    tryCatch({
      if (missing(DistanceMatrix)) {
        result = cccd::nng(x = Data, k = k, mutual = TRUE, 
                           method = "Euclidean")
      }
      if (missing(Data)) {
        result = cccd::nng(dx = DistanceMatrix, k = k, mutual = TRUE, 
                           method = NULL)
      }
      KNNGraphAdjMatrix = igraph::get.adjacency(result, sparse = FALSE, 
                                                type = "both")
    }, error = function(e) {
      warning(paste0("KNNGraphAdjMatrix(): ", e))
      KNNGraphAdjMatrix = matrix(0, nrow(Data), ncol(Data))
    })
    return(KNNGraphAdjMatrix)
  }
  
  #k>1 nicht in papern definiert!
  #requireNamespace("Distances")
  #requireNamespace("GraphAlgorithms")
  InputD = as.matrix(dist(x))
  OutputD = as.matrix(dist(y))
  spath = KNNGraph(OutputD, k = k)
  swiring = KNNGraph(InputD, k = k)
  return(c(
    MinimalPathlength = sum(InputD * spath),
    MinimWiring = sum(OutputD * swiring)
  ))
  
}
