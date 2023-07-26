KendallsTau=function(InputDists,OutputDists){
# tau=KendallsTau(InputDists,OutputDists)
# Berechnet den statistischen Zusammenhang nach Kendall
#
# INPUT
# InputDists             Matrize der Distanzen des Eingaberaumes
# OutputDists            Matrize der Distanzen des Ausgaberaumes 
# 
# OUTPUT
# tau										 numeric, Kendalls tau
# Author: MT 10/2015
  requireNamespace('pcaPP')
  if(is.vector(InputDists)){
    requireNamespace('pracma')
    InputDists = pracma::squareform(InputDists)
  }
  if(is.vector(OutputDists)){
    requireNamespace('pracma')
    OutputDists = pracma::squareform(OutputDists)
  }
  
  x=InputDists[lower.tri(InputDists, diag = FALSE)]
  y=OutputDists[lower.tri(OutputDists, diag = FALSE)]
  
  #return(cor(x,y,method='kendall'))
  return(pcaPP::cor.fk(x,y))
  
}