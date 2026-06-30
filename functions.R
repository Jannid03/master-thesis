make_expression <- function(i) {
  if (i == 36){
    return (paste("if(cell.id==36,",rnormTrunc(1,value,sd=1,min=0),",",value,")",sep=''))
  }
  else {
    expression <- "if(cell.id=="
    expression <- paste(expression,i,",",rnormTrunc(1,value,sd=1,min=0),",",make_expression(i+1),")",sep='')
    return (expression)
  }
}


setup <- function() {
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(xml2)
  library(XML)
  library(EnvStats)
  
  ### Preparation
  setwd("D:/Uni/Masterarbeit/data")
  file_xml <- read_xml("model_base.xml")
  
  print("Setup done")
}