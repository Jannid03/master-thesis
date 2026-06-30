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

init <- function(parameter="tmax") {
  parameter = parameter
  path <- paste("//CellTypes//Property[@symbol=","\"",parameter,"\"]",sep = "")
  value <- xml_double(xml_find_all(file_xml,paste(path, "//@value")))
  output <- paste("runs/",parameter,sep="")
  
  
  expr <- make_expression(1)
  
  expr_path <- (xml_find_first(file_xml,"//CellPopulations//Population"))
  xml_add_child(expr_path,"InitProperty")
  xml_set_attr(xml_find_first(file_xml, "//CellPopulations//Population//InitProperty"), "symbol-ref",parameter)
  xml_add_child(xml_find_first(file_xml, "//CellPopulations//Population//InitProperty"),"Expression",expr)
  
  
  print("Init done")
}