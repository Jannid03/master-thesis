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
  file_xml <<- read_xml("model_base.xml")
  
  print("Setup done")
}

init <- function(parameter="tmax") {
  parameter <<- parameter
  path <<- paste("//CellTypes//Property[@symbol=","\"",parameter,"\"]",sep = "")
  value <<- xml_double(xml_find_all(file_xml,paste(path, "//@value")))
  output <<- paste("runs/",parameter,sep="")
  
  
  expr <- make_expression(1)
  
  expr_path <<- (xml_find_first(file_xml,"//CellPopulations//Population"))
  xml_add_child(expr_path,"InitProperty")
  xml_set_attr(xml_find_first(file_xml, "//CellPopulations//Population//InitProperty"), "symbol-ref",parameter)
  xml_add_child(xml_find_first(file_xml, "//CellPopulations//Population//InitProperty"),"Expression",expr)
  
  
  print("Init done")
}

standard_plots <- function(df) {

  df |> filter(cell.id == "20") |>
    ggplot(mapping=aes(x=time))+
    ggtitle("NFKB over time")+
    geom_line(aes(y=NFKB.n,color="NFKB.n"))+
    geom_line(aes(y=NFKB, color='NFKB'))+
    geom_line(aes(y=total_NFKB,color="Total NFKB"))+
    geom_line(aes(y=NFKB_IKBA, color="NFKB|IKBA"))+
    geom_line(aes(y=IKK_NFKB_IKBA, color="NFKB|IKK|IKBA"))+
    scale_color_manual(values=c("blue","red","green",'yellow','black'))+
    guides(color=guide_legend(title="Component"))+
    geom_vline(xintercept=0, alpha=0.5, linetype="dashed")+
    geom_vline(xintercept=60,alpha=0.5, linetype="dashed")
  
  ggsave(filename="cell_20_NFKB.png",path = output, width=3000, height=2000, units="px")
  
  df |> filter(cell.id == "20") |>
    ggplot(mapping=aes(x=time))+
    ggtitle("IKK over time")+
    geom_line(aes(y=IKK,color="IKK"))+
    geom_line(aes(y=total_IKK,color="Total IKK"))+
    geom_line(aes(y=IKK_IKBA, color="IKK|IKBA"))+
    geom_line(aes(y=IKK_NFKB_IKBA, color="NFKB|IKK|IKBA"))+
    scale_color_manual(values=c("blue","red","green",'yellow'))+
    guides(color=guide_legend(title="Component"))+
    geom_vline(xintercept=0, alpha=0.5, linetype="dashed")+
    geom_vline(xintercept=60,alpha=0.5, linetype="dashed")
  
  ggsave(filename="cell_20_IKK.png",path = output, width=3000, height=2000, units="px")
  
  df |> filter(cell.id == "20") |>
    ggplot(mapping=aes(x=time))+
    ggtitle("IKBA over time")+
    geom_line(aes(y=IKBA.m,color="IKBA mRNA"))+
    geom_line(aes(y=IKBA.n, color='IKBA Nucleus'))+
    geom_line(aes(y=IKBA,color="IKBA"))+
    geom_line(aes(y=NFKB_IKBA, color="NFKB|IKBA"))+
    geom_line(aes(y=IKK_NFKB_IKBA, color="NFKB|IKK|IKBA"))+
    scale_color_manual(values=c("blue","red","green",'yellow','black'))+
    guides(color=guide_legend(title="Component"))+
    geom_vline(xintercept=0, alpha=0.5, linetype="dashed")+
    geom_vline(xintercept=60,alpha=0.5, linetype="dashed")
  
  ggsave(filename="cell_20_IKBA.png",path = output, width=3000, height=2000, units="px")
  
  
  df |> filter(cell.id == "20") |>
    ggplot(mapping=aes(x=time))+
    ggtitle("TNFa over time")+
    geom_line(aes(y=eTNFa,color="eTNFa"))+
    geom_line(aes(y=iTNFa, color='iTNFa'))+
    geom_line(aes(y=TNFa.m,color="TNF mRNA"))+
    geom_line(aes(y=TNFR, color="TNFR"))+
    geom_line(aes(y=TNFa_TNFR, color="TNFa|TNFR"))+
    geom_line(aes(y=TNFR.i, color="internal TNFR"))+
    scale_color_manual(values=c("blue","red","green",'yellow','black','purple'))+
    guides(color=guide_legend(title="Component"))+
    geom_vline(xintercept=0, alpha=0.5, linetype="dashed")+
    geom_vline(xintercept=60,alpha=0.5, linetype="dashed")
  
  ggsave(filename="cell_20_TNFa.png",path = output, width=3000, height=2000, units="px")
}