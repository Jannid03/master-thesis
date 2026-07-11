make_expression <- function(i) {
  if (i == 36){
    return (paste("if(cell.id==36,",rlnorm(1,log(value^2/sqrt(value^2+sd^2)),log(1+(sd^2/value^2))),",",value,")",sep=''))
  }
  else if (i == 20){
    return (paste("if(cell.id==20,",value,",",make_expression(i+1),")",sep=''))
  }
  else {
    expression <- paste("if(cell.id==",i,",",rlnorm(1,log(value^2/sqrt(value^2+sd^2)),log(1+(sd^2/value^2))),",",make_expression(i+1),")",sep='')
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

init <- function(parameter="tmax", seed = "42", sd=1) {
  parameter <<- parameter
  sd <<- sd
  path <<- paste("//CellTypes//Property[@symbol=","\"",parameter,"\"]",sep = "")
  value <<- xml_double(xml_find_all(file_xml,paste(path, "//@value")))
  output <<- paste("runs/",parameter,sep="")
  
  
  ### Cells
  expr <- make_expression(1)
  
  expr_path <<- (xml_find_first(file_xml,"//CellPopulations//Population"))
  xml_add_child(expr_path,"InitProperty")
  xml_set_attr(xml_find_first(file_xml, "//CellPopulations//Population//InitProperty"), "symbol-ref",parameter)
  xml_add_child(xml_find_first(file_xml, "//CellPopulations//Population//InitProperty"),"Expression",expr)
  
  
  ###Seed
  xml_set_attr(xml_find_first(file_xml, "//Time//RandomSeed"), "value",seed)
  
  print("Init done")
}

standard_plots <- function(df, cellid = 20, param = "base", runn = -1) {
  if (runn == -1) {
    save_path <<- paste(output,"/",dir(output)[length(dir(output))],sep="")
  }
  else {
    save_path <<- paste("runs/",param,"/run_",formatC(runn,width=3,flag='0'),sep="")
  }
  
  df |> filter(cell.id == cellid) |>
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
  
  ggsave(filename=paste("cell_",cellid,"_NFKB.png",sep=''),path = save_path, width=3000, height=2000, units="px")
  
  df |> filter(cell.id == cellid) |>
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
  
  ggsave(filename=paste("cell_",cellid,"_IKK.png",sep=''),path = save_path, width=3000, height=2000, units="px")
  
  df |> filter(cell.id == cellid) |>
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
  
  ggsave(filename=paste("cell_",cellid,"_IKBA.png",sep=''),path = save_path, width=3000, height=2000, units="px")
  
  
  df |> filter(cell.id == cellid) |>
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
  
  ggsave(filename=paste("cell_",cellid,"_TNFa.png",sep=''),path = save_path, width=3000, height=2000, units="px")
}

run <- function(n=1) {
  
  if (length(dir(output)) == 0) {
    last <-0
  } else {
    last <- as.numeric(substr(dir(output)[length(dir(output))],5,8))
  }
  
  for (i in 1:n) {
    # dir <- paste(output,"/",parameter,"=",round(mod_val,3),sep="")
    output <- paste(output,"/run_",formatC(last+i,width=3,flag='0'),sep="")
    dir.create((output), recursive = TRUE)
    write_xml(file_xml, paste(output,"/model.xml",sep=""))
    
    
    ## File Output
    command <- paste("py ausfuehren.py ", paste(output,"/model.xml",sep=""), " ",output)
    command <- sprintf(command)
    command_output <- system(command, intern = TRUE)
    write(command_output, paste(output,"/output.txt",sep=""))
  }
}

load <- function(parameter="base", runn=-1) {
  if (runn == -1){
    path <- paste(output,"/",dir(output)[length(dir(output))],sep="")
  }
  else {
    path <- paste("runs/",parameter,"/run_",formatC(run,width=3,flag='0'),sep='')
  }
  
  print(path)
  
  df <- read.csv(paste(path,"/logger_2.csv",sep=''), header = TRUE, dec = '.', sep = "\t")
  df <- as_tibble(df)
  
  cell_pos <- read.csv(paste(path,"/logger_1.csv",sep=''), header = TRUE, dec = '.', sep = "\t")
  cell_pos <- as_tibble(cell_pos)
  
  cell_pos <<- cell_pos |> group_by(time) |> mutate(new_dist = sqrt((cell.center.x-cell.center.x[20])^2+(cell.center.y-cell.center.y[20])^2))
  
  df <- df |> mutate("dist" = cell_pos$dist)
  
  # df |> group_by(cell.id) |> select(tmax) |> slice(36) |> print()
  
  df <<- df |> mutate(activated_frac = TNFa_TNFR/(TNFa_TNFR+TNFR))
}