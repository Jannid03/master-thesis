make_expression <- function(i) {
  if (i == 36){
    return (paste("if(cell.id==36,",rlnorm(1,log(value^2/(sqrt(value^2+sd_s^2))),sqrt(log(1+(sd_s^2/value^2)))),",",value,")",sep=''))
  }
  # else if (i == 20){
  #   return (paste("if(cell.id==20,",value,",",make_expression(i+1),")",sep=''))
  # }
  else {
    expression <- paste("if(cell.id==",i,",",rlnorm(1,log(value^2/(sqrt(value^2+sd_s^2))),sqrt(log(1+(sd_s^2/value^2)))),",",make_expression(i+1),")",sep='')
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

init <- function(parameter, seed = "42", sd_s=2) {
  set.seed(seed)
  parameter <<- parameter
  if(parameter != "base") {
    sd_s <<- sd_s
    path <<- paste("//CellTypes//Property[@symbol=","\"",parameter,"\"]",sep = "")
    value <<- xml_double(xml_find_all(file_xml,paste(path, "//@value")))
    
    
    ### Cells
    expr <- make_expression(1)
    
    expr_path <<- (xml_find_first(file_xml,"//CellPopulations//Population"))
    xml_add_child(expr_path,"InitProperty")
    xml_set_attr(xml_find_first(file_xml, "//CellPopulations//Population//InitProperty"), "symbol-ref",parameter)
    xml_add_child(xml_find_first(file_xml, "//CellPopulations//Population//InitProperty"),"Expression",expr)
    }
  
  
  ###Seed
  xml_set_attr(xml_find_first(file_xml, "//Time//RandomSeed"), "value",seed)
  
  cellorder <<- c(3,2,3,3,3,2,4,2,4,1,3,3,2,2,4,1,1,3,3,0,2,4,2,1,4,3,1,3,2,2,2,3,2,3,3,3)
  output <<- paste("runs/",parameter,sep="")
  
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
  
  print("Run done")
}

load <- function(parameter="base", runn=-1) {
  if (runn == -1){
    path <- paste(output,"/",dir(output)[length(dir(output))],sep="")
  }
  else {
    path <- paste("runs/",parameter,"/run_",formatC(runn,width=3,flag='0'),sep='')
  }
  
  print(path)
  
  df <- read.csv(paste(path,"/logger_2.csv",sep=''), header = TRUE, dec = '.', sep = "\t")
  df <- as_tibble(df)
  
  cell_pos <- read.csv(paste(path,"/logger_1.csv",sep=''), header = TRUE, dec = '.', sep = "\t")
  cell_pos <- as_tibble(cell_pos)
  
  cell_pos <<- cell_pos |> group_by(time) |> mutate(new_dist = sqrt((cell.center.x-cell.center.x[20])^2+(cell.center.y-cell.center.y[20])^2))
  
  df <- df |> mutate("dist" = cell_pos$dist)
  
  # df |> group_by(cell.id) |> select(tmax) |> slice(36) |> print()
  
  value_df <<- df |> mutate(activated_frac = TNFa_TNFR/(TNFa_TNFR+TNFR))
  save_path <<- path
}

auc <- function(x) {
  sum <- 0
  for (j in 1:(length(x)-1)) {
    sum = sum + 1/2 * (x[j]+x[j+1])
  }
  
  return(sum)
}

rocs <- function(value_df, val="NFKB.n") {
  erg <- c()
  for(i in (1:36)) {
    df <- value_df |> filter(cell.id == i)
    x <- df[[val]]
    erg <- c(erg,auc(x))
  }
  return (erg)
}

response_time <- function(valuedf, value="NFKB.n", id=20, t) {
  #-1 because the logger file has -7.XXXe-12 as timepoint 0
  t1 <- valuedf |> filter(time > -1) |> filter(time <= t-1) |> filter(cell.id==id)
  # print(length(t1[[value]]))
  
  t2 <- valuedf |> filter(time >= 1) |> filter(cell.id==id)
  # print(length(t2[[value]]))
  t3 <- (t2-t1)[[value]]
  
  newy <- c(t1[[value]][1])
  for (i in 1:length(t3)) {
    # print(newy[length(newy)]+abs(t3[i]))
    newy <- c(newy, newy[length(newy)]+abs(t3[i]))
  }
  # print(newy)
  maxy <- max(newy)
  auc_t3 <- auc(newy)
  print(auc_t3)
  auc_max <- t*maxy
  A <- auc_max - auc_t3
  
  return(A/(maxy))
}

all_response_times <- function(valuedf,value="NFKB.n",t=800) {
  erg <- c()
  for (i in 1:36) {
    erg <- c(erg,response_time(valuedf, value, i,t))
  }
  return (erg)
}

all_cells <- function(valuedf,ploty="NFKB.n") {
  valuedf |> group_by(cell.id) |>
    ggplot(mapping=aes(x=time, y=.data[[ploty]], group=cell.id,
                       colour=log(tmax)))+
    geom_line()+
    scale_colour_gradient(high="#FF0000", low = "#0000FF")+
    geom_vline(xintercept=0, alpha=0.5, linetype="dashed")+
    geom_vline(xintercept=60,alpha=0.5, linetype="dashed")
  
  
  ggsave(filename=paste("all_cells_",ploty,".png"),path = save_path, scale=3)
}

kymograph <- function(valuedf,plott="NFKB.n",ploty="dist") {
  value_df |> group_by(cell.id) |>
    ggplot(mapping=aes(x=time,y=as.factor(.data[[ploty]])))+
    geom_raster(mapping=aes(fill=.data[[plott]]))+
    scale_fill_gradient(high="#FF0000", low = "#0000FF")
  
  ggsave(filename=paste("kymograph_",plott,"_vs._",ploty,".png"),path = save_path, scale=3)
}

maxima <- function(valuedf,plott="NFKB.n") {
  co <- lm(time ~ log(tmax), as.data.frame(value_df |> group_by(cell.id) |> slice_max(.data[[plott]])))$coefficients
  
  value_df |> group_by(cell.id) |> slice_max(.data[[plott]]) |>
    ggplot(mapping=aes(x=log(tmax)))+
    ggtitle(paste(plott," Maxima"))+
    geom_point(aes(y=time, color=.data[[plott]]))+
    geom_line(mapping=aes(y=co[1]+co[2]*log(tmax)), alpha=0.5, linetype="dashed")+
    scale_colour_gradient(high="#FF0000", low = "#0000FF")
  
  ggsave(filename=paste("maximum_",plott,".png"),path = save_path, scale=3)
}