##### Functions to use in the Script #####

#### TECHNICAL ####
linestyle <- function () {
  n <- rep(1,36)
  n[20] <- 2
  
  return(rep(n,each=1051))
}

pointstyle <- function() {
  n <- rep(21,36)
  n[20] <- 22
  
  return(n)
}

###Setup for libraries etc.
setup <- function() {
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(xml2)
  library(XML)
  library(EnvStats)
  library(patchwork)
  library(ggpubr)
  
  print("Setup done")
}

### Initializing the desired XML File
init <- function(parameter, seed = "42", sd_s=2) {
  ### Preparation
  file_xml <<- read_xml("model_base.xml")
  
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

#Recursive function, drawing parameter values from lognormal distribution
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

### Running the modified XML file
run <- function(n=TRUE,num=1) {
  
  if(n) {
    last <- num-1
  }
  else if (length(dir(output)) == 0) {
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

### Loading the desired results
load <- function(parameter="base", runn=-1) {
  if (runn == -1){
    path <- paste(output,"/",dir(output)[length(dir(output))],sep="")
  }
  else {
    path <- paste("runs/",parameter,"/run_",formatC(runn,width=3,flag='0'),sep='')
  }
  
  df <- read.csv(paste(path,"/logger_2.csv",sep=''), header = TRUE, dec = '.', sep = "\t")
  df <- as_tibble(df)
  
  cell_pos <- read.csv(paste(path,"/logger_1.csv",sep=''), header = TRUE, dec = '.', sep = "\t")
  cell_pos <- as_tibble(cell_pos)
  
  cell_pos <- cell_pos |> group_by(time) |> mutate(new_dist = sqrt((cell.center.x-cell.center.x[20])^2+(cell.center.y-cell.center.y[20])^2))
  
  df <- df |> mutate("dist" = cell_pos$dist)
  df <- df |> mutate("order" = rep(cellorder,times = 1051))
  
  # df |> group_by(cell.id) |> select(tmax) |> slice(36) |> print()
  
  df <- df |> mutate(activated_frac = TNFa_TNFR/(TNFa_TNFR+TNFR))
  
  return(list("df"=df,"save_path"=path, "cell_pos"=cell_pos))
}

#### MATH FUNCTIONS ####

### Area under the curve
auc <- function(x) {
  sum <- 0
  for (j in 1:(length(x)-1)) {
    sum = sum + 1/2 * (x[j]+x[j+1])
  }
  
  return(sum)
}

### Area under all curves
all_auc <- function(value_df, val="NFKB.n") {
  erg <- c()
  for(i in (1:36)) {
    df <- value_df |> filter(cell.id == i)
    x <- df[[val]]
    erg <- c(erg,auc(x))
  }
  return (erg)
}

### Responstime according to Llorens 1999
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
  # print(auc_t3)
  auc_max <- t*maxy
  A <- auc_max - auc_t3
  
  return(A/(maxy))
}

### Response time for all curves
all_response_times <- function(valuedf,value="NFKB.n",t=800) {
  erg <- c()
  for (i in 1:36) {
    erg <- c(erg,response_time(valuedf, value, i,t))
  }
  return (erg)
}

#### PLOTTING ####

### Plotting all variable species for a specific cell.id
standard_plots <- function(dflist, cellid = 20) {
  df <- dflist$df
  save_path <- dflist$save_path
  
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

### Plotting all curves of all cell ids 
all_cells <- function(dflist,ploty="NFKB.n", color="foldtmax", scaled=FALSE) {
  valuedf <- dflist$df
  save_path <- dflist$save_path
  
  valuedf <- valuedf |> group_by(cell.id)
  
  if(substr(color,1,3) == "log") {
    color <- substring(color,first=4)
    color_v <- log(valuedf[[color]])
    logs <- "_log_"
    name <- paste("Log",color)
  } else if (substr(color,1,4) == "fold") {
    color <- substring(color,first=5)
    color_v <- (valuedf[[color]])/value
    logs <- "_fold_"
    name <- paste("Fold",color)
  }
  else {
    color_v <- valuedf[[color]]
    logs <- "_"
    name <- color
  }
  
  if(scaled) {
    color_v <- scales::rescale(color_v,to=c(0,1))
    mp <- 0.5
  }
  else {
    mp <- (min(color_v) + max(color_v) )/2
  }
  
  pl <- valuedf |>
    ggplot(mapping=aes(x=time, y=.data[[ploty]], group=cell.id))+
    #geom_line(linewidth=0.55, color="black")+
    geom_line(aes(colour=color_v),linetype=linestyle())+
    scale_colour_gradient2(high="#FF0000", low = "#0000FF", mid="#FFFFFF", midpoint=mp, name=name)+
    geom_vline(xintercept=0, alpha=0.5, linetype="dashed")+
    geom_vline(xintercept=60,alpha=0.5, linetype="dashed")
  
  
  ggsave(filename=paste("all_cells_",ploty,logs,color,".png"),path = save_path, width=3000, height=2000, units="px")
  
  return(pl)
}

### Plotting kymograph 
kymograph <- function(dflist,plott="NFKB.n",ploty="dist") {
  valuedf <- dflist$df
  save_path <- dflist$save_path
  
  pl <- valuedf |> group_by(cell.id) |>
    ggplot(mapping=aes(x=time,y=as.factor(.data[[ploty]])))+
    geom_raster(mapping=aes(fill=.data[[plott]]))+
    scale_fill_gradient(high="#FF0000", low = "#0000FF")
  
  ggsave(filename=paste("kymograph_",plott,"_vs_",ploty,".png"),path = save_path, width=3000, height=2000, units="px")
  
  return(pl)
}

### Plotting time of Maximas of plott vs. plotx
maxima <- function(dflist,plott="NFKB.n",plotx="foldtmax",scalex=FALSE) {
  valuedf <- dflist$df
  save_path <- dflist$save_path
  
  valuedf <- valuedf |> group_by(cell.id) |> slice_max(.data[[plott]])
  if(substr(plotx,1,3) == "log") {
    x <- substring(plotx,first=4)
    x_v <- log(valuedf[[x]])
    logs <- "_log_"
    namex <- paste("Log",x)
  } else if (substr(plotx,1,4) == "fold") {
    x <- substring(plotx,first=5)
    x_v <- (valuedf[[x]])/value
    logs <- "_fold_"
    namex <- paste("Fold",x)
  } else {
    x <- plotx
    x_v <- valuedf[[x]]
    logs <- "_"
    namex <- x
  }
  
  if(scalex) {
    x_v <- scales::rescale(x_v,to=c(0,1))
  }
  
  mp <- (min(valuedf[[plott]]) + max(valuedf[[plott]]) )/2
  co <- lm(time ~ x_v, as.data.frame(valuedf))$coefficients

  pl <- valuedf |>
    ggplot(mapping=aes(x=x_v))+
    ggtitle(paste(plott," Maxima"))+
    geom_point(shape = pointstyle(),stroke=0.5, color="black",aes(y=time,fill=.data[[plott]]))+
    geom_line(mapping=aes(y=co[1]+co[2]*x_v), alpha=0.5, linetype="dashed")+
    scale_fill_gradient2(high="#FF0000", low = "#0000FF", mid="#FFFFFF", midpoint=mp, name="NFKB value")+
    xlab(namex)
  
  ggsave(filename=paste("maximum_",plott,logs,x,".png"),path = save_path, width=3000, height=2000, units="px")
  
  return(pl)
}

### Plotting AUCs of all plott curves vs. plotx
auc_plot <- function(dflist,plott="NFKB.n",plotx="foldtmax",color="dist") {
  valuedf <- dflist$df
  save_path <- dflist$save_path
  
  auc_val <- all_auc(valuedf,plott)
  
  valuedf <- valuedf |> group_by(cell.id) |> filter(time==1)
  
  if(substr(color,1,3) == "log") {
    color <- substring(color,first=4)
    color_v <- log(valuedf[[color]])
    logs <- "_log_"
    name <- paste("Log",color)
  } else if (substr(color,1,4) == "fold") {
    color <- substring(color,first=5)
    color_v <- (valuedf[[color]])/value
    logs <- "_fold_"
    name <- paste("Fold",color)
  }else {
    color_v <- valuedf[[color]]
    logs <- "_"
    name <- color
  }
  
  
  if(substr(plotx,1,3) == "log") {
    x <- substring(plotx,first=4)
    x_v <- log(valuedf[[x]])
    logs_x <- "_log_"
  } else if (substr(plotx,1,4) == "fold") {
    x <- substring(plotx,first=5)
    x_v <- (valuedf[[x]])/value
    logs_x <- "_fold_"
    namex <- paste("Fold",x)
  } else {
    x <- plotx
    x_v <- valuedf[[x]]
    logs_x <- "_"
  }
  
  mp <- (min(color_v) + max(color_v) )/2
  
  pl <- valuedf |>
    ggplot(mapping=aes(x=x_v))+
    geom_point(shape = 21,stroke=0.5, color="black",aes(y=auc_val,fill=color_v))+
    scale_fill_gradient2(high="#FF0000", low = "#0000FF", mid="#FFFFFF", midpoint=mp, name=name)+
    xlab(namex)
  
  ggsave(filename=paste("AUC_",plott,logs_x,x,logs,color,".png"),path = save_path, width=3000, height=2000, units="px")
  
  return(pl)
}

### Plotting response times of all plott curves
response_plot <- function(dflist,plott="NFKB.n",plotx="foldtmax",color="dist",t=800) {
  valuedf <- dflist$df
  save_path <- dflist$save_path
  
  resptimes <- all_response_times(valuedf,plott,t)
  valuedf <- valuedf |> group_by(cell.id) |> filter(time==1)
  
  if(substr(color,1,3) == "log") {
    color <- substring(color,first=4)
    color_v <- log(valuedf[[color]])
    logs <- "_log_"
    name <- paste("Log",color)
  } else if (substr(color,1,4) == "fold") {
    color <- substring(color,first=5)
    color_v <- (valuedf[[color]])/value
    logs <- "_fold_"
    name <- paste("Fold",color)
  } else {
    color_v <- valuedf[[color]]
    logs <- "_"
    name <- color
  }
  
  
  if(substr(plotx,1,3) == "log") {
    x <- substring(plotx,first=4)
    x_v <- log(valuedf[[x]])
    logs_x <- "_log_"
    namex <- paste("Log",x)
  } else if (substr(plotx,1,4) == "fold") {
    x <- substring(plotx,first=5)
    x_v <- (valuedf[[x]])/value
    logs_x <- "_fold_"
    namex <- paste("Fold",x)
  } else {
    x <- plotx
    x_v <- valuedf[[x]]
    logs_x <- "_"
    namex <- x
  }
  
  
  co <- lm(resptimes ~ x_v, as.data.frame(valuedf))$coefficients
  mp <- (min(color_v) + max(color_v) )/2
  
  pl <- valuedf |>
    ggplot(mapping=aes(x=x_v))+
    geom_point(shape = 21,stroke=0.5, color="black",aes(y=resptimes,fill=color_v))+
    geom_line(mapping=aes(y=co[1]+co[2]*x_v), alpha=0.5, linetype="dashed")+
    scale_fill_gradient2(high="#FF0000", low = "#0000FF", mid="#FFFFFF", midpoint=mp, name=name)+
    xlab(namex)
  
  ggsave(filename=paste("response_time_",plott,logs_x,x,logs,color,".png"),path = save_path, width=3000, height=2000, units="px")
  
  return(pl)
  
}