library(ggplot2)
library(dplyr)
library(tidyr)
library(xml2)
library(XML)
library(EnvStats)

### Preparation
setwd("D:/Uni/Masterarbeit/data")
file_xml <- read_xml("model_base.xml")


parameter = "tmax"
path <- paste("//CellTypes//Property[@symbol=","\"",parameter,"\"]",sep = "")
value <- xml_double(xml_find_all(file_xml,paste(path, "//@value")))
n <- 1
output <- paste("runs/",parameter,sep="")

source("functions.R")
expr <- make_expression(1)

expr_path <- (xml_find_first(file_xml,"//CellPopulations//Population"))
xml_add_child(expr_path,"InitProperty")
xml_set_attr(xml_find_first(file_xml, "//CellPopulations//Population//InitProperty"), "symbol-ref",parameter)
xml_add_child(xml_find_first(file_xml, "//CellPopulations//Population//InitProperty"),"Expression",expr)





for (i in 1:n) {
  # dir <- paste(output,"/",parameter,"=",round(mod_val,3),sep="")
  output <- paste(output,"/run_",formatC(i,width=3,flag='0'),sep="")
  dir.create((output), recursive = TRUE)
  write_xml(file_xml, paste(output,"/model.xml",sep=""))

  
  ## File Output
  command <- paste("py ausfuehren.py ", paste(output,"/model.xml",sep=""), " ",output)
  command <- sprintf(command)
  system(command)
}

# base <- read.csv("base/logger_2.csv", header = TRUE, dec = '.', sep = "\t")

df <- read.csv("runs/tmax/run_001/logger_2.csv", header = TRUE, dec = '.', sep = "\t")
df <- as_tibble(df)

df |> group_by(cell.id) |> select(tmax) |> slice(36) |> print()

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

ggsave(filename="cell_20_NFKB.png",path = output, scale=3)

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

ggsave(filename="cell_20_IKK.png",path = output, scale=3)

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

ggsave(filename="cell_20_IKBA.png",path = output, scale=3)


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

ggsave(filename="cell_20_TNFa.png",path = output, scale=3)

### maximum NFKB
df |> group_by(cell.id) |> slice_max(NFKB.n) |> select(time, NFKB.n, tmax) |>
ggplot(mapping=aes(x=tmax))+
  ggtitle("NFKB Maxima")+
  geom_point(aes(y=time, color=NFKB.n))+
  scale_colour_gradient(high="#FF0000", low = "#0000FF")

ggsave(filename="maximum_NFKB.png",path = output, scale=3)

###maximum TNFa

df |> group_by(cell.id) |> slice_max(eTNFa) |> select(eTNFa, time,tmax) |>
ggplot(mapping=aes(x=tmax))+
  ggtitle("TNFa Maxima")+
  geom_point(aes(y=time, color=eTNFa))+
  scale_colour_gradient(high="#FF0000", low = "#0000FF")

ggsave(filename="maximum_TNFa.png",path = output, scale=3)

## all cells plotted
df |> group_by(cell.id) |>
ggplot(mapping=aes(x=time, y=NFKB.n, group=cell.id,
                              colour=tmax))+
  geom_line()+
  scale_colour_gradient(high="#FF0000", low = "#0000FF")+
  geom_vline(xintercept=0, alpha=0.5, linetype="dashed")+
  geom_vline(xintercept=60,alpha=0.5, linetype="dashed")


ggsave(filename="all_cells_NFKB.png",path = output, scale=3)
