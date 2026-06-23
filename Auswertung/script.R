library(ggplot2)
library(dplyr)
library(tidyr)
library(xml2)
library(XML)
library(EnvStats)

### Preparation
setwd("D:/Uni/Masterarbeit/data")
file_xml <- read_xml("model_base.xml")


parameter = "a1"
path <- paste("//CellTypes//System//Constant[@symbol=","\"",parameter,"\"]",sep = "")
value <- xml_double(xml_find_all(file_xml,paste(path, "//@value")))
n <- 1
output <- paste("runs/",parameter,sep="")

for (i in 1:n) {
  # mod_val <- rnormTrunc(1,value,sd=1,min=0)
  mod_val <- value
  xml_set_attr(xml_find_all(file_xml, path), "value",mod_val)
  dir <- paste(output,"/",parameter,"=",round(mod_val,3),sep="")
  dir.create((dir), recursive = TRUE)
  write_xml(file_xml, paste(dir,"/model.xml",sep=""))

  
  ## File Output
  command <- paste("py ausfuehren.py ", paste(dir,"/model.xml",sep=""), " ",dir)
  command <- sprintf(command)
  output <- system(command)
}

df <- read.csv("runs/a1/a1=1.35/logger_1.csv", header = TRUE, dec = '.', sep = "\t")
df <- as_tibble(df)
df_60 <- df |> slice_head(n=200)
  
ggplot(data=df_60, mapping=aes(x=time))+
  ggtitle("NFKB over time")+
  geom_line(aes(y=NFKB.n,color="NFKB.n"))+
  geom_line(aes(y=NFKB, color='NFKB'))+
  geom_line(aes(y=total_NFKB,color="Total NFKB"))+
  geom_line(aes(y=NFKB_IKBA, color="NFKB|IKBA"))+
  geom_line(aes(y=IKK_NFKB_IKBA, color="NFKB|IKK|IKBA"))+
  scale_color_manual(values=c("blue","red","green",'yellow','black'))+
  guides(color=guide_legend(title="Component"))

ggplot(data=df_60, mapping=aes(x=time))+
  ggtitle("IKK over time")+
  geom_line(aes(y=IKK,color="IKK"))+
  geom_line(aes(y=total_IKK,color="Total IKK"))+
  geom_line(aes(y=IKK_IKBA, color="IKK|IKBA"))+
  geom_line(aes(y=IKK_NFKB_IKBA, color="NFKB|IKK|IKBA"))+
  scale_color_manual(values=c("blue","red","green",'yellow'))+
  guides(color=guide_legend(title="Component"))

ggplot(data=df_60, mapping=aes(x=time))+
  ggtitle("IKBA over time")+
  geom_line(aes(y=IKBA.m,color="IKBA mRNA"))+
  geom_line(aes(y=IKBA.n, color='IKBA Nucleus'))+
  geom_line(aes(y=IKBA,color="IKBA"))+
  geom_line(aes(y=NFKB_IKBA, color="NFKB|IKBA"))+
  geom_line(aes(y=IKK_NFKB_IKBA, color="NFKB|IKK|IKBA"))+
  scale_color_manual(values=c("blue","red","green",'yellow','black'))+
  guides(color=guide_legend(title="Component"))


### maximum NFKB
all_cells <- read.csv("model_base/logger_2.csv", header = TRUE, dec = '.', sep = "\t")
all_cells <- as_tibble(all_cells)

by_cellid <- all_cells |> group_by(cell.id)

maxima <- by_cellid |> slice_max(NFKB.n) |> select(time, NFKB.n, dist)

maximum <- ggplot(data=maxima, mapping=aes(x=dist))+
  ggtitle("NFKB Maxima")+
  geom_point(aes(y=time, color=NFKB.n))+
  scale_colour_gradient(high="#FF0000", low = "#0000FF")

plot(maximum)
print(all_cells)

###maximum TNFa

maxima_tnf <- by_cellid |> slice_max(eTNFa) |> select(eTNFa, time, dist)

ggplot(data=maxima_tnf, mapping=aes(x=dist))+
  ggtitle("TNFa Maxima")+
  geom_point(aes(y=time, color=eTNFa))+
  scale_colour_gradient(high="#FF0000", low = "#0000FF")

## all cells plotted
ggplot(all_cells, mapping=aes(x=time, y=NFKB.n, group=cell.id,
                              colour=dist))+
  geom_line()+
  scale_colour_gradient(high="#FF0000", low = "#0000FF")+
  geom_vline(xintercept=10)
