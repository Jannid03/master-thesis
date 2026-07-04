setwd("D:/Uni/Masterarbeit/data")
source("functions.R")
setup()
init("tmax",12)


n <- 1

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
  system(command)
}

# base <- read.csv("base/logger_2.csv", header = TRUE, dec = '.', sep = "\t")

df <- read.csv("runs/tmax/run_002/logger_2.csv", header = TRUE, dec = '.', sep = "\t")
df <- as_tibble(df)

cell_pos <- read.csv("runs/tmax/run_002/logger_1.csv", header = TRUE, dec = '.', sep = "\t")
cell_pos <- as_tibble(cell_pos)

cell_pos <- cell_pos |> group_by(time) |> mutate(new_dist = sqrt((cell.center.x-cell.center.x[20])^2+(cell.center.y-cell.center.y[20])^2))

df <- df |> mutate("dist" = cell_pos$dist)

df |> group_by(cell.id) |> select(tmax) |> slice(36) |> print()

standard_plots(df)


###### Extra Plots
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
