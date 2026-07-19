setwd("D:/Uni/Masterarbeit/data")
source("functions.R")
setup()



init("tmax",7364,5) #12
run(1)


# base <- read.csv("base/logger_2.csv", header = TRUE, dec = '.', sep = "\t")

load()
standard_plots(value_df)
standard_plots(value_df,22)

###### Extra Plots
### maximum NFKB
value_df |> group_by(cell.id) |> slice_max(NFKB.n) |> select(time, NFKB.n, tmax) |>
ggplot(mapping=aes(x=log(tmax)))+
  ggtitle("NFKB Maxima")+
  geom_point(aes(y=time, color=NFKB.n))+
  scale_colour_gradient(high="#FF0000", low = "#0000FF")

ggsave(filename="maximum_NFKB.png",path = save_path, scale=3)

###maximum TNFa

value_df |> group_by(cell.id) |> slice_max(eTNFa) |> select(eTNFa, time,tmax) |>
ggplot(mapping=aes(x=log(tmax)))+
  ggtitle("TNFa Maxima")+
  geom_point(aes(y=time, color=eTNFa))+
  scale_colour_gradient(high="#FF0000", low = "#0000FF")

ggsave(filename="maximum_TNFa.png",path = save_path, scale=3)

## all cells plotted
value_df |> group_by(cell.id) |>
ggplot(mapping=aes(x=time, y=NFKB.n, group=cell.id,
                              colour=log(tmax)))+
  geom_line()+
  scale_colour_gradient(high="#FF0000", low = "#0000FF")+
  geom_vline(xintercept=0, alpha=0.5, linetype="dashed")+
  geom_vline(xintercept=60,alpha=0.5, linetype="dashed")


ggsave(filename="all_cells_NFKB.png",path = save_path, scale=3)

## all cells plotted
value_df |> group_by(cell.id) |>
  ggplot(mapping=aes(x=time, y=eTNFa, group=cell.id,
                     colour=log(tmax)))+
  geom_line()+
  scale_colour_gradient(high="#FF0000", low = "#0000FF")+
  geom_vline(xintercept=0, alpha=0.5, linetype="dashed")+
  geom_vline(xintercept=60,alpha=0.5, linetype="dashed")


ggsave(filename="all_cells_TNFa.png",path = save_path, scale=3)


###Activated receptor fraction
value_df |> group_by(cell.id) |>
  ggplot(mapping=aes(x=time,group=cell.id, color=log(tmax)))+
  geom_line(aes(y=activated_frac))+
  scale_colour_gradient(high="#FF0000", low = "#0000FF")+
  ggtitle("Fraction of activated TNF Receptors")

ggsave(filename="all_cells_TNFR_frac.png",path = save_path, scale=3)

### "Recovery time" of NFKB
value_df |> group_by(cell.id) |> filter(time > 60) |> filter(NFKB.n < 1e-5) |> slice_min(time) |>
  ggplot(mapping=aes(x=log(tmax)))+
  ggtitle("Timepoint when NFKB.n is back close to normal")+
  geom_point(aes(y=time, color=dist))+
  scale_colour_gradient(high="#FF0000", low = "#0000FF")

ggsave(filename="NFKB_calm_down.png",path = save_path, scale=3)


### When are receptors satisfied
value_df |> group_by(cell.id) |> filter(activated_frac >= 0.95) |> slice_min(time) |>
  ggplot(mapping=aes(x=log(tmax)))+
  ggtitle("Timepoint when TNF Receptors are fully satisfied")+
  geom_point(aes(y=time, color=dist))+
  scale_colour_gradient(high="#FF0000", low = "#0000FF")

ggsave(filename="TNFR_fully_satisfied.png",path = save_path, scale=3)

### When are receptors calmed down
value_df |> group_by(cell.id) |> filter(time > 60) |> filter(activated_frac <= 0.05) |> slice_min(time) |>
  ggplot(mapping=aes(x=log(tmax)))+
  ggtitle("Timepoint when TNF Receptors are back to normal")+
  geom_point(aes(y=time, color=dist))+
  scale_colour_gradient(high="#FF0000", low = "#0000FF")

ggsave(filename="TNFR_calmed_down.png",path = save_path, scale=3)


##Kyrogram
value_df |> group_by(cell.id) |>
  ggplot(mapping=aes(x=cell.id,y=time))+
  geom_bar(stat="identity", aes(color=eTNFa))+
  scale_colour_gradient(high="#FF0000", low = "#0000FF")+
  coord_flip()

# value_df |> group_by(cell.id) |> filter(time == 800) |>
#   ggplot(mapping=aes(x=eTNFa))+
#   ggtitle("eTNFa vs. Frac")+
#   geom_point(aes(y=activated_frac, color=dist))+
#   scale_colour_gradient(high="#FF0000", low = "#0000FF")
# 
# ggsave(filename="TNFR_vs_eTNFa.png",path = save_path, scale=3)

x <- seq(0,10,0.001)
m<-0.01
v<-1
plot(x,dlnorm(x,log(0.01),log(1)))
plot(x,plnorm(x,log(m^2/sqrt(m^2+v)),log(1+(v/m^2))))
hist((rlnorm(1000,log(0.01),log(2))))
max(rlnorm(1000,log(m^2/sqrt(m^2+v)),log(1+(v/m^2))))
     