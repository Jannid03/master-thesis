setwd("D:/Uni/Masterarbeit/data")
source("functions.R")
setup()

#### Loading all Data Frames ####
dfs_base<- load_all(param = "base", until=100)

dfs_tr4b <- load_all(param="tr4b")
result_all_tr4b <- all_analysis(append(dfs_tr4b,dfs_base),param="tr4b")


#### All cells all runs ####
####Final NFKB
#DIst
result_all_tr4b |>
  ggplot()+
  geom_point(aes(x=tr4b,y=final_NFKB,color=dist))+
  facet_wrap(vars(var),ncol=1)+
  scale_color_gradient(high="#FF0000", low = "#0000FF")
#DIst
result_all_tr4b |> filter(cell.id != 20) |>filter(span > 260) |>
  ggplot()+
  geom_point(aes(x=tr4b,y=span,color=dist))+
  facet_wrap(vars(var),ncol=1)+
  scale_color_gradient(high="#FF0000", low = "#0000FF")

#tr4b
result_all_tr4b |> 
  ggplot()+
  geom_point(aes(x=tr4b,y=final_NFKB,color=tr4b))+
  facet_wrap(vars(var),ncol=1)+
  scale_colour_gradient2(high="#FF0000", low = "#0000FF", mid="#d9bd1e",name="tr4b",midpoint=0.01)

model <- result_all_tr4b |> filter(cell.id !=20)
final_NFKB_model <- lm(final_NFKB ~ dist+tr4b, data=model)
summary(final_NFKB_model)

####Max NFKB
#Dist
result_all_tr4b |> filter(cell.id != 20) |>
  ggplot()+
  geom_point(aes(x=tr4b,y=max_NFKB,color=dist))+
  facet_wrap(vars(var),ncol=1)+
  scale_color_gradient(high="#FF0000", low = "#0000FF")

#tr4b
result_all_tr4b |>
  ggplot()+
  geom_point(aes(x=dist,y=max_NFKB,color=tr4b))+
  facet_wrap(vars(var),ncol=1)+
  scale_colour_gradient2(high="#FF0000", low = "#0000FF", mid="#d9bd1e",name="tr4b",midpoint=0.01)

#Time max NFKB
result_all_tr4b |> filter(cell.id != 20) |>
  ggplot()+
  geom_point(aes(x=tr4b,y=NFKB_time,color=dist))+
  facet_wrap(vars(var),ncol=1)+
  scale_color_gradient(high="#FF0000", low = "#0000FF")

#final eTNFa
result_all_tr4b |>
  ggplot()+
  geom_point(aes(x=tr4b,y=eTNFa_final,color=dist))+
  facet_wrap(vars(var),ncol=1)+
  scale_color_gradient(high="#FF0000", low = "#0000FF")

#max eTNFa
result_all_tr4b |> filter(cell.id != 20) |>
  ggplot()+
  geom_point(aes(x=tr4b,y=max_eTNFa,color=dist))+
  facet_wrap(vars(var),ncol=1)+
  scale_color_gradient(high="#FF0000", low = "#0000FF")

result_all_tr4b |>
  ggplot()+
  geom_point(aes(x=dist,y=max_eTNFa,color=tr4b))+
  facet_wrap(vars(var),ncol=1)+
  scale_colour_gradient2(high="#FF0000", low = "#0000FF", mid="#d9bd1e",name="tr4b",midpoint=0.01)

result_all_tr4b |>
  ggplot()+
  geom_point(aes(x=order,y=max_eTNFa,color=tr4b))+
  facet_wrap(vars(var),ncol=1)+
  scale_colour_gradient2(high="#FF0000", low = "#0000FF", mid="#d9bd1e",name="tr4b",midpoint=0.01)

#Time max eTNFa
result_all_tr4b |> filter(cell.id != 20) |>
  ggplot()+
  geom_point(aes(x=tr4b,y=eTNFa_time,color=dist))+
  facet_wrap(vars(var),ncol=1)+
  scale_color_gradient(high="#FF0000", low = "#0000FF")


#TNFR frac end
result_all_tr4b |>
  ggplot()+
  geom_point(aes(x=(tr4b),y=iTNFa_final,color=dist))+
  facet_wrap(vars(var),ncol=1)+
  scale_color_gradient(high="#FF0000", low = "#0000FF")




#tr4b vs Dist, color:max_NFKB
result_all_tr4b |>
  ggplot()+
  geom_point(aes(x=tr4b,y=dist,color=max_NFKB))+
  scale_colour_gradient2(high="#FF0000", low = "#0000FF", mid="#d9bd1e",name="NFKB value",midpoint=0.005)+
  facet_wrap(vars(var),ncol=1)

#tr4b vs Dist, color:max_eTNFa
result_all_tr4b |>
  ggplot()+
  geom_point(aes(x=tr4b,y=dist,color=max_eTNFa))+
  scale_colour_gradient2(high="#FF0000", low = "#0000FF", mid="#d9bd1e",name="eTNFa value",midpoint=0.02)

#tr4b vs Dist, color:last_NFKB
result_all_tr4b |>
  ggplot()+
  geom_point(aes(x=tr4b,y=dist,color=final_NFKB))+
  scale_colour_gradient2(high="#FF0000", low = "#0000FF", mid="#d9bd1e",name="NFKB value",midpoint=3e-10)

#tr4b vs Dist, color:last_eTNFa
result_all_tr4b |>
  ggplot()+
  geom_point(aes(x=tr4b,y=dist,color=eTNFa_final))+
  scale_colour_gradient2(high="#FF0000", low = "#0000FF", mid="#d9bd1e",name="eTNFa value",midpoint=3e-13)


#Dist vs max_eTNFa, color:tr4b
result_all |> filter(cell.id !=20) |>
  ggplot()+
  geom_point(aes(x=dist,y=max_eTNFa,color=tr4b))+
  scale_colour_gradient2(high="#FF0000", low = "#0000FF", mid="#d9bd1e",name="tr4b",midpoint=0.01)

#Order vs max_eTNFa, color:tr4b
result_all |> filter(cell.id !=20) |>
  ggplot()+
  geom_point(aes(x=order,y=max_eTNFa,color=tr4b))+
  scale_colour_gradient2(high="#FF0000", low = "#0000FF", mid="#d9bd1e",name="tr4b",midpoint=0.01)

#tr4b vs max_eTNFa, color:Dist
result_all |>
  ggplot()+
  geom_point(aes(x=tr4b,y=max_eTNFa,color=dist))+
  scale_colour_gradient2(high="#FF0000", low = "#0000FF", mid="#d9bd1e",name="tr4b",midpoint=35)

result_all |> 
  ggplot()+
  geom_histogram(aes(x=tr4b, fill=var))+
  facet_wrap(vars(var),ncol=1)

base_var <- as.factor(0)
base_max_NFKB <- base_df$df |> group_by(cell.id) |> slice_max(NFKB.n)
base_max_eTNFa <- base_df$df |> group_by(cell.id) |> slice_max(eTNFa)
base_final <- base_df$df |> group_by(cell.id) |> filter(time==800)

temp <- data.frame(rep(as.factor(0),36),base_max_NFKB[["NFKB.n"]],base_max_NFKB[["time"]],base_final[["NFKB.n"]],rep(0,36),1:36,base_max_eTNFa[["eTNFa"]],base_max_eTNFa[["time"]],base_final[["eTNFa"]],base_final[["activated_frac"]],base_final[["dist"]],base_final[["order"]],base_final[["tr4b"]])

###Hist of NFKB_max
result_all |> 
  ggplot()+
  geom_boxplot(aes(x=var,y=max_NFKB))

#### Linear regression
model <- result_all |> filter(cell.id !=20) |> mutate(dist_mean=mean(dist)) |> mutate(sd_dist=sd(dist)) |> mutate(norm_dist=(dist-dist_mean)/sd_dist) |> mutate(tr4b_mean=mean(tr4b)) |> mutate(sd_tr4b=sd(tr4b)) |> mutate(norm_tr4b=(tr4b-tr4b_mean)/sd_tr4b)
max_NFKB_model <- lm(max_NFKB ~ norm_tr4b+norm_dist, data=model)
summary(max_NFKB_model)

time_NFKB_model <- lm(NFKB_time ~ norm_tr4b+norm_dist, data=model)
summary(time_NFKB_model)

time_eTNFa_model <- lm(eTNFa_time ~ norm_tr4b+norm_dist, data=model)
summary(time_eTNFa_model)

max_eTNFa_model <- lm(max_eTNFa ~  norm_tr4b+norm_dist, data=model)
summary(max_eTNFa_model)

#### Run wise####
summ_tr4b <- result_all_tr4b |> filter(cell.id!=20) |> group_by(run) |> summarise(avg_NFKB_high = mean(max_NFKB), Variation = first(var), avg_tr4b = mean(tr4b), avg_NFKB_time = mean(NFKB_time), avg_eTNFa = mean(max_eTNFa), avg_eTNFa_time = mean(eTNFa_time),sd_tr4b = sd(tr4b), sd_NFKB_time = sd(NFKB_time), sd_eTNFa = sd(max_eTNFa), sd_eTNFa_time = sd(eTNFa_time),sd_NFKB_high = sd(max_NFKB), avg_TNFR_50 = mean(TNFR_50), sd_TNFR_50 = sd(TNFR_50), avg_iTNFa=mean(max_iTNFa))

summ_tr4b |>
  ggplot(aes(x=Variation,y=avg_iTNFa, color=Variation))+
  geom_boxplot()+
  stat_compare_means(comparisons=list(c("0","10"),c("0","25"),c("0","50")),label="p.signif",method = "wilcox.test")+
  ylab("NFKB concentration")+
  xlab("")+
  ggtitle("Average Maximum iTNFa concentration", subtitle="Per run")

##NFKB high
summ_tr4b |>
  ggplot(aes(x=Variation,y=avg_NFKB_high, color=Variation))+
  geom_boxplot()+
  stat_compare_means(comparisons=list(c("0","10"),c("0","25"),c("0","50")),label="p.signif",method = "wilcox.test")+
  ylab("NFKB concentration")+
  xlab("")+
  ggtitle("Average Maximum nuclear NFKB concentration", subtitle="Per run")

#Avg tr4b vs avg_NFKB

summ_tr4b |>
  ggplot(aes(x=Variation,y=avg_NFKB_high, color=Variation))+
  geom_boxplot()+
  stat_compare_means(comparisons=list(c("0","10"),c("0","25"),c("0","50")),label="p.signif",method = "wilcox.test")+
  ylab("NFKB concentration")+
  xlab("")+
  ggtitle("Average Maximum nuclear NFKB concentration", subtitle="Per run")

#Avg tr4b vs avg_NFKB_time

summ_tr4b |>
  ggplot(aes(x=Variation,y=avg_NFKB_time, color=Variation))+
  geom_boxplot()+
  stat_compare_means(comparisons=list(c("0","10"),c("0","25"),c("0","50")),label="p.signif",method = "wilcox.test")+
  ylab("Time of Maximum")+
  xlab("")+
  ggtitle("Average Maximum nuclear NFKB Time", subtitle="Per run")

summ_tr4b |> 
  ggplot(aes(x=avg_NFKB_time))+
  geom_histogram(bins = 50)+
  facet_wrap(vars(Variation),ncol=1)

#Avg tr4b vs avg_eTNFa

summ_tr4b |>
  ggplot(aes(x=Variation,y=avg_eTNFa, color=Variation))+
  geom_boxplot()+
  stat_compare_means(comparisons=list(c("0","10"),c("0","25"),c("0","50")),label="p.signif",method = "wilcox.test")+
  ylab("eTNFa concentration")+
  xlab("")+
  ggtitle("Average Maximum external TNFa concentration", subtitle="Per run")

#Avg tr4b vs avg_eTNFa_time
summ_tr4b |>
  ggplot(aes(x=Variation,y=avg_eTNFa_time, color=Variation))+
  geom_boxplot()+
  stat_compare_means(comparisons=list(c("0","10"),c("0","25"),c("0","50")),label="p.signif",method = "wilcox.test")+
  ylab("eTNFa concentration")+
  xlab("")+
  ggtitle("Average Maximum external TNfa time", subtitle="Per run")

#sd tr4b vs sd_NFKB

summ_tr4b |>
  ggplot(aes(x=Variation,y=sd_NFKB_high, color=Variation))+
  geom_boxplot()+
  stat_compare_means(comparisons=list(c("0","10"),c("0","25"),c("0","50")),label="p.signif",method = "wilcox.test")+
  ylab("SD")+
  xlab("")+
  ggtitle("SD Maximum nuclear NFKB concentration", subtitle="Per run")

#sd tr4b vs sd_NFKB_time
summ_tr4b |>
  ggplot(aes(x=Variation,y=sd_NFKB_time, color=Variation))+
  geom_boxplot()+
  stat_compare_means(comparisons=list(c("0","10"),c("0","25"),c("0","50")),label="p.signif",method = "wilcox.test")+
  ylab("SD")+
  xlab("")+
  ggtitle("SD Maximum nuclear NFKB Time", subtitle="Per run")

#sd tr4b vs sd_eTNFa
summ_tr4b |>
  ggplot(aes(x=Variation,y=sd_eTNFa,color=Variation))+
  geom_boxplot()+
  stat_compare_means(comparisons=list(c("0","10"),c("0","25"),c("0","50")),label="p.signif",method = "wilcox.test")+
  ylab("SD")+
  xlab("")+
  ggtitle("SD Maximum external TNFa concentration", subtitle="Per run")

#sd tr4b vs sd_eTNFa_time
summ_tr4b |>
  ggplot(aes(x=Variation,y=sd_eTNFa_time, color=Variation))+
  geom_boxplot()+
  stat_compare_means(comparisons=list(c("0","10"),c("0","25"),c("0","50")),label="p.signif",method = "wilcox.test")+
  ylab("SD")+
  xlab("")+
  ggtitle("SD Maximum external TNfa time", subtitle="Per run")

#sd tr4b vs avg_TNFR50
summ_tr4b |>
  ggplot(aes(x=Variation,y=avg_TNFR_50, color=Variation))+
  geom_boxplot()+
  stat_compare_means(comparisons=list(c("0","10"),c("0","25"),c("0","50")),label="p.signif",method = "wilcox.test")+
  ylab("Time")+
  xlab("")+
  ggtitle("Average TNFR 50% Time", subtitle="Per run")

#sd tr4b vs sd_TNFR50
summ_tr4b |>
  ggplot(aes(x=Variation,y=sd_TNFR_50, color=Variation))+
  geom_boxplot()+
  stat_compare_means(comparisons=list(c("0","10"),c("0","25"),c("0","50")),label="p.signif",method = "wilcox.test")+
  ylab("SD")+
  xlab("")+
  ggtitle("SD TNFR 50% Time", subtitle="Per run")

#### GGpairs####
ggpairs(summ_tr4b,columns=c("avg_tr4b","avg_NFKB_high", "avg_eTNFa", "sd_NFKB_high"),aes(color=Variation, alpha=.5, fill=Variation), diag=list(continuous="barDiag"))
ggpairs(result_all_tr4b,columns=c("dist","tr4b","max_NFKB","NFKB_time"),aes(color=var, alpha=.05))

###### Extra Plots ####
value_df <- load(parameter = "tr4b", runn = 002) 
standard_plots(value_df, cellid = 12)
### all cells plotted
all_cells(value_df, color="tr4b", startt=-500)
all_cells(value_df,"iTNFa", color="tr4b", startt=-500)
all_cells(value_df, "activated_frac", color="tr4b")
all_cells(value_df, "TNFaflux", color="tr4b")

### maxima NFKB
maxima(value_df,plotx="dist")
maxima(value_df,ploty="NFKB.n",plotx="tr4b")
maxima(value_df,ploty="dist",max="eTNFa",color="tr4b")
maxima(value_df,ploty="eTNFa",plotx="dist", color="tr4b")
maxima(value_df,"NFKB.n", plotx="tr4b")
maxima(value_df,"eTNFa", plotx="tr4b")
maxima(value_df,ploty="eTNFa",plotx="dist",color="tr4b")

##Kymographs
kymograph(value_df)
kymograph(value_df,"eTNFa")

###AUCS
auc_plot(value_df)
auc_plot(value_df,"eTNFa")

### Response times
response_plot(value_df)
response_plot(value_df,"eTNFa")

plotlist <- arranging(c(load(parameter = "tr4b", runn = 1),
                        load(parameter = "tr4b", runn = 101),
                        load(parameter = "tr4b", runn = 201),
                        load(parameter = "base", runn = 301)),title=c('10%','25%','50%','base'),color="tr4b")

ggarrange(plotlist[4][[1]],plotlist[1][[1]],plotlist[2][[1]], plotlist[3][[1]], ncol=2, nrow=2, common.legend = TRUE, legend="right")
