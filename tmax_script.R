setwd("D:/Uni/Masterarbeit/data")
source("functions.R")
setup()

#### Loading all Data Frames ####
dfs_base<- load_all(param = "base", from=301,until=400)

dfs_rRef <- load_all()
result_all_rRef <- all_analysis(append(dfs_rRef,dfs_base),param="rRef")
#

#### All cells all runs ####
####Final NFKB
#DIst
result_all_rRef |>
ggplot()+
  geom_point(aes(x=rRef,y=final_NFKB,color=dist))+
  facet_wrap(vars(var),ncol=1)+
  scale_color_gradient(high="#FF0000", low = "#0000FF")

result_all_rRef |> filter(cell.id != 20) |>
  ggplot()+
  geom_point(aes(x=rRef,y=span,color=dist))+
  facet_wrap(vars(var),ncol=1)+
  scale_color_gradient(high="#FF0000", low = "#0000FF")

#rRef
result_all_rRef |>
  ggplot()+
  geom_point(aes(x=rRef,y=final_NFKB,color=rRef))+
  facet_wrap(vars(var),ncol=1)+
  scale_colour_gradient2(high="#FF0000", low = "#0000FF", mid="#d9bd1e",name="rRef",midpoint=0.01)

model <- result_all_rRef |> filter(cell.id !=20)
final_NFKB_model <- lm(final_NFKB ~ dist+rRef, data=model)
summary(final_NFKB_model)

####Max NFKB
#Dist
result_all_rRef |>
  ggplot()+
  geom_point(aes(x=rRef,y=max_NFKB,color=dist))+
  facet_wrap(vars(var),ncol=1)+
  scale_color_gradient(high="#FF0000", low = "#0000FF")

#rRef
result_all_rRef |>
  ggplot()+
  geom_point(aes(x=dist,y=max_NFKB,color=rRef))+
  facet_wrap(vars(var),ncol=1)+
  scale_colour_gradient2(high="#FF0000", low = "#0000FF", mid="#d9bd1e",name="rRef",midpoint=0.01)

#Time max NFKB
result_all_rRef |> 
  ggplot()+
  geom_point(aes(x=rRef,y=NFKB_time,color=dist))+
  facet_wrap(vars(var),ncol=1)+
  scale_color_gradient(high="#FF0000", low = "#0000FF")

#final eTNFa
result_all_rRef |>
  ggplot()+
  geom_point(aes(x=rRef,y=eTNFa_final,color=dist))+
  facet_wrap(vars(var),ncol=1)+
  scale_color_gradient(high="#FF0000", low = "#0000FF")

#max eTNFa
result_all_rRef |>
  ggplot()+
  geom_point(aes(x=rRef,y=max_eTNFa,color=dist))+
  facet_wrap(vars(var),ncol=1)+
  scale_color_gradient(high="#FF0000", low = "#0000FF")

result_all_rRef |>
  ggplot()+
  geom_point(aes(x=dist,y=max_eTNFa,color=rRef))+
  facet_wrap(vars(var),ncol=1)+
  scale_colour_gradient2(high="#FF0000", low = "#0000FF", mid="#d9bd1e",name="rRef",midpoint=0.01)

result_all_rRef |>
  ggplot()+
  geom_point(aes(x=order,y=max_eTNFa,color=rRef))+
  facet_wrap(vars(var),ncol=1)+
  scale_colour_gradient2(high="#FF0000", low = "#0000FF", mid="#d9bd1e",name="rRef",midpoint=0.01)

#Time max eTNFa
result_all_rRef |>
  ggplot()+
  geom_point(aes(x=rRef,y=eTNFa_time,color=dist))+
  facet_wrap(vars(var),ncol=1)+
  scale_color_gradient(high="#FF0000", low = "#0000FF")


#TNFR frac end
result_all_rRef |>
  ggplot()+
  geom_point(aes(x=(rRef),y=TNFR_frac_end,color=dist))+
  facet_wrap(vars(var),ncol=1)+
  scale_color_gradient(high="#FF0000", low = "#0000FF")

result_all_rRef |>
  ggplot()+
  geom_point(aes(x=(rRef),y=TNFR_50,color=dist))+
  facet_wrap(vars(var),ncol=1)+
  scale_color_gradient(high="#FF0000", low = "#0000FF")


#rRef vs Dist, color:max_NFKB
result_all_rRef |>
  ggplot()+
  geom_point(aes(x=rRef,y=dist,color=max_NFKB))+
  scale_colour_gradient2(high="#FF0000", low = "#0000FF", mid="#d9bd1e",name="NFKB value",midpoint=0.005)+
  facet_wrap(vars(var),ncol=1)

#rRef vs Dist, color:max_eTNFa
result_all_rRef |>
  ggplot()+
  geom_point(aes(x=rRef,y=dist,color=max_eTNFa))+
  scale_colour_gradient2(high="#FF0000", low = "#0000FF", mid="#d9bd1e",name="eTNFa value",midpoint=0.02)

#rRef vs Dist, color:last_NFKB
result_all_rRef |>
  ggplot()+
  geom_point(aes(x=rRef,y=dist,color=final_NFKB))+
  scale_colour_gradient2(high="#FF0000", low = "#0000FF", mid="#d9bd1e",name="NFKB value",midpoint=3e-10)

#rRef vs Dist, color:last_eTNFa
result_all_rRef |>
  ggplot()+
  geom_point(aes(x=rRef,y=dist,color=eTNFa_final))+
  scale_colour_gradient2(high="#FF0000", low = "#0000FF", mid="#d9bd1e",name="eTNFa value",midpoint=3e-13)


#Dist vs max_eTNFa, color:rRef
result_all |> filter(cell.id !=20) |>
  ggplot()+
  geom_point(aes(x=dist,y=max_eTNFa,color=rRef))+
  scale_colour_gradient2(high="#FF0000", low = "#0000FF", mid="#d9bd1e",name="rRef",midpoint=0.01)

#Order vs max_eTNFa, color:rRef
result_all |> filter(cell.id !=20) |>
  ggplot()+
  geom_point(aes(x=order,y=max_eTNFa,color=rRef))+
  scale_colour_gradient2(high="#FF0000", low = "#0000FF", mid="#d9bd1e",name="rRef",midpoint=0.01)

#rRef vs max_eTNFa, color:Dist
result_all |>
  ggplot()+
  geom_point(aes(x=rRef,y=max_eTNFa,color=dist))+
  scale_colour_gradient2(high="#FF0000", low = "#0000FF", mid="#d9bd1e",name="rRef",midpoint=35)

result_all |> 
  ggplot()+
  geom_histogram(aes(x=rRef, fill=var))+
  facet_wrap(vars(var),ncol=1)

base_var <- as.factor(0)
base_max_NFKB <- base_df$df |> group_by(cell.id) |> slice_max(NFKB.n)
base_max_eTNFa <- base_df$df |> group_by(cell.id) |> slice_max(eTNFa)
base_final <- base_df$df |> group_by(cell.id) |> filter(time==800)

temp <- data.frame(rep(as.factor(0),36),base_max_NFKB[["NFKB.n"]],base_max_NFKB[["time"]],base_final[["NFKB.n"]],rep(0,36),1:36,base_max_eTNFa[["eTNFa"]],base_max_eTNFa[["time"]],base_final[["eTNFa"]],base_final[["activated_frac"]],base_final[["dist"]],base_final[["order"]],base_final[["rRef"]])

###Hist of NFKB_max
result_all |> 
  ggplot()+
  geom_boxplot(aes(x=var,y=max_NFKB))

#### Linear regression
model <- result_all |> filter(cell.id !=20) |> mutate(dist_mean=mean(dist)) |> mutate(sd_dist=sd(dist)) |> mutate(norm_dist=(dist-dist_mean)/sd_dist) |> mutate(rRef_mean=mean(rRef)) |> mutate(sd_rRef=sd(rRef)) |> mutate(norm_rRef=(rRef-rRef_mean)/sd_rRef)
max_NFKB_model <- lm(max_NFKB ~ norm_rRef+norm_dist, data=model)
summary(max_NFKB_model)

time_NFKB_model <- lm(NFKB_time ~ norm_rRef+norm_dist, data=model)
summary(time_NFKB_model)

time_eTNFa_model <- lm(eTNFa_time ~ norm_rRef+norm_dist, data=model)
summary(time_eTNFa_model)

max_eTNFa_model <- lm(max_eTNFa ~  norm_rRef+norm_dist, data=model)
summary(max_eTNFa_model)

#### Run wise####
summ_rRef <- result_all_rRef |> filter(cell.id!=20) |> group_by(run) |> summarise(avg_NFKB_high = mean(max_NFKB), Variation = first(var), avg_rRef = mean(rRef), avg_NFKB_time = mean(NFKB_time), avg_eTNFa = mean(max_eTNFa), avg_eTNFa_time = mean(eTNFa_time),sd_rRef = sd(rRef), sd_NFKB_time = sd(NFKB_time), sd_eTNFa = sd(max_eTNFa), sd_eTNFa_time = sd(eTNFa_time),sd_NFKB_high = sd(max_NFKB), avg_TNFR_50 = mean(TNFR_50), sd_TNFR_50 = sd(TNFR_50), avg_span = mean(span), sd_span = sd(span))

summ_rRef |>
  ggplot(aes(x=sd_rRef, y=sd_NFKB_high, color=Variation))+
  geom_point()
#Avg rRef vs avg_NFKB

summ_rRef |>
  ggplot(aes(x=Variation,y=avg_NFKB_high, color=Variation))+
  geom_boxplot()+
  stat_compare_means(comparisons=list(c("0","10"),c("0","25"),c("0","50")),label="p.signif",method = "wilcox.test",paired=FALSE)+
  ylab("NFKB concentration")+
  xlab("")+
  ggtitle("Average Maximum nuclear NFKB concentration", subtitle="Per run")

#Avg rRef vs avg_NFKB_time
summ_rRef |> 
  ggplot(aes(x=avg_NFKB_time))+
  geom_point(aes(y=Variation))

summ_rRef |> group_by(Variation) |> summarise(mean=mean(avg_NFKB_time),media=median(avg_NFKB_time))

x <- summ_rRef |> filter(Variation==0)
x <- x[["avg_NFKB_time"]]

y <- summ_rRef |> filter(Variation==10)
y <- y[["avg_NFKB_time"]]

z <- summ_rRef |> filter(Variation==25)
z <- z[["avg_NFKB_time"]]

ub <- 1.5*(quantile(x)[4]-quantile(x)[2])+quantile(x)[4]
lb <- -1.5*(quantile(x)[4]-quantile(x)[2])+quantile(x)[2]
x<- x[x<=ub & x>= lb]

ub <- 1.5*(quantile(y)[4]-quantile(y)[2])+quantile(y)[4]
lb <- -1.5*(quantile(y)[4]-quantile(y)[2])+quantile(y)[2]
y<- y[y<=ub & y>= lb]

ub <- 1.5*(quantile(z)[4]-quantile(z)[2])+quantile(z)[4]
lb <- -1.5*(quantile(z)[4]-quantile(z)[2])+quantile(z)[2]
z<- z[z<=ub & z>= lb]

wilcox.test(x,y,paired=TRUE)
wilcox.test(x,z,paired=TRUE)

summ_rRef |>
  ggplot(aes(y=avg_NFKB_time, x=Variation,color=Variation))+
  geom_boxplot()+
  stat_compare_means(comparisons=list(c("0","10"),c("0","25"),c("0","50")),method = "wilcox.test",paired=FALSE,method.args = list(mu = 0))+
  ylab("Time of Maximum")+
  xlab("")+
  ggtitle("Average Maximum nuclear NFKB Time", subtitle="Per run")

summ_rRef |> group_by(Variation) |> summarize(avg = median(avg_NFKB_time))
#Avg rRef vs avg_eTNFa

summ_rRef |>
  ggplot(aes(x=Variation,y=avg_eTNFa, color=Variation))+
  geom_boxplot()+
  stat_compare_means(comparisons=list(c("0","10"),c("0","25"),c("0","50")),label="p.signif",method = "wilcox.test",paired=FALSE)+
  ylab("eTNFa concentration")+
  xlab("")+
  ggtitle("Average Maximum external TNFa concentration", subtitle="Per run")

#Avg rRef vs avg_eTNFa_time
summ_rRef |>
  ggplot(aes(x=Variation,y=avg_eTNFa_time, color=Variation))+
  geom_boxplot()+
  stat_compare_means(comparisons=list(c("0","10"),c("0","25"),c("0","50")),label="p.signif",method = "wilcox.test",paired=FALSE)+
  ylab("eTNFa concentration")+
  xlab("")+
  ggtitle("Average Maximum external TNfa time", subtitle="Per run")

#sd rRef vs sd_NFKB

summ_rRef |>
  ggplot(aes(x=Variation,y=sd_NFKB_high, color=Variation))+
  geom_boxplot()+
  stat_compare_means(comparisons=list(c("0","10"),c("0","25"),c("0","50")),label="p.signif",method = "wilcox.test",paired=FALSE)+
  ylab("SD")+
  xlab("")+
  ggtitle("SD Maximum nuclear NFKB concentration", subtitle="Per run")

#sd rRef vs sd_NFKB_time
summ_rRef |>
  ggplot(aes(x=Variation,y=sd_NFKB_time, color=Variation))+
  geom_boxplot()+
  stat_compare_means(comparisons=list(c("0","10"),c("0","25"),c("0","50")),label="p.signif",method = "wilcox.test",paired=FALSE)+
  ylab("SD")+
  xlab("")+
  ggtitle("SD Maximum nuclear NFKB Time", subtitle="Per run")

#sd rRef vs sd_eTNFa
summ_rRef |>
  ggplot(aes(x=Variation,y=sd_eTNFa,color=Variation))+
  geom_boxplot()+
  stat_compare_means(comparisons=list(c("0","10"),c("0","25"),c("0","50")),label="p.signif",method = "wilcox.test",paired=FALSE)+
  ylab("SD")+
  xlab("")+
  ggtitle("SD Maximum external TNFa concentration", subtitle="Per run")

#sd rRef vs sd_eTNFa_time
summ_rRef |>
  ggplot(aes(x=Variation,y=sd_eTNFa_time, color=Variation))+
  geom_boxplot()+
  stat_compare_means(comparisons=list(c("0","10"),c("0","25"),c("0","50")),label="p.signif",method = "wilcox.test",paired=FALSE)+
  ylab("SD")+
  xlab("")+
  ggtitle("SD Maximum external TNfa time", subtitle="Per run")

#sd rRef vs avg_TNFR50
summ_rRef |>
  ggplot(aes(x=Variation,y=avg_TNFR_50, color=Variation))+
  geom_boxplot()+
  stat_compare_means(comparisons=list(c("0","10"),c("0","25"),c("0","50")),label="p.signif",method = "wilcox.test",paired=FALSE)+
  ylab("Time")+
  xlab("")+
  ggtitle("Average TNFR 50% Time", subtitle="Per run")

#sd rRef vs sd_TNFR50
summ_rRef |>
  ggplot(aes(x=Variation,y=sd_TNFR_50, color=Variation))+
  geom_boxplot()+
  stat_compare_means(comparisons=list(c("0","10"),c("0","25"),c("0","50")),label="p.signif",method = "wilcox.test",paired=FALSE)+
  ylab("SD")+
  xlab("")+
  ggtitle("SD TNFR 50% Time", subtitle="Per run")

#sd rRef vs sd_span
summ_rRef |>
  ggplot(aes(x=Variation,y=sd_span, color=Variation))+
  geom_boxplot()+
  stat_compare_means(comparisons=list(c("0","10"),c("0","25"),c("0","50")),label="p.signif",method = "wilcox.test",paired=FALSE)+
  ylab("SD")+
  xlab("")+
  ggtitle("SD Span Time", subtitle="Per run")


#sd rRef vs avg_span
summ_rRef |>
  ggplot(aes(x=Variation,y=avg_span, color=Variation))+
  geom_boxplot()+
  stat_compare_means(comparisons=list(c("0","10"),c("0","25"),c("0","50")),label="p.signif",method = "wilcox.test",paired=FALSE)+
  ylab("SD")+
  xlab("")+
  ggtitle("Avg Span Time", subtitle="Per run")

#### GGpairs####
ggpairs(summ_rRef,columns=c("avg_rRef", "avg_NFKB_high", "sd_NFKB_high"),aes(color=Variation, alpha=.5, fill=Variation), diag=list(continuous="barDiag"))
ggpairs(result_all_rRef,columns=c("dist","rRef","max_NFKB","NFKB_time"),aes(color=var, alpha=.05))

###### Extra Plots ####
value_df <- load(parameter = "rRef", runn = 201) 
value_df <- load(parameter = "base", runn = 301) 

value_df$df <- value_df$df |> filter(cell.id!=20)
standard_plots(value_df)
### all cells plotted
d <- all_cells(value_df, color="rRef")
all_cells(value_df,"eTNFa")
all_cells(value_df, "activated_frac")
all_cells(value_df, "TNFaflux")
all_cells(value_df,"IKBA")

### maxima NFKB
maxima(value_df,plotx="dist")
maxima(value_df,ploty="NFKB.n",color="dist",plotx="rRef")
maxima(value_df,ploty="dist",max="eTNFa",color="rRef")
maxima(value_df,ploty="eTNFa",plotx="dist")
maxima(value_df,"NFKB.n")
maxima(value_df,"eTNFa")
maxima(value_df,ploty="eTNFa",plotx="dist",color="rRef")
maxima(value_df,plotx="rRef",color="dist")
maxima(value_df,plotx="dist",ploty="rRef",color="NFKB.n")
##Kymographs
kymograph(value_df)
kymograph(value_df,"eTNFa")

###AUCS
auc_plot(value_df)
auc_plot(value_df,"eTNFa")

### Response times
response_plot(value_df)
response_plot(value_df,"eTNFa")



#### Multiple ####
# multiple <- list(
#   c("rRef",56),
#   c("rRef",57),
#   c("rRef",58),
#   c("base",1)
# )
# 
# plots <- list()
# for (i in 1:(length(multiple))) {
#   par <- multiple[[i]][1]
#   runs <- multiple[[i]][2]
#   print(runs)
#   
#   dfs <- load(par,as.double(runs))
#   plots[[as.character(i)]] <- all_cells(dfs,ploty="NFKB.n")
# }
# 
# 
# ggarrange(plots[[1]],plots[[2]],plots[[3]],plots[[4]],ncol=2,nrow =2,common.legend = FALSE,labels=c("σ²=0.001","σ²=0.01","σ²=0.1","Base"),
# label.x = 0.5,font.label=list(size=10,face="plain"))
# 
# ggsave("compare_TNFa2.png",path="compares",width=3000, height=2000, units="px")

### "Recovery time" of NFKB
# co <- lm(time ~ log(rRef), as.data.frame(value_df |> group_by(cell.id) |> filter(time > 100) |> filter(NFKB.n < 1e-5) |> slice_min(time)))$coefficients
# 
# value_df |> group_by(cell.id) |> filter(time > 100) |> filter(NFKB.n < 1e-5) |> slice_min(time) |>
#   ggplot(mapping=aes(x=log(rRef)))+
#   ggtitle("Timepoint when NFKB.n is back close to normal")+
#   geom_point(aes(y=time, color=dist))+
#   geom_line(mapping=aes(y=co[1]+co[2]*log(rRef)), alpha=0.5, linetype="dashed")+
#   scale_colour_gradient(high="#FF0000", low = "#0000FF")
# 
# ggsave(filename="NFKB_calm_down.png",path = save_path, scale=3)
# 
# 
# ### When are receptors satisfied
# co <- lm(time ~ log(rRef), as.data.frame(value_df |> group_by(cell.id) |> filter(activated_frac >= 0.95) |> slice_min(time)))$coefficients
# 
# value_df |> group_by(cell.id) |> filter(activated_frac >= 0.95) |> slice_min(time) |>
#   ggplot(mapping=aes(x=log(rRef)))+
#   ggtitle("Timepoint when TNF Receptors are fully satisfied")+
#   geom_point(aes(y=time, color=dist))+
#   geom_line(mapping=aes(y=co[1]+co[2]*log(rRef)), alpha=0.5, linetype="dashed")+
#   scale_colour_gradient(high="#FF0000", low = "#0000FF")
# 
# ggsave(filename="TNFR_fully_satisfied.png",path = save_path, scale=3)
# 
# ### When are receptors calmed down
# co <- lm(time ~ log(rRef), as.data.frame(value_df |> group_by(cell.id) |> filter(time > 250) |> filter(activated_frac <= 0.05) |> slice_min(time)))$coefficients
# 
# value_df |> group_by(cell.id) |> filter(time > 250) |> filter(activated_frac <= 0.05) |> slice_min(time) |>
#   ggplot(mapping=aes(x=log(rRef)))+
#   ggtitle("Timepoint when TNF Receptors are back to normal")+
#   geom_point(aes(y=time, color=dist))+
#   geom_line(mapping=aes(y=co[1]+co[2]*log(rRef)), alpha=0.5, linetype="dashed")+
#   scale_colour_gradient(high="#FF0000", low = "#0000FF")
# 
# ggsave(filename="TNFR_calmed_down.png",path = save_path, scale=3)


# value_df |> group_by(cell.id) |> filter(cell.id %in% c(6,10,16,17,24,27,30,31)) |>
#   ggplot(mapping=aes(x=time, y=NFKB.n, group=cell.id,
#                      colour=as.factor(cell.id)))+
#   geom_line()+
#   geom_vline(xintercept=0, alpha=0.5, linetype="dashed")+
#   geom_vline(xintercept=60,alpha=0.5, linetype="dashed")
# 
# ggsave(filename="response_time_selected_cells.png",path = save_path, scale=3)

# value_df |> group_by(cell.id) |> filter(time>-1) |> slice_max(eTNFa) |>
#   ggplot(mapping=aes(x=time, y=resptimes, color=as.factor(cellorder)))+
#   geom_point()
#   # scale_colour_gradient(high="#FF0000", low = "#0000FF")
# 
# ggsave(filename="response_time_NFKB_vs_timemax.png",path = save_path, scale=3)
# 
# df2 <- value_df |> group_by(cell.id) |> filter(time>-1)
# plot(all_response_times(value_df),all_auc(value_df))




#### TRYING OUT ####
# x <- 0:800
# y1 <- x
# y2 <- x/2
# y3 <- x/3
# y4 <- x^(1/2)
# plot(x,y4, type = 'l')
# lines(x,y4)
# 
# 
# df <- data.frame("time" = c(x), "cell.id" = rep(c(1),each=801), "NFKB.n" = c(x^(3)))
# all_response_times(df)
# plot(df[["time"]],df[["NFKB.n"]], type='l')
# abline(v=599.9997)
# abline(h=0)
# abline(v=800)
# abline(v=0)
# abline(h=(800)^(3))
# 599.9997*(800)^(3)
# (800)^(3)*800-auc((x)^(3))
# auc(sqrt(x)[1:266])
# 
# 
# source("functions.R")
# df <- data.frame("time" = c(0:10), "cell.id" = rep(c(1),each=11), "NFKB.n" = c(0,0,0,0,0,0,0,2,2,2,2))
# all_response_times(df,t=10)
# 
# plot(df[["time"]],df[["NFKB.n"]])
# 
# x <- seq(0,0.3,0.001)
# y <- rlnorm(10000,10,0.1)
# plot(x,dlnorm(x,log(0.01),0.001))
# hist((rlnorm(10000,log(0.01),0.001)))
# sd(log(rlnorm(10000,log(0.01),(3))))
# mean(log(rlnorm(10000,(0.01),log(3))))
# hist(y)
# hist(log(y))
# mean(log(y))
# sd(log(y))
# 
# m <- 0.01
# vari <- (0.5)
# y2 <- rlnorm(100000,log(m^2/(sqrt(m^2+vari))),sqrt(log(1+(vari/(m^2)))))
# max(y2)
# h <- (hist(y2,probability = TRUE, breaks=30))
# plot(y2,y2)
# hist(log(y2))
# mean(log(y2))
# var(log(y2))
# sd(log(y2))
# mean(y2)
# var(y2)
# sd(y2)
# plot((seq(0,0.01,0.00001)),plnorm(seq(0,0.01,0.00001),log(m^2/(sqrt(m^2+v))),sqrt(log(1+(v/m^2)))))

#### Präsi ####
# rlnorm(1,log(value^2/(sqrt(value^2+sd_s^2))),sqrt(log(1+(sd_s^2/value^2))))
# ggplot()+
#   geom_line(aes(x=log(seq(0,0.05,0.0001)),y=dlnorm((seq(0,0.05,0.0001)),log(0.01^2/(sqrt(0.001^2+0.01^2))),sqrt(log(1+(0.001^2/0.01^2)))),color="0.001"))+
#   geom_line(aes(x=log(seq(0,0.05,0.0001)),y=dlnorm((seq(0,0.05,0.0001)),log(0.01^2/(sqrt(0.01^2+0.01^2))),sqrt(log(1+(0.01^2/0.01^2)))),color="0.01"))+
#   geom_line(aes(x=log(seq(0,0.05,0.0001)),y=dlnorm((seq(0,0.05,0.0001)),log(0.01^2/(sqrt(0.1^2+0.01^2))),sqrt(log(1+(0.1^2/0.01^2)))),color="0.1"))+
#   ylab("Density")+
#   xlab("x")+
#   ggtitle("Lognormal distribution with mean = 0.01, differing vars")
# 
# 
# ggplot()+
#   geom_histogram(aes(log(rlnorm(10000,log(0.01^2/(sqrt(0.001^2+0.01^2))),sqrt(log(1+(0.001^2/0.01^2))))),color="0.001"))+
#   geom_histogram(aes(log(rlnorm(10000,log(0.01^2/(sqrt(0.01^2+0.01^2))),sqrt(log(1+(0.01^2/0.01^2))))),color="0.01"))+
#   geom_histogram(aes(log(rlnorm(10000,log(0.01^2/(sqrt(0.1^2+0.01^2))),sqrt(log(1+(0.1^2/0.01^2))))),color="0.1"))+
#   ylab("Density")+
#   xlab("x")+
#   ggtitle("Lognormal distribution with mean = 0.01, differing vars")
# 
# 
# ggplot()+
#   geom_line(aes(x=seq(0.5,1.5,0.01),y=dnorm(seq(0.5,1.5,0.01),1,0.2/2)))
# 
# pnorm(1.1,1,0.1/3)-pnorm(0.9,1,0.1/3)

x <- dnorm(seq(0.4,1.6,length.out=10000),1,0.1/2)
x <- c(x,dnorm(seq(0.4,1.6,length.out=10000),1,0.25/2))
x <- c(x,dnorm(seq(0.4,1.6,length.out=10000),1,0.5/2))
y <- c(rep(1,10000),rep(2,10000),rep(3,10000))

distribu <- data.frame(x=x,var=y,y=rep(seq(0.4,1.6,length.out=10000),3))

bound <- data.frame(ub=c(1+0.1,1+0.25,1+0.5),lb=c(1-0.1,1-0.25,1-0.5),var=c(1,2,3))

distribu |> 
  ggplot(aes(x=y))+
  geom_line(aes(y=x))+
  facet_wrap(vars(var),ncol=1,labeller = as_labeller(c('1' = "10%", '2' = "25%", '3' = "50%")))+
  geom_vline(xintercept=1,linetype=2,color='red')+
  geom_vline(aes(xintercept=ub),data=bound,linewidth=0.25,color='red')+
  geom_vline(aes(xintercept=lb),data=bound,linewidth=0.25,color='red')+
  xlab("X")+
  ylab("Density")

source("functions.R")
plotlist <- arranging(c(load(parameter = "rRef", runn = 1),
            load(parameter = "rRef", runn = 101),
            load(parameter = "rRef", runn = 201),
            load(parameter = "base", runn = 301)),title=c('10%','25%','50%','base'))

ggarrange(plotlist[4][[1]],plotlist[1][[1]],plotlist[2][[1]], plotlist[3][[1]], ncol=2, nrow=2, common.legend = TRUE, legend="right")

value_df <- load(parameter = "rRef", runn = 1) 
a <- all_cells(value_df, color="rRef",startt=-1)

value_df <- load(parameter = "rRef", runn = 101) 
b <- all_cells(value_df, color="rRef",startt=-1)

value_df <- load(parameter = "rRef", runn = 201) 
c <- all_cells(value_df, color="rRef",startt=-1)

value_df <- load(parameter = "base", runn = 301) 
d <- all_cells(value_df, color="rRef",startt=-1)

ggarrange(d, a, b, c, ncol=2, nrow=2, common.legend = TRUE, legend="bottom")
