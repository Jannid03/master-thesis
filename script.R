setwd("D:/Uni/Masterarbeit/data")
source("functions.R")
setup()

#base_df <- load(parameter = "base", runn=2)
## For base simulation
#init("base",43)
init("tmax",1,0.1/2) #12
run(FALSE)

dfs_tmax <- load_all()
result_all_tmax <- all_analysis(dfs_tmax, param="tmax")


# ggplot(data=results)+
#   geom_boxplot(mapping=aes(x=(var),y=average_rise))
# 
# ggplot(data=results)+
#   geom_boxplot(mapping=aes(x=var,y=non_activated_premax))
# 
# ggplot(data=results)+
#   geom_boxplot(mapping=aes(x=var,y=non_activated_t0))
# 
# ggplot(data=results)+
#   geom_jitter(mapping=aes(x=var,y=non_activated_tfinal,alpha=0.5))
# 
# ggplot(data=results)+
#   geom_jitter(mapping=aes(x=var,y=percentage_under_10,alpha=0.5))


### All cells all runs
####Final NFKB
#DIst
result_all |>
ggplot()+
  geom_point(aes(x=tmax,y=final_NFKB,color=dist))+
  facet_wrap(vars(var),ncol=1)+
  scale_color_gradient(high="#FF0000", low = "#0000FF")

#Tmax
result_all |>
  ggplot()+
  geom_point(aes(x=tmax,y=final_NFKB,color=tmax))+
  facet_wrap(vars(var),ncol=1)+
  scale_colour_gradient2(high="#FF0000", low = "#0000FF", mid="#d9bd1e",name="tmax",midpoint=0.01)

model <- result_all |> filter(cell.id !=20)
final_NFKB_model <- lm(final_NFKB ~ dist+tmax, data=model)
summary(final_NFKB_model)

####Max NFKB
#Dist
result_all |>
  ggplot()+
  geom_point(aes(x=tmax,y=max_NFKB,color=dist))+
  facet_wrap(vars(var),ncol=1)+
  scale_color_gradient(high="#FF0000", low = "#0000FF")

#Tmax
result_all |>
  ggplot()+
  geom_point(aes(x=dist,y=max_NFKB,color=tmax))+
  facet_wrap(vars(var),ncol=1)+
  scale_colour_gradient2(high="#FF0000", low = "#0000FF", mid="#d9bd1e",name="tmax",midpoint=0.01)

#Time max NFKB
result_all |> 
  ggplot()+
  geom_point(aes(x=tmax,y=NFKB_time,color=dist))+
  facet_wrap(vars(var),ncol=1)+
  scale_color_gradient(high="#FF0000", low = "#0000FF")

#final eTNFa
result_all |>
  ggplot()+
  geom_point(aes(x=tmax,y=eTNFa_final,color=dist))+
  facet_wrap(vars(var),ncol=1)+
  scale_color_gradient(high="#FF0000", low = "#0000FF")

#max eTNFa
result_all |>
  ggplot()+
  geom_point(aes(x=tmax,y=max_eTNFa,color=dist))+
  facet_wrap(vars(var),ncol=1)+
  scale_color_gradient(high="#FF0000", low = "#0000FF")

result_all |>
  ggplot()+
  geom_point(aes(x=dist,y=max_eTNFa,color=tmax))+
  facet_wrap(vars(var),ncol=1)+
  scale_colour_gradient2(high="#FF0000", low = "#0000FF", mid="#d9bd1e",name="tmax",midpoint=0.01)

result_all |>
  ggplot()+
  geom_point(aes(x=order,y=max_eTNFa,color=tmax))+
  facet_wrap(vars(var),ncol=1)+
  scale_colour_gradient2(high="#FF0000", low = "#0000FF", mid="#d9bd1e",name="tmax",midpoint=0.01)

#Time max eTNFa
result_all |>
  ggplot()+
  geom_point(aes(x=tmax,y=eTNFa_time,color=dist))+
  facet_wrap(vars(var),ncol=1)+
  scale_color_gradient(high="#FF0000", low = "#0000FF")


#TNFR frac end
result_all |>
  ggplot()+
  geom_point(aes(x=(tmax),y=TNFR_frac_end,color=dist))+
  facet_wrap(vars(var),ncol=1)+
  scale_color_gradient(high="#FF0000", low = "#0000FF")




#Tmax vs Dist, color:max_NFKB
result_all |>
  ggplot()+
  geom_point(aes(x=tmax,y=dist,color=max_NFKB))+
  scale_colour_gradient2(high="#FF0000", low = "#0000FF", mid="#d9bd1e",name="NFKB value",midpoint=0.005)+
  facet_wrap(vars(var),ncol=1)

#Tmax vs Dist, color:max_eTNFa
result_all |>
  ggplot()+
  geom_point(aes(x=tmax,y=dist,color=max_eTNFa))+
  scale_colour_gradient2(high="#FF0000", low = "#0000FF", mid="#d9bd1e",name="eTNFa value",midpoint=0.02)

#Tmax vs Dist, color:last_NFKB
result_all |>
  ggplot()+
  geom_point(aes(x=tmax,y=dist,color=final_NFKB))+
  scale_colour_gradient2(high="#FF0000", low = "#0000FF", mid="#d9bd1e",name="NFKB value",midpoint=3e-10)

#Tmax vs Dist, color:last_eTNFa
result_all |>
  ggplot()+
  geom_point(aes(x=tmax,y=dist,color=eTNFa_final))+
  scale_colour_gradient2(high="#FF0000", low = "#0000FF", mid="#d9bd1e",name="eTNFa value",midpoint=3e-13)


#Dist vs max_eTNFa, color:tmax
result_all |> filter(cell.id !=20) |>
  ggplot()+
  geom_point(aes(x=dist,y=max_eTNFa,color=tmax))+
  scale_colour_gradient2(high="#FF0000", low = "#0000FF", mid="#d9bd1e",name="tmax",midpoint=0.01)

#Order vs max_eTNFa, color:tmax
result_all |> filter(cell.id !=20) |>
  ggplot()+
  geom_point(aes(x=order,y=max_eTNFa,color=tmax))+
  scale_colour_gradient2(high="#FF0000", low = "#0000FF", mid="#d9bd1e",name="tmax",midpoint=0.01)

#Tmax vs max_eTNFa, color:Dist
result_all |>
  ggplot()+
  geom_point(aes(x=tmax,y=max_eTNFa,color=dist))+
  scale_colour_gradient2(high="#FF0000", low = "#0000FF", mid="#d9bd1e",name="tmax",midpoint=35)

result_all |> 
  ggplot()+
  geom_histogram(aes(x=tmax, fill=var))+
  facet_wrap(vars(var),ncol=1)

base_var <- as.factor(0)
base_max_NFKB <- base_df$df |> group_by(cell.id) |> slice_max(NFKB.n)
base_max_eTNFa <- base_df$df |> group_by(cell.id) |> slice_max(eTNFa)
base_final <- base_df$df |> group_by(cell.id) |> filter(time==800)

temp <- data.frame(rep(as.factor(0),36),base_max_NFKB[["NFKB.n"]],base_max_NFKB[["time"]],base_final[["NFKB.n"]],rep(0,36),1:36,base_max_eTNFa[["eTNFa"]],base_max_eTNFa[["time"]],base_final[["eTNFa"]],base_final[["activated_frac"]],base_final[["dist"]],base_final[["order"]],base_final[["tmax"]])

###Hist of NFKB_max
result_all |> 
  ggplot()+
  geom_boxplot(aes(x=var,y=max_NFKB))

#### Linear regression
model <- result_all |> filter(cell.id !=20) |> mutate(dist_mean=mean(dist)) |> mutate(sd_dist=sd(dist)) |> mutate(norm_dist=(dist-dist_mean)/sd_dist) |> mutate(tmax_mean=mean(tmax)) |> mutate(sd_tmax=sd(tmax)) |> mutate(norm_tmax=(tmax-tmax_mean)/sd_tmax)
max_NFKB_model <- lm(max_NFKB ~ norm_tmax+norm_dist, data=model)
summary(max_NFKB_model)

time_NFKB_model <- lm(NFKB_time ~ norm_tmax+norm_dist, data=model)
summary(time_NFKB_model)

time_eTNFa_model <- lm(eTNFa_time ~ norm_tmax+norm_dist, data=model)
summary(time_eTNFa_model)

max_eTNFa_model <- lm(max_eTNFa ~  norm_tmax+norm_dist, data=model)
summary(max_eTNFa_model)

### Run wise
summ <- result_all |> group_by(run) |> summarise(mean_NFKB_high = mean(max_NFKB), vars = first(var), avg_tmax = mean(tmax), mean_NFKB_time = mean(NFKB_time), avg_eTNFa = mean(max_eTNFa), avg_eTNFa_time = mean(eTNFa_time),sd_tmax = sd(tmax), sd_NFKB_time = sd(NFKB_time), sd_eTNFa = sd(max_eTNFa), sd_eTNFa_time = sd(eTNFa_time),sd_NFKB_high = sd(max_NFKB))

#Vars vs avg_NFKB
summ |>
  ggplot()+
  geom_point(aes(x=vars,y=mean_NFKB_high, color=avg_tmax))+
  scale_colour_gradient2(high="#FF0000", low = "#0000FF", mid="#d9bd1e",name="tmax",midpoint=0.01)

#Avg TMAX vs avg_NFKB
summ |>
  ggplot()+
  geom_point(aes(x=avg_tmax,y=mean_NFKB_high, color=vars))

#Avg TMAX vs avg_NFKB_time
summ |>
  ggplot()+
  geom_point(aes(x=avg_tmax,y=mean_NFKB_time, color=vars))

#Avg TMAX vs avg_eTNFa
summ |>
  ggplot()+
  geom_point(aes(x=avg_tmax,y=avg_eTNFa, color=vars))

#Avg TMAX vs avg_eTNFa_time
summ |>
  ggplot()+
  geom_point(aes(x=avg_tmax,y=avg_eTNFa_time, color=vars))

#sd TMAX vs sd_NFKB
summ |>
  ggplot()+
  geom_point(aes(x=sd_tmax,y=sd_NFKB_high, color=vars))

#sd TMAX vs sd_NFKB_time
summ |>
  ggplot()+
  geom_point(aes(x=sd_tmax,y=sd_NFKB_time, color=vars))

#sd TMAX vs sd_eTNFa
summ |>
  ggplot()+
  geom_point(aes(x=sd_tmax,y=sd_eTNFa, color=vars))

#sd TMAX vs sd_eTNFa_time
summ |>
  ggplot()+
  geom_point(aes(x=sd_tmax,y=sd_eTNFa_time, color=vars))


###### Extra Plots ####
value_df <- load(parameter = "t11", runn = 2) 
standard_plots(value_df)
### all cells plotted
all_cells(value_df, color="t11")
all_cells(value_df,"eTNFa")
all_cells(value_df, "activated_frac")
all_cells(value_df, "TNFaflux")

### maxima NFKB
maxima(value_df,plotx="dist")
maxima(value_df,ploty="NFKB.n")
maxima(value_df,ploty="dist",max="eTNFa",color="tmax")
maxima(value_df,ploty="eTNFa",plotx="dist")
maxima(value_df,"NFKB.n")
maxima(value_df,"eTNFa")
maxima(value_df,ploty="eTNFa",plotx="dist",color="tmax")

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
#   c("tmax",56),
#   c("tmax",57),
#   c("tmax",58),
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
# co <- lm(time ~ log(tmax), as.data.frame(value_df |> group_by(cell.id) |> filter(time > 100) |> filter(NFKB.n < 1e-5) |> slice_min(time)))$coefficients
# 
# value_df |> group_by(cell.id) |> filter(time > 100) |> filter(NFKB.n < 1e-5) |> slice_min(time) |>
#   ggplot(mapping=aes(x=log(tmax)))+
#   ggtitle("Timepoint when NFKB.n is back close to normal")+
#   geom_point(aes(y=time, color=dist))+
#   geom_line(mapping=aes(y=co[1]+co[2]*log(tmax)), alpha=0.5, linetype="dashed")+
#   scale_colour_gradient(high="#FF0000", low = "#0000FF")
# 
# ggsave(filename="NFKB_calm_down.png",path = save_path, scale=3)
# 
# 
# ### When are receptors satisfied
# co <- lm(time ~ log(tmax), as.data.frame(value_df |> group_by(cell.id) |> filter(activated_frac >= 0.95) |> slice_min(time)))$coefficients
# 
# value_df |> group_by(cell.id) |> filter(activated_frac >= 0.95) |> slice_min(time) |>
#   ggplot(mapping=aes(x=log(tmax)))+
#   ggtitle("Timepoint when TNF Receptors are fully satisfied")+
#   geom_point(aes(y=time, color=dist))+
#   geom_line(mapping=aes(y=co[1]+co[2]*log(tmax)), alpha=0.5, linetype="dashed")+
#   scale_colour_gradient(high="#FF0000", low = "#0000FF")
# 
# ggsave(filename="TNFR_fully_satisfied.png",path = save_path, scale=3)
# 
# ### When are receptors calmed down
# co <- lm(time ~ log(tmax), as.data.frame(value_df |> group_by(cell.id) |> filter(time > 250) |> filter(activated_frac <= 0.05) |> slice_min(time)))$coefficients
# 
# value_df |> group_by(cell.id) |> filter(time > 250) |> filter(activated_frac <= 0.05) |> slice_min(time) |>
#   ggplot(mapping=aes(x=log(tmax)))+
#   ggtitle("Timepoint when TNF Receptors are back to normal")+
#   geom_point(aes(y=time, color=dist))+
#   geom_line(mapping=aes(y=co[1]+co[2]*log(tmax)), alpha=0.5, linetype="dashed")+
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
rlnorm(1,log(value^2/(sqrt(value^2+sd_s^2))),sqrt(log(1+(sd_s^2/value^2))))
ggplot()+
  geom_line(aes(x=log(seq(0,0.05,0.0001)),y=dlnorm((seq(0,0.05,0.0001)),log(0.01^2/(sqrt(0.001^2+0.01^2))),sqrt(log(1+(0.001^2/0.01^2)))),color="0.001"))+
  geom_line(aes(x=log(seq(0,0.05,0.0001)),y=dlnorm((seq(0,0.05,0.0001)),log(0.01^2/(sqrt(0.01^2+0.01^2))),sqrt(log(1+(0.01^2/0.01^2)))),color="0.01"))+
  geom_line(aes(x=log(seq(0,0.05,0.0001)),y=dlnorm((seq(0,0.05,0.0001)),log(0.01^2/(sqrt(0.1^2+0.01^2))),sqrt(log(1+(0.1^2/0.01^2)))),color="0.1"))+
  ylab("Density")+
  xlab("x")+
  ggtitle("Lognormal distribution with mean = 0.01, differing vars")


ggplot()+
  geom_histogram(aes(log(rlnorm(10000,log(0.01^2/(sqrt(0.001^2+0.01^2))),sqrt(log(1+(0.001^2/0.01^2))))),color="0.001"))+
  geom_histogram(aes(log(rlnorm(10000,log(0.01^2/(sqrt(0.01^2+0.01^2))),sqrt(log(1+(0.01^2/0.01^2))))),color="0.01"))+
  geom_histogram(aes(log(rlnorm(10000,log(0.01^2/(sqrt(0.1^2+0.01^2))),sqrt(log(1+(0.1^2/0.01^2))))),color="0.1"))+
  ylab("Density")+
  xlab("x")+
  ggtitle("Lognormal distribution with mean = 0.01, differing vars")


ggplot()+
  geom_line(aes(x=seq(0.5,1.5,0.01),y=dnorm(seq(0.5,1.5,0.01),1,0.2/2)))

pnorm(1.1,1,0.1/3)-pnorm(0.9,1,0.1/3)
