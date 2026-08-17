setwd("D:/Uni/Masterarbeit/data")
source("functions.R")
setup()

## For base simulation
# init("base",42)
# init("tmax",4870,sqrt(0.1)) #12
# run()
# for (i in 1:100) {
#   init("tmax",i,sqrt(0.001)) #12
#   run(n=TRUE,i)
# }

dfs <- c()
for (i in 1:300){
  print(i)
  value_df <- load(parameter = "tmax", runn = i)

  dfs <- c(dfs, value_df)
}

vals <- c()
for (i in 0:299){
  print(i)
  value_df <- dfs[3*i+1]
  
  
  ### Avergae of first time point vs. average of last time point
  # first <- value_df$df |> group_by(cell.id )|> filter(time == -1) |> select(NFKB.n,cell.id)
  # end <- value_df$df |> group_by(cell.id )|> filter(time == 800) |> select(NFKB.n,cell.id)
  # 
  # vals <- c(vals, mean(end[["NFKB.n"]])/mean(first[["NFKB.n"]]))
  
  max_pre <- value_df$df |> group_by(cell.id) |> filter(time <= -1) |> slice_max(NFKB.n)
  
  max_post <- value_df$df |> group_by(cell.id) |> filter(time > -1) |> slice_max(NFKB.n)
  
  vals <- c(vals, sum(max_pre[["NFKB.n"]]*0.9 > max_post[["NFKB.n"]]))
  
}

# results <- data.frame("var"=as.factor(rep(c(0.001,0.01,0.1),each=100)),"average_rise"=vals)

results <- cbind(results,data.frame(""=vals))

ggplot(data=results)+
  geom_boxplot(mapping=aes(x=(var),y=average_rise))

ggplot(data=results)+
  geom_boxplot(mapping=aes(x=var,y=non_activated_premax))
# 
# plot(1:100,ends)
# value_df <- load(parameter = "tmax", runn = 241)
# value_df <- load()
# standard_plots(value_df)
# # standard_plots(value_df,27)
# 
# ###### Extra Plots
# ### all cells plotted
# all_cells(value_df)
# all_cells(value_df,"eTNFa")
# all_cells(value_df, "activated_frac")
# 
# ### maxima NFKB
# maxima(value_df,plotx="dist")
# maxima(value_df,"eTNFa")
# maxima(value_df,"activated_frac")
# 
# ##Kymographs
# kymograph(value_df)
# kymograph(value_df,"eTNFa")
# 
# ###AUCS
# auc_plot(value_df)
# auc_plot(value_df,"eTNFa")
# 
# ### Response times
# response_plot(value_df)
# response_plot(value_df,"eTNFa")



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
