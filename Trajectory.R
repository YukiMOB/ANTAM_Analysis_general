ppi <- 300 # resolution of figures
ofset <- 100
options(scipen=100)

plot_trajectory.merge <- function(df,t.data){
  abs_xy_max <- max(df$trajectory_xy_max[1:length(df$num)]) + 100 # 
  for(i in 1:  length(table(df$lx))){ # lxの種類 実験条件の数を出してloop
      lx <- paste(unique(df$lx)[i],"lx",sep = "")
      hum <- paste(unique(df$humidity),"%",sep = "")
      x <- subset(t.data$x.V1,grepl(lx,t.data$path) == TRUE & grepl(hum,t.data$path) == TRUE)  
      y <-  subset(t.data$y.V1,grepl(lx,t.data$path) == TRUE & grepl(hum,t.data$path) == TRUE)
      output.path <- paste(unique(df$lx)[i],"_",unique(df$humidity),".pdf",sep="")
      pdf(output.path)
      plot(x,y,xlim = c(-abs_xy_max, abs_xy_max), ylim = c(-abs_xy_max, abs_xy_max),
      xlab = "x (mm)", ylab = "y (mm)", type = "p", col = 1, cex = 0.01, cex.axis = 0.8,
      asp = 1)
      dev.off()
  }
}

#　軌跡データの表示
plot_trajectory <- function(df,t.data){
  for(i in 1: length(df$num)){
    abs_xy_max <- df$trajectory_xy_max[i] # グラフの最大最小値の設定
    x <- subset(t.data$x.V1,t.data$id == i)
    y <- subset(t.data$y.V1,t.data$id == i)
    trajectory.df <- data.frame(x = x, y = y) # ggplot用のdata frameを作成
    g <- ggplot(trajectory.df,aes(x =x,y = y))
    g <- g + geom_point(data = trajectory.df,aes(x = x, y = y)) +
      scale_shape_identity()
    
    # theme setting
    g <- g + theme_bw() + scale_x_continuous(limits = c(-abs_xy_max, abs_xy_max)) + scale_y_continuous(limits = c(-abs_xy_max, abs_xy_max)) + 
      theme(axis.text=element_text(size=18),
            axis.title=element_text(size=20,face="bold"),
            legend.position = 'none') +  xlab(xlab) + ylab(ylab) +
      theme_classic() +theme(axis.text=element_text(size=25),
                             axis.title=element_text(size=30,face="bold"),
                             legend.position = 'none')
    plot(g)
  }
}

# グラフの色対応＆一斉出力
plot_trajectory.merge.col <- function(df,t.data){
  c <- c('#EB6965','#2BB70D','#4486FE')
  abs_xy_max <- max(df$trajectory_xy_max[1:length(df$num)]) + 100 #
  output.path <- "tra.pdf"
  for(i in 1: length(table(df$lx))){
    lx <- paste(unique(df$lx)[i],"lx",sep = "")
    hum <- paste(unique(df$humidity),"%",sep = "")
    x <- subset(t.data$x.V1,grepl(lx,t.data$path) == TRUE & grepl(hum,t.data$path) == TRUE)  
    y <-  subset(t.data$y.V1,grepl(lx,t.data$path) == TRUE & grepl(hum,t.data$path) == TRUE)
    plot(x,y,xlim = c(-abs_xy_max, abs_xy_max), ylim = c(-abs_xy_max, abs_xy_max),
    xlab = "x (mm)", ylab = "y (mm)", type = "p", col = 1, cex = 0.01, cex.axis = 2.3,cex.lab = 2.3,
    asp = 1)
    par(new = T)
  }
  # dev.off()
}

plot_trajectory.merge.col.all <- function(df,t.data,arc){
  c <- c('#EB6965','#2BB70D','#4486FE')
  abs_xy_max <- max(df$trajectory_xy_max[1:length(df$num)]) + 100 #
  output.path <- "tra.pdf"
  for(i in 1: length(table(df$lx))){
    lx <- paste(unique(df$lx)[i],"lx",sep = "")
    hum <- paste(unique(df$humidity),"%",sep = "")
    x <- subset(t.data$x.V1,grepl(lx,t.data$path) == TRUE & t.data$arc == arc)  
    y <-  subset(t.data$y.V1,grepl(lx,t.data$path) == TRUE & t.data$arc == arc)
    plot(x,y,xlim = c(-abs_xy_max, abs_xy_max), ylim = c(-abs_xy_max, abs_xy_max),
    xlab = "x (mm)", ylab = "y (mm)", type = "p", col = c[i], cex = 0.01, cex.axis = 2.3,cex.lab = 2.3,
    asp = 1)
    abline(v=0,lty = 2)
    par(new = T)
  }
  # dev.off()
}

# men_xy_minmax <- c(abs(min(df$x.V1)), abs(min(df$y.V1)), abs(max(df$x.V1)), abs(max(df$y.V1)))
# abs_xy_max <- max(men_xy_minmax)
# 
# for(i in 1: length(fl)){
#     x <- subset(df$x.V1,df$id == i)
#     y <- subset(df$y.V1,df$id == i)
#     plot(x,y,xlim = c(-abs_xy_max, abs_xy_max), ylim = c(-abs_xy_max, abs_xy_max),
#     xlab = "x (mm)", ylab = "y (mm)", type = "p", col = 1, cex = 0.01, cex.axis = 0.8)
# }