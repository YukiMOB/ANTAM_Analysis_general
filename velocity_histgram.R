threshould <- 1.5

# 速度の閾値を導出するためのプログラム
get.diff <- function(value,dt){
    # 速度導出
    d <- c()
    for (i in 1:max(dt$id)) {
        d <- c(d,diff(subset(value,dt$id == i)))
    }
    return(d)
}

get.thre <- function(){
    return(threshould)
}

dx <- get.diff(dt$x.V1,dt)
dy <- get.diff(dt$y.V1,dt)
v <- sqrt(dx^2 + dy^2) * 5

v.df <- data.frame(velocity = v)

g <- ggplot(v.df,aes(x = velocity))
g <- g +
    geom_histogram(binwidth = 0.25) +
    scale_x_continuous(breaks=seq(0,30,2.5)) +
    xlim(0, 30) + ylim(c(0, 6000)) +
    xlab("Velocity (mm/sec)") +
    ylab("Frequency") +
    geom_vline(xintercept = threshould,
            color = "blue",
            linetype = "dashed") +
    theme(axis.text=element_text(size=25),
          axis.title=element_text(size=30,face="bold"),legend.position = 'none')
plot(g)