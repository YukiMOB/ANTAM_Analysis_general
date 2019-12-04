plot.graph <- function(result,value,ylab,xlab,ymax){
    # ggplotのグラフ作成
    g <- ggplot(result, aes(x = condition, y = value))
    g <- g + stat_summary(fun.y = mean, geom = "bar",colour="black", fill="white",width = 0.5)
    g <- g + stat_summary(fun.data = mean_se,
                    geom = "errorbar",
                    width = 0.25)
    g <- g + geom_point(data = result,aes(x = condition,y = value))
    g <- g + theme_bw() + scale_y_continuous(limits = c(0, ymax)) + 
    theme(axis.text=element_text(size=18),
            axis.title=element_text(size=20,face="bold"),
            legend.position = 'none') +  xlab(xlab) + ylab(ylab) +
            theme_classic() +theme(axis.text=element_text(size=25),
            axis.title=element_text(size=30,face="bold"),
    legend.position = 'none')
    plot(g)
}