# ANTAM analysis 2019
# author : Yuki Kawano

########## setup ###########################################

library(data.table)
library(dplyr)
library(ggplot2)
library(ggsignif)


options(encoding="UTF-8")
options(scipen=100) # scipen : setting a threshold of avable index value

############ function #######################################
get.datatable <- function(file.list){
  dt.return <- data.frame()
  t0 <- proc.time() # csv読み込み時間の計測用変数
  for (i in 1:length(file.list)) {
    print(proc.time() - t0) # 処理時間の計測
    dt <- datatable.read(file.list[i],i) # ファイルパスからcsvを読み込み
    dt.return <- rbindlist(list(dt.return,dt)) # 元のdata tableと直前に読み込んだdtとの結合
  }
  dt <- as.data.frame(NULL) # 初期化
  return(dt.return)
}

datatable.read <- function(f,i){
  data <- fread(f,stringsAsFactors=FALSE) # ファイル読み込み
  n <- seq(1,nrow(data),by = freq) # 周波数に応じた行列を作成
  # csvデータのダウンサンプリング
  x.downsamp<- get.value.downsampling(data$x * xcalib,n)
  y.downsamp <- get.value.downsampling(data$y * ycalib,n)

  # data tableの作成
  dt <- data.table(t = data$time[n], x = x.downsamp, y = y.downsamp,id = i,path = f,arc = data$arc[n],pos = data$pos[n])
  dt <- subset(dt,dt$t > time.start & dt$t < time.end)

  # calibration for data analysis
  data$arc <- ifelse(data$arc == 0,-1,data$arc)

  return(dt)
}

get.value.downsampling <- function(value,n){
  value.dt <- data.table()
  value.dt <- rbindlist(list(value.dt,list(value[1])))
  for (i in 2:length(n)) {
    v <- list(sum(value[n[i-1]:n[i]]) / freq)
    value.dt <- rbindlist(list(value.dt,v))
  }
  return(value.dt)
}
