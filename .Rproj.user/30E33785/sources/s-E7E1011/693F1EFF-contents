library(rvest)
library(dplyr)
library(stringr)
library(magrittr)
library(ggplot2)
library(psych)
library(GPArotation)

session <- html_session('http://baseball-data.com/')

# 2016年の12球団の打撃成績データの取得
carp16 <- session %>% jump_to("16/stats/hitter2-c/tpa-4.html") %>%  html_nodes(xpath = '//*[@id="tbl"]') %>% html_table() %>% extract2(1)
giants16 <- session %>% jump_to("16/stats/hitter2-g/tpa-4.html") %>%  html_nodes(xpath = '//*[@id="tbl"]') %>%  html_table() %>% extract2(1)
dena16 <- session %>% jump_to("16/stats/hitter2-yb/tpa-4.html") %>%  html_nodes(xpath = '//*[@id="tbl"]') %>% html_table() %>% extract2(1)
tigers16 <- session %>% jump_to("16/stats/hitter2-t/tpa-4.html") %>%  html_nodes(xpath = '//*[@id="tbl"]') %>% html_table() %>% extract2(1)
swallows16 <- session %>% jump_to("16/stats/hitter2-s/tpa-4.html") %>%  html_nodes(xpath = '//*[@id="tbl"]') %>% html_table() %>% extract2(1)
dragons16 <- session %>% jump_to("16/stats/hitter2-d/tpa-4.html") %>%  html_nodes(xpath = '//*[@id="tbl"]') %>% html_table() %>% extract2(1)
fighters16 <- session %>% jump_to("16/stats/hitter2-f/tpa-4.html") %>%  html_nodes(xpath = '//*[@id="tbl"]') %>% html_table() %>% extract2(1)
hawks16 <- session %>% jump_to("16/stats/hitter2-h/tpa-4.html") %>%  html_nodes(xpath = '//*[@id="tbl"]') %>% html_table() %>% extract2(1)
marines16 <- session %>% jump_to("16/stats/hitter2-m/tpa-4.html") %>%  html_nodes(xpath = '//*[@id="tbl"]') %>% html_table() %>% extract2(1)
lions16 <- session %>% jump_to("16/stats/hitter2-l/tpa-4.html") %>%  html_nodes(xpath = '//*[@id="tbl"]') %>% html_table() %>% extract2(1)
eagles16 <- session %>% jump_to("16/stats/hitter2-e/tpa-4.html") %>%  html_nodes(xpath = '//*[@id="tbl"]') %>% html_table() %>% extract2(1)
buffaloes16 <- session %>% jump_to("16/stats/hitter2-bs/tpa-4.html") %>%  html_nodes(xpath = '//*[@id="tbl"]') %>% html_table() %>% extract2(1)
# 2015年の12球団の打撃成績データの取得
carp15 <- session %>% jump_to("15/stats/hitter2-c/tpa-4.html") %>%  html_nodes(xpath = '//*[@id="tbl"]') %>% html_table() %>% extract2(1)
giants15 <- session %>% jump_to("15/stats/hitter2-g/tpa-4.html") %>%  html_nodes(xpath = '//*[@id="tbl"]') %>% html_table() %>% extract2(1)
dena15 <- session %>% jump_to("15/stats/hitter2-yb/tpa-4.html") %>%  html_nodes(xpath = '//*[@id="tbl"]') %>% html_table() %>% extract2(1)
tigers15 <- session %>% jump_to("15/stats/hitter2-t/tpa-4.html") %>%  html_nodes(xpath = '//*[@id="tbl"]') %>% html_table() %>% extract2(1)
swallows15 <- session %>% jump_to("15/stats/hitter2-s/tpa-4.html") %>%  html_nodes(xpath = '//*[@id="tbl"]') %>% html_table() %>% extract2(1)
dragons15 <- session %>% jump_to("15/stats/hitter2-d/tpa-4.html") %>%  html_nodes(xpath = '//*[@id="tbl"]') %>% html_table() %>% extract2(1)
fighters15 <- session %>% jump_to("15/stats/hitter2-f/tpa-4.html") %>%  html_nodes(xpath = '//*[@id="tbl"]') %>% html_table() %>% extract2(1)
hawks15 <- session %>% jump_to("15/stats/hitter2-h/tpa-4.html") %>%  html_nodes(xpath = '//*[@id="tbl"]') %>% html_table() %>% extract2(1)
marines15 <- session %>% jump_to("15/stats/hitter2-m/tpa-4.html") %>%  html_nodes(xpath = '//*[@id="tbl"]') %>% html_table() %>% extract2(1)
lions15 <- session %>% jump_to("15/stats/hitter2-l/tpa-4.html") %>%  html_nodes(xpath = '//*[@id="tbl"]') %>% html_table() %>% extract2(1)
eagles15 <- session %>% jump_to("15/stats/hitter2-e/tpa-4.html") %>%  html_nodes(xpath = '//*[@id="tbl"]') %>% html_table() %>% extract2(1)
buffaloes15 <- session %>% jump_to("15/stats/hitter2-bs/tpa-4.html") %>%  html_nodes(xpath = '//*[@id="tbl"]') %>% html_table() %>% extract2(1)
#2011年から2014年の上記コードは省略



#球団名カラムの追加
#2016年
carp16$team <- "広島"
giants16$team <- "巨人"
dena16$team <- "横浜"
tigers16$team <- "阪神"
swallows16$team <- "ヤクルト"
dragons16$team <- "中日"
fighters16$team <- "日本ハム"
hawks16$team <- "ソフトバンク"
marines16$team <- "ロッテ"
lions16$team <- "西武"
eagles16$team <- "楽天"
buffaloes16$team <- "オリックス"
#2015年
carp15$team <- "広島"
giants15$team <- "巨人"
dena15$team <- "横浜"
tigers15$team <- "阪神"
swallows15$team <- "ヤクルト"
dragons15$team <- "中日"
fighters15$team <- "日本ハム"
hawks15$team <- "ソフトバンク"
marines15$team <- "ロッテ"
lions15$team <- "西武"
eagles15$team <- "楽天"
buffaloes15$team <- "オリックス"
#2011年から2014年は省略

#年度カラムを追加
#2016年
carp16$year <- 2016
giants16$year <- 2016
dena16$year <- 2016
tigers16$year <- 2016
swallows16$year <- 2016
dragons16$year <- 2016
fighters16$year <- 2016
hawks16$year <- 2016
marines16$year <- 2016
lions16$year <- 2016
eagles16$year <- 2016
buffaloes16$year <- 2016
#2015年
carp15$year <- 2015
giants15$year <- 2015
dena15$year <- 2015
tigers15$year <- 2015
swallows15$year <- 2015
dragons15$year <- 2015
fighters15$year <- 2015
hawks15$year <- 2015
marines15$year <- 2015
lions15$year <- 2015
eagles15$year <- 2015
buffaloes15$year <- 2015
#2011年から2014年は省略


hitter <- rbind(carp16,giants16,dena16,tigers16,swallows16,dragons16,fighters16,hawks16,marines16,lions16,eagles16,buffaloes16,
                carp15,giants15,dena15,tigers15,swallows15,dragons15,fighters15,hawks15,marines15,lions15,eagles15,buffaloes15)
                

hitter2 <- hitter %>% dplyr::select(year,team,選手名,打率,安打,二塁打,三塁打,本塁打,打点,得点,盗塁,盗塁刺,
                                                                         犠打,犠飛,四球,死球,併殺打,三振)
hitter3 <- na.omit(hitter2[,1:18])

dim(hitter3)

fa.parallel(hitter3[,4:18])

          