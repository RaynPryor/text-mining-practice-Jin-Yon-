#簡易辭典建立
#原理：直接爬wiki上的資料
#參考：https://yongfu.name/2018/07/31/jieba-dict.html
#P.S.：就是因為不知道這招害我弄半天......

# 這邊使用維基百科的神鵰俠侶角色列表作為詞庫的來源。以下使用rvest套件清理此頁面：
library(rvest)
library(dplyr)
library(magrittr)
library(knitr)

Sys.setlocale(category = "LC_ALL", locale = "UTF-8") # 避免中文亂碼

path <- "https://zh.wikipedia.org/wiki/%E7%A5%9E%E9%B5%B0%E4%BF%A0%E4%BE%B6%E8%A7%92%E8%89%B2%E5%88%97%E8%A1%A8" 
# 這裡已先行下載網頁，若無可直接使用網址

data <- read_html(path) %>% 
  html_nodes("ul") %>% html_nodes("li") %>%
  html_nodes("a") %>% html_text()

data <- unique(data)

#目錄
data[1:5]
# [1] "1 主角"     "2 桃花島"   "3 天下五絕" "4 古墓派"   "5 全真教"  
##目標參數
data[19:23]
# [1] "楊過"       "射鵰英雄傳" "楊康"       "穆念慈"     "全真教"   

##初估頁尾
data[196:200]
# [1] "鬼門龍王"           "杨过"               "西山一窟鬼"        
# [4] "樊一翁"             "射雕英雄传角色列表"

# 我們要的內容介在data[19](楊過)至data[199](樊一翁)之間。此外，亦可手動加入連結中沒有的詞彙：
data <- as_tibble(data[19:199]) %>% 
  rbind("過兒", "靖哥哥") # 手動額外輸入

head(data, 4) %>% kable("markdown", align="c")

#匯出為txt檔
write.table(data,file="dictionary.txt",row.names = F,col.names = F,fileEncoding = "UTF-8")
