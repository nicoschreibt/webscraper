library(rvest)
library(tidyverse)
setwd('/home/pi/R/projects')
apisrf<- read_xml('https://www.srf.ch/news/bnf/rss/1646')
urls_srf <- apisrf %>% html_nodes('link') %>% html_text()
zeit_srf <- apisrf %>% html_nodes('pubDate') %>% html_text()
zeit_srf <- str_sub(zeit_srf,1,-7)
titel_srf <- apisrf %>% html_nodes('title') %>% html_text()
dfsrf_link_titel_zeit <- data.frame(urls_srf,titel_srf,zeit_srf)
srf_text <- do.call(rbind, lapply(urls_srf, function(x) {
  paste0(read_html(x) %>% html_nodes('.article-content p') %>% html_text(), collapse = "\n\n")
  }))
df_srf_newsletter<-data.frame(dfsrf_link_titel_zeit,srf_text)
df_srf_newsletter_ohnehome <-df_srf_newsletter[-1,]
df_srf <- df_srf_newsletter_ohnehome %>%
  select(urls_srf, titel_srf, zeit_srf, srf_text) %>%
  filter(!titel_srf %in% c("Die wichtigsten Updates und Diskussionen zum Coronavirus","Der Newsletter "))
colnames(df_srf) <- c("URL","Titel","Zeit","Text")
write_csv(df_srf, paste0("ernte_srf_",format(Sys.time(),"%d_%b_%Y_%H.%M"),".csv"))
