require('rvest')
require('tidyverse')
library('XML')
setwd("/home/pi/R/projects")
site_blick <- xml2::read_html('https://www.blick.ch/')
site_blick_htmltext <- htmlParse(site_blick)
urls_blick_ungefiltert <- getHTMLLinks(site_blick_htmltext) %>%
  data.frame()
urls_blick_gefiltert_news <- urls_blick_ungefiltert %>%
  filter(str_detect(urls_blick_ungefiltert$.,"/news/"))
urls_blick_gefiltert_meinung<- urls_blick_ungefiltert %>%
  filter(str_detect(urls_blick_ungefiltert$.,"/meinung/"))
urls_blick_gefiltert <- rbind(urls_blick_gefiltert_news,urls_blick_gefiltert_meinung)
colnames(urls_blick_gefiltert) <- "url"
urls_blick_gefiltert <- urls_blick_gefiltert %>%
  filter(str_detect(urls_blick_gefiltert$url,"/video/", negate = TRUE))
urls_blick_gefiltert <- urls_blick_gefiltert %>%
  filter(str_detect(urls_blick_gefiltert$url,"/auto/", negate = TRUE))
urls_blick_gefiltert <- urls_blick_gefiltert %>%
  filter(str_detect(urls_blick_gefiltert$url,".html"))
urls_mithomepage <- paste0("https://www.blick.ch",urls_blick_gefiltert$url)
text <- do.call(rbind, lapply(urls_mithomepage, function(texte_lesen) {
  paste0(read_html(texte_lesen) %>% html_nodes('p') %>% html_text(), collapse = "\n\n")
}))
titel <- do.call(rbind, lapply(urls_mithomepage, function(texte_lesen) {
  paste0(read_html(texte_lesen) %>% html_nodes('.title-catchword-wrapper span') %>% html_text(), collapse = "\n\n")
}))
zeit <- do.call(rbind, lapply(urls_mithomepage, function(texte_lesen) {
  paste0(read_html(texte_lesen) %>% html_nodes('.article-date:nth-child(1)') %>% html_text(), collapse = "\n\n")
}))
df_blick <- cbind.data.frame(urls_mithomepage,titel,zeit,text)
colnames(df_blick) <- c("URL","Titel","Zeit","Text")
df_blick <- df_blick[-c(1),]
write_csv(df_blick,paste0("ernte_blick_",format(Sys.time(),"%d_%b_%Y_%H.%M"),".csv"))