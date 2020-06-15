library(rvest)
library(tidyverse)
library(XML)
setwd('/home/pi/R/projects')
site_20min <- xml2::read_html("https://www.20min.ch")
site_20min_htmltext <- htmlParse(site_20min)
urls_20min_ungefiltert <- getHTMLLinks(site_20min_htmltext)
pattern_story <- str_detect(urls_20min_ungefiltert, "/story/")
df_urls_20min_ungefiltert <- data.frame(urls_20min_ungefiltert, pattern_story)
urls_20min_df <-df_urls_20min_ungefiltert[df_urls_20min_ungefiltert$pattern_story == TRUE,]
urls_gefiltert <- urls_20min_df[1:35,1]
urls_41 <- paste0("https://www.20min.ch", urls_gefiltert)
text <- do.call(rbind, lapply(urls_41, function(texte_lesen) {
  paste0(read_html(texte_lesen) %>% html_nodes('.eFjN6vFe p') %>% html_text(), collapse = "\n\n")
}))
zeit <- do.call(rbind, lapply(urls_41, function(zeit_lesen) {
  paste0(read_html(zeit_lesen) %>% html_nodes('._1aVhoBNj') %>% html_text(), collapse = "\n\n")
}))
titel <-do.call(rbind, lapply(urls_41, function(titel_lesen) {
  paste0(read_html(titel_lesen) %>% html_nodes('._1ApuEpN1 h1') %>% html_text(), collapse = "\n\n")
})) 
df20min_tickerleer <- data.frame(urls_41,titel,zeit,text)
colnames(df20min_tickerleer) <- c("URL","Titel","Zeit","Text")
df20min_nurticker <- df20min_tickerleer %>%
  filter(Titel == "")
tickerurls_factor <- df20min_nurticker[,1]
tickerurls <- as.character(tickerurls_factor)
textticker <- do.call(rbind, lapply(tickerurls, function(texte_lesen) {
  paste0(read_html(texte_lesen) %>% html_nodes('._30svfhe7 p') %>% html_text(), collapse = "\n\n")
}))
titelticker <- do.call(rbind, lapply(tickerurls, function(texte_lesen) {
  paste0(read_html(texte_lesen) %>% html_nodes('._1tPOSzDH') %>% html_text(), collapse = "\n\n")
}))
zeitticker <- do.call(rbind, lapply(tickerurls, function(texte_lesen) {
  paste0(read_html(texte_lesen) %>% html_nodes('._1ezm6l3B:nth-child(1) .tBIwfLFE') %>% html_text(), collapse = "\n\n")
}))
df20min_ticker <- data.frame(tickerurls,titelticker, zeitticker, textticker)
colnames(df20min_ticker)  <- c("URL","Titel","Zeit","Text")
df20min_bdt_dopplungen <- rbind(df20min_ticker,df20min_tickerleer)
df20min_bdt <- df20min_bdt_dopplungen[!duplicated(df20min_bdt_dopplungen$URL),]
df20min <- df20min_bdt %>%
  filter(!Titel %in% c(" : Die Bilder des Tages","Leser-Fotos: 1 Bild sagt mehr als 1000 Worte",""))
aktualisiert <- df20min %>%
  filter(str_detect(Zeit, "Aktualisiert vor", negate = TRUE)) %>%
  filter(str_detect(Zeit, "Aktualisiert"))
aktualisiert_string <- str_sub(aktualisiert$Zeit, -16)
aktualisiert$Zeitpunkt <- lubridate::dmy_hm(aktualisiert_string)
daten <- df20min %>%
  filter(str_detect(Zeit, "Aktualisiert", negate = TRUE))%>%
  filter(str_detect(Zeit, ":"))
daten$Zeitpunkt <- lubridate::dmy_hm(daten$Zeit)
tage <- df20min %>%
  filter(str_detect(Zeit, ","))
tage_string <- str_sub(tage$Zeit, -10) 
tage_abzug <- lubridate::dmy(tage_string)
tage$Zeitpunkt <- tage_abzug + lubridate::seconds(1)
stunden <- df20min %>%
  filter(str_detect(Zeit, "ch", negate = TRUE)) %>%
  filter(str_detect(Zeit, "h"))
stunden_string <- str_sub(stunden$Zeit, -3,-2) %>%
  trimws()
stunden_abzug <- lubridate::hours(stunden_string)
Sys.time <- Sys.time()
stunden$Zeitpunkt <- Sys.time - stunden_abzug
minuten <- df20min %>%
  filter(str_detect(Zeit, "min"))
minuten_string <- str_sub(minuten$Zeit, -6, -4) %>%
  trimws()
minuten_abzug <- lubridate::minutes(minuten_string)
minuten$Zeitpunkt <- Sys.time - minuten_abzug
df20min <- rbind(aktualisiert, daten, tage, stunden, minuten)
df20min$Zeit <- df20min$Zeitpunkt 
df20min <- subset(df20min, select = -c(Zeitpunkt)) %>%
  drop_na()
write_csv(df20min, paste0("ernte_20min_",format(Sys.time(),"%d_%b_%Y_%H.%M"),".csv"))
