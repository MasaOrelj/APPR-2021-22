# 2. faza: Uvoz podatkov


library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(plyr)
library(tibble)
library(stringr)
library(xml2)
library(rvest)

sl <- locale("sl", decimal_mark=",", grouping_mark=".")


  
tabela_izobrazba <- read_csv(
  "podatki/izobrazba.csv",
  skip = 1, na=c(":"),
  locale = locale(encoding = "Windows-1250"),
  col_types = cols(
  .default = col_guess()),
  col_names = "Stolpec") 


tabela_izobrazba <- tabela_izobrazba %>% 
  mutate(
    Stolpec =
      str_replace_all(Stolpec, "y,", "y-")
  ) %>%
  separate(col = Stolpec, into = c("Leto","Država", "Spol", "Starost", "Odstotek", "Stopnja izobrazbe","Vrednost"), sep = ",", convert = FALSE) %>%
  select(-"Starost", -"Odstotek") %>%
  mutate_all(str_replace_all, '"', '') %>%
  mutate(Vrednost = parse_number(Vrednost), Leto = parse_number(Leto))
 

#lapply(tabela_izobrazba, gsub, pattern = "y,", replacement = "y-")
#tabela_izobrazba <- as.data.frame(tabela_izobrazba_popravek) 
#tabela_izobrazba <- separate(data = tabela_izobrazba, col = Stolpec, into = c("Leto","Država", "Spol", "Starost", "Odstotek", "Stopnja izobrazbe","Vrednost"), sep = ",", convert = TRUE) %>% select(-"Starost", -"Odstotek") 
#tabela_izobrazba <- lapply(tabela_izobrazba, gsub, pattern = "y-", replacement = "y,")
#tabela_izobrazba <- as.data.frame(tabela_izobrazba) 
#tabela_izobrazba <- tabela_izobrazba %>% mutate_all(str_replace_all, '"', '')


    

tabela_sodelovanje <- read_csv(
  "podatki/sodelovanje_v_turizmu.csv",
  skip = 1, na=c(":"),
  locale = locale(encoding = "Windows-1250"),
  col_types = cols(
    .default = col_guess()),
  col_names = c("Leto", "Država", "Enota", "Trajanje", "Lokacija", "Spol", "Število sodelujočih")) %>% select(-"Trajanje", -8)


tabela_stevilo_potovanj <- read_csv(
  "podatki/stevilo_potovanj.csv",
  skip = 1, na=c(":"),
  locale = locale(encoding = "Windows-1250"),
  col_types = cols(
    .default = col_guess()),
  col_names = c("Leto", "Država", "Enota", "Namen", "Trajanje", "Lokacija", "Spol", "Število potovanj")) %>% select(-"Enota", -"Trajanje", -9)

tabela_potrosnja <- read_csv(
  "podatki/potrosnja.csv",
  skip = 1, na=c(":"),
  locale = locale(encoding = "Windows-1250"),
  col_types = cols(
    .default = col_guess()),
  col_names = c("Leto", "Država", "Enota", "Namen", "Trajanje", "Lokacija", "Nastanitev", "Nočitev/Potovanje", "Povprečna potrošnja")) %>% select(-"Enota", -"Trajanje", -"Nastanitev")




link <- "https://en.wikipedia.org/wiki/List_of_European_countries_by_average_wage"
stran <- html_session(link) %>% read_html()
tabela <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable mW-datatable']") %>%
  .[[2]] %>% html_table(dec=",")

tabela <- tabela[-1,c(1,2,7)] %>% dplyr::rename(TIME = Year) %>% dplyr::rename(GEO = Country)

tabela$TIME <- as.numeric(tabela$TIME)


uvozi.povp_placa <- function() {
  
  link <- "https://en.wikipedia.org/wiki/List_of_European_countries_by_average_wage"
  page <- html_session(link) %>% read_html()
  table <- page %>% html_nodes(xpath="//table[@class='wikitable nowrap sortable mw-datatable']") %>% .[[1]] %>%
    html_table(dec=".")
  
  
  #for (i in 1:ncol(table())) {
  # if (is.character(table[[i]])) {
  #   Encoding(table[[i]]) <- "UTF-8"
  # }
  #}
  
  colnames(table) <- c("Countries", "Total alcohol", "Recorded consumption", "Unrecorded consumption", "Beer(%)",
                       "Wine(%)", "Spirits(%)", "Other(%)", "2015 projection")
  
  table$`2015 projection`<- NULL
  table$`Recorded consumption` <- NULL
  table$`Unrecorded consumption`<- NULL
  
  # for (col in c("Beer", "Wine", "Spirits", "Other", "Total alcohol")) {
  #   if (is.character(table[[col]])) {
  #     table[[col]] <- parse_number(table[[col]], na=c("0", "0.0", ""))
  #   }
  # }
  return(table)
}
kolicina_pc <- uvozi.povp_placa()









