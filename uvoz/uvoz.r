# 2. faza: Uvoz podatkov



sl <- locale("sl", decimal_mark=",", grouping_mark=".")


  
tabela_izobrazba <- read_csv2(
  "podatki/izobrazba_EU.csv",
  skip = 1, na=c(":"),
  locale = locale(encoding = "Windows-1250"),
  col_types = cols(
  .default = col_guess()),
  col_names = "Stolpec") 


tabela_izobrazba <- tabela_izobrazba %>% 
  mutate(
    Stolpec =
      str_replace_all(Stolpec, "primary,", "primary-")
  ) %>%
  separate(col = Stolpec, into = c("Leto","Drzava", "Spol", "Starost", "Odstotek", "Stopnja.izobrazbe","Vrednost"), sep = ",", convert = FALSE) %>%
  dplyr::select(-"Starost", -"Odstotek") %>%
  mutate_all(str_replace_all, '"', '') %>%
  mutate(Vrednost = parse_number(Vrednost), Leto = parse_number(Leto))
 

    

tabela_sodelovanje <- read_csv(
  "podatki/sodelovanje_v_tur.csv",
  skip = 1, na=c(":"),
  locale = locale(encoding = "Windows-1250"),
  col_types = cols(
    .default = col_guess()),
  col_names = c("Leto", "Drzava", "Enota", "Trajanje", "Lokacija", "Spol", "Stevilo.sodelujocih")) %>% dplyr::select(-"Trajanje", -8)


tabela_stevilo_potovanj <- read_csv(
  "podatki/st_potovanj.csv",
  skip = 1, na=c(":"),
  locale = locale(encoding = "Windows-1250"),
  col_types = cols(
    .default = col_guess()),
  col_names = c("Leto", "Drzava", "Enota", "Namen", "Trajanje", "Lokacija", "Stevilo.potovanj")) %>% dplyr::select(-"Enota", -"Trajanje", -8)

tabela_potrosnja <- read_csv(
  "podatki/potrosnja.csv",
  skip = 1, na=c(":"),
  locale = locale(encoding = "Windows-1250"),
  col_types = cols(
    .default = col_guess()),
  col_names = c("Leto", "Drzava", "Enota", "Namen", "Trajanje", "Lokacija", "Nastanitev", "Nocitev/Potovanje", "Povprecna.potrosnja")) %>% dplyr::select(-"Enota", -"Trajanje", -"Nastanitev")


link <- "https://en.wikipedia.org/wiki/List_of_European_countries_by_average_wage"
stran <- html_session(link) %>% read_html()
place_evropa <- stran %>% 
  html_nodes(xpath="//table[@class='wikitable sortable']") %>%
  .[[3]] %>% 
  html_table(fill=TRUE)

place_evropa <- place_evropa[-1:-2,-2:-15] 
place_evropa <- place_evropa[,-9]

izbris <- c("United Kingdom",
                      "Serbia",
                      "North Macedonia",
                      "Montenegro",
                      "Kosovo",
                      "Georgia",
                      "Bosnia and Herzegovina",
                      "Armenia",
                      "Albania",
                      "Ukraine",
                      "Iceland",
                      "Moldova",
                      "Norway"
)

colnames(place_evropa) <- c("Drzava", 2012:2018)

place_evropa <- place_evropa[ ! place_evropa$Drzava %in% izbris, ]


  
place_evropa <- place_evropa %>% 
  mutate(Drzava=gsub("\\[[^]]*\\]","", Drzava)) %>%
  pivot_longer(-c(1), names_to = "Leto", values_to = "Placa") %>%
  mutate(Placa=parse_number(Placa, na=c("-", "NA", "")), Leto=parse_number(Leto)) %>% arrange(Leto) 
place_evropa <- place_evropa[c(2,1,3)]


#združevanje tabel

potrosnja_stolpec <- tabela_potrosnja[,6]
potrosnja_razdeljeni_stolpec1 <- potrosnja_stolpec[seq(1, nrow(potrosnja_stolpec),2),]

potrosnja_razdeljeni_stolpec2 <- potrosnja_stolpec[seq(2, nrow(potrosnja_stolpec),2),]

tabela_potrosnja2 <- tabela_potrosnja[,-5:-6]
tabela_potrosnja2 <- tabela_potrosnja2[seq(1, nrow(tabela_potrosnja2),2),]
tabela_potrosnja2 <- cbind(tabela_potrosnja2, potrosnja_razdeljeni_stolpec1, potrosnja_razdeljeni_stolpec2)
colnames(tabela_potrosnja2)[5:6] <- c("Izlet", "Nocitev")



tabela_potrosnja_izleti <- tabela_stevilo_potovanj %>%
  left_join(tabela_potrosnja2, by = c("Drzava", "Leto", "Namen", "Lokacija"))


colnames(tabela_potrosnja_izleti)[5] <- c(" ")
tabela_potrosnja_izleti <- tabela_potrosnja_izleti %>% mutate("Skupaj" =  tabela_potrosnja_izleti[,5] * tabela_potrosnja_izleti[,6] ) 


stolpec_potrosnja <- tabela_potrosnja_izleti[["Skupaj"]]

tabela_potrosnja_izleti <- tabela_potrosnja_izleti %>% dplyr::select(-"Skupaj")
tabela_potrosnja_izleti <- cbind(tabela_potrosnja_izleti, stolpec_potrosnja)

colnames(tabela_potrosnja_izleti) <- c("Leto", "Drzava", "Namen", "Lokacija", "Stevilo.potovanj", "Povprecna.potrosnja.na.izlet", "Povprecna.potrosnja.na.noc", "Skupna.potrosnja")

tabela_potrosnja_izleti$Drzava[tabela_potrosnja_izleti$Drzava == "Germany (until 1990 former territory of the FRG)"] <- "Germany"


Potovanja.Skupaj <- tabela_potrosnja_izleti %>% 
  group_by(Leto) %>%
  summarize(Stevilo.potovanj=sum(Stevilo.potovanj[!is.na(Stevilo.potovanj)]))

Potovanja.Skupaj$Stevilo.potovanj <- Potovanja.Skupaj$Stevilo.potovanj/1000000

Potrosnja.Skupaj <- tabela_potrosnja_izleti %>% 
  group_by(Leto) %>%
  summarize(Skupna.potrosnja=sum(Skupna.potrosnja[!is.na(Skupna.potrosnja)]))
  

Potrosnja.Skupaj$Skupna.potrosnja <- Potrosnja.Skupaj$Skupna.potrosnja/1000000

#druga tabela - izobrazba, sodelovanje

izobrazba_stolpec <- tabela_izobrazba[,5]
izobrazba_stolpec1 <- izobrazba_stolpec[seq(1, nrow(izobrazba_stolpec),3),]
izobrazba_stolpec2 <- izobrazba_stolpec[seq(2, nrow(izobrazba_stolpec),3),]
izobrazba_stolpec3 <- izobrazba_stolpec[seq(3, nrow(izobrazba_stolpec),3),]

tabela_izobrazba2 <- tabela_izobrazba %>% dplyr::select(-"Stopnja.izobrazbe", -"Vrednost") %>% filter(row_number() %% 3 == 1)
tabela_izobrazba2 <- cbind(tabela_izobrazba2, izobrazba_stolpec1, izobrazba_stolpec2, izobrazba_stolpec3)
colnames(tabela_izobrazba2)[4:6] <- c("Less.than.primary.primary.and.lower.secondary.education", "Upper.secondary.and.post-secondary.non-tertiary.education", "Tertiary.education")


tabela_sodelovanje2 <- tabela_sodelovanje[!grepl("Number", tabela_sodelovanje$Enota),]
tabela_sodelovanje2 <- tabela_sodelovanje2 %>% dplyr::select(-"Enota")
colnames(tabela_sodelovanje2)[5] <- "Odstotek.sodelujocih"

tabela_domestic <- tabela_sodelovanje2[!grepl("All.countries.of.the.world", tabela_sodelovanje2$Lokacija),]
tabela_domestic <- tabela_domestic %>% dplyr::select(-"Lokacija")

tabela_all_countries <- tabela_sodelovanje2[!grepl("Domestic", tabela_sodelovanje2$Lokacija),]
tabela_all_countries <- tabela_all_countries %>% dplyr::select(-"Lokacija")

tabela_izobrazba_sodelovanje <- tabela_domestic %>%
  left_join(tabela_all_countries, by = c("Drzava", "Leto", "Spol"))

colnames(tabela_izobrazba_sodelovanje)[4:5] = c("Odstotek.sodelujocih.znotraj.drzave", "Odstotek.vseh.sodelujocih")

tabela_izobrazba_sodelovanje$'Odstotek.sodelujocih.izven.drzave' <- (tabela_izobrazba_sodelovanje$`Odstotek.vseh.sodelujocih` - tabela_izobrazba_sodelovanje$`Odstotek.sodelujocih.znotraj.drzave`)

tabela_izobrazba_sodelovanje <- tabela_izobrazba_sodelovanje %>% .[c(1,2,3,4,6,5)] %>%
  left_join(tabela_izobrazba2, by = c("Drzava", "Leto", "Spol"))

tabela_izobrazba_sodelovanje$Drzava[tabela_izobrazba_sodelovanje$Drzava == "Germany (until 1990 former territory of the FRG)"] <- "Germany"


#povprečna plača in potrošnja

place_evropa$Drzava[place_evropa$Drzava == "Czech Republic"] <- "Czechia"
place_evropa <- place_evropa %>%  mutate(Drzava=gsub(" ","", Drzava))

pomozna <- tibble(Drzava = c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia",
  "Finland", "Germany", "Hungary", "Ireland", "Lithuania", "Latvia", "Luxembourg", "Poland", "Portugal", "Romania",
  "Slovakia", "Slovenia", "Spain", "Sweden")
)

pomozna_potrosnja_1 <- tabela_potrosnja_izleti %>% dplyr::select(c("Leto", "Drzava", "Namen", "Lokacija","Povprecna.potrosnja.na.izlet", "Povprecna.potrosnja.na.noc")) %>%
  filter(Namen == "Personal reasons")

pomozna_potrosnja_2 <- tabela_potrosnja_izleti %>% dplyr::select(c("Leto", "Drzava","Namen", "Lokacija", "Povprecna.potrosnja.na.izlet", "Povprecna.potrosnja.na.noc")) %>%
  filter(Namen == "Professional, business")

tabela_potrosnja_1 <- pomozna_potrosnja_1 %>% dplyr::select("Leto", "Drzava", "Lokacija")
tabela_potrosnja_1$Potrosnja.izlet <- ((pomozna_potrosnja_1$Povprecna.potrosnja.na.izlet + pomozna_potrosnja_2$Povprecna.potrosnja.na.izlet)/2)
tabela_potrosnja_1$Potorsnja.noc <- ((pomozna_potrosnja_1$Povprecna.potrosnja.na.noc + pomozna_potrosnja_2$Povprecna.potrosnja.na.noc)/2)

pomozna_potrosnja_3 <- tabela_potrosnja_1 %>% filter(Lokacija == "Domestic country")
pomozna_potrosnja_4 <- tabela_potrosnja_1 %>% filter(Lokacija == "Outbound")
tabela_potrosnja_1 <- pomozna_potrosnja_3 %>% dplyr::select("Leto","Drzava") 
tabela_potrosnja_1$Potrosnja.na.izlet <- ((pomozna_potrosnja_3$Potrosnja.izlet + pomozna_potrosnja_4$Potrosnja.izlet)/2) 
tabela_potrosnja_1$Potrosnja.na.noc <- ((pomozna_potrosnja_3$Potorsnja.noc + pomozna_potrosnja_4$Potorsnja.noc)/2)

tabela_potrosnja_1 <- tabela_potrosnja_1[tabela_potrosnja_1$Drzava %in% 
                                           c("Austria", "Belgium", "Latvia", "Bulgaria", "Croatia", "Cyprus",
                                             "Czechia", "Denmark", "Estonia",
                                            "Finland", "Germany", "Hungary", "Ireland", "Lithuania", "Luxembourg",
                                             "Poland", "Portugal", "Romania","Slovakia", "Slovenia", "Spain", "Sweden"),]  

tabela_potrosnja_1 <- tabela_potrosnja_1[!grepl("2019", tabela_potrosnja_1$Leto),]
tabela_potrosnja_1 <- tabela_potrosnja_1 %>% arrange(Drzava) 


tabela_place_potrosnja <- pomozna %>% 
  left_join(place_evropa, by = "Drzava")
tabela_place_potrosnja[with(tabela_place_potrosnja, order(Leto, Drzava)), ]



tabela_place_potrosnja <- tabela_place_potrosnja %>%
  left_join(tabela_potrosnja_1, by = c("Drzava", "Leto")) 


# Glavne tri tabele: tabela_izobrazba_sodelovanje, tabela_potrosnja_izleti, tabela_place_potrosnja, dve pomembni manjsi tabeli sta Potovanja.Skupaj in Potrosnja.Skupaj
