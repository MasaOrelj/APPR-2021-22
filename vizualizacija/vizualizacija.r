# 3. faza: Vizualizacija podatkov

library(ggplot2)

library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(plyr)
library(tibble)
library(stringr)
library(xml2)
library(rvest)
library(plotly)

#Sodelovanje v turizmu med visoko izobraženimi državami glede na spol

Visoka.izobrazba <- tabela_izobrazba_sodelovanje %>%
  filter(Spol == "Total") %>%
  group_by(Drzava) %>%
  summarise(Povprecna.visoka.izobrazba=mean(Tertiary.education[!is.na(Tertiary.education)])) %>% arrange(Povprecna.visoka.izobrazba)
Drzave1 <- Visoka.izobrazba[21:27,1]
Drzave1 <- Drzave1[!grepl("Luxembourg", Drzave1$Drzava),]

Drzave2 <- Visoka.izobrazba[1:6,1]

tabela_izobrazba_sodelovanje <-na.omit(tabela_izobrazba_sodelovanje)
         
graf1 <- tabela_izobrazba_sodelovanje %>% 
  filter(Drzava %in% Drzave1$Drzava) %>%
  
  ggplot(
    mapping = aes(x = Leto, y = Odstotek.vseh.sodelujocih , color = Spol)
  ) +
  labs(
    x = "Leto",
    y = "Odstotek",
    title = "Sodelovanje v turizmu med visoko izobraženimi državami glede na spol"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    axis.title.x = element_text(vjust = 0)
  )+
  geom_line(aes(linetype = Spol)) +
  geom_point()+
  scale_linetype_manual(values=c("solid", "solid","twodash")) +
  
  facet_wrap(. ~ Drzava, ncol = 3) +
  coord_cartesian(ylim = c(20,100))

graf1

#Sodelovanje v turizmu med nizko izobraženimi državami glede na spol

graf2 <- tabela_izobrazba_sodelovanje %>%
  filter(Drzava %in% Drzave2$Drzava) %>%
  
  ggplot(
    mapping = aes(x = Leto, y = Odstotek.vseh.sodelujocih , color = Spol)
  ) +
  labs(
    x = "Leto",
    y = "Odstotek",
    title = "Sodelovanje v turizmu med nizko izobraženimi državami glede na spol"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    axis.title.x = element_text(vjust = 0)
  )+
  geom_line(aes(linetype = Spol)) +
  geom_point()+
  scale_linetype_manual(values=c("solid", "solid","twodash")) +
  
  facet_wrap(. ~ Drzava, ncol = 3) +
  coord_cartesian(ylim = c(20, 100))

graf2

#Dokaz, da spol ni pomemben faktor v sodelovanju v turizmu

graf3 <- tabela_izobrazba_sodelovanje %>%
  ggplot(
    mapping = aes(x = Spol, y = Odstotek.vseh.sodelujocih)
  ) +
  geom_boxplot() +
  labs(
    x = "Spol",
    y = "Odstotek vseh sodelujočih",
    title = "Povezava med sodelovanjem v turizmu in spolom v državah EU"
  )
ggplotly(graf3)


#Sodelovanje v turizmu med visoko izobraženimi državami glede na lokacijo

graf4 <- tabela_izobrazba_sodelovanje %>% filter(Spol == "Total", Drzava %in% Drzave1$Drzava) %>%
  ggplot(
    mapping= aes(x = Leto)
  ) +
  geom_line(aes(y = Odstotek.sodelujocih.znotraj.drzave), color="Blue", size = 1) +
  geom_line(aes(y = Odstotek.sodelujocih.izven.drzave), color="Green", size = 1) +
  geom_line(aes(y= Odstotek.vseh.sodelujocih), color="red") +
  facet_wrap(. ~ Drzava, ncol = 3) +
  theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 0.5),
      axis.title.x = element_text(vjust = 0)
    )+
  labs(
    x = "Leto",
    y = "Odstotek",
    title = "Sodelovanje v turizmu med visoko izobraženimi državami glede na lokacijo") 
  
  
ggplotly(graf4)

#Sodelovanje v turizmu med nizko izobraženimi državami glede na lokacijo
colors1 <- c("Znotraj države" = "blue", "Izven države" = "green", "Skupaj" = "red")
graf5 <- tabela_izobrazba_sodelovanje %>% filter(Spol == "Total", Drzava %in% Drzave2$Drzava) %>%
  ggplot(
    mapping= aes(x = Leto)
  ) +
  geom_line(aes(y = Odstotek.sodelujocih.znotraj.drzave, color="Znotraj države"), size = 1) +
  geom_line(aes(y = Odstotek.sodelujocih.izven.drzave, color="Izven države"), size = 1) +
  geom_line(aes(y= Odstotek.vseh.sodelujocih, color="Skupaj")) +
  facet_wrap(. ~ Drzava, ncol = 3) +
  labs(
    x = "Leto",
    y = "Odstotek",
    title = "Sodelovanje v turizmu med nizko izobraženimi državami glede na lokacijo",
    color = "Legend"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    axis.title.x = element_text(vjust = 0)
  )+
  scale_color_manual(values = colors1)

ggplotly(graf5)



#Skupna potrošnja v državah EU od 2012 do 2019

graf6 <- Potrosnja.Skupaj%>%
  ggplot(mapping=aes(x=Leto, y=Skupna.potrosnja)) + geom_bar(stat= "identity", color="blue", fill="light blue") +
  labs(
    x = "Leto",
    y = "Skupna potrošnja v milijonih",
    title = "Skupna potrošnja držav EU"
  ) +
  coord_cartesian(ylim = c(75000, 175000)) +
  stat_summary(geom ="line",
               fun = mean,
               size = 1)
ggplotly(graf6)

#Skupno število potovanj v državah EU od leta 2012 do 2019
graf7 = Potovanja.Skupaj %>%
  ggplot(
    mapping = aes(x = Leto, y = Stevilo.potovanj)
  ) +
  labs(
    x = "Leto",
    y = "Skupno število potovanj",
    title = "Skupno število potovanj držav EU"
  ) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    axis.title.x = element_text(vjust = 0)
  )+
  geom_line(color='light blue', arrow = arrow(angle = 20), size=2)+
  geom_point(size = 2, color='blue')

graf7

#Povezava med povprečno neto plačo in potrošnjo v turizmu v EU
tabela_place_potrosnja <- na.omit(tabela_place_potrosnja)
colors <- c("Potrosnja.na.izlet" = "blue", "Potrosnja.na.noc" = "green")
graf8 <- tabela_place_potrosnja %>%
  ggplot(mapping= aes(x = Placa),
    geom = "point",
    stat = "identity",
    position = "identity"
  ) +
  geom_point(aes(y= Potrosnja.na.izlet, color = "Potrosnja.na.izlet")) +
  geom_smooth(aes(y =Potrosnja.na.izlet), method=lm)+
  geom_point(aes(y=Potrosnja.na.noc, color = "Potrosnja.na.noc"))+
  geom_smooth(aes(y=Potrosnja.na.noc), method=lm, color = "green")+
  labs(
    x = "Povprečna neto plača",
    y = "Povprečna potrošnja",
    title = "Povezava med povprečno neto plačo in povprečno potrošnjo na izlet in na noč v turizmu",
    color = "Legend") +
  scale_color_manual(values = colors)
    
  ggplotly(graf8)


#Povezava med stopnjo visoko izobraženih in sodelujočih v izvendržavnem turizmu
visokoizobrazeni_sodelovanje <- tabela_izobrazba_sodelovanje %>%
  filter(Spol == "Total")
visokoizobrazeni_sodelovanje <- na.omit(visokoizobrazeni_sodelovanje)

graf9 <- visokoizobrazeni_sodelovanje %>%
  ggplot(
    mapping = aes(x = Odstotek.sodelujocih.izven.drzave, y = Tertiary.education, color = Leto),
    geom = "point",
    stat = "identity",
    position = "identity"
  ) +
  labs(
    x = "Odstotek sodelujočih v turizmu izven države",
    y = "Odstotek visokoizobraženega prebivalstva",
    title = "Povezava med visoko izobrazbo in sodelovanjem v turizmu izven države"
  ) +
  geom_point(size = 3)+
  geom_smooth() 
graf9

#Zemljevid odstotka sodelujočih v turizmu v državah EU leta 2020

library(rgdal)
library(rgeos)
library(raster)
library(tmap)
library(sp)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

source("lib/uvozi.zemljevid.r", encoding="UTF-8")

Data(World) 
Europe <- World %>% filter(continent == "Europe")

lvls <- (Europe$name)

tabela_zemljevid <- tabela_izobrazba_sodelovanje %>% filter(Spol == "Total", Leto =="2020")
tabela_zemljevid <- tabela_zemljevid[c(2,6,1,3,4,5,7,8,9)]
tabela_zemljevid <- tabela_zemljevid[,1:2]
tabela_zemljevid$Drzava[tabela_zemljevid$Drzava =="Czechia"] = "Czech Rep."
primerjava <- data.frame(Drzava = lvls) %>% left_join(tabela_zemljevid, by = "Drzava")
manjkajoci <- primerjava[is.na(primerjava$Odstotek.vseh.sodelujocih), ]


df = data.frame(drzava = manjkajoci$Drzava, Drzava = c(  "Albania", 
                                                         
                                                         "Bosnia and Herz.",
                                                         "Belarus",
                                                         "Switzerland",
                                                         "United Kingdom",
                                                         "Iceland",
                                                         "Kosovo",
                                                         "Moldova",
                                                         "Macedonia",
                                                         "Montenegro",
                                                         "Norway",
                                                         "Russia",
                                                         "Serbia",
                                                         "Ukraine"
                                                         ))



tabela_zemljevid <- tabela_zemljevid %>% left_join(df) %>% mutate(Drzava=ifelse(is.na(drzava), Drzava, drzava))
tabela_zemljevid <- tabela_zemljevid[,1:2]
podatki <- merge(Europe, tabela_zemljevid, by.x = "name", by.y = "Drzava")


tmap_mode("view")
z2 <- tm_shape(podatki) + tm_polygons("Odstotek.vseh.sodelujocih", popup.vars=c("Odstotek: " = "Odstotek.vseh.sodelujocih"))



zemljevid <- podatki %>% ggplot() +
  geom_sf() +
  coord_sf(xlim = c(-25,50), ylim = c(35,70), expand = FALSE) +
  aes(fill = Odstotek.vseh.sodelujocih) +
  scale_fill_gradient(low="pink", high="magenta") +
  ggtitle('Katera država v EU najbolj sodeluje v turizmu?') +
  labs(fill = "Odstotek sodelujočih v turizmu leta 2020")+
  theme_classic() +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )+
  geom_sf_text(aes(label = name), color = "black", size = 2)









