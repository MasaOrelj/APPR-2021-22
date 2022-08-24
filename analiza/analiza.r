# 4. faza: Napredna analiza podatkov

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

library(cluster)
library(rgdal)
library(rgeos)
library(raster)
library(tmap)
library(sp)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)


obrisi = function(podatki, hc = TRUE, od = 2, do = NULL) {
  n = nrow(podatki)
  if (is.null(do)) {
    do = n - 1
  }
  
  razdalje = dist(podatki)
  
  k.obrisi = tibble()
  for (k in od:do) {
    if (hc) {
      o.k = hclust(razdalje) %>%
        cutree(k) %>%
        silhouette(razdalje)
    } else {
      set.seed(42) # zato, da so rezultati ponovljivi
      o.k = kmeans(podatki, k)$cluster %>%
        silhouette(razdalje)
    }
    k.obrisi = k.obrisi %>% bind_rows(
      tibble(
        k = rep(k, n),
        obrisi = o.k[, "sil_width"]
      )
    )
  }
  k.obrisi$k = as.ordered(k.obrisi$k)
  
  k.obrisi
}

obrisi.povprecje = function(k.obrisi) {
  k.obrisi.povprecje = k.obrisi %>%
    group_by(k) %>%
    summarize(obrisi = mean(obrisi))
}

obrisi.k = function(k.obrisi) {
  obrisi.povprecje(k.obrisi) %>%
    filter(obrisi == max(obrisi)) %>%
    summarize(k = min(k)) %>%
    unlist() %>%
    as.character() %>%
    as.integer()
}


diagram.obrisi = function(k.obrisi) {
  ggplot() +
    geom_boxplot(
      data = k.obrisi,
      mapping = aes(x = k, y = obrisi)
    ) +
    geom_point(
      data = obrisi.povprecje(k.obrisi),
      mapping = aes(x = k, y = obrisi),
      color = "red"
    ) +
    geom_line(
      data = obrisi.povprecje(k.obrisi),
      mapping = aes(x = as.integer(k), y = obrisi),
      color = "red"
    ) +
    geom_point(
      data = obrisi.povprecje(k.obrisi) %>%
        filter(obrisi == max(obrisi)) %>%
        filter(k == min(k)),
      mapping = aes(x = k, y = obrisi),
      color = "blue"
    ) +
    xlab("število skupin (k)") +
    ylab("Obrisi (povprečje obrisov)") +
    ggtitle(paste("Maksimalno povprečje obrisov pri k =", obrisi.k(k.obrisi))) +
    theme_classic()
}



sodelovanje = tabela_izobrazba_sodelovanje %>% 
  filter(
    Leto == 2020,
    Spol == "Total",
  ) %>% dplyr::select(Drzava, Odstotek.sodelujocih.znotraj.drzave, Odstotek.sodelujocih.izven.drzave.)





#dendrogram, hierarhično razvrščanje v skupine
Drzave = sodelovanje$Drzava %>% unlist()
razdalje = sodelovanje[,2:3] %>% dist()
dendrogram = razdalje %>% hclust(method = "ward.D")

hc.kolena = function(dendrogram, od = 1, do = NULL, eps = 0.5) {
  # število primerov in nastavitev parametra do
  n = length(dendrogram$height) + 1
  if (is.null(do)) {
    do = n - 1
  }
  # k.visina je tabela s štirimi stolpci
  # (1) k, število skupin
  # (2) višina združevanja
  # (3) sprememba višine pri združevanju
  # (4) koleno: ali je točka koleno?
  k.visina = tibble(
    k = as.ordered(od:do),
    visina = dendrogram$height[do:od]
  ) %>%
    # sprememba višine
    mutate(
      dvisina = visina - lag(visina)
    ) %>%
    # ali se je intenziteta spremembe dovolj spremenila?
    mutate(
      koleno = lead(dvisina) - dvisina > eps
    )
  k.visina
}

# iz tabele k.visina vrne seznam vrednosti k,
# pri katerih opazujemo koleno
hc.kolena.k = function(k.visina) {
  k.visina %>%
    filter(koleno) %>%
    dplyr::select(k) %>%
    unlist() %>%
    as.character() %>%
    as.integer()
}

# izračunamo tabelo s koleni za dendrogram
r = hc.kolena(dendrogram)

sodelovanje_obrnjena <- data.frame(t(sodelovanje[-1]))
colnames(sodelovanje_obrnjena) <- Drzave
razdalje2 <- sodelovanje_obrnjena[,-1] %>% dist()
dendogram2 <- razdalje2 %>% hclust(method = "ward.D")

hierarhično_razvrščanje <- plot(
  dendrogram2,
  labels = Drzave,
  ylab = "višina",
  main = NULL
)

diagram.kolena = function(k.visina) {
  k.visina %>% ggplot() +
    geom_point(
      mapping = aes(x = k, y = visina),
      color = "red"
    ) +
    geom_line(
      mapping = aes(x = as.integer(k), y = visina),
      color = "red"
    ) +
    geom_point(
      data = k.visina %>% filter(koleno),
      mapping = aes(x = k, y = visina),
      color = "blue", size = 2
    ) +
    ggtitle(paste("Kolena:", paste(hc.kolena.k(k.visina), collapse = ", "))) +
    xlab("število skupin (k)") +
    ylab("Razdalja pri združevanju skupin") +
    theme_classic()
}

diagram.kolena(r)

diagram.skupine = function(podatki, oznake, skupine, k) {
  podatki = podatki %>%
    bind_cols(skupine) %>%
    dplyr::rename(skupina = ...4)
  
  d = podatki %>%
    ggplot(
      mapping = aes(
        x = x, y = y, color = skupina
      )
    ) +
    geom_point() +
    geom_label(label = oznake, size = 2) +
    scale_color_hue() +
    theme_classic()
  
  for (i in 1:k) {
    d = d + geom_encircle(
      data = podatki %>%
        filter(skupina == i)
    )
  }
  d
}

b <- transform(sodelovanje, Znotraj= as.numeric(unlist(sodelovanje[,2])), 
               Izven = as.numeric(unlist(sodelovanje[,3]))) %>%
               dplyr::select(Znotraj, Izven)
skupine = b[, -1] %>%
  kmeans(centers = 3) %>%
  getElement("cluster") %>%
  as.ordered()

print(skupine)

r.hc = sodelovanje[, -1] %>% obrisi(hc = TRUE)
r.km = sodelovanje[, -1] %>% obrisi(hc = FALSE)

diagram.obrisi(r.hc)
diagram.obrisi(r.km)
























