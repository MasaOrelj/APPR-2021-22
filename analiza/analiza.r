# 4. faza: Napredna analiza podatkov


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
    summarise(obrisi = mean(obrisi))
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
  ) %>% dplyr::select(-Leto, -Spol)
sodelovanje <- sodelovanje[,1:3]



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


hierarhicno_razvrscanje <- plot(
  dendrogram,
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

d_kolena <- diagram.kolena(r)
d_kolena

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

r.hc = sodelovanje[, 2:3] %>% obrisi(hc = TRUE)
r.km = sodelovanje[, 2:3] %>% obrisi(hc = FALSE)



d_obrisi1 <- diagram.obrisi(r.hc)

d_obrisi2 <- diagram.obrisi(r.km)

#Optimalno število skupin je torej 2 ali 4/5.

drzave.x.y =
  as_tibble(razdalje %>% cmdscale(k = 2)) %>%
  bind_cols(Drzave) %>%
  dplyr::select(drzava = ...3, x = V1, y = V2)

k = obrisi.k(r.hc)
skupine = sodelovanje[, -1] %>%
  dist() %>%
  hclust(method = "ward.D") %>%
  cutree(k = k) %>%
  as.ordered()
diagram_skup1 <- diagram.skupine(drzave.x.y, drzave.x.y$drzava, skupine, k)
diagram_skup1

k = obrisi.k(r.km)
set.seed(42) # ne pozabimo na ponovljivost rezultatov
skupine = sodelovanje[, -1] %>%
  kmeans(centers = k) %>%
  getElement("cluster") %>%
  as.ordered()
diagram_skup2 <- diagram.skupine(drzave.x.y, drzave.x.y$drzava, skupine, k)
diagram_skup2

set.seed(42)
skupine = sodelovanje[, -1] %>%
  kmeans(centers = 3) %>%
  getElement("cluster") %>%
  as.ordered()
diagram_skup3 <- diagram.skupine(drzave.x.y, drzave.x.y$drzava, skupine, 3)
diagram_skup3


###########################PRECNO.PREVERJANJE#############################

#Napovedali bomo vrednost spremenljivke Odstotek.vseh.sodelujocih na podlagi spremenljivke Tertiary.education

tabela_napovedovanje <- tabela_izobrazba_sodelovanje %>% filter(Spol == "Total", Leto %in% c(2017,2018,2019)) %>%
  dplyr::select(Odstotek.sodelujocih.izven.drzave, Odstotek.vseh.sodelujocih, Tertiary.education)


povezave <- ggpairs(tabela_napovedovanje)
povezave

#Visoka izobrazba je najbolj povezana s skupnim odstotokom vseh sodelujočih v turizmu


gg <- ggplot(tabela_napovedovanje, aes(x=Odstotek.vseh.sodelujocih, y=Tertiary.education)) + geom_point()


podatki <- tabela_napovedovanje
k <- 5
formula <- Odstotek.vseh.sodelujocih ~ Tertiary.education 

napaka.cv <- function(podatki, k, formula) {
  n <- nrow(podatki)
  r <- sample(1:n)
  
  razrez <- cut(1:n, k, labels = FALSE)
  
  razbitje <- split(r, razrez)
  
  pp.napovedi = rep(0, n)
  for (i in 1:length(razbitje)) {
    #Naučimo se modela na množici S/Si
    model = podatki[ -razbitje[[i]], ] %>% lm(formula = formula)
    #Naučen model uporabimo za napovedi na Si
    pp.napovedi[ razbitje[[i]] ] = predict(object = model, newdata = podatki[ razbitje [[i]], ] )
    
  }
  
  #model so učni podatki in ostalo ( razbitje[[i]]) so testni podatki. 
  
  napaka <- mean((pp.napovedi - podatki$Odstotek.vseh.sodelujocih)^2)
  return(napaka)
}
napaka.cv(tabela_napovedovanje, 5, formula)

formule <- c(Odstotek.vseh.sodelujocih ~ Tertiary.education ,
             Odstotek.vseh.sodelujocih ~ Tertiary.education + I(Tertiary.education ^2),
             Odstotek.vseh.sodelujocih ~ Tertiary.education + I(Tertiary.education ^2) + I(Tertiary.education ^3),
             Odstotek.vseh.sodelujocih ~ Tertiary.education  + I(Tertiary.education ^2) + I(Tertiary.education ^3) + I(Tertiary.education ^4),
             Odstotek.vseh.sodelujocih ~ Tertiary.education + I(Tertiary.education ^2) + I(Tertiary.education ^3) + I(Tertiary.education ^4) + I(Tertiary.education ^5))
napake <- rep(0, 5)
for (i in 1:5){
  formula <- formule[[i]]
  napaka <- napaka.cv(tabela_napovedovanje, 5, formula)
  napake[i] <- napaka
}
which.min(napake)

model <- lm(data = tabela_napovedovanje, formula = Odstotek.vseh.sodelujocih ~ Tertiary.education + I(Tertiary.education ^2) + I(Tertiary.education ^3) + I(Tertiary.education ^4) + I(Tertiary.education ^5))

graf_precno_preverjanje <- ggplot(tabela_napovedovanje, aes(x= Tertiary.education, y=Odstotek.vseh.sodelujocih)) +  geom_point() + geom_smooth(formula = y ~ x + x^2 + x^3 + x^4 + x^5,
                                                                                   se = FALSE, fullrange = TRUE, color = "green")

graf_precno_preverjanje

########################################NAPOVEDOVANJE#####################################
#Napovedovanje potrosnje v EU

tabela_napoved_potrosnja <- Potrosnja.Skupaj
names(tabela_napoved_potrosnja)[2] <- "potrosnja"

zamakni <- function(x, n){c(rep(NA, n), x)[1:length(x)]}

naredi.df <- function(x){
  data.frame(potrosnja = x,
             potrosnja1 = zamakni(x, 1),
             potrosnja2 = zamakni(x, 2),
             potrosnja3 = zamakni(x, 3),
             potrosnja4 = zamakni(x, 4)
             )
}

df2 <- naredi.df(tabela_napoved_potrosnja$potrosnja)
model1 = ranger(formula = potrosnja ~ ., data = df2 %>% drop_na())

n = nrow(df2)

df3 <- naredi.df(c(tabela_napoved_potrosnja$potrosnja, NA))
napoved1 <- predict(model1, data = df3[n+1,])$predictions
df3[n+1,1] = napoved1



#novi_stolpec <- df3 %>% dplyr::select(potrosnja)
#df4 <- naredi.df(novi_stolpec$potrosnja)
#model2 = ranger(formula = potrosnja ~ ., data = df4 %>% drop_na())
#
#n = nrow(df4)
#
#df5 <- naredi.df(c(novi_stolpec$potrosnja, NA))
#napoved2 <- predict(model2, data = df5[n+1,])$predicitions
#df5[n+1,1] <- napoved2

Leto <- c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)

pricakovanje <- as_tibble(data.frame(
  Leto,
  df3$potrosnja))


napoved.graf <- ggplot(pricakovanje) + geom_line(mapping = aes(x = Leto, y = df3.potrosnja), color = "red")+
  geom_line(mapping = aes(x = Leto, y = df3.potrosnja, color = Leto ), show.legend = FALSE)+
  labs(
    x = "Leto",
    y = "Skupna potrošnja držav EU (v milijonih)",
    title = "Gibanje skupne potrošnje držav EU med leti 2012 in 2021"
  )
napoved_graf <- ggplotly(napoved.graf)
napoved_graf









