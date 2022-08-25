# Analiza turizma v Evropski uniji

Vzorčni repozitorij za projekt pri predmetu APPR v študijskem letu 2021/22. 

## Tematika

V moji projektni nalogi se bom posvetila analizi turizma v državah Evropske unije 
in ga primerjala s stopnjo izobrazbe. Naloge se bom lotila tako, da bom za začetek analizirala
odstotek ljudi, ki so v zadnjem letu potovali, izobrazbo prebivalstva in pa število potovanj. Nato se bom posvetila potrošnji posameznih držav v turizmu, na koncu pa bom dobljene rezultate primerjala s povprečno neto plačo prebivalstva.

## Vir podatkov
Podatke bom črpala iz [Eurostata](https://ec.europa.eu/eurostat/web/main/home) ter iz Wikipedie.

## Tabele
Podatke bom zbrala v treh tabelah:
* `1. tabela` : Vsebuje podatke o izobrazbi in sodelovanju prebivalcev držav Evropske Unije v turizmu.
Stolpci: Leto, Država, Spol, Odstotek sodelujočih v turizmu znotraj države, Odstotek sodelujočih v turizmu izven države, Odstotek vseh sodelujočih, Odstotek ljudi s primarno izobrazbo, Odstotek ljudi s sekundarno izobrazbo, Odstotek ljudi s terciarno izobrazbo.

* `2. tabela` : Vsebuje podatke o potrošnji v turizmu in številu izletov držav Evropske Unije. 
Stolpci: Leto, Država, Namen, Lokacija, Število potovanje, Povprečna potrošnjan na izlet, Povprečna potrošnja na noč, Skupna potrošnja.

* `3. tabela` : Vsbuje podatke o povprečni neto plači in povprečnipotrošnji na noč/izlet.
Stolpci: Leto, Država, Plača, Povprečna potrošnja na izlet, Povprečna potrošnja na noč

Vhodne podatke za naštete tabele bom pridobila iz CSV datotek in ene html datoteke.

## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`.
Ko ga prevedemo, se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`.
Potrebne knjižnice so v datoteki `lib/libraries.r`
Podatkovni viri so v mapi `podatki/`.
Zemljevidi v obliki SHP, ki jih program pobere,
se shranijo v mapo `../zemljevidi/` (torej izven mape projekta).

