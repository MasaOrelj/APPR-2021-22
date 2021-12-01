# Analiza potovanj v Evropski uniji

Vzorčni repozitorij za projekt pri predmetu APPR v študijskem letu 2021/22. 

## Tematika

V moji projektni nalogi se bom posvetila analizi turizma (natančneje potovanj) v državah Evropske unije 
in ga primerjala s stopnjo izobrazbe. Naloge se bom lotila tako, da bom za začetek analizirala
odstotek ljudi, ki so v zadnjem letu potovali in pa število potovanj. Nato se bom posvetila potrošnji
posameznih držav v turizmu, na koncu pa bom dobljene rezultate primerjala s stopnjo izobrazbe prebivalstva.

## Vir podatkov
Podatke bom črpala iz [Eurostata](https://ec.europa.eu/eurostat/web/main/home).

## Tabele
Podatke bom zbrala v treh tabelah:
* `1. tabela` : Vsebuje podatke o sodelovanju prebivalcev držav Evropske unije v turizmu. 
Stolpci: država, leto, spol, lokacija, odstotek sodelujočih v turizmu, število izletov.
(Lokacija se bo delila le na turizem v tujini in domači turizem.)

* `2. tabela` : Vsebuje podatke o potrošnji v turizmu. 
Stolpci: država, leto, lokacija, skupna potošnja (v tisoč evrih), povprečna cena na noč(v evrih).

* `3. tabela` : Vsbuje podatke o stopnji izobrazbe.
Stolpci: država, leto, stopnja izobrazbe, spol, odstotek populacije.

Vhodne podatke za naštete tabele bom pridobila iz CSV datotek.

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

