Laboration 2
================

## Fågelspaning i kungliga nationalparken

-   **Tabell på de 10 mest rapporterade arterna.** Först löser vi
    problemet med att kolummen “antal” är läst som text. Vi använder den
    kolummen för att skapa en ny, med samma information men läst som
    numerisk. Därefter skapar vi en tabell genom att ta ut antal
    rapporteringar och välja top 10.

``` r
st <- pull(artportal, "antal") %>%
 str_replace_all(c("noterad" = "1", "Ej återfunnen" = "1")) %>%
 as.numeric()
 
artportal <- add_column(artportal, st) 

artportal %>%
  group_by(artnamn) %>%
  summarise(totalt = sum(st)) %>%
  arrange(desc(totalt)) %>%
  slice_head(n = 10) %>%
  knitr::kable(caption = "Fig. 1: 10 mest rapporterade arterna.")
```

| artnamn       | totalt |
|:--------------|-------:|
| Grönsiska     |  20237 |
| Sothöna       |   8393 |
| Gräsand       |   7296 |
| Storskrake    |   6800 |
| Vitkindad gås |   6391 |
| Storskarv     |   5307 |
| Björktrast    |   3232 |
| Koltrast      |   2895 |
| Vigg          |   2404 |
| Skrattmås     |   2379 |

Fig. 1: 10 mest rapporterade arterna.

-   **Tabell på de 10 rapportörerna som registrerat flest olika arter.**
    På liknande sätt som ovan tar vi ut antal registrerade arter per
    rapportör och tar ut top 10.

``` r
artportal %>%
  group_by(rapportor, artnamn) %>%
  summarise(n = n_distinct("rapportor")) %>%
  summarise(reg_arter = sum(n)) %>%
  arrange(desc(reg_arter)) %>%
  slice_head(n = 10) %>%
  knitr::kable(caption = "Fig. 2: 10 rapportörerna som registrerar flest olika arter.")
```

| rapportor        | reg_arter |
|:-----------------|----------:|
| Svante Söderholm |       143 |
| Björn Lindkvist  |       100 |
| Ari Määttä       |        99 |
| Adrian Trost     |        96 |
| Folke K Larsson  |        94 |
| Henrik Spovin    |        91 |
| jesper sollerman |        90 |
| Magnus Edberg    |        85 |
| Erik Jonsson     |        84 |
| Mattias Öberg    |        84 |

Fig. 2: 10 rapportörerna som registrerar flest olika arter.

-   **Illustration över den veckovisa andelen av alla observationer för
    respektive art, för de 12 mest rapporterade arterna.** Först rankar
    vi arterna efter antal rappoteringar och sammanfogar den
    informationen med tidigare data, med hjälp av källan
    [community.rstudio](https://community.rstudio.com/t/is-there-a-way-to-do-a-filtered-rank-that-preserves-a-full-data-frame/20822/2 "community r studio"),
    nu kan vi sortera ut de tolv mest rapporterade samtidigt som vi
    behåller nödvändiga kolummer. Därefter tar vi fram andel
    observationer per art samt vecka. Vi flyttar även fram vecka 52.

``` r
rank_art <- artportal %>%
  group_by(artnamn) %>%
  summarise(totalt = sum(st)) %>%
  mutate(rank = min_rank(desc(totalt)))

left_join(artportal, rank_art) %>%
  mutate(vecka = lubridate::isoweek(startdatum)) %>%
  filter(rank < 13) %>%
  group_by(artnamn) %>%
  summarise(andel = st / sum(st), vecka) %>%
  mutate(vecka = factor(vecka)) %>%
  ggplot() +
  geom_col(mapping = aes(x = forcats::fct_relevel(vecka, "52"), y = andel, fill = artnamn),
           width = 1,
           show.legend = FALSE) +
  facet_wrap(vars(artnamn), scale = "free_y") +
  labs(
    title = "Fig. 3: Veckovis observation för respektive art påverkas om de är flyttfåglar",
    x = "Vecka",
    y = "Andel observation"
  ) +
  scale_x_discrete(breaks = seq(1, 23, by = 3))
```

![](Laboration2_files/figure-gfm/andel_veckovis-1.png)<!-- -->

Arterna som tydligaste är flyttfåglar är Gråhäger, Storskarv,
Storskrake, även Vitkindad gås och Skrattmås skulle kunna vara det, men
är ej bekräftad av [naturhistoriska
riksmuseet](https://www.nrm.se/faktaomnaturenochrymden/djur/faglar/vanligafaglar/vanligavarfaglarao.5780.html "naturhistoriska riksmuseet").
Notera dock att andelen observationer även kan bero på annat, som tid
ute i fält, exempelvis togs inga observationer under veckorna kring
24-51.

-   **Illustration för tid på året för parning och ungar.** Först skapas
    en ny variabel för aktiviteter som innehåller “parning” eller
    “ungar”, där resten katogoriserar som NA. Sedan plottas detta.

``` r
 artportal %>%
     mutate(aktivitet = case_when( 
       str_detect(aktivitet, "parning") ~ "Parning",
       str_detect(aktivitet, "ungar") ~ "Ungar"
     )) %>%
     filter(!is.na(aktivitet)) %>%
     ggplot() +
     geom_density(mapping = aes(x = startdatum, color = aktivitet)) + 
     labs(
       title = "Fig.4: Tid på året för respektive aktivitet är tydligt skiljbara",
       x = "Startdatum",
       y = "Densitet"
     )
```

![](Laboration2_files/figure-gfm/aktivitet-1.png)<!-- -->

-   **Illustration över hur första observationsdag varierat med åren för
    Sädesärla.** Först skapar vi nya variablar för dag och år samt tar
    ut första dagarna för observation varje år. Därefter plottas detta
    med flera lager.

``` r
artportal_arla %>%
  mutate(dag = lubridate::yday(startdatum), ar = lubridate::year(startdatum)) %>%
  group_by(ar) %>%
  slice_min(dag) %>%
  ggplot(mapping = aes(x = ar, y = dag)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(
    title = "Fig. 5: Sädesärlan, som anses vara ett vårtecken, upptäcks allt tidigare",
    caption = "Tidigare upptäckt kan bero på global uppvärming, men även ökning av antalet observationer överlag.",
    x = "År", y = "Dag på året"
  )
```

![](Laboration2_files/figure-gfm/arla-1.png)<!-- -->

# Massvaccinering

-   **Återskapning av liknande figur från FHM, vaccinerad andel av
    befolkning fast för en region.** Vi filtrerar ut region och plottar.

``` r
vacc_alder %>%
   filter(region == "Stockholm") %>%
   group_by(aldersgrupp) %>%
   ggplot(mapping = aes(x = aldersgrupp, y = andel_vaccinerade)) +
   geom_col() +
   scale_y_continuous(labels = scales::label_percent()) +
   labs(
     title = "Fig. 6: Vaccinerade med tre doser, per åldersgrupp, andel av befolkning",
     subtitle = "Region Stockholm",
     x = "",
     y = ""
   )
```

![](Laboration2_files/figure-gfm/vacc_alder-1.png)<!-- -->

-   **Återskapning av figur för framgång av vaccination mellan
    kommuner.** Eftersom vi är ute efter andel doser för specifika
    kommuner filtrerar vi ut dem, summerar respektive kolumm och
    illustrerar.

``` r
vacc_kommun %>%
   filter(lan_namn %in% c(
     "Stockholms län", "Skåne län", "Norrbottens län"
   )) %>%
   group_by(kommun_namn) %>%
   summarise(
     en_dos = sum(andel_minst_1_dos),
     två_doser = sum(andel_minst_2_doser),
     Län = lan_namn,
     Befolkning = sum(befolkning
   )) %>%
   ggplot() +
   geom_point(mapping = aes(
     x = två_doser,
     y = en_dos,
     color = Län,
     size = Befolkning
   )) +
   scale_size_continuous(labels = scales::label_number()) +
   scale_x_continuous(labels = scales::label_percent(scale = 10)) +
   scale_y_continuous(labels = scales::label_percent(scale = 10)) +
   labs(
     title = "Fig.7: Vaccinationsframgång för olika regioner", 
     x = "Andel minst 2 doser",
     y = "Andel minst 1 dos",
   )
```

![](Laboration2_files/figure-gfm/vacc_kommun-1.png)<!-- -->

-   **Illustration över antalet vaccinationer utförda per vecka och
    region.** Till att börja med är antal vaccinationer kumulativa,
    vilket vi ändrar till att visa antal per vecka. Dessutom vill vi ha
    veckonummer som visar hela perioden och inte tid på året. Sedan
    plottar vi regionvis.

``` r
   vacc_tid %>%
     filter(region != "| Sverige |") %>%
     group_by(region) %>%
     mutate(antal_vaccinationer = antal_vaccinationer - lag(antal_vaccinationer)) %>%
     mutate(Vecka = vecka - 51 + (ar - 2020) * 53) %>%
     replace_na(list(antal_vaccinationer = 0)) %>%
     ggplot() +
     geom_col(
       mapping = aes(x = Vecka, y = antal_vaccinationer, fill = region),
       show.legend = FALSE,
       width = 1
     ) +
     facet_wrap(vars(region), scales = "free_y") +
     scale_y_continuous(labels = scales::label_number()) +
     labs(
       title = "Fig. 8: Antal vaccinationer för olika regioner följer någorlunda lik trend.",
       y = "Antal"
      )
```

![](Laboration2_files/figure-gfm/vacc_tid-1.png)<!-- -->
