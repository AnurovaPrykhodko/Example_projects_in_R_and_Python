Laboration 3
================

### Kommundata

-   **A:** Andel invånare med utländsk härkomst

``` r
Forgein <- forgein %>% 
  # Kolumm för år skapas
  pivot_longer(
    c(`2002`:`2021`),
    names_to = "year",
    values_to = "count",
    names_transform = list(year = as.double)
  ) %>%
  # Bakgrund delas upp i två
  pivot_wider(
    names_from = `utländsk/svensk bakgrund`,
    values_from = count
  ) %>%
  # Tar ut procent utländska 
  mutate(forgein_perc = `utländsk bakgrund` / (`utländsk bakgrund` + `svensk bakgrund`) * 100) %>%
  separate(region, into = c("code", "name"), extra = "merge") %>%
  select(code, name, year, forgein_perc)

Forgein
```

    ## # A tibble: 5,800 × 4
    ##    code  name            year forgein_perc
    ##    <chr> <chr>          <dbl>        <dbl>
    ##  1 0114  Upplands Väsby  2002         27.1
    ##  2 0114  Upplands Väsby  2003         27.6
    ##  3 0114  Upplands Väsby  2004         28.0
    ##  4 0114  Upplands Väsby  2005         28.5
    ##  5 0114  Upplands Väsby  2006         28.8
    ##  6 0114  Upplands Väsby  2007         29.3
    ##  7 0114  Upplands Väsby  2008         30.0
    ##  8 0114  Upplands Väsby  2009         30.6
    ##  9 0114  Upplands Väsby  2010         31.4
    ## 10 0114  Upplands Väsby  2011         32.6
    ## # … with 5,790 more rows

-   **B:** Medelinkomst

``` r
# Tar ut det som ska användas
Inkomst <- inkomst %>%
  select("region", "år", "Totalsumma, mnkr", "Antal personer") %>% 
  separate(region, into = c("code", "name"), extra = "merge") %>%
  rename("year" = år) %>%
  group_by(code, name, year) %>% 
  summarise(summa = sum(`Totalsumma, mnkr`), personer = sum(`Antal personer`)) %>%
  # Tar fram medelmånadslön
  mutate(income = summa / (personer * 12) * 1000) %>%
  select(code, name, year, income)

Inkomst
```

    ## # A tibble: 6,380 × 4
    ## # Groups:   code, name [290]
    ##    code  name            year income
    ##    <chr> <chr>          <dbl>  <dbl>
    ##  1 0114  Upplands Väsby  1999   16.3
    ##  2 0114  Upplands Väsby  2000   17.1
    ##  3 0114  Upplands Väsby  2001   17.9
    ##  4 0114  Upplands Väsby  2002   18.3
    ##  5 0114  Upplands Väsby  2003   18.7
    ##  6 0114  Upplands Väsby  2004   18.9
    ##  7 0114  Upplands Väsby  2005   19.3
    ##  8 0114  Upplands Väsby  2006   19.8
    ##  9 0114  Upplands Väsby  2007   20.5
    ## 10 0114  Upplands Väsby  2008   21.3
    ## # … with 6,370 more rows

-   **C:** Våldsbrott

``` r
Brott <- brott %>% 
  # Skapar kolumm för år
  pivot_longer(
    c(`2000`:`2021`),
    names_to = "year",
    values_to = "viol_crime",
    names_transform = list(year = as.double)
    ) %>%
# Tar ut det vi vill ha
  rename("name" = "Område") %>%
  select(name, year, viol_crime) %>%
  filter(name != "Alla kommuner")

Brott
```

    ## # A tibble: 6,380 × 3
    ##    name   year viol_crime
    ##    <chr> <dbl>      <dbl>
    ##  1 Ale    2000       433.
    ##  2 Ale    2001       359.
    ##  3 Ale    2002       426.
    ##  4 Ale    2003       350.
    ##  5 Ale    2004       384.
    ##  6 Ale    2005       523.
    ##  7 Ale    2006       556.
    ##  8 Ale    2007       543.
    ##  9 Ale    2008       703.
    ## 10 Ale    2009       661.
    ## # … with 6,370 more rows

-   **D:** Största parti

``` r
# Skapar komunkod
Parti <- parti %>%
  unite("code","LÄNSKOD", "KOMMUNKOD", sep = "") %>%
  mutate(code = str_pad(string = code, width = 4, pad = 0, side = "left")) %>%
  right_join(parti) %>%
  # Skapar kolumm för parti och tar ut de med flest röster i varje kommun
  pivot_longer(
    c("M":"FI"),
    names_to = "largest_party",
    values_to = "roster") %>%
  group_by(code) %>%
  top_n(1) %>%
  rename("name" = "KOMMUNNAMN") %>%
  select("KOMMUNKOD", "code", "name", "largest_party")

Parti
```

    ## # A tibble: 290 × 4
    ## # Groups:   code [290]
    ##    KOMMUNKOD code  name           largest_party
    ##        <dbl> <chr> <chr>          <chr>        
    ##  1        14 0114  Upplands Väsby S            
    ##  2        15 0115  Vallentuna     M            
    ##  3        17 0117  Österåker      M            
    ##  4        20 0120  Värmdö         M            
    ##  5        23 0123  Järfälla       S            
    ##  6        25 0125  Ekerö          M            
    ##  7        26 0126  Huddinge       S            
    ##  8        27 0127  Botkyrka       S            
    ##  9        28 0128  Salem          M            
    ## 10        36 0136  Haninge        S            
    ## # … with 280 more rows

-   **E:** Andel elever ej behöriga till gymnasiet

``` r
# Ser till så att data från respektive år är märkt
gymnasium2 <- gymnasium %>%
  separate("file_name", into = c("file", "years"), sep = -6 ) %>%
  separate("years", into = c("ar"), sep = 2 ) %>%
  mutate(
    ar = str_pad(string = ar, width = 3, pad = 0, side = "left"),
    ar = str_pad(string = ar, width = 4, pad = 2, side = "left")
    )

# Gör year till double
year <- pull(gymnasium2, `ar`) %>% as.numeric()

gymnasium3 <- add_column(gymnasium2, year)

# Tar ut data för samtliga
Gymnasium <- gymnasium3 %>%
  rename(
    "not_qualified_perc" = "Andel elever (%) ej behöriga",
    "code" = "Kommun-kod",
    "name" = "Kommun",
    ) %>%
  filter(`Kön` == "Samtliga" & `Typ av huvudman` == "Samtliga") %>%
  select(code, name, year, not_qualified_perc) 

Gymnasium
```

    ## # A tibble: 1,449 × 4
    ##    code  name        year not_qualified_perc
    ##    <chr> <chr>      <dbl>              <dbl>
    ##  1 1440  Ale         2017               19.3
    ##  2 1489  Alingsås    2017               13.5
    ##  3 0764  Alvesta     2017               29.6
    ##  4 0604  Aneby       2017               26.7
    ##  5 1984  Arboga      2017               13.9
    ##  6 2506  Arjeplog    2017               NA  
    ##  7 2505  Arvidsjaur  2017               26.2
    ##  8 1784  Arvika      2017               18  
    ##  9 1882  Askersund   2017               13.1
    ## 10 2084  Avesta      2017               30.5
    ## # … with 1,439 more rows

Här ser vi att för Arjeplog står det NA för 2017, skulle kunna vara så
att det inte togs någon information alternativt att alla blev behöriga.
Vid närmare titt visas det att NA kan vara för både och, i detta fall
blev alla i Arjeplog behöriga, men det gäller inte för alla rader med
NA.

-   **Slå samman och illustrera**

``` r
# Separerar kommunkod och namn
  kommun_data2 <- kommun_data %>%
  separate(region, into = c("code", "name"), extra = "merge") %>%
  rename("year" = "år")

# Kombinerar tabeller
kommun_allt <- kommun_data2 %>%
  full_join(Forgein, by = c("code", "year")) %>%
  full_join(Inkomst, by = c("code", "year")) %>%
  full_join(Brott, by = c("name", "year")) %>%
  full_join(Gymnasium, by = c("code", "year")) %>%
  full_join(Parti, by = c("code"))

# Filtrerar ut Stockholms län 2020 samt illustrerar
kommun_allt %>%
  filter(str_detect(code, "^01"), year == "2020", !is.na(not_qualified_perc)) %>%
  ggplot() +
  geom_text(label = "2020", x = 11, y = 40, alpha = 0.005, size = 30) +
  geom_point(aes(not_qualified_perc, forgein_perc, color = name, size = Folkmängd)) +
  labs(
    title = "Med utländsk härkomst ökar ej behöriga",
    x = "Ej behöriga till gymnasiet (%)",
    y = "Utländsk härkomst (%)",
    color = "Kommunnamn") 
```

![](laboration3_files/figure-gfm/kombinera-1.png)<!-- -->

Andel elever ej behöriga till gymnasiet verkar vara relaterbart med
utländsk härkomst, barnen kan exempelvis ha svårare för språk
alternativt ha föräldrar som inte kan hjälpa till med just svenska och
engelska. Vi ser även att ställen där andel utländska är mindre och kan
ha en högre andel ej behöriga, av den uppenbara anledningen att även
andra faktorer spelar in.

-   **Illustrera på karta**

Först tar vi ut den kommunen som har olika namn i de olika tabellerna.

``` r
# Ser vad som ej kommer med
anti_join(swe_map, kommun_allt, by = c("KnNamn" = "name")) %>%
  glimpse()
```

    ## Rows: 1
    ## Columns: 3
    ## $ KnKod    <chr> "2023"
    ## $ KnNamn   <chr> "Malung"
    ## $ geometry <MULTIPOLYGON [m]> MULTIPOLYGON (((376107.9 67...

``` r
# Kollar upp vad kommunen heter i denna tabell
kommun_allt %>%
  filter(code == "2023") %>%
  select(name)
```

    ## # A tibble: 54 × 1
    ##    name        
    ##    <chr>       
    ##  1 Malung-Sälen
    ##  2 Malung-Sälen
    ##  3 Malung-Sälen
    ##  4 Malung-Sälen
    ##  5 Malung-Sälen
    ##  6 Malung-Sälen
    ##  7 Malung-Sälen
    ##  8 Malung-Sälen
    ##  9 Malung-Sälen
    ## 10 Malung-Sälen
    ## # … with 44 more rows

Vi ser att det är Malung, som i kommun_allt heter Malung-Sälen. För
nästa uppgift, att plotta variabel för ett viss län, väljer vi länskod
20 och ändrar därmed namnet till Malung för att kunna få med den datan.

``` r
# Ändrar namn till Malung, kombinerar tabeller
kommun_allt %>%
  mutate(name = str_replace_all(name, "Malung-Sälen", "Malung")) %>%
  full_join(swe_map, by = c("code" = "KnKod")) %>%
  # Filtrerar ut Dalarna, år 2018, illustrerar
  filter(str_detect(code, "^20"), year == "2018") %>%
  ggplot(aes(geometry = geometry, fill = largest_party)) +
  geom_sf() +
  geom_sf_text(aes(label = name)) +
  labs(
    title = "År 2018 röstade majoriteten i Dalarna på center och socialdemokraterna",
    fill = "Största parti"
    ) +
  theme_void()
```

![](laboration3_files/figure-gfm/karta-1.png)<!-- --> Här ser vi en tom
bit på kartan, skulle kunna innebära förlorad data, men i detta fall är
det en sjö.

### Tunnelbanerelationer

-   **Kombination till en tabell**

``` r
# Tabellerna sammanfogas med all dess information utifrån keys
tunnelbana <- Line %>%
  full_join(LinePlatform, by = "LineNumber") %>%
  full_join(Platform, by = "PlatformNumber") %>%
  # För vissa rader saknas det information, de tar vi bort
  filter(!is.na(LineNumber))

tunnelbana
```

    ## # A tibble: 407 × 7
    ##    LineNumber LineName   PlatformNumber Direction StationName Longitude Latitude
    ##         <dbl> <chr>               <dbl>     <dbl> <chr>           <dbl>    <dbl>
    ##  1         10 tunnelban…           3031         1 Kungsträdg…      18.1     59.3
    ##  2         10 tunnelban…           3051         1 T-Centralen      18.1     59.3
    ##  3         10 tunnelban…           3131         1 Rådhuset         18.0     59.3
    ##  4         10 tunnelban…           3151         1 Fridhemspl…      18.0     59.3
    ##  5         10 tunnelban…           3161         1 Stadshagen       18.0     59.3
    ##  6         10 tunnelban…           3201         1 Västra sko…      18.0     59.3
    ##  7         10 tunnelban…           3411         1 Huvudsta         18.0     59.3
    ##  8         10 tunnelban…           3421         1 Solna stra…      18.0     59.4
    ##  9         10 tunnelban…           3431         1 Sundbyberg…      18.0     59.4
    ## 10         10 tunnelban…           3441         1 Duvbo            18.0     59.4
    ## # … with 397 more rows

Nu har vi en tabell som är enklare att arbeta med.

-   **Stationer som trafikeras av fler än en linje**

``` r
# För varje station tas antal unika linjer ut
tunnelbana %>%
  group_by(StationName) %>%
  summarise(Antal_linjer = n_distinct(LineName)) %>%
  # Filtrerar bort de som endast trafikeras av en linje
  filter(Antal_linjer > 1)
```

    ## # A tibble: 4 × 2
    ##   StationName  Antal_linjer
    ##   <chr>               <int>
    ## 1 Fridhemsplan            2
    ## 2 Gamla stan              2
    ## 3 Slussen                 2
    ## 4 T-Centralen             3

Att stationerna trafikeras av flera linjer är enkelt att bekräfta av mig
som bor i Stockholm.

-   **Unika plattformar per linje**

``` r
# För varje linje tas antal unika plattformar ut
tunnelbana %>%
  group_by(LineName) %>%
  summarise(Platforms = n_distinct(PlatformNumber))
```

    ## # A tibble: 3 × 2
    ##   LineName                 Platforms
    ##   <chr>                        <int>
    ## 1 tunnelbanans blå linje          45
    ## 2 tunnelbanans gröna linje       107
    ## 3 tunnelbanans röda linje         77

Gröna linjen trafikerar flest plattformar, rimligt eftersom den delas
upp mer än de andra. Medan blå trafikerar minst eftersom den endast går
två håll från Kungsträgården.
