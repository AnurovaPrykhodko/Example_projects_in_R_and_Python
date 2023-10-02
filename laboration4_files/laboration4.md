Laboration 4
================

## SMHI

-   **Funktion för att få temperaturer**

``` r
get_temp <- function(station){
#Anpassar url efter station
  url <- paste0(
    "https://opendata-download-metobs.smhi.se/api/version/1.0/parameter/1/station/",
    station,
    "/period/latest-months/data.json"
    )
  
#Hämtar API 
  response <- httr::GET(url)
  
  smhi <- httr::content(response, as = "text") %>% jsonlite::fromJSON()

#Gör om till tibble samt tar fram datum och tid
  as_tibble(c(smhi$station, smhi$value)) %>%
    mutate(
      station = key,
      temp = value,
      datetime = lubridate::as_datetime(date / 1000),
      date = format(datetime, "%Y-%m-%d"),
      time = format(datetime, "%T")
    ) %>%
    select(station, date, time, temp)
}

#testar att den fungerar
get_temp(159880)
```

    ## # A tibble: 3,129 × 4
    ##    station date       time     temp 
    ##    <chr>   <chr>      <chr>    <chr>
    ##  1 159880  2022-04-03 01:00:00 -6.3 
    ##  2 159880  2022-04-03 02:00:00 -7.0 
    ##  3 159880  2022-04-03 03:00:00 -7.4 
    ##  4 159880  2022-04-03 04:00:00 -7.6 
    ##  5 159880  2022-04-03 05:00:00 -8.8 
    ##  6 159880  2022-04-03 06:00:00 -7.3 
    ##  7 159880  2022-04-03 07:00:00 -5.3 
    ##  8 159880  2022-04-03 08:00:00 -3.7 
    ##  9 159880  2022-04-03 09:00:00 -3.0 
    ## 10 159880  2022-04-03 10:00:00 -3.1 
    ## # … with 3,119 more rows

-   **Funktion för tropiska datum**

``` r
tropical_dates <- function(station, mintemp = 18){
  
  temp_station <- get_temp(station) %>%
    mutate(temp = temp %>% as.double())
  
#Delar in i två grupper, ena över mintemp och andra under
  high <- temp_station %>%
    group_by(date, station) %>%
    filter(temp >= mintemp) 
  
  low <- temp_station %>%
    group_by(date, station) %>%
    filter(temp < mintemp) 

#Vi vill endast ha de datum i grupp "high" som inte finns i "low"
  anti_join(high, low, by = "date")
}

#testar funktion
tropical_dates(108320)
```

    ## # A tibble: 168 × 4
    ## # Groups:   date, station [7]
    ##    station date       time      temp
    ##    <chr>   <chr>      <chr>    <dbl>
    ##  1 108320  2022-06-26 00:00:00  19.6
    ##  2 108320  2022-06-26 01:00:00  18.7
    ##  3 108320  2022-06-26 02:00:00  18.4
    ##  4 108320  2022-06-26 03:00:00  18.7
    ##  5 108320  2022-06-26 04:00:00  19.7
    ##  6 108320  2022-06-26 05:00:00  20.8
    ##  7 108320  2022-06-26 06:00:00  22.1
    ##  8 108320  2022-06-26 07:00:00  24.8
    ##  9 108320  2022-06-26 08:00:00  25.5
    ## 10 108320  2022-06-26 09:00:00  26  
    ## # … with 158 more rows

-   **Figur för tropiska datum**

``` r
#Sparar stationerna och dess namn
station <- c(107440, 78550, 89230, 71420, 68560, 65090, 87440, 83420, 98160, 
            99450, 77210, 66110, 108320, 78280)

name <- c("Eggegrund", "Fårösund", "Gotska Sandön", "Göteborg", 
                            "Hoburg", "Karlskrona-Söderstjerna", "Landsort", "Naven", 
                            "Skarpö", "Söderarm", "Ölands norra udde", "Ölands södra udde", 
                            "Örskär", "Östergarnsholm")

stations <- tibble(station, name)

#tar fram datum för varje station
map_df(station, tropical_dates) %>%
  mutate(
    station = station %>% as.double(),
    month = lubridate::month(date, label = TRUE),
    day = lubridate::day(date)
    ) %>%
  unite(datum, month, day, sep = " ") %>%
  mutate(datum = factor(datum)) %>%
#får med staionernas namn, min temp och ger x-axeln rätt ordning
  inner_join(stations) %>%
  summarise(min = min(temp), datum, name) %>%
  ggplot(aes(x = forcats::fct_relevel(datum, "Jun 25" ), name, color = min)) +
  geom_point() +
  theme_bw() +
  labs(
    title = "Tropiska (eller nästan tropiska) datum 2022",
    x = NULL,
    y = NULL,
    color = "Min temp"
  ) +
  theme(axis.text.x = element_text(angle = 90))
```

![](laboration4_files/figure-gfm/figur%20tropiska-1.png)<!-- -->

-   **Karta över temperaturer** Här har källa använts:
    [https://medium.com/@Periscopic/cozy-collecting-part-2-5e717588e37b](källa).

``` r
plot_temp <- function(){

#Hämtar API
  response <- httr::GET("https://opendata-download-metobs.smhi.se/api/version/1.0/parameter/1/station-set/all/period/latest-hour/data.json")
  
  temperatur_nu <- httr::content(response, as = "text") %>% jsonlite::fromJSON()

#Gör om till tibble, ena kolummen innehåller en lista så vi tar ut den med unnest 
  temperatur_tibble <- as_tibble(temperatur_nu$station)%>%
    unnest_wider(value) %>%
    filter(!is.na(value)) %>%
    select(longitude, latitude, value)

#Omvandlar temperatur från chr till num
  temp <- pull(temperatur_tibble, value) %>% as.numeric
  temperatur <- add_column(temperatur_tibble, temp)

#Bakgrundskarta  
  map <- rnaturalearth::ne_countries(country = "sweden", scale = "medium", returnclass = "sf")
  
#Illustrerar
  ggplot(map) +
    geom_sf() +
    geom_point(data = temperatur, aes(x = longitude, y = latitude, color = temp)) +
    scale_color_gradient(low = "yellow", high = "red") +
    theme_void() +
    labs(title = "Temperatur i Sverige 2022-08-11 12:00:00", color = "°C")
  }

plot_temp()
```

![](laboration4_files/figure-gfm/karta-1.png)<!-- -->

## Allsvenskans tabeller

-   **Funktion för att generera tabell**

``` r
read_allsvenskan <- function(year){
  #anpassar webadressen beroende på år och tar ut tabell
  allsvenskan <- paste0("https://sv.wikipedia.org/wiki/Mall:Po%C3%A4ngtabell_f%C3%B6r_Fotbollsallsvenskan_", year)
  page <- rvest::read_html(allsvenskan)
  elements <- rvest::html_elements(page, css = "table")
  table <- rvest::html_table(elements[[2]])
  
  #Om kolummen Lag har en fotnot tas den bort
  if("Lag[1]" %in% names(table)) {
    table <- rename(table, "Lag" = "Lag[1]")
  }
  
  #Tar bort noteringar 
  table %>%
    mutate(
      Lag = str_remove_all(Lag, " \\(U\\)"),
      Lag = str_remove_all(Lag, " \\(RM\\)"),
      Sasong = year) %>%
    select(Sasong, Nr, Lag, S, V, O, F, GM, IM, MS, P)
  } 

read_allsvenskan(2021)
```

    ## # A tibble: 16 × 11
    ##    Sasong    Nr Lag                S     V     O     F    GM    IM MS        P
    ##     <dbl> <int> <chr>          <int> <int> <int> <int> <int> <int> <chr> <int>
    ##  1   2021     1 Malmö FF          30    17     8     5    58    30 +28      59
    ##  2   2021     2 AIK               30    18     5     7    45    25 +20      59
    ##  3   2021     3 Djurgårdens IF    30    17     6     7    46    30 +16      57
    ##  4   2021     4 IF Elfsborg       30    17     4     9    51    35 +16      55
    ##  5   2021     5 Hammarby IF       30    15     8     7    54    41 +13      53
    ##  6   2021     6 Kalmar FF         30    13     8     9    41    39 +2       47
    ##  7   2021     7 IFK Norrköping    30    13     5    12    45    41 +4       44
    ##  8   2021     8 IFK Göteborg      30    11     8    11    42    39 +3       41
    ##  9   2021     9 Mjällby AIF       30     9    11    10    34    27 +7       38
    ## 10   2021    10 Varbergs BoIS     30     9    10    11    35    38 −3       37
    ## 11   2021    11 IK Sirius         30    10     7    13    39    53 −14      37
    ## 12   2021    12 BK Häcken         30     9     9    12    46    46 0        36
    ## 13   2021    13 Degerfors IF      30    10     4    16    34    51 −17      34
    ## 14   2021    14 Halmstads BK      30     6    14    10    21    26 −5       32
    ## 15   2021    15 Örebro SK         30     4     6    20    23    58 −35      18
    ## 16   2021    16 Östersunds FK     30     3     5    22    24    59 −35      14

-   **Illustration över målskillnad**

``` r
#Kombinerar tabeller
map_df(seq(2008,2021), read_allsvenskan) %>%
#Grupperar och räknar ut målskillnad, illustrerar sedan
  group_by(Sasong, Lag) %>%
  mutate(Malskillnad = (GM - IM) / S) %>%
  ggplot(aes(Sasong, Malskillnad)) +
  geom_line() +
  geom_point(shape = 21, colour = "black", fill = "white", size = 1) +
  facet_wrap(~Lag) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(breaks = seq(2009, 2021, by = 2)) +
  labs(
    title = "Genomsnittliga målskillnad per match och säsong",
    caption = "Data från Wikipedia",
    x = "Säsong",
    y = "Målskillnad"
    )
```

![](laboration4_files/figure-gfm/målskillnad-1.png)<!-- -->

-   **Tabellplaceringar**

``` r
#Tar fram placeringar för de olika åren
allsvenskan_summering <- map_df(seq(2008,2021), read_allsvenskan)

allsvenskan_summering %>%
  ggplot(aes(Sasong, Nr)) +
  geom_line() +
#Ger all data vita cirklar i plot
  geom_point(shape = 21, colour = "black", fill = "white", size = 1) + 
#Filtrerar ut varje års vinnare, ger de svarta cirklar i plot
  geom_point(data = allsvenskan_summering %>% filter(Nr == "1"), color = "black", size = 1) +
  facet_wrap(~Lag) +
  theme_bw() +
  labs(
    title = "Tabellplaceringar i Allsvenskan säsongerna 2008-2021",
    subtitle = "Svenska mästare markerad med ifylld cirkel",
    caption = "Data från Wikipedia",
    x = NULL,
    y= NULL
  ) +
  scale_y_reverse() +
  scale_x_continuous(breaks = seq(2009, 2021, by = 2)) +
  theme(axis.text.x = element_text(angle = 90))
```

![](laboration4_files/figure-gfm/tabellplaceringar-1.png)<!-- -->
