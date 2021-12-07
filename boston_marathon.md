Boston Marathon
================

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.4     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(viridis)
```

    ## Loading required package: viridisLite

``` r
library(ggridges)
library(patchwork)
```

# DATA CLEANING (HUGO)

``` r
years_1 <- c(1900:2012, 2014)
years_2 <- c(2015:2019)

importing_data = function(x){
 
  if(str_detect(x, str_c(years_1, collapse = "|"))) {
  read_csv(x, na = c("NULL", "", "0"), col_types = "cicccciiiicc") 
  } 
  
  else if(str_detect(x, str_c(years_2, collapse = "|"))){
    read_csv(x, na = c("NULL", "", "0"), col_types = "cccicccccccccccccccccciiiiccc")
  }
}

boston_df <- 
  tibble(list.files("data", full.names = TRUE)) %>% 
  setNames("file_name") %>% 
  mutate(data = map(file_name, importing_data)) %>% 
  unnest(data) %>% 
  mutate(year = readr::parse_number(file_name),
         city = coalesce(city, residence),
         display_name = str_replace_all(display_name, "[^a-zA-Z0-9]", " ")) %>% 
  mutate(country_residence = replace(country_residence, country_residence == "AHO", "Netherland Antilles")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "ALB", "Albania")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "ALG", "Algeria")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "AND", "Andorra")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "ARG", "Argentina")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "Argenti", "Argentina")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "AUS", "Australia")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "Austral", "Australia")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "AUT", "Austria")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "BAH", "Bahamas")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "BAR", "Barbados")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "Barbado", "Barbados")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "BDI", "Burundi")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "BLR", "Belarus")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "BEL", "Belgium")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "BER", "Bermuda")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "BRA", "Brazil")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "BRN", "Brunei")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "BUL", "Bulgaria")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "CAN", "Canada")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "CAY", "Cayman")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "CHI", "Chile")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "CHN", "China")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "COL", "Colombia")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "Colombi", "Colombia")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "CRC", "Costa Rica")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "Costa R", "Costa Rica")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "CRO", "Croatia")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "CYP", "Cyprus")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "CZE", "Czech Republic")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "Czech R", "Czech Republic")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "DEN", "Denmark")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "DOM", "Dominican Republic")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "Dominic", "Dominican Republic")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "ECU", "Ecuador")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "EGY", "Egypt")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "El Salv", "El Salvador")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "ESA", "El Salvador")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "ESP", "Spain")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "EST", "Estonia")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "ETH", "Ethiopia")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "Ethiopi", "Ethiopia")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "Faroe I", "Faroe Islands")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "FIN", "Finland")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "FLK", "Falkland Islands")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "FRA", "France")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "GBR", "England")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "GER", "Germany")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "GRE", "Greece")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "GRN", "Greenland")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "GUA", "Guatemala")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "Guatema", "Guatemala")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "HKG", "Hong Kong")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "Hong Ko", "Hong Kong")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "HON", "Honduras")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "Hondura", "Honduras")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "HUN", "Hungary")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "INA", "Indonesia")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "Indones", "Indonesia")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "IND", "India")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "IRL", "Ireland")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "ISL", "Iceland")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "ISR", "Israel")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "ITA", "Italy")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "JAM", "Jamaica")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "JPN", "Japan")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "JOR", "Jordan")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "KEN", "Kenya")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "KOR", "Korea")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "Korea,", "Korea")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "KSA", "Saudi Arabia")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "KUW", "Kuwait")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "LAT", "Latvia")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "LIE", "Liechtenstein")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "Liechte", "Liechtenstein")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "Lithuan", "Lithuania")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "LTU", "Lithuania")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "LUX", "Luxembourg")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "Luxembo", "Luxembourg")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "Macao S", "Macao")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "Macedon", "Macedonia")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "Malaysi", "Malaysia")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "MAR", "Martinique")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "Martini", "Martinique")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "MAS", "Malaysia")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "MEX", "Mexico")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "MGL", "Mongolia")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "MLT", "Malta")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "NCA", "Nicaragua")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "NED", "Netherlands")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "Netherl", "Netherlands")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "New Zea", "New Zealand")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "NGR", "Nigeria")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "NOR", "Norway")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "NZL", "New Zealand")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "OMA", "Oman")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "PAK", "Pakistan")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "Palesti", "Palestine")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "PAN", "Panama")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "PAR", "Paraguay")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "Paragua", "Paraguay")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "PER", "Peru")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "PHI", "Philippines")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "Philipp", "Philippines")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "POL", "Poland")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "POR", "Portugal")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "Portuga", "Portugal")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "Puerto", "Puerto Rico")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "QAT", "Qatar")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "ROU", "Romania")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "Saudi A", "Saudi Arabia")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "SIN", "Singapore")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "Singapo", "Singapore")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "SLO", "Slovenia")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "Slovaki", "Slovakia")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "Sloveni", "Slovenia")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "SMR", "San Marino")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "South A", "South Africa")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "SRB", "Serbia")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "Sri Lan", "Sri Lanka")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "SUI", "Switzerland")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "SVK", "Slovakia")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "SWE", "Sweden")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "Switzer", "Switzerland")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "TCA", "Turks and Caicos")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "THA", "Thailand")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "Thailan", "Thailand")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "TPE", "Taipei")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "TRI", "Trinidad")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "Trinida", "Trinidad")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "TUR", "Turkey")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "TWN", "Taiwan")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "UAE", "United Arab Emirates")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "UGA", "Uganda")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "UKR", "Ukraine")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "United", "United States")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "URU", "Uruguay")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "USA", "United States")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "VEN", "Venezuela")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "Venezue", "Venezuela")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "VGB", "Virgin Islands")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "VIE", "Vietnam")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "ZIM", "Zimbabwe")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "RSA", "South Africa")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "RUS", "Russia")) %>%
  mutate(country_residence = replace(country_residence, country_residence == "Russian", "Russia")) %>%
  filter(!is.na(display_name)) %>% 
  select(-file_name, -residence, -first_name, -last_name)
```

# SUHANI

# TAMARA

# CLAIRE

# SYDNEY
