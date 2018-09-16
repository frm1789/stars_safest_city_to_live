library(readr)
library(stringr)
library(ggplot2)
library(dplyr)
library(base)

## 1. Data
url_mete <- 'https://raw.githubusercontent.com/frm1789/stars_safest_city_to_live/master/meteorite-landings.csv'
url_fire <- 'https://raw.githubusercontent.com/frm1789/stars_safest_city_to_live/master/cneos_fireball_data.csv'

df_fire <- read_csv(url(url_fire))
df_mete <- read_csv(url(url_mete))

## 1.1 Modify the name of the columns to lat / long 
colnames(df_mete)[c(8,9)] <- c("lat", "long") # df_meteorite 
colnames(df_fire)[c(2,3)] <- c("lat", "long") # df_fireball

## 1.2 Data conversion for df_fireballs
df_fire <- convert_latlong(df_fire[ , 2],df_fire[ , 3])

## 2. Por cada meteorito / fireball, determino el pais que le corresponde utilizando el API de Google
## Call function get_countries for df_mete
countries <- get_countries(df)

## Call function get_countries for df_fire
df_fire <- head(df_fire,3)  
countries_fire <- get_countries(df_fire)

## 2.1 Reorganize results
countries_fire[countries_fire == ""] = NA  
countries_fire <- countries_fire[!is.na(countries_fire)]

countries[countries == ""] = NA  
countries <- countries[(!is.na(countries)) ]


## 3. Sumarizar los resultados
df_countries <- as.data.frame(table(countries))
colnames(df_countries)[c(1,2)] <- c("countries", "values")

df_countries_fire <- as.data.frame(table(countries_fire))
colnames(df_countries_fire)[c(1,2)] <- c("countries", "values")

df_countries_fire <- dplyr::filter(df_countries_fire, 
                                   !countries == "")

df_finale <- merge(df_countries_fire, df_countries, by="countries", all=TRUE) 
df_finale[is.na(df_finale)] <- 0
df_finale$values <- df_finale$values.x + df_finale$values.y
df_finale <- df_finale[,-(2:3)]

## 4. Reorganize data
## 4.1 Delete data from countries with internal problems.
c_countries_problems = c("Afghanistan", "Azerbaijan", 
                         "Bosnia and Herzegovina", "Central African Republic", 
                         "Chad", "Ghana","Jordan","Laos","Lebanon", "Papua New Guinea", "Rwanda",
                         "Somalia", "Swaziland", "Syria", "Zambia", "Cambodia", "Iraq","Iran", "Lesotho",
                         "Yemen", "Zimbabwe","Myanmar (Burma)")

df_finale <- dplyr::filter(df_finale, 
                           !countries %in% c_countries_problems)


df_finale <- df_finale %>%
  group_by(values = df_finale$values) %>%
  summarise(countries = paste0(countries, collapse = ", "))



## 5. Visualization
## Colors from Viridis Palette

library(scales)
n_df_finale <- as.integer(nrow(df_finale))
v_colores <-  viridis(n_df_finale, option = "A")

df_finale$values[df_finale$countries == "Antarctica"] <- 500

#Order by 
df_finale <-dplyr::arrange(df_finale, desc(values))# sort
df_finale$countries <- factor(df_finale$countries, levels = df_finale$countries)  # convert to factor to retain sorted order in plot.

library(viridis)
require(cowplot)
require(grid)

ggplot(df_finale, aes(x=countries, y=values)) + 
  geom_bar(stat='identity', aes(fill=v_colores), width=.5)  +
  scale_fill_manual(values = v_colores,
                    guide=FALSE) +
  labs(subtitle="Paises mas seguros e inseguros segun tendencia historica", 
       title= "Donde vivirias para evitar meteoritos y fireballs",
       caption = "NASA Fireball & Meteorites datasets | by thinkingondata.com") + 
  coord_flip() +
  theme_minimal() +
  scale_y_continuous(position = "right") +
  theme(
    axis.text.x = element_text(size = 12, face = "bold")
  )
