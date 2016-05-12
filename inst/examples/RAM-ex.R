
library("readr")
library("ggplot2")
library("dplyr")

values     <- read_csv("http://berkeley.carlboettiger.info/espm-88b/fish/data/values.csv")
assessment <- read_csv("http://berkeley.carlboettiger.info/espm-88b/fish/data/assessment.csv")
stock      <- read_csv("http://berkeley.carlboettiger.info/espm-88b/fish/data/stock.csv")
units      <- read_csv("http://berkeley.carlboettiger.info/espm-88b/fish/data/units.csv")
area       <- read_csv("http://berkeley.carlboettiger.info/espm-88b/fish/data/area.csv")

lmestock   <- read_csv("http://berkeley.carlboettiger.info/espm-88b/fish/data/lmestock.csv")
lmerefs    <- read_csv("http://berkeley.carlboettiger.info/espm-88b/fish/data/lmerefs.csv")

tbl <-
  values %>%
  left_join(assessment) %>%
  left_join(stock) %>%
  left_join(units) %>%
  left_join(area) %>%
  left_join(lmestock) %>%
  left_join(lmerefs) %>%
  select(scientificname, commonname, tsyear, r, ssb, total, catch_landings, r_unit, ssb_unit, total_unit, catch_landings_unit, country, lme_number, lme_name)


# Sum over all assessments of a given species in a given year that are harvested
tbl %>%
  filter(catch_landings_unit == 'MT') %>%
  filter(tsyear >= 1950) %>%
  filter(tsyear <= 2006) %>%
  group_by(tsyear) %>%
  summarise(catch_landings = sum(catch_landings, na.rm=TRUE)) %>%
  ggplot() + geom_line(aes(tsyear, catch_landings))



## Newfoundland Atlantic Cod Catch
tbl %>%
  group_by(commonname, lme_name, tsyear) %>%
  summarise(catch_landings = sum(catch_landings, na.rm = TRUE)) %>%
  filter(lme_name == "Newfoundland-Labrador Shelf") %>%
  filter(commonname == "Atlantic cod") %>%
  ggplot(aes(tsyear, catch_landings)) + geom_line()


tbl %>%
  filter(total_unit == catch_landings_unit) %>%
  mutate(escapement = total - catch_landings) ->
  tbl


## Newfoundland Atlantic Cod escapement
tbl %>%
  group_by(commonname, lme_name, tsyear) %>%
  summarise(escapement = sum(escapement, na.rm = TRUE), total = sum(total, na.rm = TRUE)) %>%
  filter(lme_name == "Newfoundland-Labrador Shelf") %>%
  filter(commonname == "Atlantic cod") %>%
  ggplot(aes(total, escapement)) + geom_line()


stock_country_lme <-
  stock %>%
  left_join(area) %>%
  left_join(lmestock) %>%
  left_join(lmerefs) %>%
  select(scientificname, commonname, country, lme_name, areatype)

usa_lmes <-
  stock_country_lme %>%
  filter(country == "USA") %>%
  select(lme_name) %>%
  unique()

for(lme in usa_lmes){
tbl %>%
  group_by(commonname, lme_name, tsyear) %>%
  summarise(escapement = sum(escapement, na.rm = TRUE)) %>%
  filter(lme_name %in% lme) %>%
  ggplot(aes(tsyear, escapement, col = commonname)) + geom_line() + facet_wrap(~lme_name)
}


