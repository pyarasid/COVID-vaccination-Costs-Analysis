library(tidyverse)
library(readxl)
library(rnaturalearth)
library(sf)
library(leaflet)
library(scales)
library(xlsx)

#loading the data
covid_price <- read_excel("COVID 19 vaccine price tag_LIC_V8_01292021.xlsx", sheet = "Results")

View(covid_price)
colnames(covid_price)

#creating three variables that will be used in wrangling later
per_hrd <- .70
per_hrd_cvx <- .22
per_hrd_blt <- .55
num_does <- 2
VCstDse_cvx <- 3
VCstDse_blt <- 13.6

#cleaning and wrangling the data
covid_price_clean <- covid_price %>% 
  select(Country,`COVAX AMC eligible?`,`Baseline spending`, `Baseline spending (2020 USD)`,`Year`,`Total population`,`Adult population (>18)`,
         `Under 18`,`High risk population %`,`# of health professionals`,`Median delivery cost per dose`,
         `Total cost per dose_COVAX  (vaccine cost + delivery cost)`,
         `Total cost per dose_bilateral deal  (vaccine cost + delivery cost)`,
         `Total cost to vaccinate all health professionals_Bilateral (US$ million)`,
         `Total cost for high risk population_Bilateral (US$ million)`, 
         `Total cost  to reach herd immunity (US$ million)`) %>% 
  cbind(per_hrd, VCstDse_cvx, VCstDse_blt, num_does, per_hrd_blt, per_hrd_cvx) %>%
  rename(spend2020=`Baseline spending (2020 USD)`,
         spendImu=`Baseline spending`,
         Cvx_elgiblty= `COVAX AMC eligible?`, 
         tot_pop=`Total population`,
         adult_pop= `Adult population (>18)`,
         under_18pop=`Under 18`,
         HghRskPop=`High risk population %`,
         NumHthPrf=`# of health professionals`,
         DelCstDse=`Median delivery cost per dose`,
         TtCstDseCvx= `Total cost per dose_COVAX  (vaccine cost + delivery cost)`,
         TtCstDseBlt=`Total cost per dose_bilateral deal  (vaccine cost + delivery cost)`, 
         VCstHthPrfB=`Total cost to vaccinate all health professionals_Bilateral (US$ million)`,
         VCstPpRskB=`Total cost for high risk population_Bilateral (US$ million)`,
         VCst75PpBC=`Total cost  to reach herd immunity (US$ million)`) %>% 
  mutate(TtCstDseCvx= (DelCstDse+VCstDse_cvx),
         TtCstDseBlt=(DelCstDse+VCstDse_blt),
         VCstHthPrfB=(NumHthPrf*TtCstDseBlt*num_does),
         VCstPpRskB=(adult_pop*HghRskPop*TtCstDseBlt*num_does),
         VCst75PpBC=ifelse(Cvx_elgiblty=="Yes", (adult_pop*TtCstDseCvx*num_does*per_hrd_cvx)+(adult_pop*TtCstDseBlt*num_does*per_hrd_blt),
                           (adult_pop*TtCstDseBlt*num_does*per_hrd_cvx)+(adult_pop*TtCstDseBlt*num_does*per_hrd_blt)))

#rounding the digits to zero decimal places
covid_price_clean <- covid_price_clean %>% 
  mutate_at(vars(VCstHthPrfB, VCstPpRskB,VCst75PpBC), funs(round(., 0)))

#Importing excel with countries longitude and latitude
world_country <- read_excel("World_country.xlsx")

#removing the abbreviation of country column
world_country <- world_country %>% 
  select(-country) %>% 
  rename(country=name) %>% 
  mutate(country=recode(country, `Congo [DRC]`="Democratic Republic of the Congo", `Myanmar [Burma]`="Myanmar")) #recode: renaming an observation in the data frame

#Adding south sudan in the world_country dataframe
world_country <- rbind(world_country, c(6.8770, 31.3070, "South Sudan"))

#checking which country name from covid_price_clean data does not match with the world_country data
setdiff(covid_price_clean$Country, world_country$country)

#renaming the covid_price_clean df names to match world country names 
covid_price_clean <- covid_price_clean %>% 
  mutate(Country=recode(Country,`Micronesia (Fed. States of)`= "Micronesia",
                        `Venezuela (Bolivarian Republic of)`= "Venezuela")) 

#renaming the world country names to match the covid_price_clean df names
world_country <- world_country %>% 
  mutate(country=recode(country, 
                        `North Korea`="Dem. People's Republic of Korea",
                        `Syria`="Syrian Arab Republic",
                        `Tanzania`= "United Republic of Tanzania",
                        `Laos`="Lao People's Democratic Republic" ,
                        `Congo [Republic]`="Congo",
                        `Gaza Strip`="West Bank and Gaza",
                        `Moldova`= "Republic of Moldova",
                        `Swaziland`= "Eswatini",
                        `Cape Verde`= "Cabo Verde",
                        `São Tomé and Príncipe`= "Sao Tome and Principe",
                        `Russia`= "Russian Federation",
                        `Macedonia [FYROM]`= "North Macedonia")) 

#converting the charter values of longitude and latitude as numeric
world_country$longitude <- as.numeric(world_country$longitude)
world_country$latitude <- as.numeric(world_country$latitude)

#adding new empty columns in the covid_price_clean dataframe
covid_price_clean[,"lat"] <- as.numeric(NA)
covid_price_clean[, "long"] <- as.numeric(NA)

#filling the longitude and latitude column in covid_price_clean df with the values from world_country df
for (i in seq_along(covid_price_clean$Country)){
  if (any(str_detect(world_country$country, covid_price_clean$Country[i])==TRUE)){
    covid_price_clean$lat[i] <- as.numeric(world_country[which((world_country$country== covid_price_clean$Country[i])==TRUE),1])
    covid_price_clean$long[i] <- as.numeric(world_country[which((world_country$country==covid_price_clean$Country[i])==TRUE),2])
  }
}

#converting the ne_coutries() polygons from sp class to sf class
world_polygon <- st_as_sf(ne_countries())
#View(world_polygon)
#class(world_polygon)

#checking which names in covid_price_clean df and not in world_polygon
setdiff(covid_price_clean$Country, world_polygon$name_long)

#Following are the island countries which are not present in the sf file that i am using-----
#comoros #Cabo Verde #Sao Tome and Principe #Kiribati #Micronesia #Maldives #Samoa #Saint Lucia
#Grenada #Saint Vincent and the Grenadines #Tonga #Dominica #Marshall Islands  #Tuvalu
missing_countries <- c("Comoros","Cabo Verde", "Sao Tome and Principe", "Kiribati", "Micronesia", 
                       "Maldives", "Samoa", "Saint Lucia", "Grenada", "Saint Vincent and the Grenadines", "Tonga", 
                       "Dominica", "Marshall Islands", "Tuvalu")

#removing the countries from covid_price_clean df which are not present in world_polygon
covid_price_clean <- covid_price_clean %>% 
  filter(!Country %in% missing_countries) 

#renaming covid_price_clean df names to match the world_polygon df
covid_price_clean <- covid_price_clean %>% 
  mutate(Country=recode(Country, 
                        `Dem. People's Republic of Korea`= "Dem. Rep. Korea",
                        `Gambia`="The Gambia",
                        `Syrian Arab Republic`="Syria",
                        `United Republic of Tanzania`="Tanzania",
                        `Lao People's Democratic Republic`="Lao PDR",
                        `Congo`="Republic of Congo",
                        `West Bank and Gaza`="Palestine",
                        `Republic of Moldova`="Moldova",
                        `Eswatini`="Swaziland",
                        `North Macedonia`="Macedonia"))

#Joining the covid_price_clean dataframe with the world_polygon and creating a new sf object
world_polygon_new <- left_join(world_polygon, covid_price_clean, by= c(name_long="Country"))
View(world_polygon_new)

#Removing NA from long i.e. keeping only countries which are in covid_price_clean dataframe
world_polygon_new <- world_polygon_new %>% 
  filter(!is.na(long)) %>% 
  select(name_long, Cvx_elgiblty,spendImu, spend2020, Year, tot_pop, adult_pop, under_18pop, HghRskPop,  NumHthPrf,
         DelCstDse, TtCstDseCvx, TtCstDseBlt, VCstHthPrfB, VCstPpRskB, VCst75PpBC, per_hrd, VCstDse_cvx,
         VCstDse_blt, num_does, per_hrd_blt, per_hrd_cvx, lat, long, geometry) 

#Writing the world_polygon_new onto disk=========== final
st_write(world_polygon_new,"world_polygon_app.shp")

#Creating a clean dataframe of covid prices and then write it out as excel============
`% for herd immunity` <- 70
`% to be covered by COVAX` <- 22
`remaining % to reach herd immunity` <- 55
`Number of doses needed` <- 2
`Vaccine price per dose (COVAX, US$)` <- 3
`Vaccine price per dose (Bilateral, US$)` <- 13.6

baseline_data <- read_excel("COVID 19 vaccine price tag_LIC_V8_01292021.xlsx", sheet = "Results") %>% 
  select(Country,`COVAX AMC eligible?`, `Baseline spending`,`Year`,`Total population`,`Adult population (>18)`,
         `Under 18`,`High risk population %`,`# of health professionals`,`Median delivery cost per dose`,
         `Total cost per dose_COVAX  (vaccine cost + delivery cost)`,
         `Total cost per dose_bilateral deal  (vaccine cost + delivery cost)`,
         `Total cost to vaccinate all health professionals_Bilateral (US$ million)`,
         `Total cost for high risk population_Bilateral (US$ million)`, 
         `Total cost  to reach herd immunity (US$ million)`) %>%
  filter(!Country %in% c("Comoros","Cabo Verde", "Sao Tome and Principe", "Kiribati", "Micronesia", 
                         "Maldives", "Samoa", "Saint Lucia", "Grenada", "Saint Vincent and the Grenadines", "Tonga", 
                         "Dominica", "Marshall Islands", "Tuvalu", "West Bank and Gaza", "Eswatini")) %>% 
  cbind(`% for herd immunity`, `% to be covered by COVAX`, `remaining % to reach herd immunity`,
        `Number of doses needed`, `Vaccine price per dose (COVAX, US$)`, `Vaccine price per dose (Bilateral, US$)`) %>%
  relocate(c(`% for herd immunity`, `% to be covered by COVAX`, `remaining % to reach herd immunity`,
             `Number of doses needed`, `Vaccine price per dose (COVAX, US$)`, `Vaccine price per dose (Bilateral, US$)`) , 
           .before= `Median delivery cost per dose`) %>% 
  rename("Baseline spending on Immunization (2020 US$)"= `Baseline spending`,
         `Median delivery cost per dose (US$)`=`Median delivery cost per dose`,
         `Population % to be covered by COVAX`=`% to be covered by COVAX`,
         `Population % covered by bilateral deal`=`remaining % to reach herd immunity`,
         `Total cost to vaccinate all health professionals (US$)`=`Total cost to vaccinate all health professionals_Bilateral (US$ million)`,
         `Total cost to vaccinate population at risk (US$)`=`Total cost for high risk population_Bilateral (US$ million)`,
         `Total cost  to reach herd immunity (US$)`= `Total cost  to reach herd immunity (US$ million)`,
         `Total cost per dose_COVAX  (vaccine cost + delivery cost) (US$)`=`Total cost per dose_COVAX  (vaccine cost + delivery cost)`,
         `Total cost per dose_bilateral deal  (vaccine cost + delivery cost) (US$)`=`Total cost per dose_bilateral deal  (vaccine cost + delivery cost)`) %>%
  mutate(`total_pop`=`Total population`,
         `num_health_prof`=`# of health professionals`) %>% 
  mutate(`Baseline spending on Immunization (2020 US$)`= comma_format(accuracy=1.0)(`Baseline spending on Immunization (2020 US$)`),
         `Total cost to vaccinate all health professionals (US$)`= comma_format()(`num_health_prof`*`Total cost per dose_bilateral deal  (vaccine cost + delivery cost) (US$)`*
                                                                                    `Number of doses needed`),
         `Total cost to vaccinate population at risk (US$)`= comma_format()(`Adult population (>18)`*`High risk population %`*
                                                                              `Total cost per dose_bilateral deal  (vaccine cost + delivery cost) (US$)`*
                                                                              `Number of doses needed`),
         `Total cost  to reach herd immunity (US$)`= comma_format()(ifelse(`COVAX AMC eligible?`=="Yes",
                                                                           (`Adult population (>18)`*`Total cost per dose_COVAX  (vaccine cost + delivery cost) (US$)`*
                                                                              `Number of doses needed`*`Population % to be covered by COVAX`/100)+
                                                                             (`Adult population (>18)`*`Total cost per dose_bilateral deal  (vaccine cost + delivery cost) (US$)`*
                                                                                `Number of doses needed`*`Population % covered by bilateral deal`/100),
                                                                           (`Adult population (>18)`*`Total cost per dose_bilateral deal  (vaccine cost + delivery cost) (US$)`*
                                                                              `Number of doses needed`*`Population % to be covered by COVAX`/100)+
                                                                             (`Adult population (>18)`*`Total cost per dose_bilateral deal  (vaccine cost + delivery cost) (US$)`*
                                                                                `Number of doses needed`*`Population % covered by bilateral deal`/100))),
         `Total population`=comma_format(accuracy = 1.0)(`Total population`),
         `Adult population (>18)`=comma_format(accuracy = 1.0)(`Adult population (>18)`),
         `Under 18`= comma_format(accuracy = 1.0)(`Under 18`),
         `# of health professionals`=comma_format(accuracy=1.0)(`# of health professionals`),
         `Total cost per dose_COVAX  (vaccine cost + delivery cost) (US$)`= comma_format(accuracy=.01)(`Median delivery cost per dose (US$)`+`Vaccine price per dose (COVAX, US$)`),
         `Total cost per dose_bilateral deal  (vaccine cost + delivery cost) (US$)`= comma_format(accuracy=.01)(`Median delivery cost per dose (US$)`+
                                                                                                                  `Vaccine price per dose (Bilateral, US$)`),
         `Median delivery cost per dose (US$)`= comma_format(accuracy=.01)(`Median delivery cost per dose (US$)`))%>% 
  select(-c(`total_pop`, `num_health_prof`)) 

write.xlsx(baseline_data, file = "baseline_data.xlsx")


