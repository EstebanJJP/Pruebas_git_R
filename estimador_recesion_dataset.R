library(eurostat)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(stringr)
library(purrr)
library(ggplot2)

#Importamos los datos economicos que vamos a usar mediante el paquete eurostat que nos permite acceder directamente
#a los datos necesarios. Para ello cargamos los dataset filtrando por España
gdp<-get_eurostat("namq_10_gdp", time_format = "date") %>% filter(geo=="ES") %>% 
  filter(geo=="ES")

infl <- get_eurostat("prc_hicp_midx", time_format = "date") %>%
  filter(geo == "ES")

unemp <- get_eurostat("une_rt_m", time_format = "date") %>%
  filter(geo == "ES")

ipi <- get_eurostat("sts_inpr_m", time_format = "date") %>%
  filter(geo == "ES") %>%
  filter(!is.na(values))

ggplot(data=gpd, aes(x=TIME_PERIOD, y=log(values))) + geom_line(color="blue") + labs(title = "GPD over time in Spain")

#Ahora vamos a quedarnos solo con los valores y la variable tiempo para poder unirlas

gdp2 <- gdp %>%
  select(TIME_PERIOD, values) %>%
  group_by(TIME_PERIOD) %>%
  summarise(gdp = mean(values), na.rm=TRUE)

infl2 <- infl %>%
  select(TIME_PERIOD, values) %>%
  group_by(TIME_PERIOD) %>%
  summarise(inflation = mean(values), na.rm=TRUE)

unemp2 <- unemp %>%
  select(TIME_PERIOD, values) %>%
  group_by(TIME_PERIOD) %>%
  summarise(unemployment = mean(values), na.rm=TRUE)

ipi2 <- ipi %>%
  select(TIME_PERIOD, values) %>%
  group_by(TIME_PERIOD) %>%
  summarise(industrial_production = mean(values), na.rm=TRUE)
ipi2 <- ipi2 %>% filter(TIME_PERIOD>"1996-01-01")


#Ahora hacemos un full join para por la columna tiempo

dataset <- gdp2 %>%
  full_join(infl2, by = "TIME_PERIOD") %>%
  full_join(unemp2, by = "TIME_PERIOD") %>%
  full_join(ipi2, by = "TIME_PERIOD") %>%
  arrange(TIME_PERIOD)

dataset <- dataset %>% select(-na.rm.x, -na.rm.y, -na.rm.x.x, -na.rm.y.y) 

dataset <- dataset %>%
  drop_na()
        
#Con esta función generamos nuestra variable objetivo,
#con lead miramos el siguiente de la variable gdp y luego restamos el gdp actual
#Y luego un if else para ver si el crecimiento ha sido positivo o negativo y lo 
#etiquetamos con 0 o 1.
dataset <- dataset %>%
  arrange(TIME_PERIOD) %>%
  mutate(gdp_future = (lead(gdp, 1)-gdp),
         target = ifelse(gdp_future < 0, 1, 0))
#Como estamos calculando con el futuro se va a generar un na en la ultima observación
#la eliminamos para nuestro dataset final
dataset_model <- dataset %>% drop_na()

write_csv(dataset_model, "prueba_calculo_recesion.csv")


