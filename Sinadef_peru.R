# 01 ----------------------------
# Cargar librerias
library(vroom)
library(ggplot2)
library(lubridate)
library(tidyverse)

# 02 ----------------------------
# Cargar informacion

col_spec <- cols(
  .default = col_character(),
  EDAD = col_double(),
  MES = col_number()
)

sinadef_1 <- vroom("fallecidos_sinadef.csv", 
                   delim = ";", trim_ws = TRUE, 
                   skip = 2, na = c("", "SIN REGISTRO", "NA"),
                   col_types = col_spec,
                   col_select = 1:31) %>% 
  janitor::clean_names()
sinadef_1 %>% colnames()

# 03 ---------------------------------
# procesar informacion 

sinadef_1 <- sinadef_1 %>%
  separate(
    col = cod_number_ubigeo_domicilio,
    into = c("cod", "number", "dept", "prov", "dist", "domicilio"),
    sep = "-",
    convert = FALSE)

sinadef_1 <- sinadef_1 %>% 
  mutate(time=lubridate::parse_date_time(fecha,c("ymd")))

sinadef_1 %>% str()

# 04 ----------------------------------
# Datos descriptivos (gráficos)

sinadef_1 %>%
  filter(time>"2019-12-31") %>%
  filter(time<"2021-03-13") %>% 
  count(time) %>% 
  ggplot(aes(x=time, y=n, fill=n))+
  geom_line()+
  theme_classic()+
  labs(y="N fallecidos",
       x="",
       title="Exceso de Fallecidos")
