# Дроздова А.А. постройте картосхему средних диаметров стволов деревьев родов Липа и Пихта если их высота более 5м.

# Установим необходимые пакеты (если не установлены)
# install.packages("sf")
# install.packages("ggplot2")
# install.packages("readr")
# install.packages("dplyr")
# install.packages("tidyr")

library(sf)
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)

# Очистим полностью память 
rm(list=ls())

# Проверка рабочей директории
getwd()
# Рабочая директория
setwd ("D:/AR/lika/Zad2")

#считаем данные в переменные
greendb= read.csv("greendb.csv"); greendb
map=sf :: read_sf("moscow.geojson")

# график с заливкой
ggplot(map)+geom_sf(aes(fill=NAME))+theme(legend.position="none")

# переменные
spec=greendb$species_ru
genus=stringr::str_split(spec, pattern=" ",simplify=T)[,1]
data=greendb%>%mutate(Genus=genus)

# Фильтрация по диаметру и группировка по региону и роду
sr=data %>% group_by(adm_region,Genus)%>% filter(height_m > 5)%>%
  summarise(s_r=mean(d_trunk_m), na.rm = T)%>% 
  filter(Genus %in% c("Липа","Пихта"))
sr=pivot_wider(sr,names_from = Genus, values_from = s_r)

# Объединяем данные с картой
map=map %>% mutate(adm_region=NAME)
map=left_join(map, sr, by="adm_region")

# Построение картосхемы для Липы
ggplot(map)+
  geom_sf(aes(fill=`Липа`))+theme()

# Построение картосхемы для Пихта
ggplot(map)+
  geom_sf(aes(fill=`Пихта`))+theme()



