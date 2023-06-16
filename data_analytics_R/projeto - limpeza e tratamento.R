####################################################
####             TRATAMENTO DE DADOS           #####
####################################################

setwd('C:/Users/ISAAC/Documents/Analise de Dados com R/Projeto 1')

if(!require(dplyr)) install.packages("dplyr")
library("dplyr")


covid_sp <- read.csv("dados-covid-sp-master/data/dados_covid_sp.csv", sep=";", encoding = "UTF-8")
View(covid_sp)

## Alterando nome de colunas
covid_sp <- rename(covid_sp, municipio = nome_munic, data=datahora, rotulo_mapa = map_leg, codigo_mapa = map_leg_s) 
View(covid_sp)


covid_sp$cod_ra = NULL
covid_sp$rotulo_mapa = NULL

covid_sp = subset(covid_sp, select = -c(codigo_ibge, cod_drs, nome_ra, nome_drs, codigo_mapa, latitude, longitude))


## Filtrando o dataframe para todos os registro cujo o municipio não seja 'Ignorado'

covid_sp = covid_sp%>%filter(municipio != "Ignorado")

################################################
###          Tratando valores missing        ###
################################################

# Verificando

# NAN = Not A Number
# NA  = Valores Ausentes

sapply(covid_sp, function(x) sum(is.na(x)))
sapply(covid_sp, function(x) sum(is.nan(x)))

library(tidyr)

## trocando valores NA
covid_sp2 <- covid_sp%>%mutate_all(replace_na, 54)
View(covid_sp2)

## OU

covid_sp2 <- replace(x=covid_sp, list = is.na(covid_sp), values = 54)

covid_sp2$semana_epidem[covid_sp2$semana_epidem == 54] <- 2021

covid_sp2$semana_epidem[covid_sp2$data >= '2021-01-01' & covid_sp2$data <= '2021-01-07'] <- 54

covid_sp2$semana_epidem[covid_sp2$data >= '2021-01-08' & covid_sp2$data <= '2021-01-14'] <- 55


str(covid_sp2)
glimpse(covid_sp2)


covid_sp2$semana_epidem = as.integer(covid_sp2$semana_epidem)
glimpse(covid_sp2)

covid_sp2$data = as.Date(covid_sp2$data, format = '%Y-%m-%d')
glimpse(covid_sp2)

covid_sp2["Idoso(%)"] = 100*covid_sp2$pop_60/covid_sp2$pop


## Exportando dataset

write.table(covid_sp2, file = "covid_sp2.csv", sep= ",")
