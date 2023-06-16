#####################################################
####           Analise Exploratória              ####
#####################################################

library(dplyr)
library(tidyr)
if(!require(rstatix)) install.packages("rstatix")
library(rstatix)

setwd('C:/Users/ISAAC/Documents/Analise de Dados com R/Projeto 1')

covid_sp_analise = read.csv2("covid_sp2.csv", sep=',')
View(covid_sp_analise)

glimpse(covid_sp_analise)

covid_sp_analise$data = as.Date(covid_sp_analise$data, format='%Y-%m-%d')

covid_sp_analise$Idoso = as.numeric(covid_sp_analise$Idoso)

covid_sp_analise = select(covid_sp_analise, -c(17))

glimpse(covid_sp_analise)

covid_sp_analise = rename(covid_sp_analise, perc_idoso = Idoso)


# Filtando por cidade

covid_campinas = covid_sp_analise%>%filter(municipio == "Campinas")
covid_campinas["area"] = covid_campinas$area/100
covid_campinas["dens_demo"] = covid_campinas$pop/covid_campinas$area


summarise_at(covid_campinas, vars(obitos_novos, casos_novos), mean)

plot(covid_campinas$data,ylab="Casos", xlab="Data",covid_campinas$casos_mm7d, col="blue")
title(main="Média Móvel de casos")
plot(covid_campinas$data,ylab="Mortes", xlab="Data",covid_campinas$obitos_mm7d, col="red")
title(main="Média Móvel de óbitos")


moda = function(m){
  valor_unico = unique(m)
  valor_unico[which.max(tabulate(match(m, valor_unico)))]
}


covid_julho_campinas = covid_campinas%>%filter(mes==7)

hist(covid_julho_campinas$obitos_novos, col="blue")
hist(covid_julho_campinas$casos_novos, col="blue")

hist(covid_campinas$obitos_novos, col="blue")

min(covid_julho_campinas$obitos_novos)
max(covid_julho_campinas$obitos_novos)


boxplot(covid_julho_campinas$obitos_novos, horizontal = T)
boxplot(covid_julho_campinas$casos_novos, horizontal = T)

boxplot(covid_campinas$casos_novos, horizontal = T)
boxplot(covid_campinas$obitos_novos, horizontal = T)

covid_campinas%>%identify_outliers(casos_novos)
outliers = c(boxplot.stats(covid_campinas$casos_novos)$out)

covid_campinas_out = covid_campinas[-c(which(covid_campinas$casos_novos%in%outliers)),]

boxplot(covid_campinas_out$casos_novos, horizontal = T)


plot(covid_campinas$data,ylab="Casos", xlab="Data",covid_campinas$letalidade, col="blue")
title(main="Letalidade ao longo do tempo")
lines(y = covid_campinas$letalidade, x = covid_campinas$data, col = "red")



########### Testes de Normalidade ###########


if(!require(nortest)) install.packages("nortest")
library(nortest)


hist(covid_campinas$casos_novos, probability = T, col="red")
lines(density(covid_campinas$casos_novos), col="blue")


qqnorm(covid_campinas$casos_novos)
qqline(covid_campinas$casos_novos)


### Shapiro-Wilk
if(shapiro.test(covid_campinas$casos_novos)$p.value > 0.05) print("Distribuição é Normal") else print("Distribuição não é normal")


### Anderson-Darling
if(ad.test(covid_campinas$casos_novos)$p.value > 0.05) print("Distribuição é Normal") else print("Distribuição não é normal")


plot(covid_campinas$casos, covid_campinas$obitos)
cor(covid_campinas$casos, covid_campinas$obitos, method = "spearman")


regressao = lm(formula = obitos~casos, data=covid_campinas)
regressao$coefficients
summary(regressao)


library(ggplot2)
library(ggpubr)
library(corrplot)


ggplot(data = covid_campinas, mapping=aes(x = casos, y = obitos)) +
  geom_point() +
  geom_smooth(method = "lm", col="red")
  stat_regline_equation(aes(label = paste(..eq.label.., ..adj.rr.label..,
                            sep = "*plain(\",  \")~~")), label.x = 15000,
                            label.y=1800)+
  theme_gray()
  
  
matriz_corr = cor(covid_campinas[5:13], method = "spearman")
View(matriz_corr)


corrplot(matriz_corr, method = "color")


covid_cidades = covid_sp_analise%>%filter(municipio %in% c("Campinas", "Guarulhos", "Sorocaba"))


ggplot(covid_cidades, aes(x=casos, y=obitos, color=municipio)) +
  geom_line() +
  labs(title="Evolução dos obitos em função dos casos de COVID-19",
       x = "Casos",
       y = "Óbitos",
       color = "Cidades")+
  theme_classic()


