########### Carregando pacotes #############

if(!require(ggplot2)) install.packages("ggplot2")
if(!require(rstatix)) install.packages("rstatix")
if(!require(dplyr)) install.packages("dplyr")
if(!require(plotly)) install.packages("plotly")

library(dplyr)
library(rstatix)
library(ggplot2)
library(plotly)


########## Carregando os dados sobre Sindrome Respiratória Aguda Grave ##########

srag_sp = read.csv2("C:/Users/ISAAC/Documents/Analise de Dados com R/Projeto 1/dados-covid-sp-master/data/SRAG_2020.csv", sep=';')
View(srag_sp)

srag_sp2 = select(srag_sp, -c(51:133))
srag_sp2 = select(srag_sp2, -c(5:8))
srag_sp2 = select(srag_sp2, -c(6,8))
glimpse(srag_sp2)

#### Trocando o tipo de dado
srag_sp2$DT_NOTIFIC = as.Date(srag_sp2$DT_NOTIFIC, format="%m/%d/%Y")
glimpse(srag_sp2)

#### Renomeando as colunas
srag_sp2 = rename(srag_sp2, sexo=CS_SEXO, idade= NU_IDADE_N)

#### Procurando Missing Values

sapply(srag_sp2, function(x) sum(is.na(x)))
sapply(srag_sp2, function(x) sum(is.nan(x)))


#### Análises gráficas ####

ggplot(srag_sp2, aes(x=sexo))+
  geom_bar(fill = "blue") +
  labs(title=" Quantidade por sexo", subtitle = "SRAG", x = "Sexo", y= "Quantidade")


srag_sp2$CS_RACA[which(is.na(srag_sp2$CS_RACA))] = 9


srag_sp2$CS_RACA[srag_sp2$CS_RACA == 1] = "Branca"
srag_sp2$CS_RACA[srag_sp2$CS_RACA == 2] = "Preta"
srag_sp2$CS_RACA[srag_sp2$CS_RACA == 3] = "Amarela"
srag_sp2$CS_RACA[srag_sp2$CS_RACA == 4] = "Parda"
srag_sp2$CS_RACA[srag_sp2$CS_RACA == 5] = "Indigena"
srag_sp2$CS_RACA[srag_sp2$CS_RACA == 9] = "Ignorado"


ggplot(srag_sp2, aes(x=CS_RACA))+
geom_bar(fill="red")+
labs(title="Quantidade por etnia", subtitle="SRAG", x= "Etnia", y="Quantidade")


sapply(srag_sp2, function(x) sum(is.na(x)))
srag_sp2$CS_ZONA[which(is.na(srag_sp2$CS_ZONA))] = 9

srag_sp2$CS_ZONA[srag_sp2$CS_ZONA == 1] = 'Urbana'
srag_sp2$CS_ZONA[srag_sp2$CS_ZONA == 2] = 'Rural'
srag_sp2$CS_ZONA[srag_sp2$CS_ZONA == 3] = 'Periurbana'
srag_sp2$CS_ZONA[srag_sp2$CS_ZONA == 9] = 'Ignorado'



ggplot(srag_sp2, aes(x=CS_RACA, y=sexo, fill=factor(CS_ZONA))) +
  geom_col(position="dodge") +
  labs(title='Região por sexo e raça', x="Raça", y= "Sexo", fill="Região")


gr = aggregate(idade ~ sexo + CS_ZONA, data = srag_sp2, FUN = mean)
?aggregate
ggplot(gr, aes(x= CS_ZONA, y=idade, fill=factor(sexo))) +
  geom_col(position="stack")


plot_ly(srag_sp2, x = ~CS_RACA) %>%
  layout(xaxis= list(title="Raça"), yaxis=list(title="Quantidade"))



srag_sp2$idade[srag_sp2$TP_IDADE == 2] = 0
srag_sp2$idade[srag_sp2$TP_IDADE == 1] = 0

summary(srag_sp2$idade)
boxplot(srag_sp2$idade)


srag_sp2%>% identify_outliers(idade)
outliers = c(boxplot.stats(srag_sp2$idade)$out)

srag_final = srag_sp2[-c(which(srag_sp2$idade %in% outliers)), ]

boxplot(srag_final$idade)


srag_sp2 %>% filter(!is.na(idade)) %>%
  ggplot(aes(x= '', y= idade)) +
  geom_boxplot(width = .3, outlier.color = "purple")


plot_ly(srag_sp2, y=srag_sp2$idade,
        type = 'box')%>%
        layout(title="BOXPLOT POR IDADE", yaxis = list(title='Idade'))


ggplot(srag_final, aes(x= factor(sexo), y= idade)) +
  geom_boxplot(fill = "dodgerblue")+
  labs(y="sexo",
       x="Idade" ,
       title="Distribuição das idades por sexo")

plot_ly(srag_final, y=srag_final$idade, color=srag_final$sexo,
        type = 'box')%>%
        layout(title="BOXPLOT POR IDADE", xaxis=list(title="Sexo"), yaxis = list(title='Idade'))


