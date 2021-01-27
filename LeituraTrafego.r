library(readxl)
Traf <- read_excel("C:/Users/leona/OneDrive - PRODESP/Tráfego - TXT´s/Trafego_2021.xlsx", 
                   col_types = c("numeric", "text", "text", 
                                 "text", "text", "text", "numeric", 
                                 "text", "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "skip", "skip", "skip"))

library(dplyr)
Traf$Data <- format(as.Date(Traf$Data,origin="1899-12-30"), '%b %Y')

#Lotes
Lote01 <- filter(Traf,`Lote:`==1)

#Análise_Comparação de Grupos

Eixos_Tot <- Traf %>%
  group_by(`Lote:`,`Ano:`) %>%
  summarise(eixo_total=sum(Equivalente),eixo_medio=mean(Equivalente)) 
  ungroup()

summary(Eixos_Tot)

# cruzando variáveis
Eixos_Tot2020 <- filter(Eixos_Tot, `Ano:`==2019)
Eixos_Lote01 <- filter(Eixos_Tot, `Lote:`==01)

plot(x=Eixos_Lote01$`Ano:`, y=Eixos_Lote01$eixo_total, 
     xlab="Lotes", ylab="Eixos")


plot(x=Eixos_Tot$`Lote:`, y=Eixos_Tot$eixo_total, 
     xlab="Lotes", ylab="Eixos")

#histograma
library(ggplot2)

ggplot(Traf, aes(x=Data)) +
  geom_histogram(fill ="blue",colour="black")  +
  facet_grid(`Lote:` ~.)

#boxplot

Por_Ano <- Traf2020 %>%
  group_by(Traf2020$`Lote:`, Traf2020$Mês)%>%
  summarise(eixo_total = sum(Equivalente),quantidade =n()) %>%
  ggplot(Traf2020, aes(x=`Lote:`, y=Equivalente))+
  geom_boxplot(aes(fill=`Rodovia:`))%>%
  ungroup()

ggplot(Lote01, aes(x=`Ano:`, y=Arrecadação)) + 
  geom_boxplot(aes(fill=`Rodovia:`))


