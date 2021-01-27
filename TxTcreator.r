#install.packages("readr")
#install.packages("writexl")
library("writexl")
library("readr")

startDate <- as.Date("2021-01-01")
dia <- seq(startDate, by="1 days", length.out=31)

lote <- 30
# 21 praca de ped�gios; 
# Vale para num da Rodovia e Km do Ped�gio (03-11; 9 d�gitos)
# praca <- 999999999:999999979; praca
rod <- c(310,310,225,225,225,308,304,304,304,294,294,294,294,294,294,425,284,284,425,294,293)
km <- c(181,217,106,143,199,180,183,210,256,366,426,477,551,581,623,433,457,532,400,668,'001')
praca <- paste(rod,km)
praca
sentido <- c('N','S')
dia <- format(as.Date(dia), "%d%m%Y")
hora <- 24:47; hora
pgto<- c(1,2)
pgto

#random Cat
prob<- c(0.45,0.1,0.1,0.05,0.1,0.1,0.04,0.03,0.03); sum(prob)
cat <- c(1:9); cat
result <- rep(cat, round(100*prob))
cat_sub <- as.data.frame(table(result)) ; cat_sub
tf_prat <- 410
tarifa <- paste('0000',tf_prat); tarifa
arrec <- cat_sub[,2]*tf_prat/100 ; arrec
eixosusp <- round(runif(1,0,1),0); eixosusp
tf_es <- tf_prat-0.3
es_arrec <-  paste("0000",tf_es*cat_sub[,2])
es_arrec
dummii <- ("123456789012355E+4412345678979845")
nchar(dummii, type = "chars")
# Creating data frame; setting variables
dias<-length(dia) ; dias
horas <- c(0:23); horas
datalist =list()
cats <- length(cat); cats

df_total = data.frame()
cats
n=0

#Adding categorias
for (ct in 1:cats){
  #Adding pmt method
  for (pmt in 1:2){
    #Adding data
    for (i in 1:dias){
      #Adding horas
      for (h in 1:24){
        n <-n+1
        simulador <- data.frame(
        lote,praca,dia[i],horas[h],pmt, ct, tarifa, dummii)
        datalist[[n]] <- simulador
      }
    }
  }
}

#big_data = do.call(rbind, datalist)  
big_data <-dplyr::bind_rows(datalist)
tail(big_data)
head(big_data)
summary(big_data)

str(big_data)

setwd("C:/Users/leona/OneDrive - PRODESP/RStudio/Resultados")

# Writing data to a txt file
write_tsv(big_data, path = "Lote30.txt")

# Writing data to a csv file
write_csv(big_data, path = "Lote30.csv")

# Writing data to a xlsx file
write_xlsx(big_data, "Lote30.xlsx" )

