library(dplyr)
library(lubridate)
library(ggplot2)
library(ggcorrplot)
library(stats)
library(zoo)

#http://sistemas.saude.rj.gov.br/tabnetbd/dhx.exe?covid19/esus_sivep.def

inicio_x_casos <- read_delim("inicio_x_casos.csv", 
                             ";", escape_double = FALSE, 
                             locale = locale(encoding = "Latin1"), 
                             trim_ws = TRUE)

X12_04_Dados <- read_delim("12.04 - Dados.csv", 
                           ";", escape_double = FALSE, 
                           locale = locale(decimal_mark = ",", encoding = "Latin1"), 
                           trim_ws = TRUE,
                           col_types = cols(Metro_Tot = col_number(), 
                                            Trem_Tot = col_number(), 
                                            Barcas_Tot = col_number(), 
                                            RMRJ = col_number(), 
                                            Intermunicipais = col_number()))

# Limpeza e ajuste do Dataframe de casos de Covid-19 

casos=inicio_x_casos[-c(1,597),]
colnames(casos)[1] <- "Data"
colnames(casos)[2] <- "Casos"
casos$Data <- as.Date(casos$Data)

# Limpeza e ajuste do Dataframe de transportes

df=X12_04_Dados[-c(400:615),-c(1,4:6,8:17,19:26,28:31,34:57)]
df <- rename(df, Data = Dia)
df$Data <- as.Date(df$Data)

df["Municipais"] <- df$RMRJ - df$Intermunicipais
df["Total"] <- df$Metro_Tot + df$Trem_Tot + df$Barcas_Tot + df$RMRJ 
df <- df[,-6]

# Remoção das datas com valores ausentes de número de casos

data = df$Data
data <- as_data_frame(data)
colnames (data)[1] <- "Data"
x = left_join(data,casos,by= "Data")

df["Casos"] <- x$Casos

#Evolução Diária por modo de transporte

ggplot(df, aes(x=Data, y=Metro_Tot))+
  geom_line(aes(col= "Metrô"))+
  geom_line(aes(y=Trem_Tot, col= "Trem"))+
  geom_line(aes(y=Barcas_Tot, col= "Barcas"))+
  geom_line(aes(y=Municipais, col= "Ônibus Municipais"))+
  geom_line(aes(y=Intermunicipais, col= "Ônibus Intermunicipais"))+
  geom_line(aes(y=Casos, col= "Casos"))+
  theme_classic()+
  labs(x="Data",
       y="Modo de Transporte",
       color=NULL)+
  theme(legend.position = "top")

# Padronização das variáveis

df_scale <- df
df_scale = data_frame("Data"            = df_scale$Data,
                      "Metro_Tot"       = scale(df_scale$Metro_Tot),
                      "Trem_Tot"        = scale(df_scale$Trem_Tot),
                      "Barcas_Tot"      = scale(df_scale$Barcas_Tot),
                      "Municipais"      = scale(df_scale$Municipais),
                      "Intermunicipais" = scale(df_scale$Intermunicipais),
                      "Casos"           = scale(df_scale$Casos)
)

# Evolução diária padronizada para estudo do comportamento do gráfico

ggplot(df_scale, aes(x=Data, y=Metro_Tot))+
  geom_line(aes(col= "Metrô"))+
  geom_line(aes(y=Trem_Tot, col= "Trem"))+
  geom_line(aes(y=Barcas_Tot, col= "Barcas"))+
  geom_line(aes(y=Municipais, col= "Ônibus Municipais"))+
  geom_line(aes(y=Intermunicipais, col= "Ônibus Intermunicipais"))+
  geom_line(aes(y=Casos, col= "Casos"))+
  geom_vline(data = subset(df_scale, Data == "2020-03-16"), 
             aes(xintercept = Data),
             size = 0.3, colour = "black")+
  geom_text(data=subset(df_scale, Data == "2020-03-16"),
            mapping=aes(x=Data, y=0, 
                        label= "Decretada situação de emergência"), 
            size=4, angle=90, vjust=1, hjust=-0.7) +
  theme_classic()+
  labs(x="Data",
       y="Modo de Transporte",
       color=NULL)+
  theme(legend.position = "top")

# Elaboração da Matriz de Correlação de Pearson

x = df_scale[,c("Metro_Tot","Trem_Tot","Barcas_Tot","Municipais",
                "Intermunicipais","Casos")]

matriz <- round(cor(x),2)
ggcorrplot(matriz,type = "lower",
           lab = TRUE,
           lab_size = 3,
           colors = c('firebrick',"white", 'dodgerblue4'),
           title = "Matriz de Correlação de Pearson",
           ggtheme = theme_classic)

# Séries Temporais

series <- df[,c("Data","Total","Casos")]

ts_total = ts(series$Total,frequency = 7)
ts_casos = ts(series$Casos,frequency = 7)

plot(cbind(ts_total,ts_casos), main="Séries Temporais")

# Remoção de tendência e de sazonalidade

decompose_total = decompose(ts_total, type = 'additive')
plot(decompose_total)
r_total = decompose_total$random

decompose_casos = decompose(ts_casos, type = 'additive')
plot(decompose_casos)
r_casos = decompose_casos$random

plot(cbind(r_total,r_casos), main=" ")

# Correlação Cruzada

ccf = ccf(r_total,r_casos, type = "correlation",
          na.action = na.pass,
          ylab = "Correlação",
          main = "Correlação Cruzada")

cor = ccf$acf[,,1]
lag = ccf$lag[,,1]
res = data.frame(cor,lag)
lag_max = res[which.max(res$cor),]$lag
lag_max
cor_max = res[which.max(res$cor),]$cor
cor_max
dias = lag_max *7
dias
