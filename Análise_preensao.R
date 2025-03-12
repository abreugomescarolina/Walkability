#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("readxl")
#install.packages("ggplot2")
#install.packages("janitor")
library("dplyr")
library("ggplot2")
library("janitor")
library("grid")
library("gridExtra")
library("broom")
setwd("C:\\Users\\casqu\\Documentos Carol\\Internato Médico\\Internato Específico\\Estágio de Investigação\\Análise Dados_R")
dados_unidos <- read.csv("dados_unidos.csv", header = TRUE, sep = ",")

dados_din <- dados_unidos %>%
  mutate(DOM_max = pmax(DIN1_DOM_2, DIN2_DOM_2, DIN3_DOM_2, na.rm = TRUE)) %>%
  mutate(NDOM_max = pmax(DIN1_NDOM_2, DIN2_NDOM_2, DIN3_NDOM_2, na.rm = TRUE)) %>%
  mutate(DOM_med = rowMeans(across(c(DIN1_DOM_2, DIN2_DOM_2, DIN3_DOM_2)), na.rm = TRUE)) %>%
  mutate(NDOM_med = rowMeans(across(c(DIN1_NDOM_2, DIN2_NDOM_2, DIN3_NDOM_2)), na.rm = TRUE))
  
modelo_max <- lm(dados_din$DOM_max ~ dados_din$NDOM_max)
modelo_med <- lm(dados_din$DOM_med ~ dados_din$NDOM_med)
modelo_DOM <- lm(dados_din$DOM_max ~ dados_din$DOM_med)
modelo_NDOM <- lm(dados_din$NDOM_max ~ dados_din$NDOM_med)

r_modelo_max$modelo <- "Modelo Max"
r_modelo_med$modelo <- "Modelo Med"
r_modelo_DOM$modelo <- "Modelo DOM"
r_modelo_NDOM$modelo <- "Modelo NDOM"

r_modelo_max <- tidy(modelo_max)
r_modelo_med <- tidy(modelo_med)
r_modelo_DOM <- tidy(modelo_DOM)
r_modelo_NDOM <- tidy(modelo_NDOM)

tabela <- bind_rows(r_modelo_max, r_modelo_med, r_modelo_DOM, r_modelo_NDOM)

plot(dados_din$DOM_max, dados_din$NDOM_max, main = "Gráfico de Regressão Linear",
     xlab = "DOM_max", ylab = "NDOM_max",
     pch = 19, col = "blue")

plot(dados_din$DOM_med, dados_din$NDOM_med, main = "Gráfico de Regressão Linear",
     xlab = "DOM_med", ylab = "NDOM_med",
     pch = 19, col = "blue")

plot(dados_din$DOM_max, dados_din$DOM_med, main = "Gráfico de Regressão Linear",
     xlab = "DOM_max", ylab = "DOM_med",
     pch = 19, col = "blue")

