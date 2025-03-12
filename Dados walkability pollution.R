#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("readxl")
#install.packages("ggplot2")
#install.packages("janitor")
#install.packages("nortest")
library("dplyr")
library("tidyverse")
library("readxl")
library("ggplot2")
library("janitor")
library("nortest")

setwd("C:\\Users\\casqu\\Documentos Carol\\Internato Médico\\Internato Específico\\Estágio de Investigação\\Análise Dados_R")
poll_walk <- read_excel("tabela poluição e walkability - epiporto.xlsx")

sum(complete.cases(poll_walk))

#n complete cases pollution + walkability = 2422
poll_walk <- poll_walk %>%
  mutate(across(everything(), ~na_if(.x, -7777))) %>%
  mutate(across(everything(), ~na_if(.x, -9998))) %>%
  rename_with(~ gsub("_wi_z$", "", .x)) %>%
  rename("ID"="ID1")
sum(complete.cases(poll_walk))

#selecionar w1 (baseline) to w4 (follow-up 3)
poll_walk_w1w4 <- poll_walk %>%
  select( "ID", matches("w1|w2|w3|w4"))
colnames(poll_walk_w1w4)

#n missings por variável
missing_poll_walk_w1w4 <- poll_walk_w1w4 %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "Variável", values_to = "Missing_n") %>%
  filter(Missing_n > 0)
print(missing_poll_walk_w1w4, n=60)

#Normalidade das variáveis
lillie_results_df <- poll_walk_w1w4 %>%
  map_df(~{
    result <- lillie.test(.x)
    tibble(p_value = result$p.value, result = ifelse(result$p.value > 0.05, "Normal", "Not Normal"))
  }, .id = "Variável")
print(lillie_results_df, n=30)

#Tabela Estatística Descritiva
tabela_pol <- poll_walk_w1w4 %>%
  select(-ID) %>%
  select(-starts_with("w")) %>%
  summarise(across(everything(), list(
    n = ~sum(!is.na(.)),
    Mínimo = ~round(min(. , na.rm = TRUE), 2),
    Máximo = ~round(max(. , na.rm = TRUE), 2),
    Mediana = ~round(median(. , na.rm = TRUE), 2),
    IQR = ~round(IQR(. , na.rm = TRUE), 2),
    Média = ~round(mean(. , na.rm = TRUE),2),
    Desvio.Padrão = ~round(sd(. , na.rm = TRUE),2)
  ), .names = "{col}_{fn}")) %>%
  pivot_longer(cols = everything(), 
               names_to = c("Variable", "Statistic"), 
               names_pattern = "(.*)_(.*)", 
               values_to = "Value") %>%
  pivot_wider(names_from = Statistic, values_from = Value)
print(tabela_pol, n=30)
#write.csv(tabela_pol, "tabela_pol.csv", row.names = FALSE)

tabela_walk <- poll_walk_w1w4 %>%
  select(-ID) %>%
  select(starts_with("w")) %>%
  summarise(across(everything(), list(
    n = ~sum(!is.na(.)),
    Mínimo = ~round(min(. , na.rm = TRUE), 2),
    Máximo = ~round(max(. , na.rm = TRUE), 2),
    Mediana = ~round(median(. , na.rm = TRUE), 2),
    IQR = ~round(IQR(. , na.rm = TRUE), 2),
    Média = ~round(mean(. , na.rm = TRUE),2),
    Desvio.Padrão = ~round(sd(. , na.rm = TRUE),2)
  ), .names = "{col}_{fn}")) %>%
  pivot_longer(cols = everything(), 
               names_to = c("Variable", "Statistic"), 
               names_pattern = "(.*)_(.*)", 
               values_to = "Value") %>%
  pivot_wider(names_from = Statistic, values_from = Value)
print(tabela_walk, n=30)
#write.csv(tabela_walk, "tabela_walk.csv", row.names = FALSE)