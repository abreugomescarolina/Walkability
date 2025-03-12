##com base no script Diogo
library("dplyr")
library("tidyverse")
library("lcmm")
library("ggplot2")
library("gridExtra")

setwd("C:\\Users\\casqu\\Documentos Carol\\Internato Médico\\Internato Específico\\Estágio de Investigação\\Análise Dados_R")
poll_walk <- read.csv("dados_exp_outc.csv", header = TRUE, sep = ",")
#poll_walk_complete <- poll_walk %>%
  mutate(across(everything(), ~na_if(.x, -7777))) %>%
  mutate(across(everything(), ~na_if(.x, -9998))) %>%
  rename_with(~ gsub("_wi_z$", "", .x))
poll_walk_complete_w1w4 <- poll_walk %>%
  select( "ID", matches("w1|w2|w3|w4"))

#Buffer 100
dados_walk_100 <- poll_walk_complete_w1w4 %>%
  select("ID", contains(("_100"))) %>%
  rename_with(~ gsub("_100", "", .x)) %>%
  filter(complete.cases(.))

dados_walk_100 <- dados_walk_100 %>%
  pivot_longer(
    cols = starts_with("w"),
    names_to = "wave_100",
    names_prefix = "w",
    values_to = "value")
dados_walk_100$wave_100 <- as.numeric(dados_walk_100$wave_100)

modelo_traj_100_1c <- hlme(
  fixed = value ~ wave_100,
  random = ~ wave_100,
  subject = "ID",
  ng = 1,
  data = dados_walk_100)

modelo_traj_100_2c <- hlme(
    fixed = value ~ wave_100,
    mixture = ~ wave_100,
    random = ~ wave_100,
    subject = "ID",
    ng = 2,
    data = dados_walk_100,
    B=modelo_traj_100_1c)

modelo_traj_100_3c <- hlme(
  fixed = value ~ wave_100,
  mixture = ~ wave_100,
  random = ~ wave_100,
  subject = "ID",
  ng = 3,
  data = dados_walk_100,
  B=modelo_traj_100_1c)

modelo_traj_100_4c <- hlme(
  fixed = value ~ wave_100,
  mixture = ~ wave_100,
  random = ~ wave_100,
  subject = "ID",
  ng = 4,
  data = dados_walk_100,
  B=modelo_traj_100_1c)

modelo_traj_100_5c <- hlme(
  fixed = value ~ wave_100,
  mixture = ~ wave_100,
  random = ~ wave_100,
  subject = "ID",
  ng = 5,
  data = dados_walk_100,
  B=modelo_traj_100_1c)

#Buffer 300
dados_walk_300 <- poll_walk_complete_w1w4 %>%
  select("ID", contains(("_300"))) %>%
  rename_with(~ gsub("_300", "", .x)) %>%
  filter(complete.cases(.))

dados_walk_300 <- dados_walk_300 %>%
  pivot_longer(
    cols = starts_with("w"),
    names_to = "wave_300",
    names_prefix = "w",
    values_to = "value")
dados_walk_300$wave_300 <- as.numeric(dados_walk_300$wave_300)

modelo_traj_300_1c <- hlme(
  fixed = value ~ wave_300,
  random = ~ wave_300,
  subject = "ID",
  ng = 1,
  data = dados_walk_300)

modelo_traj_300_2c <- hlme(
  fixed = value ~ wave_300,
  mixture = ~ wave_300,
  random = ~ wave_300,
  subject = "ID",
  ng = 2,
  data = dados_walk_300,
  B=modelo_traj_300_1c)

modelo_traj_300_3c <- hlme(
  fixed = value ~ wave_300,
  mixture = ~ wave_300,
  random = ~ wave_300,
  subject = "ID",
  ng = 3,
  data = dados_walk_300,
  B=modelo_traj_300_1c)

modelo_traj_300_4c <- hlme(
  fixed = value ~ wave_300,
  mixture = ~ wave_300,
  random = ~ wave_300,
  subject = "ID",
  ng = 4,
  data = dados_walk_300,
  B=modelo_traj_300_1c)

modelo_traj_300_5c <- hlme(
  fixed = value ~ wave_300,
  mixture = ~ wave_300,
  random = ~ wave_300,
  subject = "ID",
  ng = 5,
  data = dados_walk_300,
  B=modelo_traj_300_1c)

#Buffer 500
dados_walk_500 <- poll_walk_complete_w1w4 %>%
  select("ID", contains(("_500"))) %>%
  rename_with(~ gsub("_500", "", .x)) %>%
  filter(complete.cases(.))

dados_walk_500 <- dados_walk_500 %>%
  pivot_longer(
    cols = starts_with("w"),
    names_to = "wave_500",
    names_prefix = "w",
    values_to = "value")
dados_walk_500$wave_500 <- as.numeric(dados_walk_500$wave_500)

modelo_traj_500_1c <- hlme(
  fixed = value ~ wave_500,
  random = ~ wave_500,
  subject = "ID",
  ng = 1,
  data = dados_walk_500)

modelo_traj_500_2c <- hlme(
  fixed = value ~ wave_500,
  mixture = ~ wave_500,
  random = ~ wave_500,
  subject = "ID",
  ng = 2,
  data = dados_walk_500,
  B=modelo_traj_500_1c)

modelo_traj_500_3c <- hlme(
  fixed = value ~ wave_500,
  mixture = ~ wave_500,
  random = ~ wave_500,
  subject = "ID",
  ng = 3,
  data = dados_walk_500,
  B=modelo_traj_500_1c)

modelo_traj_500_4c <- hlme(
  fixed = value ~ wave_500,
  mixture = ~ wave_500,
  random = ~ wave_500,
  subject = "ID",
  ng = 4,
  data = dados_walk_500,
  B=modelo_traj_500_1c)

modelo_traj_500_5c <- hlme(
  fixed = value ~ wave_500,
  mixture = ~ wave_500,
  random = ~ wave_500,
  subject = "ID",
  ng = 5,
  data = dados_walk_500,
  B=modelo_traj_500_1c)

modelos_trajet <- summarytable(modelo_traj_100_1c, modelo_traj_100_2c, modelo_traj_100_3c, modelo_traj_100_4c, modelo_traj_100_5c, modelo_traj_300_1c, modelo_traj_300_2c, modelo_traj_300_3c, modelo_traj_300_4c, modelo_traj_300_5c, modelo_traj_500_1c, modelo_traj_500_2c, modelo_traj_500_3c, modelo_traj_500_4c, modelo_traj_500_5c, which = c("G", "loglik", "conv", "npm", "AIC", "BIC", "SABIC", "entropy", "%class"))
write.csv(modelos_trajet, "modelos_trajet.csv", row.names = TRUE)

#Melhor modelo (menor BIC) = 3
postprob(modelo_traj_100_3c)
classes <- as.data.frame(modelo_traj_100_3c$pprob[,1:2])
dados_walk_100$classes <- factor(classes$class[match(dados_walk_100$ID, classes$ID)])
p01 <- ggplot(dados_walk_100, aes(wave_100, value, group=ID, colour=classes)) + geom_line(na.rm=TRUE) + geom_smooth(aes(group=classes), method="loess", linewidth=2, se=FALSE, na.rm=TRUE)+ scale_y_continuous(limits = c(0,0.6))  + labs(x="x",y="y",colour="Latent Class") 
p02 <- ggplot(dados_walk_100, aes(wave_100, value, group=ID, colour=classes)) + geom_smooth(aes(group=ID, colour=classes), linewidth=0.5, se=F) + geom_smooth(aes(group=classes), method="loess", linewidth=2, se=T)+ scale_y_continuous(limits = c(0,0.6))
grid.arrange(p01,p02, ncol=2)