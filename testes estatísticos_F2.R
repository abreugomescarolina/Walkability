library("dplyr")
library("ggplot2")
setwd("C:\\Users\\casqu\\Documentos Carol\\Internato Médico\\Internato Específico\\Estágio de Investigação\\Análise Dados_R")
dados_exp_outc <- read.csv("dados_exp_outc.csv", header = TRUE, sep = ",")

dados_exp_outc <- dados_exp_outc %>%
  mutate(DOM_max = pmax(DIN1_DOM_2, DIN2_DOM_2, DIN3_DOM_2, na.rm = TRUE)) %>%
  mutate(NDOM_max = pmax(DIN1_NDOM_2, DIN2_NDOM_2, DIN3_NDOM_2, na.rm = TRUE)) %>%
  mutate(DOM_med = rowMeans(across(c(DIN1_DOM_2, DIN2_DOM_2, DIN3_DOM_2)), na.rm = TRUE)) %>%
  mutate(NDOM_med = rowMeans(across(c(DIN1_NDOM_2, DIN2_NDOM_2, DIN3_NDOM_2)), na.rm = TRUE))

#Teste Shapiro-Wilk
#H0: distribuição é normal
normalidade <- tibble(
  Variável = c("w1_100", "w1_300", "w1_500", "w2_100", "w2_300", "w2_500",
               "w3_100", "w3_300", "w3_500", "walk1_temp_2", "walk2_temp_2",
               "DIN1_DOM_2", "DIN1_NDOM_2", "DIN2_DOM_2", "DOM_med" ,"DIN2_NDOM_2",
               "DIN3_DOM_2", "DIN3_NDOM_2", "NDOM_med", "idade_2014", "intensidade_n", 
               "pm25_w1", "no2_w1",
               "MMSE_total_2", "MoCA_total_2", "ENSINO_2", "ESCOL_2"),
  Teste_Shapiro = c(
    shapiro.test(dados_exp_outc$w1_100)$p.value,
    shapiro.test(dados_exp_outc$w1_300)$p.value,
    shapiro.test(dados_exp_outc$w1_500)$p.value,
    shapiro.test(dados_exp_outc$w2_100)$p.value,
    shapiro.test(dados_exp_outc$w2_300)$p.value,
    shapiro.test(dados_exp_outc$w2_500)$p.value,
    shapiro.test(dados_exp_outc$w3_100)$p.value,
    shapiro.test(dados_exp_outc$w3_300)$p.value,
    shapiro.test(dados_exp_outc$w3_500)$p.value,
    shapiro.test(dados_exp_outc$walk1_temp_2)$p.value,
    shapiro.test(dados_exp_outc$walk2_temp_2)$p.value,
    shapiro.test(dados_exp_outc$DIN1_DOM_2)$p.value,
    shapiro.test(dados_exp_outc$DIN2_DOM_2)$p.value,
    shapiro.test(dados_exp_outc$DIN3_DOM_2)$p.value,
    shapiro.test(dados_exp_outc$DOM_med)$p.value,
    shapiro.test(dados_exp_outc$DIN1_NDOM_2)$p.value,
    shapiro.test(dados_exp_outc$DIN2_NDOM_2)$p.value,
    shapiro.test(dados_exp_outc$DIN3_NDOM_2)$p.value,
    shapiro.test(dados_exp_outc$NDOM_med)$p.value,
    shapiro.test(dados_exp_outc$idade_2014)$p.value,
    shapiro.test(dados_exp_outc$intensidade_n)$p.value,
    shapiro.test(dados_exp_outc$pm25_w1)$p.value,
    shapiro.test(dados_exp_outc$no2_w1)$p.value,
    shapiro.test(dados_exp_outc$MMSE_total_2)$p.value,
    shapiro.test(dados_exp_outc$MoCA_total_2)$p.value,
    shapiro.test(dados_exp_outc$ENSINO_2)$p.value,
    shapiro.test(dados_exp_outc$ESCOL_2)$p.value
  ),
    Interpretação = c(
      ifelse(shapiro.test(dados_exp_outc$w1_100)$p.value < 0.05, "Rejeita normalidade", "Não rejeita normalidade"),
      ifelse(shapiro.test(dados_exp_outc$w1_300)$p.value < 0.05, "Rejeita normalidade", "Não rejeita normalidade"),
      ifelse(shapiro.test(dados_exp_outc$w1_500)$p.value < 0.05, "Rejeita normalidade", "Não rejeita normalidade"),
      ifelse(shapiro.test(dados_exp_outc$w2_100)$p.value < 0.05, "Rejeita normalidade", "Não rejeita normalidade"),
      ifelse(shapiro.test(dados_exp_outc$w2_300)$p.value < 0.05, "Rejeita normalidade", "Não rejeita normalidade"),
      ifelse(shapiro.test(dados_exp_outc$w2_500)$p.value < 0.05, "Rejeita normalidade", "Não rejeita normalidade"),
      ifelse(shapiro.test(dados_exp_outc$w3_100)$p.value < 0.05, "Rejeita normalidade", "Não rejeita normalidade"),
      ifelse(shapiro.test(dados_exp_outc$w3_300)$p.value < 0.05, "Rejeita normalidade", "Não rejeita normalidade"),
      ifelse(shapiro.test(dados_exp_outc$w3_500)$p.value < 0.05, "Rejeita normalidade", "Não rejeita normalidade"),
      ifelse(shapiro.test(dados_exp_outc$walk1_temp_2)$p.value < 0.05, "Rejeita normalidade", "Não rejeita normalidade"),
      ifelse(shapiro.test(dados_exp_outc$walk2_temp_2)$p.value < 0.05, "Rejeita normalidade", "Não rejeita normalidade"),
      ifelse(shapiro.test(dados_exp_outc$DIN1_DOM_2)$p.value < 0.05, "Rejeita normalidade", "Não rejeita normalidade"),
      ifelse(shapiro.test(dados_exp_outc$DIN1_NDOM_2)$p.value < 0.05, "Rejeita normalidade", "Não rejeita normalidade"),
      ifelse(shapiro.test(dados_exp_outc$DIN2_DOM_2)$p.value < 0.05, "Rejeita normalidade", "Não rejeita normalidade"),
      ifelse(shapiro.test(dados_exp_outc$DIN2_NDOM_2)$p.value < 0.05, "Rejeita normalidade", "Não rejeita normalidade"),
      ifelse(shapiro.test(dados_exp_outc$DIN3_DOM_2)$p.value < 0.05, "Rejeita normalidade", "Não rejeita normalidade"),
      ifelse(shapiro.test(dados_exp_outc$DIN3_NDOM_2)$p.value < 0.05, "Rejeita normalidade", "Não rejeita normalidade"),
      ifelse(shapiro.test(dados_exp_outc$DOM_med)$p.value < 0.05, "Rejeita normalidade", "Não rejeita normalidade"),
      ifelse(shapiro.test(dados_exp_outc$NDOM_med)$p.value < 0.05, "Rejeita normalidade", "Não rejeita normalidade"),
      ifelse(shapiro.test(dados_exp_outc$idade_2014)$p.value < 0.05, "Rejeita normalidade", "Não rejeita normalidade"),
      ifelse(shapiro.test(dados_exp_outc$intensidade_n)$p.value < 0.05, "Rejeita normalidade", "Não rejeita normalidade"),
      ifelse(shapiro.test(dados_exp_outc$pm25_w1)$p.value < 0.05, "Rejeita normalidade", "Não rejeita normalidade"),
      ifelse(shapiro.test(dados_exp_outc$no2_w1)$p.value < 0.05, "Rejeita normalidade", "Não rejeita normalidade"),
      ifelse(shapiro.test(dados_exp_outc$MMSE_total_2)$p.value < 0.05, "Rejeita normalidade", "Não rejeita normalidade"),
      ifelse(shapiro.test(dados_exp_outc$MoCA_total_2)$p.value < 0.05, "Rejeita normalidade", "Não rejeita normalidade"),
      ifelse(shapiro.test(dados_exp_outc$ENSINO_2)$p.value < 0.05, "Rejeita normalidade", "Não rejeita normalidade"),
      ifelse(shapiro.test(dados_exp_outc$ESCOL_2)$p.value < 0.05, "Rejeita normalidade", "Não rejeita normalidade")))

#write.csv(normalidade, "normalidade.csv", row.names = TRUE)

#Q-Q plot
# Lista das variáveis
variaveis <- c("w1_100", "w1_300", "w1_500", "w2_100", "w2_300", "w2_500",
               "w3_100", "w3_300", "w3_500", "walk1_temp_2", "walk2_temp_2",
               "DIN1_DOM_2", "DIN1_NDOM_2", "DIN2_DOM_2", "DOM_med", "DIN2_NDOM_2",
               "DIN3_DOM_2", "DIN3_NDOM_2", "NDOM_med", "idade_2014", "intensidade_n", 
               "pm25_w1", "no2_w1", "MMSE_total_2", "MoCA_total_2", 
               "ENSINO_2", "ESCOL_2")

# Criação dos Q-Q plots
par(mfrow = c(2,2), mar=c(2,2,2,1))
for (var in variaveis) {
  if (var %in% colnames(dados_exp_outc)) {  # Verifica se a variável existe na base de dados
    qqnorm(dados_exp_outc[[var]], main = paste("Q-Q Plot de", var), ylab = "Quantis da amostra", xlab = "Quantis teóricos")
    qqline(dados_exp_outc[[var]], col = "red")
  } else {
    message(paste("Variável", var, "não encontrada na base de dados."))
  }
}
par(mfrow = c(1, 1))  # Restaura o layout padrão

# Exportação dos Q-Q plots para PNG
caminho <- "qq_plots"
if (!dir.exists(caminho)) {dir.create(caminho)
  message("Diretório 'qq_plots' criado.")}
for (var in variaveis) {
  if (var %in% colnames(dados_exp_outc)) {
    filename <- file.path(caminho, paste0("QQ_Plot_", var, ".png"))
    png(filename = filename, width = 800, height = 600, res = 100)
    qqnorm(na.omit(dados_exp_outc[[var]]), 
           main = paste("Q-Q Plot de", var), 
           ylab = "Quantis da amostra", 
           xlab = "Quantis teóricos")
    qqline(na.omit(dados_exp_outc[[var]]), col = "red")
    dev.off()
    message(paste("Gráfico guardado em:", filename))
  } else {
    message(paste("Variável", var, "não encontrada na base de dados."))}}

#Box-plots
box_plot <- c( "DOM_med", "DIN2_NDOM_2", "NDOM_med", "idade_2014", "no2_w1")
for (var in box_plot) {
  boxplot(dados_exp_outc[[var]], main = paste("Box Plot de ", var), ylab = "Valores")}

#Exportar box-plots
if (!dir.exists("Box plot")) {
  dir.create("Box plot")
  message("Diretório 'Box plot' criado.")}
for (var in box_plot) {
  if (var %in% colnames(dados_exp_outc)) {
    filename <- file.path("Box plot", paste0("Box_Plot_", var, ".png"))
    
    # Exportação para PNG
    png(filename = filename, width = 800, height = 600, res = 100)
    boxplot(na.omit(dados_exp_outc[[var]]), 
            main = paste("Box Plot de", var), 
            ylab = "Valores", 
            col = "lightblue", 
            border = "darkblue")
    dev.off()
    message(paste("Gráfico guardado em:", filename))
  } else {
    message(paste("Variável", var, "não encontrada na base de dados."))}}

#Box-plot com poucos outliers
box_plot_d <- c("DIN1_NDOM_2", "DIN2_DOM_2", "DIN3_DOM_2", "DIN3_NDOM_2")
for (var in box_plot_d) {
  boxplot(dados_exp_outc[[var]], main = paste("Box Plot de ", var), ylab = "Valores")}

#Hitogramas
if (!dir.exists("Histogramas")) {
  dir.create("Histogramas")
  message("Diretório 'Histogramas' criado.")}
for (var in variaveis) {
  if (var %in% colnames(dados_exp_outc)) {
    filename <- file.path("Histogramas", paste0("Histograma_", var, ".png"))
    png(filename = filename, width = 800, height = 600, res = 100)
    hist(na.omit(dados_exp_outc[[var]]), 
         main = paste("Histograma de", var),
         xlab = var,
         col = "lightblue",
         border = "darkblue",
         breaks = 20) # Ajuste do número de classes (bins)
    dev.off()
    message(paste("Histograma guardado em:", filename))
  } else {message(paste("Variável", var, "não encontrada na base de dados."))}}

#Logarítmo base e
variaveis_transformar <- c("DIN1_DOM_2", "DIN1_NDOM_2", "DIN2_DOM_2", 
                           "DIN3_DOM_2", "DIN3_NDOM_2", "ESCOL_2", 
                           "intensidade_n", "pm25_w1", "walk2_temp_2")
for (var in variaveis_transformar) {
  if (var %in% colnames(dados_exp_outc)) {
    # Para lidar com zeros, adicionamos 1 antes de calcular o logaritmo
    dados_exp_outc[[paste0("ln_", var)]] <- log(dados_exp_outc[[var]] + 1)
    message(paste("Transformação ln aplicada na variável:", var))
  } else {message(paste("Variável", var, "não encontrada na base de dados."))}}

#Q-Q plots das variáveis ln
variaveis_ln <- c("ln_DIN1_DOM_2", "ln_DIN1_NDOM_2", "ln_DIN2_DOM_2", 
                             "ln_DIN3_DOM_2", "ln_DIN3_NDOM_2", "ln_ESCOL_2", 
                             "ln_intensidade_n", "ln_pm25_w1", "ln_walk2_temp_2")
if (!dir.exists("qq_plots_ln")) {
  dir.create("qq_plots_ln")
  message("Diretório 'qq_plots_ln' criado.")}
for (var in variaveis_ln) {
  if (var %in% colnames(dados_exp_outc)) {
    filename <- file.path("qq_plots_ln", paste0("qq_plot_", var, ".png"))
    png(filename = filename, width = 800, height = 600, res = 100)
    qqnorm(dados_exp_outc[[var]], 
           main = paste("Q-Q Plot de", var),
           ylab = "Quantis da amostra",
           xlab = "Quantis teóricos")
    qqline(dados_exp_outc[[var]], col = "red")
    dev.off()
    message(paste("Q-Q Plot guardado em:", filename))
  } else {message(paste("Variável", var, "não encontrada na base de dados."))}}


#Transformadas reciproca (u=1/x)
variaveis_transformar_rec <- c("DIN1_DOM_2", "DIN1_NDOM_2", "DIN2_DOM_2", 
                               "DIN3_DOM_2", "DIN3_NDOM_2", "ESCOL_2", 
                               "intensidade_n", "pm25_w1", "walk2_temp_2")
for (var in variaveis_transformar_rec) {
  if (var %in% colnames(dados_exp_outc)) {
    # Evitar divisão por zero, adicionando um valor pequeno (1e-6)
    dados_exp_outc[[paste0("rec_", var)]] <- 1 / (dados_exp_outc[[var]] + 1e-6)
    message(paste("Transformação recíproca aplicada na variável:", var))
  } else {message(paste("Variável", var, "não encontrada na base de dados."))}}

#Q-Q plots das variáveis recíprocas
variaveis_reciprocas <- c("rec_DIN1_DOM_2", "rec_DIN1_NDOM_2", "rec_DIN2_DOM_2", 
                          "rec_DIN3_DOM_2", "rec_DIN3_NDOM_2", "rec_ESCOL_2", 
                          "rec_intensidade_n", "rec_pm25_w1", "rec_walk2_temp_2")
if (!dir.exists("qq_plot_reciproca")) {
  dir.create("qq_plot_reciproca")
  message("Diretório 'qq_plot_reciproca' criado.")}
for (var in variaveis_reciprocas) {
  if (var %in% colnames(dados_exp_outc)) {
    filename <- file.path("qq_plot_reciproca", paste0("qq_plot_", var, ".png"))
    png(filename = filename, width = 800, height = 600, res = 100)
    qqnorm(dados_exp_outc[[var]], 
           main = paste("Q-Q Plot de", var),
           ylab = "Quantis da amostra",
           xlab = "Quantis teóricos")
    qqline(dados_exp_outc[[var]], col = "red")
    dev.off()
    message(paste("Q-Q Plot guardado em:", filename))
  } else {message(paste("Variável", var, "não encontrada na base de dados."))}}


#Transformadas quadráticamente
variaveis_transformar_quadrado <- c("MMSE_total_2", "MoCA_total_2")
for (var in variaveis_transformar_quadrado) {
  if (var %in% colnames(dados_exp_outc)) {
    # Aplicar transformação quadrática (x^2)
    dados_exp_outc[[paste0("quad_", var)]] <- dados_exp_outc[[var]]^2
    message(paste("Transformação quadrática aplicada na variável:", var))
  } else {
    message(paste("Variável", var, "não encontrada na base de dados."))}}

#Q-Q plots das variáveis quadráticas
variaveis_quadraticas <- c("quad_MMSE_total_2", "quad_MoCA_total_2")
if (!dir.exists("qq_plot_quadratica")) {
  dir.create("qq_plot_quadratica")
  message("Diretório 'qq_plot_quadratica' criado.")}
for (var in variaveis_quadraticas) {
  if (var %in% colnames(dados_exp_outc)) {
    filename <- file.path("qq_plot_quadratica", paste0("QQ_Plot_", var, ".png"))
    png(filename = filename, width = 800, height = 600, res = 100)
    qqnorm(dados_exp_outc[[var]], 
           main = paste("Q-Q Plot de", var),
           ylab = "Quantis da amostra",
           xlab = "Quantis teóricos")
    qqline(dados_exp_outc[[var]], col = "red")
    dev.off()
    message(paste("Q-Q Plot guardado em:", filename))
  } else {
    message(paste("Variável", var, "não encontrada na base de dados."))}}


############################
#Destribuição = Weibull
#install.packages("gamlss")
library(gamlss)

#MoCA
dados_MoCA <- as.data.frame(dados_exp_outc$MoCA_total_2)
# Ajuste das distribuições
mod_norm <- gamlss(dados_exp_outc$MoCA_total_2 ~ 1, family = NO, data = dados_MoCA)    # Normal
mod_lognorm <- gamlss(dados_exp_outc$MoCA_total_2 ~ 1, family = LOGNO, data = dados_MoCA) # Log-Normal
mod_gamma <- gamlss(dados_exp_outc$MoCA_total_2 ~ 1, family = GA, data = dados_MoCA)    # Gamma
mod_weibull <- gamlss(dados_exp_outc$MoCA_total_2 ~ 1, family = WEI, data = dados_MoCA) # Weibull
mod_expon <- gamlss(dados_exp_outc$MoCA_total_2 ~ 1, family = EXP, data = dados_MoCA)   # Exponencial
# Tabela com critérios AIC e BIC
MoCA <- data.frame(
  Modelo = c("Normal", "Log-Normal", "Gamma", "Weibull", "Exponencial"),
  AIC = c(AIC(mod_norm), AIC(mod_lognorm), AIC(mod_gamma), AIC(mod_weibull), AIC(mod_expon)),
  BIC = c(BIC(mod_norm), BIC(mod_lognorm), BIC(mod_gamma), BIC(mod_weibull), BIC(mod_expon)))
# Ordenação pelo melhor AIC
#print(MoCA[order(MoCA$AIC), ])


#MMSE
dados_MMSE <- as.data.frame(dados_exp_outc$MMSE_total_2)
# Ajuste das distribuições
mod_norm_MMSE <- gamlss(dados_exp_outc$MMSE_total_2 ~ 1, family = NO, data = dados_MMSE)    # Normal
mod_lognorm_MMSE <- gamlss(dados_exp_outc$MMSE_total_2 ~ 1, family = LOGNO, data = dados_MMSE) # Log-Normal
mod_gamma_MMSE <- gamlss(dados_exp_outc$MMSE_total_2 ~ 1, family = GA, data = dados_MMSE)    # Gamma
mod_weibull_MMSE <- gamlss(dados_exp_outc$MMSE_total_2 ~ 1, family = WEI, data = dados_MMSE) # Weibull
mod_expon_MMSE <- gamlss(dados_exp_outc$MMSE_total_2 ~ 1, family = EXP, data = dados_MMSE)   # Exponencial
# Tabela com critérios AIC e BIC
MMSE <- data.frame(
  Modelo = c("Normal", "Log-Normal", "Gamma", "Weibull", "Exponencial"),
  AIC = c(AIC(mod_norm_MMSE), AIC(mod_lognorm_MMSE), AIC(mod_gamma_MMSE), AIC(mod_weibull_MMSE), AIC(mod_expon_MMSE)),
  BIC = c(BIC(mod_norm_MMSE), BIC(mod_lognorm_MMSE), BIC(mod_gamma_MMSE), BIC(mod_weibull_MMSE), BIC(mod_expon_MMSE)))
# Ordenação pelo melhor AIC
#print(MMSE[order(MMSE$AIC), ])

#Qual o melhor modelo? = DPO (Double Poisson Distribution)
fit_MMSE <- fitDist(dados_exp_outc$MMSE_total_2, type = "counts", k = 2)  # usa BIC (k = log(n)) se preferir
fit_MMSE$fits

#DOM_med
dados_DOM_med <- as.data.frame(dados_exp_outc$DOM_med)
# Ajuste das distribuições
mod_norm <- gamlss(dados_exp_outc$DOM_med ~ 1, family = NO, data = dados_DOM_med)    # Normal
mod_lognorm <- gamlss(dados_exp_outc$DOM_med ~ 1, family = LOGNO, data = dados_DOM_med) # Log-Normal
mod_gamma <- gamlss(dados_exp_outc$DOM_med ~ 1, family = GA, data = dados_DOM_med)    # Gamma
mod_weibull <- gamlss(dados_exp_outc$DOM_med ~ 1, family = WEI, data = dados_DOM_med) # Weibull
mod_expon <- gamlss(dados_exp_outc$DOM_med ~ 1, family = EXP, data = dados_DOM_med)   # Exponencial
# Tabela com critérios AIC e BIC
DOM_med <- data.frame(
  Modelo = c("Normal", "Log-Normal", "Gamma", "Weibull", "Exponencial"),
  AIC = c(AIC(mod_norm), AIC(mod_lognorm), AIC(mod_gamma), AIC(mod_weibull), AIC(mod_expon)),
  BIC = c(BIC(mod_norm), BIC(mod_lognorm), BIC(mod_gamma), BIC(mod_weibull), BIC(mod_expon)))
# Ordenação pelo melhor AIC
#print(DOM_med[order(DOM_med$AIC), ])
#Qual o melhor modelo? = BCPEo (Box-Cox Power Exponential Original)
fit_DOM <- fitDist(dados_exp_outc$DOM_med, type = "realplus", k = 2)  # usa BIC (k = log(n)) se preferir
fit_DOM$fits

#NDOM_med
dados_NDOM_med <- as.data.frame(dados_exp_outc$NDOM_med_total_2)
# Ajuste das distribuições
mod_norm <- gamlss(dados_exp_outc$NDOM_med_total_2 ~ 1, family = NO, data = dados_NDOM_med)    # Normal
mod_lognorm <- gamlss(dados_exp_outc$NDOM_med_total_2 ~ 1, family = LOGNO, data = dados_NDOM_med) # Log-Normal
mod_gamma <- gamlss(dados_exp_outc$NDOM_med_total_2 ~ 1, family = GA, data = dados_NDOM_med)    # Gamma
mod_weibull <- gamlss(dados_exp_outc$NDOM_med_total_2 ~ 1, family = WEI, data = dados_NDOM_med) # Weibull
mod_expon <- gamlss(dados_exp_outc$NDOM_med_total_2 ~ 1, family = EXP, data = dados_NDOM_med)   # Exponencial
# Tabela com critérios AIC e BIC
NDOM_med <- data.frame(
  Modelo = c("Normal", "Log-Normal", "Gamma", "Weibull", "Exponencial"),
  AIC = c(AIC(mod_norm), AIC(mod_lognorm), AIC(mod_gamma), AIC(mod_weibull), AIC(mod_expon)),
  BIC = c(BIC(mod_norm), BIC(mod_lognorm), BIC(mod_gamma), BIC(mod_weibull), BIC(mod_expon)))
# Ordenação pelo melhor AIC
#print(NDOM_med[order(NDOM_med$AIC), ])

#walk1_temp_2
dados_walk1_temp_2 <- as.data.frame(dados_exp_outc$walk1_temp_2)
# Ajuste das distribuições
mod_norm <- gamlss(dados_exp_outc$walk1_temp_2_total_2 ~ 1, family = NO, data = dados_walk1_temp_2)    # Normal
mod_lognorm <- gamlss(dados_exp_outc$walk1_temp_2_total_2 ~ 1, family = LOGNO, data = dados_walk1_temp_2) # Log-Normal
mod_gamma <- gamlss(dados_exp_outc$walk1_temp_2_total_2 ~ 1, family = GA, data = dados_walk1_temp_2)    # Gamma
mod_weibull <- gamlss(dados_exp_outc$walk1_temp_2_total_2 ~ 1, family = WEI, data = dados_walk1_temp_2) # Weibull
mod_expon <- gamlss(dados_exp_outc$walk1_temp_2_total_2 ~ 1, family = EXP, data = dados_walk1_temp_2)   # Exponencial
# Tabela com critérios AIC e BIC
walk1_temp_2 <- data.frame(
  Modelo = c("Normal", "Log-Normal", "Gamma", "Weibull", "Exponencial"),
  AIC = c(AIC(mod_norm), AIC(mod_lognorm), AIC(mod_gamma), AIC(mod_weibull), AIC(mod_expon)),
  BIC = c(BIC(mod_norm), BIC(mod_lognorm), BIC(mod_gamma), BIC(mod_weibull), BIC(mod_expon)))
# Ordenação pelo melhor AIC
#print(walk1_temp_2[order(walk1_temp_2$AIC), ])

#Qual o melhor modelo? = BCPE (Box-Cox Power Exponential)
fit_walk1 <- fitDist(dados_exp_outc$walk1_temp_2, type = "realplus", k = 2)  # usa BIC (k = log(n)) se preferir
fit_walk1$fits

#walk2_temp_2
dados_walk2_temp_2 <- as.data.frame(dados_exp_outc$walk2_temp_2)
# Ajuste das distribuições
mod_norm <- gamlss(dados_exp_outc$walk2_temp_2_total_2 ~ 1, family = NO, data = dados_walk2_temp_2)    # Normal
mod_lognorm <- gamlss(dados_exp_outc$walk2_temp_2_total_2 ~ 1, family = LOGNO, data = dados_walk2_temp_2) # Log-Normal
mod_gamma <- gamlss(dados_exp_outc$walk2_temp_2_total_2 ~ 1, family = GA, data = dados_walk2_temp_2)    # Gamma
mod_weibull <- gamlss(dados_exp_outc$walk2_temp_2_total_2 ~ 1, family = WEI, data = dados_walk2_temp_2) # Weibull
mod_expon <- gamlss(dados_exp_outc$walk2_temp_2_total_2 ~ 1, family = EXP, data = dados_walk2_temp_2)   # Exponencial
mod_expon <- gamlss(dados_exp_outc$walk2_temp_2_total_2 ~ 1, family = , data = dados_walk2_temp_2)   # Exponencial
# Tabela com critérios AIC e BIC
walk2_temp_2 <- data.frame(
  Modelo = c("Normal", "Log-Normal", "Gamma", "Weibull", "Exponencial"),
  AIC = c(AIC(mod_norm), AIC(mod_lognorm), AIC(mod_gamma), AIC(mod_weibull), AIC(mod_expon)),
  BIC = c(BIC(mod_norm), BIC(mod_lognorm), BIC(mod_gamma), BIC(mod_weibull), BIC(mod_expon)))
# Ordenação pelo melhor AIC
#print(walk2_temp_2[order(walk2_temp_2$AIC), ])

#Qual o melhor modelo? = BCPE (Box-Cox Power Exponential)
fit_walk2 <- fitDist(dados_exp_outc$walk2_temp_2, type = "realplus", k = 2)  # usa BIC (k = log(n)) se preferir
fit_walk2$fits

#Print tabelas
tabela_modelos <- bind_rows(
  MoCA %>% mutate(Variável = "MoCA"),
  MMSE %>% mutate(Variável = "MMSE"),
  DOM_med %>% mutate(Variável = "Dominante"),
  NDOM_med %>% mutate(Variável = "Não Dominante"),
  walk1_temp_2 %>% mutate(Variável = "WAlk Test 1"),
  walk2_temp_2 %>% mutate(Variável = "Walk Test 2")) %>%
  arrange(Variável, AIC) %>%
  dplyr::select(Variável, dplyr::everything())
#write.csv(tabela_modelos,"tabela_modelos.csv", row.names = FALSE)
print(tabela_modelos) 

################Está a dar erro............
dados_exp_outc$walk1_pos <- na.omit(dados_exp_outc$walk1_temp_2 + 0.01)  # se houver zeros
modelo_bcpeo <- gamlss(dados_exp_outc$walk1_pos ~ 1, family = BCPEo, data = dados_exp_outc)
nu_valor <- fitted(modelo_bcpeo, "nu")[1]

# Aplicar transformação Box-Cox com ??
MMSE_transf <- if (nu_valor == 0) log(dados_exp_outc$MMSE_total_2) else
  (dados_exp_outc$MMSE_total_2^nu_valor - 1) / nu_valor



#################
# Ajustando as variáveis à distribuição Weibull com fitdistrplus
ajuste_moca <- fitdist(dados_exp_outc$MoCA_total_2, "weibull")
ajuste_mmse <- fitdist(dados_exp_outc$MMSE_total_2, "weibull")
ajuste_dom_med <- fitdist(dados_exp_outc$DOM_med, "weibull")
ajuste_ndom_med <- fitdist(dados_exp_outc$NDOM_med, "weibull")
ajuste_walk1 <- fitdist(dados_exp_outc$walk1_temp_2, "weibull")
ajuste_walk2 <- fitdist(dados_exp_outc$walk2_temp_2, "weibull")

# Extrair os resultados dos resumos de cada ajuste
weilbull_ajustes <- tibble(
  Variável = c("MoCA", "MMSE", "Dominante", "Não Dominante", "Walk Test 1", "Walk Test 2"),
  #Shape = c(
  #  ajuste_moca$parameters[1], ajuste_mmse$parameters[1], ajuste_dom_med$parameters[1],
  #  ajuste_ndom_med$parameters[1], ajuste_walk1$parameters[1], ajuste_walk2$parameters[1]),
  #Shape_SE = c(
  #  ajuste_moca$parameters[2], ajuste_mmse$parameters[2], ajuste_dom_med$parameters[2],
  #  ajuste_ndom_med$parameters[2], ajuste_walk1$parameters[2], ajuste_walk2$parameters[2]),
  #Scale = c(
  #  ajuste_moca$parameters[3], ajuste_mmse$parameters[3], ajuste_dom_med$parameters[3],
  #  ajuste_ndom_med$parameters[3], ajuste_walk1$parameters[3], ajuste_walk2$parameters[3]),
  #Scale_SE = c(
  #  ajuste_moca$parameters[4], ajuste_mmse$parameters[4], ajuste_dom_med$parameters[4],
  #  ajuste_ndom_med$parameters[4], ajuste_walk1$parameters[4], ajuste_walk2$parameters[4]),
  LogLik = c(
    ajuste_moca$loglik, ajuste_mmse$loglik, ajuste_dom_med$loglik,
    ajuste_ndom_med$loglik, ajuste_walk1$loglik, ajuste_walk2$loglik),
  AIC = c(
    ajuste_moca$aic, ajuste_mmse$aic, ajuste_dom_med$aic,
    ajuste_ndom_med$aic, ajuste_walk1$aic, ajuste_walk2$aic),
  BIC = c(
    ajuste_moca$bic, ajuste_mmse$bic, ajuste_dom_med$bic,
    ajuste_ndom_med$bic, ajuste_walk1$bic, ajuste_walk2$bic))

print(weilbull_ajustes)

# Q-Q plot para Weibull
qqcomp(ajuste_moca)
qqcomp(ajuste_mmse)
qqcomp(ajuste_dom_med)
qqcomp(ajuste_ndom_med)
qqcomp(ajuste_walk1)
qqcomp(ajuste_walk2)

#########################
#Modelos Weibull
outcomes <- c("MoCA_total_2", "MMSE_total_2", "DOM_med", "NDOM_med", "walk1_temp_2", "walk2_temp_2")
modelos_wei <- list()
for (outcome in outcomes) {
  modelo <- survreg(as.formula(paste("Surv(", outcome, ") ~ w1_100")), data = dados_exp_outc, dist = "weibull")
  modelos_wei[[outcome]] <- modelo}
print(modelos_wei)





################################

#Correlação entre variáveis
#A coefficient close to 0 (roughly between -0.20 and 0.20) suggests a weak linear relationship between two variables. A coefficient closer to positive or negative one suggests a stronger linear relationship.
correlacaoMMSE <- cor.test(dados_exp_outc$w1_100, dados_exp_outc$MMSE_total_2)
correlacaoMoCA <- cor.test(dados_exp_outc$w1_100, dados_exp_outc$MoCA_total_2)
correlacaoDOM <- cor.test(dados_exp_outc$w1_100, dados_exp_outc$DOM_med)
correlacaoNDOM <- cor.test(dados_exp_outc$w1_100, dados_exp_outc$NDOM_med)
correlacaoWalk <- cor.test(dados_exp_outc$w1_100, dados_exp_outc$walk1_temp_2)

correlacao_w1 <- tibble(
  Variável = c("MMSE", "MoCA", "DOM", "NDOM", "WAlk"),
  Coeficiente = c(correlacaoMMSE$estimate,
                  correlacaoMoCA$estimate,
                  correlacaoDOM$estimate,
                  correlacaoNDOM$estimate,
                  correlacaoWalk$estimate
                  ))


#Regressão linear
modelo_w1MMSE <- lm(dados_exp_outc$MMSE_total_2 ~ dados_exp_outc$w1_100)
modelo_w1MMSE_s <- lm(dados_exp_outc$w1_100 ~ dados_exp_outc$MMSE_total_2 + dados_exp_outc$SEXO)
modelo_w1MMSE_si <- lm(dados_exp_outc$w1_100 ~ dados_exp_outc$MMSE_total_2 + dados_exp_outc$SEXO + dados_exp_outc$idade_2014)
modelo_w1MMSE_sie <- lm(dados_exp_outc$w1_100 ~ dados_exp_outc$MMSE_total_2 + dados_exp_outc$SEXO + dados_exp_outc$idade_2014 + dados_exp_outc$ESCOL_2)
modelo_w1MMSE_sief <- lm(dados_exp_outc$w1_100 ~ dados_exp_outc$MMSE_total_2 + dados_exp_outc$SEXO + dados_exp_outc$idade_2014 + dados_exp_outc$ESCOL_2 + dados_exp_outc$intensidade_n)
modelo_w1MMSE_sieft <- lm(dados_exp_outc$w1_100 ~ dados_exp_outc$MMSE_total_2 + dados_exp_outc$SEXO + dados_exp_outc$idade_2014 + dados_exp_outc$ESCOL_2 + dados_exp_outc$intensidade_n + dados_exp_outc$trabalho_cond)

plot(dados_exp_outc$w1_100, dados_exp_outc$MMSE_total_2,
     main = "Gráfico de Dispersão com Linha de Regressão", 
     xlab = "MMSE_total_2", ylab = "w1_100", 
     pch = 19, col = "blue")  # pch=19 para pontos preenchidos
abline(modelo_w1MMSE, col = "red", lwd = 2)  # lwd=2 para uma linha mais grossa

regressao_MMSE <- tibble(
  Modelo = c("W1MMSE", "W1MMSE_s", "W1MMSE_si", "W1MMSE_sie", "W1MMSE_sief", "W1MMSE_sieft"),
  R2ajustado = c(summary(modelo_w1MMSE)$r.squared, 
                 summary(modelo_w1MMSE_s)$r.squared, 
                 summary(modelo_w1MMSE_si)$r.squared,
                 summary(modelo_w1MMSE_sie)$r.squared, 
                 summary(modelo_w1MMSE_sief)$r.squared,
                 summary(modelo_w1MMSE_sieft)$r.squared))


summary(modelo_w1MMSE)
summary(modelo_w1MMSE)$coefficients[8]
summary(modelo_w1MMSE)$r.squared

colnames(dados_exp_outc)


modelo_inicial <- lm(dados_exp_outc$w1_100 ~ dados_exp_outc$MMSE_total_2, data = dados_exp_outc) # Modelo nulo
modelo_completo <- lm(dados_exp_outc$w1_100 ~ dados_exp_outc$MMSE_total_2 + dados_exp_outc$SEXO + dados_exp_outc$idade_2014 + dados_exp_outc$ESCOL_2 + dados_exp_outc$intensidade_n + dados_exp_outc$trabalho_cond)
lm(dados_exp_outc$w1_100 ~ dados_exp_outc$MMSE_total_2 + dados_exp_outc$idade_2014 + dados_exp_outc$SEXO, data = dados_exp_outc)

step(modelo_inicial, scope = list(lower = modelo_inicial, upper = modelo_completo), direction = "forward")
