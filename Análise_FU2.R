#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("readxl")
#install.packages("ggplot2")
#install.packages("janitor")
library("dplyr")
library("tidyverse")
library("readxl")
library("ggplot2")
library("janitor")
library("grid")
library("gridExtra")
setwd("C:\\Users\\casqu\\Documentos Carol\\Internato Médico\\Internato Específico\\Estágio de Investigação\\Análise Dados_R")
dados_01 <- read.csv("Consulta_P120_CG_recodificação.csv", header = TRUE, sep = ";") %>%
  rename(ID = ï..ID1) %>%
  select(ID, NASC, SEXO, ESCOL_2, ENSINO_2, pratica_ef, lev_min, lev_dia.sem, mod_min, mod_dia.sem, vig_min, vig_dia.sem, outro_min, outro_dia.sem, outro_qual)
dados_02 <- read.csv("Consulta_P120_CG_2_recodificação.csv", header = TRUE, sep = ";") %>%
  rename(ID = ï..ID1)
poll_walk <- read_excel("tabela poluição e walkability - epiporto.xlsx") %>%
  rename(ID = ID1) %>%
  select( "ID", matches("w1|w2|w3"))
dados <- merge(dados_01, dados_02, by="ID", all= FALSE)
dados_unidos <- merge(dados, poll_walk, by="ID", all= FALSE)

#Recodificação NA
dados_unidos <- dados_unidos %>%
  mutate(NASC = as.Date(NASC)) %>%
  mutate(idade_2 = na_if(idade_2, 999)) %>%
  mutate(ESCOL_2 = na_if(ESCOL_2, 99)) %>%
  mutate(ENSINO_2 = na_if(ENSINO_2, 77)) %>%
  mutate(ENSINO_2 = na_if(ENSINO_2, 88)) %>%
  mutate(ENSINO_2 = na_if(ENSINO_2, 99)) %>%
  mutate(ESCOL_2_neu = na_if(ESCOL_2_neu, 99)) %>%
  mutate(trabalho_cond = as.numeric(trabalho_cond)) %>%
  mutate(trabalho_cond = na_if(trabalho_cond, 99)) %>%
  mutate(trabalho_sit = as.numeric(trabalho_sit)) %>%
  mutate(trabalho_sit = na_if(trabalho_sit, 88)) %>%
  mutate(trabalho_sit = na_if(trabalho_sit, 99)) %>%
  mutate(rendimento_percecao = na_if(rendimento_percecao, 9)) %>%
  mutate(MMSE_total_ant = na_if(MMSE_total_ant, 77)) %>%
  mutate(MMSE_total_ant = na_if(MMSE_total_ant, 99)) %>%
  mutate(MMSE_total_ant = na_if(MMSE_total_ant, 771)) %>%
  mutate(MMSE_total_2 = as.numeric(gsub(".*\\D(\\d+)$", "\\1", MMSE_total_2))) %>%
  mutate(MMSE_total_2 = na_if(MMSE_total_2, 99)) %>%
  mutate(MoCA_total_2 = as.numeric(gsub(".*\\D(\\d+)$", "\\1", MoCA_total_2))) %>%
  mutate(MoCA_total_2 = na_if(MoCA_total_2, 99)) %>%
  filter(MoCA_realizado_2 == 1 | is.na(MoCA_realizado_2)) %>%
  mutate(apoio_2 = na_if(apoio_2, 9)) %>%
  mutate(walk1_comp_2 = na_if(walk1_comp_2, 9)) %>%
  mutate(walk1_temp_2 = as.numeric(sub("^(\\d{2}):(\\d{2})$", "\\1", walk1_temp_2)) * 3600 + as.numeric(sub("^(\\d{2}):(\\d{2})$", "\\2", walk1_temp_2)) * 60) %>%
  mutate(walk2_comp_2 = na_if(walk2_comp_2, 9)) %>%
  mutate(walk2_temp_2 = as.numeric(sub("^(\\d{2}):(\\d{2})$", "\\1", walk2_temp_2)) * 3600 + as.numeric(sub("^(\\d{2}):(\\d{2})$", "\\2", walk2_temp_2)) * 60) %>%
  mutate(DIN1_DOM_2 = na_if(DIN1_DOM_2, 999)) %>%
  mutate(DIN1_DOM_2 = na_if(DIN1_DOM_2, 777)) %>%
  mutate(DIN1_DOM_2 = na_if(DIN1_DOM_2, 99)) %>%
  mutate(DIN2_DOM_2 = na_if(DIN2_DOM_2, 999)) %>%
  mutate(DIN2_DOM_2 = na_if(DIN2_DOM_2, 777)) %>%
  mutate(DIN2_DOM_2 = na_if(DIN2_DOM_2, 99)) %>%
  mutate(DIN3_DOM_2 = na_if(DIN3_DOM_2, 999)) %>%
  mutate(DIN3_DOM_2 = na_if(DIN3_DOM_2, 777)) %>%
  mutate(DIN3_DOM_2 = na_if(DIN3_DOM_2, 7777)) %>%
  mutate(DIN3_DOM_2 = na_if(DIN3_DOM_2, 99)) %>%
  mutate(DIN1_NDOM_2 = na_if(DIN1_NDOM_2, 999)) %>%
  mutate(DIN1_NDOM_2 = na_if(DIN1_NDOM_2, 777)) %>%
  mutate(DIN1_NDOM_2 = na_if(DIN1_NDOM_2, 99)) %>%
  mutate(DIN1_NDOM_2 = na_if(DIN1_NDOM_2, 77)) %>%
  mutate(DIN2_NDOM_2 = na_if(DIN2_NDOM_2, 999)) %>%
  mutate(DIN2_NDOM_2 = na_if(DIN2_NDOM_2, 777)) %>%
  mutate(DIN2_NDOM_2 = na_if(DIN2_NDOM_2, 7777)) %>%
  mutate(DIN2_NDOM_2 = na_if(DIN2_NDOM_2, 99)) %>%
  mutate(DIN2_NDOM_2 = na_if(DIN2_NDOM_2, 77)) %>%
  mutate(DIN3_NDOM_2 = na_if(DIN3_NDOM_2, 999)) %>%
  mutate(DIN3_NDOM_2 = na_if(DIN3_NDOM_2, 777)) %>%
  mutate(DIN3_NDOM_2 = na_if(DIN3_NDOM_2, 99)) %>%
  mutate(DIN3_NDOM_2 = na_if(DIN3_NDOM_2, 77)) %>%
  mutate(DIN3_NDOM_2 = ifelse(DIN3_NDOM_2 == 262, 26, DIN3_NDOM_2)) %>%
  mutate(across(c(DIN1_NDOM_2, DIN2_NDOM_2, DIN3_NDOM_2), ~ if_else(DIN1_NDOM_2==0 & DIN2_NDOM_2==0 & DIN3_NDOM_2==0, NA_real_, .))) %>%
  mutate(across(c(DIN1_DOM_2, DIN2_DOM_2, DIN3_DOM_2), ~ if_else(DIN1_DOM_2==0 & DIN2_DOM_2==0 & DIN3_DOM_2==0, NA_real_, .))) %>%
  mutate(DIN_DOM = ifelse(!is.na(DIN1_DOM_2) & !is.na(DIN2_DOM_2) & !is.na(DIN3_DOM_2), 1, NA)) %>%
  mutate(DIN_NDOM = ifelse(!is.na(DIN1_NDOM_2) & !is.na(DIN2_NDOM_2) & !is.na(DIN3_NDOM_2), 1, NA)) %>%
  mutate(across(matches("w1|w2|w3"), ~na_if(.x, -7777))) %>%
  mutate(across(matches("w1|w2|w3"), ~na_if(.x, -9998))) %>%
  rename_with(~ gsub("_wi_z$", "", .x))

#valores estranhos
#MoCA_total_2 = 0
#dados_unidos_MoCA_0 <- dados_unidos %>%
#  filter(MoCA_total_2 == 0) %>%
#  select(ENSINO_2, MoCA_total_2,MoCA_realizado_2, MoCA_motivo_2, MMSE_total_2)
#write.csv(dados_unidos_MoCA_0, "dados_unidos_MoCA_0.csv", row.names = FALSE)

#DIN=99, =77, =262, =0
#dados_unidos_DIN_99_0 <- dados_unidos %>%
#  filter(DIN1_DOM_2 == 99 | DIN2_DOM_2 == 99 | DIN3_DOM_2 == 99 | DIN1_NDOM_2 == 99 | DIN1_NDOM_2 == 77 | DIN2_NDOM_2 == 99 | DIN2_NDOM_2 == 77 | DIN3_NDOM_2 == 99 | DIN3_NDOM_2 == 77 | DIN3_NDOM_2 == 262 | 
#          DIN1_DOM_2 == 0 | DIN2_DOM_2 == 0 | DIN3_DOM_2 == 0 | DIN1_NDOM_2 == 0 | DIN2_NDOM_2 == 0 | DIN3_NDOM_2 == 0) %>%
#  select(contains("DIN"))
#write.csv(dados_unidos_DIN_99_0, "dados_unidos_DIN_99_0.csv", row.names = FALSE)

#DIN=99, =77, =262, =0
#dados_unidos_DIN_NA <- dados_unidos %>%
#  filter(is.na(DIN1_DOM_2) | is.na(DIN2_DOM_2) | is.na(DIN3_DOM_2) | is.na(DIN1_NDOM_2) | is.na(DIN2_NDOM_2) | is.na(DIN3_NDOM_2)) %>%
#  select(contains("DIN"))

#Mismatch NASC idade_2 (FU2: 2013-2015)
#dados_unidos_idade <- dados_unidos %>%
#  mutate(idade_2013 = floor(as.numeric(difftime(as.Date("2013-12-31"), NASC, units = "days")) / 365.25)) %>%
#  mutate(idade_2014 = floor(as.numeric(difftime(as.Date("2014-12-31"), NASC, units = "days")) / 365.25)) %>%
#  mutate(idade_2015 = floor(as.numeric(difftime(as.Date("2015-12-31"), NASC, units = "days")) / 365.25))

#Correspondência NASC e idade_2
#sum((dados_unidos_idade$idade_2 = dados_unidos_idade$idade_2013) | 
#      (dados_unidos_idade$idade_2 = dados_unidos_idade$idade_2014) | 
#      (dados_unidos_idade$idade_2 = dados_unidos_idade$idade_2015), 
#    na.rm = TRUE)

#Idades não crescentes (2013-2015)
#dados_unidos_idade <- dados_unidos_idade %>%
#  select(idade_2, idade_2013, idade_2014, idade_2015)%>%
#  filter(!(idade_2015 == idade_2014 + 1 & idade_2014 == idade_2013 + 1))
#head(dados_unidos_idade)

#Correspondência ESCOL_2 e ESCOL_2_neu = TOTAL
#sum(dados_unidos$ESCOL_2 != dados_unidos$ESCOL_2_neu, na.rm = TRUE)
#dados_unidos_ESCOL <- dados_unidos %>%
#  select(ESCOL_2, ESCOL_2_neu)%>%
#  filter(!(ESCOL_2 == ESCOL_2_neu))
#print(dados_unidos_ESCOL)
  
dados_unidos <- dados_unidos %>%
  select(-ESCOL_2_neu) %>%
  mutate(idade_2014 = floor(as.numeric(difftime(as.Date("2014-12-31"), NASC, units = "days")) / 365.25))

#Ñ missings por variável
dados_unidos_2_n_missing <- dados_unidos %>%
  summarise(across(everything(), ~ sum(!is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "Variável", values_to = "n") %>%
  filter(n > 0)
print(dados_unidos_2_n_missing, n=125)
#write.csv(dados_unidos_2_n_missing, "dados_unidos_2_n_missing.csv", row.names = FALSE)

#n Walkability
dados_unidos_walk <- dados_unidos %>%
  filter(!is.na(w1_100), !is.na(w1_300), !is.na(w1_500), !is.na(w2_100), !is.na(w2_300), !is.na(w2_500), !is.na(w3_100), !is.na(w3_300), !is.na(w3_500))

#walkability + DIN DOM
dados_exp_DOM <- filter(dados_unidos_walk, !is.na(DIN_DOM))
dados_exp_DOM_n <- dados_exp_DOM %>%
  summarise(across(everything(), ~ sum(!is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "Variável", values_to = "n") %>%
  filter(n > 0)
#print(dados_exp_DOM_n, n=120)

#walkability + DIN NDOM
dados_exp_NDOM <- filter(dados_unidos_walk, !is.na(DIN_NDOM))
dados_exp_NDOM_n <- dados_exp_NDOM %>%
  summarise(across(everything(), ~ sum(!is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "Variável", values_to = "n") %>%
  filter(n > 0)
#print(dados_exp_NDOM_n, n=120)

#walkability + walk.test1
dados_exp_walk1 <- filter(dados_unidos_walk, !is.na(walk1_comp_2))
dados_exp_walk1_n <- dados_exp_walk1 %>%
  summarise(across(everything(), ~ sum(!is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "Variável", values_to = "n") %>%
  filter(n > 0)
#print(dados_exp_walk1_n, n=120)

#walkability + walk.test2
dados_exp_walk2 <- filter(dados_unidos_walk, !is.na(walk2_comp_2))
dados_exp_walk2_n <- dados_exp_walk2 %>%
  summarise(across(everything(), ~ sum(!is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "Variável", values_to = "n") %>%
  filter(n > 0)
#print(dados_exp_walk2_n, n=120)

#walkability + MMSE
dados_exp_MMSE <- filter(dados_unidos_walk, !is.na(MMSE_total_2))
dados_exp_MMSE_n <- dados_exp_MMSE %>%
  summarise(across(everything(), ~ sum(!is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "Variável", values_to = "n") %>%
  filter(n > 0)
#print(dados_exp_MMSE_n, n=120)

#walkability + MoCA
dados_exp_MoCA <- filter(dados_unidos_walk, !is.na(MoCA_total_2))
dados_exp_MoCA_n <- dados_exp_MoCA %>%
  summarise(across(everything(), ~ sum(!is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "Variável", values_to = "n") %>%
  filter(n > 0)
#print(dados_exp_MoCA_n, n=120)

#walkability + DIN + walk.test + MMSE + MoCA
dados_exp_outc <- filter(dados_unidos_walk, !is.na(walk1_comp_2), !is.na(walk2_comp_2), !is.na(DIN1_DOM_2), !is.na(DIN1_NDOM_2), !is.na(MMSE_total_2), !is.na(MoCA_total_2))
dados_exp_outc_n <- dados_exp_outc %>%
  summarise(across(everything(), ~ sum(!is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "Variável", values_to = "n") %>%
  filter(n > 0)
#print(dados_exp_outc_n, n=120)
#write.csv(dados_exp_outc, "dados_exp_outc.csv", row.names = FALSE)
#write.csv(dados_exp_outc_n, "dados_exp_outc_n.csv", row.names = FALSE)

dados_exp_poroutc <- 
  bind_rows(
    dados_unidos_2_n_missing %>% mutate(Grupo = "Geral"),
    dados_exp_DOM_n %>% mutate(Grupo = "DIN_DOM"),
    dados_exp_NDOM_n %>% mutate(Grupo = "DIN_NDOM"),
    dados_exp_walk1_n %>% mutate(Grupo = "Walk.Test1"),
    dados_exp_walk2_n %>% mutate(Grupo = "Walk.Test2"),
    dados_exp_MMSE_n %>% mutate(Grupo = "MMSE"),
    dados_exp_MoCA_n %>% mutate(Grupo = "MoCA"),
    dados_exp_outc_n %>% mutate (Grupo = "Todos Outcomes"))%>%
  pivot_wider(names_from = Grupo, values_from = n, values_fill = list(n = 0))
#print(dados_exp_poroutc)
#write.csv(dados_exp_poroutc, "dados_exp_poroutc.csv", row.names = FALSE)
#write.csv(dados_unidos, "dados_unidos.csv", row.names = FALSE)

#Estatística Descritiva
#Exposição
walk_long <- dados_exp_outc %>%
  select(ID, matches("^w[1-3]")) %>%
  pivot_longer(cols = starts_with("w"),
               names_to = "wave_buffer",
               values_to = "walk") %>%
  separate(wave_buffer, into = c("wave", "buffer"), sep = "_", extra = "merge") %>%
  mutate(
    wave = as.numeric(sub("w", "", wave)),
  ) %>%
  select(ID, wave, buffer, walk)

#Criar os tercis para cada wave e buffer separadamente
walk_long <- walk_long %>%
  group_by(wave, buffer) %>%
  mutate(tercil = ntile(walk, 3)) %>%
  ungroup()

#Criar a trajetória de cada participante para cada buffer
trajetorias_tercis_walk <- walk_long %>%
  arrange(ID, buffer, wave) %>%
  group_by(ID, buffer) %>%
  summarise(tercil_traj = paste(tercil, collapse = "-"), .groups = "drop")

#Classificar os padrões de mudança ao longo das 4 waves
trajetorias_tercis_walk <- trajetorias_tercis_walk %>%
  mutate(
    categoria = case_when(
      tercil_traj == "1-1-1" ~ "Stable low walkability",  
      tercil_traj == "2-2-2" ~ "Stable medium walkability",  
      tercil_traj == "3-3-3" ~ "Stable high walkability",  
      grepl("^1(-1)*-2(-2)*-3(-3)*$", tercil_traj) ~ "Ascending walkability",
      grepl("^1(-1)*-2(-2)*$", tercil_traj) ~ "Ascending walkability",
      grepl("^1(-1)*-3(-3)*$", tercil_traj) ~ "Ascending walkability",
      grepl("^2(-2)*-3(-3)*$", tercil_traj) ~ "Ascending walkability",
      grepl("^3(-3)*-2(-2)*-1(-1)*$", tercil_traj) ~ "Descending walkability",
      grepl("^3(-3)*-2(-2)*$", tercil_traj) ~ "Descending walkability",
      grepl("^3(-3)*-1(-1)*$", tercil_traj) ~ "Descending walkability",
      grepl("^2(-2)*-1(-1)*$", tercil_traj) ~ "Descending walkability",
      TRUE ~ "Fluctuating walkability"))

#n por trajetória e por buffer
contagem_trajetorias_tercis_walk <- trajetorias_tercis_walk %>%
  group_by(buffer, tercil_traj) %>%
  summarise(n_participantes = n(), .groups = "drop") %>%
  arrange(buffer, desc(n_participantes))

#n e % por categoria e por buffer
contagem_categorias_trajetorias_tercis_walk <- trajetorias_tercis_walk %>%
  group_by(buffer, categoria) %>%
  summarise(n_participantes = n(), .groups = "drop") %>%
  group_by(buffer) %>%  # Group by buffer to calculate percentages within each buffer
  mutate(percentagem = round(n_participantes / sum(n_participantes) * 100, 2)) %>%
  arrange(buffer, desc(n_participantes))  # Ordenar por buffer e quantidade
#print(contagem_categorias_trajetorias_tercis_walk, n=20)

#Gráfico de barras para cada buffer
ggplot(contagem_categorias_trajetorias_tercis_walk, aes(x = categoria, y = percentagem, fill = categoria)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ buffer, labeller = labeller(buffer = function(x) paste(x, "metros"))) +  # Um gráfico para cada buffer
  coord_flip() +  # Melhor visualizaÃ§Ã£o se houver muitas categorias
  labs(title = "Categorias de caminhabilidade por buffer",
       x = "Categoria de caminhabilidade",
       y = "Percentagem de participantes") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(size = 20, hjust = 0.5, color="grey40"),
        axis.title.x = element_text(size = 12, color="grey40"),
        axis.title.y = element_text(size = 12, color="grey40"),
        axis.text.x = element_text(size = 10, color="grey40"),
        axis.text.y = element_text(size = 10, color="grey40"),
        strip.text = element_text(size = 15, color="grey40")) +
  geom_text(aes(label = paste0(round(percentagem, 2), "%"), size=12),
            position = position_stack(vjust = 0.5))
#############
#Covariáveis
#Categóricas #Sexo #Estado civil #Ensino #Condição trabalho #Situação trabalho #Rendimento perceção
tabela_resumo_cat <- bind_rows(
  dados_exp_outc %>%
    mutate(SEXO = recode(SEXO, "0" = "Feminino", "1" = "Masculino")) %>%
    count(SEXO) %>%
    mutate(Percentagem = round(n / sum(n)*100,2), Categoria = "Sexo") %>%
    rename(Variável = SEXO) %>%
    arrange(desc(n)),
  dados_exp_outc %>%
    mutate(CIVIL_2 = recode(CIVIL_2, "1" = "Casado/a",	"2" = "União de facto",	"3" = "Solteiro/a",	"4" = "Separado/a",	"5" = "Divorciado/a",	"6" = "Viúvo/a")) %>%
    count(CIVIL_2) %>%
    mutate(Percentagem = round(n / sum(n)*100,2), Categoria = "Estado civil") %>%
    rename(Variável = CIVIL_2) %>%
    arrange(desc(n)),
  dados_exp_outc %>%
    mutate(ENSINO_2 = recode(ENSINO_2, "0" = "Nenhum",	"1" = "1º ciclo",	"2" = "2º ciclo",	"3" = "3º ciclo",	"4" = "Ensino secundário",	"5" = "Ensino pós-secundário",	"6" = "Bacharelato",	"7" = "Licenciatura",	"8" = "Mestrado",	"9" = "Doutoramento")) %>%
    count(ENSINO_2) %>%
    mutate(Percentagem = round(n / sum(n)*100,2), Categoria = "Nível de ensino") %>%
    rename(Variável = ENSINO_2),
  dados_exp_outc %>%
    mutate(trabalho_cond = recode(trabalho_cond, "1" = "Empregada/o a tempo inteiro",	"2" = "Empregada/o a tempo parcial", "3" = "Empregada/o menos que o tempo parcial",	"4" = "Desempregada/o",	"5" = "Estudante",	"6" = "Incapacitada/o", "7" = "Reformada/o",	"8" = "Doméstica/o")) %>%
    count(trabalho_cond) %>%
    mutate(Percentagem = round(n / sum(n)*100,2), Categoria = "Condição perante o trabalho") %>%
    rename(Variável = trabalho_cond) %>%
    arrange(desc(n)),
  dados_exp_outc %>%
    mutate(trabalho_sit = recode(trabalho_sit, "1" = "Trabalhador(a) por conta própria", "2" = "Trabalhador(a) do Estado", "3" = "Trabalhador(a) de Empresa pública",	"4" = "Trabalhador por conta de outrem no setor privado",	"5" = "Trabalhador(a) familiar remunerado",	"6" = "Trabalhador(a) familiar não remunerado",	"7" = "Trabalhador independente"))%>%
    count(trabalho_sit) %>%
    mutate(Percentagem = round(n / sum(n)*100,2), Categoria = "Situação profissional") %>%
    rename(Variável = trabalho_sit) %>%
    arrange(desc(n)),
  dados_exp_outc %>%
    mutate(rendimento_percecao = recode(rendimento_percecao, "1" = "Insuficientes",	"2" = "Tem de ter cuidado com os gastos",	"3" = "Chega para as necessidades",	"4" = "Confortáveis"))%>%
    count(rendimento_percecao) %>%
    mutate(Percentagem = round(n / sum(n)*100,2), Categoria = "Perceção do rendimento") %>%
    rename(Variável = rendimento_percecao) %>%
    arrange(desc(n))) %>%
  select(Categoria, Variável, n, Percentagem)
#write.csv(tabela_resumo_cat, "tabela_resumo_cat.csv", row.names = FALSE)

#############
#Exercício físico
#unique(dados_exp_outc$pratica_ef)
dados_exp_outc <- dados_exp_outc %>%
  mutate(outro_min=as.numeric(outro_min)) %>%
  mutate(outro_qual=na_if(outro_qual, "")) %>%
  mutate(vig_min=as.numeric(vig_min)) %>%
  mutate(vig_min=na_if(vig_min,0)) %>%
  mutate(mod_min=na_if(mod_min,0)) %>%
  mutate(mod_min=na_if(mod_min,999)) %>%
  mutate(lev_min=na_if(lev_min,0)) %>%
  mutate(lev_min=na_if(lev_min,9999)) %>%
  mutate(outro_qual_cat = case_when(
    outro_qual %in% c("PEDALEIRA", "PILATES", "PEDALEIRA ESTATICA",
                      "YOGA, PILATES", "PILATES; GINASTICA SENIOR",
                      "PILATES ", "IOGA", "PASSEAR CAO") ~ 0.5,
    outro_qual %in% c("EQUITACAO", "MARCHA PASSO A RITMO MODERADO", "GINASIO", "ARTES MARCIAIS", "HIDROGINASTICA", "CARDIOFITNESS; HIDROGINASTICA", "GINASTICA COM TREINADOR PESSOAL", "DANCA", "TAICHI E HIDROGINASTICA", "RITMOS, HIDROGINASTICA, PILATES", "HIDRIGINASTICA", "PASSADEIRA,ABDOMINAIS", "HIDROGINASTICA 135 MINUTOS; E PILATES 45 MINUTOS", "HIDROGINï¿½STICA", "XICUNGO GINASTICA ORIENTAL ORIENTADA PARA OS MAIS IDOSOS", "EXERCICIO LIVRE; MAQUINA CULTURISMO CASA", "HIDROGINASTICA 190 MINUTOS POR SEMANA; HIDROTERAPIA 45 MINUTOS POR SEMANA", "MAQUINA ELITICA", "TAI CHI", "GINASTICA MANUTENCAO", "SANDAN; ARTES MARCIAIS", "TAI-CHI", "GIANSIO COM ORIENTACAO DE PERSONAL TRAINER", "GINASTICA E GINASIO", "PILATES; PT", "ELITICA", "BICICLETA; PESOS EM CASA", "SSD; DEFESA PESSOAL; DO TIPO ARTES MARCIAIS ", "BICICLETA GINASIO: GINASTICA LOCALIZADA", "HIDROBIKE", "GINASTICA") ~ 1,
    outro_qual %in% c("GINASIO (MUSCULACAO; CARDIO)", "CARDIOMUSCULACAO", "EXERCICIOS CARDIO ESPECIFICOS PARA A IDADE", "GINASIO (CARDIO E MUSCULACAO)", "GINASIO; CARDIO", "GINASIO - MAQINAS E CARDIO", "CARDIO FITNESS; TAPETE BICICLETA ETC", "GINASIO E SPINNING", "GINASIO CARDIO E MUSCULACAO", "SURF HORA E MEIA DE 2 EM 2 SEMANAS", "SURF HORA E MEIA DE 2 EM 2 SEMANAS  ", "AULAS EM GINASIO; BODY PUMP E TOTAL ACONDICIONAMENTO", "FUTEBOL", "GINASIO; CROSSFIT TREINO FUNCIONAL", "FUTEBOL E SQUACH") ~ 2,
    TRUE ~ as.numeric(outro_qual))) %>%
  mutate(pratica_ef_q = case_when(
    lev_min > 0 & !is.na(lev_dia.sem) ~ 1,
    mod_min > 0 & !is.na(mod_dia.sem) ~ 1,
    vig_min > 0 & !is.na(vig_dia.sem) ~ 1,
    outro_min > 0 & !is.na(outro_dia.sem) ~ 1,
    TRUE ~ 0))

total <- dados_exp_outc %>%
  summarise(Total = sum(pratica_ef_q %in% c(0, 1), na.rm = TRUE)) %>%
  pull(Total)

tabela_pratica_ef <- dados_exp_outc %>%
  summarise(
    pratica_ef_sim = sum(pratica_ef_q == 1),
    pratica_ef_nao = sum(pratica_ef_q == 0),
    lev_min_n = sum(!is.na(lev_min)),
    lev_min_na = sum(is.na(lev_min)),
    lev_dia_sem_n = sum(!is.na(lev_dia.sem)),
    mod_min_n = sum(!is.na(mod_min)),
    mod_min_na = sum(is.na(mod_min)),
    mod_dia_sem_n = sum(!is.na(mod_dia.sem)),
    vig_min_n = sum(!is.na(vig_min)),
    vig_min_na = sum(is.na(vig_min)),
    vig_dia_sem_n = sum(!is.na(vig_dia.sem)),
    outro_qual_n = sum(!is.na(outro_qual)),
    outro_min_n = sum(!is.na(outro_min)),
    outro_dia.sem_n = sum(!is.na(outro_dia.sem)),
    outro_qual_cat_n = sum(!is.na(outro_qual_cat))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variável",
    values_to = "Contagem") %>%
  mutate(
    Tipo = case_when(
      Variável %in% c("pratica_ef_sim", "pratica_ef_nao") ~ "n",
      str_detect(Variável, "_n$") ~ "n",
      str_detect(Variável, "_na$") ~ "NA",
      TRUE ~ NA_character_),
    Variável = case_when(
      Variável %in% c("pratica_ef_sim", "pratica_ef_nao") ~ Variável,
      TRUE ~ str_replace(Variável, "_n$|_na$", "")),
    Percentagem = round(Contagem / total * 100, 2)) %>%
  ungroup() %>%
  mutate(
    Variável = if_else(Tipo == "NA" & !Variável %in% c("pratica_ef_sim", "pratica_ef_nao"), paste0(Variável, "_na"), Variável)) %>%
  select(Variável, Contagem, Percentagem)
#print(tabela_pratica_ef)
#write.csv(tabela_pratica_ef, "tabela_pratica_ef.csv", row.names = FALSE)

#Intensidade pratica exercício físico
dados_exp_outc <- dados_exp_outc %>%
  mutate(
    lev_dia.sem = recode(lev_dia.sem, '1' = 7, '2' = 1),
    mod_dia.sem = recode(mod_dia.sem, '1' = 7, '2' = 1),
    vig_dia.sem = recode(vig_dia.sem, '1' = 7, '2' = 1),
    outro_dia.sem = recode(outro_dia.sem, '1' = 7, '2' = 1))

dados_exp_outc <- dados_exp_outc %>%
  mutate(intensidade_n = 
           if_else(!is.na(lev_min) & !is.na(lev_dia.sem), lev_min * lev_dia.sem * 0.5, 0) + 
           if_else(!is.na(mod_min) & !is.na(mod_dia.sem), mod_min * mod_dia.sem, 0) + 
           if_else(!is.na(vig_min) & !is.na(vig_dia.sem), vig_min * vig_dia.sem * 2, 0) +
           if_else(!is.na(outro_min) & !is.na(outro_dia.sem), outro_min * outro_dia.sem * outro_qual_cat, 0)) %>%
  mutate(intensidade_q = case_when (
    intensidade_n==0 ~ "Não pratica",
    intensidade_n>=150 & intensidade_n <=300 ~ "Cumpre",
    intensidade_n<150 ~ "Não cumpre",
    intensidade_n>300 ~ "Excede",
    TRUE ~ NA_character_))

#Check se resultados diferentes variáveis correspondem
#dados_exp_outc_ef <- dados_exp_outc %>%
#  filter(pratica_ef==0)
#unique(dados_exp_outc_ef$intensidade_q)
#dados_exp_outc_np <- dados_exp_outc %>%
#  filter(intensidade_n==0)
#unique(dados_exp_outc_np$pratica_ef)
#sum(dados_exp_outc_np$pratica_ef == 1) # 6 pessoas com dados intensidade ef incompletos

tabela_intensidade_q <- dados_exp_outc %>%
  count(intensidade_q) %>%
  mutate(percentagem = n / sum(n) * 100) %>%
  arrange(n)
#print(tabela_intensidade_q)
#write.csv(tabela_intensidade_q, "tabela_intensidade_q.csv", row.names = FALSE)

#tabela_intensidade_n <- dados_exp_outc %>%
#  summarise(
#    "Variável" = c("Intensidade EF"),
#    "Mínimo" = min(intensidade_n, na.rm = TRUE),
#    "Máximo" = max(intensidade_n, na.rm=TRUE),
#    "Mediana" = median(intensidade_n, na.rm=TRUE),
#    "IQR" = IQR(intensidade_n, na.rm=TRUE),
#    "Média" = round(mean(intensidade_n, na.rm=TRUE),2),
#    "Desvio-padrão" = round(sd(intensidade_n, na.rm=TRUE),2))
#print(tabela_intensidade_n)

#############
#Histograma idade
ggplot(dados_exp_outc, aes(x = idade_2014)) +
  geom_histogram(bins = 30, alpha = 0.6, fill="orange2", color="grey50") +
  labs(title = "Distribuição de idades em 2014", x = "Idade", y = "Frequência") +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, color = "grey40"),
    axis.text.y = element_text(size = 20, color = "grey40"),
    axis.text.x = element_text(size = 20, color = "grey40"),
    axis.title.x = element_text(size = 15, color = "grey40"),
    axis.title.y = element_text(size = 15, color = "grey40"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank())

#Histograma escolaridade
ggplot(dados_exp_outc, aes(x = ESCOL_2)) +
  geom_histogram(bins = 30, alpha = 0.6, fill="orange2", color="grey50") +
  labs(title = "Escolaridade", x = "Nº de anos de escolaridade", y = "Frequência") +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, color = "grey40"),
    axis.text.y = element_text(size = 20, color = "grey40"),
    axis.text.x = element_text(size = 20, color = "grey40"),
    axis.title.x = element_text(size = 15, color = "grey40"),
    axis.title.y = element_text(size = 15, color = "grey40"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.ticks = element_line(color = "grey40", size = 0.4),
    axis.ticks.length = unit(0.3, "cm"),
    axis.line = element_line(color = "grey40", size = 0.4)) +
  scale_x_continuous(breaks = seq(min(dados_exp_outc$ESCOL_2, na.rm = TRUE),
                                  max(dados_exp_outc$ESCOL_2, na.rm = TRUE), by = 2),expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, max(table(dados_exp_outc$ESCOL_2)), by = 20),
                     expand = c(0, 0))


#Tabela resumo: #Idade #Escolaridade #Atividade física
tabela_resumo <- bind_rows(
  dados_exp_outc %>%
    summarise(
      "Variável" = c("Idade"),
      "n" = sum(!is.na(idade_2014)),
      "Mínimo" = min(idade_2014, na.rm=TRUE),
      "Máximo" = max(idade_2014),
      "Mediana" = median(idade_2014, na.rm=TRUE),
      "IQR" = IQR(idade_2014, na.rm=TRUE),
      "Média" = round(mean(idade_2014),2),
      "Desvio.Padrão" = round(sd(idade_2014),2)),
  dados_exp_outc %>%
    summarise(
      "Variável" = c("Escolaridade"),
      "n" = sum(!is.na(ESCOL_2)),
      "Mínimo" = min(ESCOL_2, na.rm = TRUE),
      "Máximo" = max(ESCOL_2, na.rm = TRUE),
      "Mediana" = median(ESCOL_2, na.rm=TRUE),
      "IQR" = IQR(ESCOL_2, na.rm=TRUE),
      "Média" = round(mean(ESCOL_2, na.rm = TRUE),2),
      "Desvio.Padrão" = round(sd(ESCOL_2, na.rm = TRUE),2)),
  dados_exp_outc %>%
    filter(intensidade_n != 0) %>%
    summarise(
      "Variável" = c("Intensidade EF"),
      "n" = sum(!is.na(intensidade_n)),
      "Mínimo" = min(intensidade_n, na.rm = TRUE),
      "Máximo" = max(intensidade_n, na.rm=TRUE),
      "Mediana" = median(intensidade_n, na.rm=TRUE),
      "IQR" = IQR(intensidade_n, na.rm=TRUE),
      "Média" = round(mean(intensidade_n, na.rm=TRUE),2),
      "Desvio.Padrão" = round(sd(intensidade_n, na.rm=TRUE),2)))
#print(tabela_resumo)
#write.csv(tabela_resumo, "tabela_resumo.csv", row.names = FALSE)

################
#NO2
#PM2,5

poll_long <- dados_exp_outc %>%
  select(ID, contains("no2"), contains("pm25")) %>%
  pivot_longer(cols = -ID,
               names_to = "poll_wave",
               values_to = "value") %>%
  separate(poll_wave, into = c("poll", "wave"), sep = "_", extra = "merge") %>%
  mutate(
    wave = as.numeric(sub("w", "", wave)),
  ) %>%
  select(ID, poll, wave, value)

poll_long <- dados_exp_outc %>%
  select(ID, contains("no2"), contains("pm25")) %>%
  pivot_longer(cols = -ID,
               names_to = "poll_wave",
               values_to = "value") %>%
  select(ID, poll_wave, value)

tabela_poll <- bind_rows(
  dados_exp_outc %>% summarise(
      "Variável" = c("NO2_w1"),
      "Mínimo" = round(min(no2_w1, na.rm = TRUE),2),
      "Máximo" = round(max(no2_w1, na.rm = TRUE),2),
      "Mediana" = round(median(no2_w1, na.rm=TRUE),2),
      "IQR" = round(IQR(no2_w1, na.rm=TRUE),2),
      "Média" = round(mean(no2_w1, na.rm = TRUE),2),
      "Desvio-padrão" = round(sd(no2_w1, na.rm = TRUE),2)),
  dados_exp_outc %>% summarise(
    "Variável" = c("NO2_w2"),
    "Mínimo" = round(min(no2_w2, na.rm = TRUE),2),
    "Máximo" = round(max(no2_w2, na.rm = TRUE),2),
    "Mediana" = round(median(no2_w2, na.rm=TRUE),2),
    "IQR" = round(IQR(no2_w2, na.rm=TRUE),2),
    "Média" = round(mean(no2_w2, na.rm = TRUE),2),
    "Desvio-padrão" = round(sd(no2_w2, na.rm = TRUE),2)),
  dados_exp_outc %>% summarise(
    "Variável" = c("NO2_w3"),
    "Mínimo" = round(min(no2_w3, na.rm = TRUE),2),
    "Máximo" = round(max(no2_w3, na.rm = TRUE),2),
    "Mediana" = round(median(no2_w3, na.rm=TRUE),2),
    "IQR" = round(IQR(no2_w3, na.rm=TRUE),2),
    "Média" = round(mean(no2_w3, na.rm = TRUE),2),
    "Desvio-padrão" = round(sd(no2_w3, na.rm = TRUE),2)),
  dados_exp_outc %>% summarise(
    "Variável" = c("PM 2.5_w1"),
    "Mínimo" = round(min(pm25_w1, na.rm = TRUE),2),
    "Máximo" = round(max(pm25_w1, na.rm = TRUE),2),
    "Mediana" = round(median(pm25_w1, na.rm=TRUE),2),
    "IQR" = round(IQR(pm25_w1, na.rm=TRUE),2),
    "Média" = round(mean(pm25_w1, na.rm = TRUE),2),
    "Desvio-padrão" = round(sd(pm25_w1, na.rm = TRUE),2)),
  dados_exp_outc %>% summarise(
    "Variável" = c("PM 2.5_w2"),
    "Mínimo" = round(min(pm25_w2, na.rm = TRUE),2),
    "Máximo" = round(max(pm25_w2, na.rm = TRUE),2),
    "Mediana" = round(median(pm25_w2, na.rm=TRUE),2),
    "IQR" = round(IQR(pm25_w2, na.rm=TRUE),2),
    "Média" = round(mean(pm25_w2, na.rm = TRUE),2),
    "Desvio-padrão" = round(sd(pm25_w2, na.rm = TRUE),2)),
  dados_exp_outc %>% summarise(
    "Variável" = c("PM 2.5_w3"),
    "Mínimo" = round(min(pm25_w3, na.rm = TRUE),2),
    "Máximo" = round(max(pm25_w3, na.rm = TRUE),2),
    "Mediana" = round(median(pm25_w3, na.rm=TRUE),2),
    "IQR" = round(IQR(pm25_w3, na.rm=TRUE),2),
    "Média" = round(mean(pm25_w3, na.rm = TRUE),2),
    "Desvio-padrão" = round(sd(pm25_w3, na.rm = TRUE),2)))
#write.csv(tabela_poll, "tabela_poll.csv", row.names = FALSE)

#Histogramas poluição
NO2_1 <- ggplot(dados_exp_outc)+
  geom_histogram(aes(x = no2_w1), bins = 30, alpha = 0.5, fill = "#FBBAB6", color="grey50") +
  labs(x = "Valor", y = "Frequência") +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, color = "grey40"),
    axis.text.y = element_text(size = 15, color = "grey40"),
    axis.text.x = element_text(size = 15, color = "grey40"),
    axis.title.x = element_text(size = 15, color = "grey40"),
    axis.title.y = element_text(size = 15, color = "grey40"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.ticks = element_line(color = "grey40", size = 0.4),
    axis.ticks.length = unit(0.3, "cm"),
    axis.line = element_line(color = "grey40", size = 0.4)) +
  scale_x_continuous(limits = c(20,80), breaks = seq(20, 80, by = 10), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 110), breaks = seq(0, 110, by = 20), expand = c(0, 0))

NO2_2 <- ggplot(dados_exp_outc)+
  geom_histogram(aes(x = no2_w2), bins = 30, alpha = 0.5, fill = "#7FDC9B", color="grey50" ) +
  labs(x = "Valor", y = "Frequência") +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, color = "grey40"),
    axis.text.y = element_text(size = 15, color = "grey40"),
    axis.text.x = element_text(size = 15, color = "grey40"),
    axis.title.x = element_text(size = 15, color = "grey40"),
    axis.title.y = element_text(size = 15, color = "grey40"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.ticks = element_line(color = "grey40", size = 0.4),
    axis.ticks.length = unit(0.3, "cm"),
    axis.line = element_line(color = "grey40", size = 0.4)) +
  scale_x_continuous(limits = c(20,80), breaks = seq(20, 80, by = 10), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 110), breaks = seq(0, 110, by = 20), expand = c(0, 0))


NO2_3 <- ggplot(dados_exp_outc)+
  geom_histogram(aes(x = no2_w3), bins = 30, alpha = 0.5, fill = "#B0CDFF", color="grey50" ) +
  labs(x = "Valor", y = "Frequência") +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, color = "grey40"),
    axis.text.y = element_text(size = 15, color = "grey40"),
    axis.text.x = element_text(size = 15, color = "grey40"),
    axis.title.x = element_text(size = 15, color = "grey40"),
    axis.title.y = element_text(size = 15, color = "grey40"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.ticks = element_line(color = "grey40", size = 0.4),
    axis.ticks.length = unit(0.3, "cm"),
    axis.line = element_line(color = "grey40", size = 0.4)) +
  scale_x_continuous(limits = c(20,80), breaks = seq(20, 80, by = 10), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 110), breaks = seq(0, 110, by = 20), expand = c(0, 0))


PM25_1 <- ggplot(dados_exp_outc)+
  geom_histogram(aes(x = pm25_w1), bins = 30, alpha = 0.5, fill = "#FBBAB6", color="grey50") +
  labs(x = "Valor", y = "Frequência") +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, color = "grey40"),
    axis.text.y = element_text(size = 15, color = "grey40"),
    axis.text.x = element_text(size = 15, color = "grey40"),
    axis.title.x = element_text(size = 15, color = "grey40"),
    axis.title.y = element_text(size = 15, color = "grey40"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.ticks = element_line(color = "grey40", size = 0.4),
    axis.ticks.length = unit(0.3, "cm"),
    axis.line = element_line(color = "grey40", size = 0.4)) +
  scale_x_continuous(limits = c(10, 30), breaks = seq(0, 30, by = 5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, by = 20), expand = c(0, 0))


PM25_2 <- ggplot(dados_exp_outc)+
  geom_histogram(aes(x = pm25_w2), bins = 30, alpha = 0.5, fill = "#7FDC9B", color="grey50" ) +
  labs(x = "Valor", y = "Frequência") +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, color = "grey40"),
    axis.text.y = element_text(size = 15, color = "grey40"),
    axis.text.x = element_text(size = 15, color = "grey40"),
    axis.title.x = element_text(size = 15, color = "grey40"),
    axis.title.y = element_text(size = 15, color = "grey40"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.ticks = element_line(color = "grey40", size = 0.4),
    axis.ticks.length = unit(0.3, "cm"),
    axis.line = element_line(color = "grey40", size = 0.4)) +
  scale_x_continuous(limits = c(10, 30), breaks = seq(0, 30, by = 5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, by = 20), expand = c(0, 0))

PM25_3 <- ggplot(dados_exp_outc)+
  geom_histogram(aes(x = pm25_w3), bins = 30, alpha = 0.5, fill = "#B0CDFF", color="grey50" ) +
  labs(x = "Valor", y = "Frequência") +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, color = "grey40"),
    axis.text.y = element_text(size = 15, color = "grey40"),
    axis.text.x = element_text(size = 15, color = "grey40"),
    axis.title.x = element_text(size = 15, color = "grey40"),
    axis.title.y = element_text(size = 15, color = "grey40"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.ticks = element_line(color = "grey40", size = 0.4),
    axis.ticks.length = unit(0.3, "cm"),
    axis.line = element_line(color = "grey40", size = 0.4)) +
  scale_x_continuous(limits = c(10, 30), breaks = seq(0, 30, by = 5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, by = 20), expand = c(0, 0))

no <- textGrob("NO", gp = gpar(fontsize = 20, col = "grey40", fontface = "bold"))
pm25 <- textGrob("PM 2.5", gp = gpar(fontsize = 20, col = "grey40", fontface = "bold"))

ensaio <- arrangeGrob(
  textGrob("Baseline", gp = gpar(fontsize = 20, col = "grey40")),
  textGrob("Follow-up 1", gp = gpar(fontsize = 20, col = "grey40")),
  textGrob("Follow-up 2", gp = gpar(fontsize = 20, col = "grey40")),
  ncol = 3)

grid.arrange(
  ensaio,
  no,
  arrangeGrob(NO2_1, NO2_2, NO2_3, ncol = 3),
  pm25,
  arrangeGrob(PM25_1, PM25_2, PM25_3, ncol = 3),
  nrow = 5,  # Four rows total
  heights = c(0.2, 0.2, 1, 0.2, 1))

##############
#Outcomes
#Tabela resumo MMSE e MoCA
tabela_MMSE_MoCA <- bind_rows(
  dados_exp_outc %>%
    summarise(
      "Variável" = c("MMSE"),
      "Mínimo" = min(MMSE_total_2, na.rm = TRUE),
      "Máximo" = max(MMSE_total_2, na.rm = TRUE),
      "Mediana" = median(MMSE_total_2, na.rm=TRUE),
      "IQR" = IQR(MMSE_total_2, na.rm=TRUE),
      "Média" = round(mean(MMSE_total_2, na.rm = TRUE),2),
      "Desvio-padrão" = round(sd(MMSE_total_2, na.rm = TRUE),2)),
  dados_exp_outc %>% summarise(
    "Variável" = c("MoCA"),
    "Mínimo" = min(MoCA_total_2, na.rm = TRUE),
    "Máximo" = max(MoCA_total_2, na.rm = TRUE),
    "Mediana" = median(MoCA_total_2, na.rm=TRUE),
    "IQR" = IQR(MoCA_total_2, na.rm=TRUE),
    "Média" = round(mean(MoCA_total_2, na.rm = TRUE),2),
    "Desvio-padrão" = round(sd(MoCA_total_2, na.rm = TRUE),2)))
#write.csv(tabela_MMSE_MoCA, "tabela_MMSE_MoCA.csv", row.names = FALSE)

#Histograma MMSE
ggplot(dados_exp_outc, aes(x = MMSE_total_2)) +
  geom_histogram(bins = 30, alpha = 0.6, fill="purple3", color="grey50") +
  labs(title = "Distribuição de MMSE", x = "Pontuação total MMSE", y = "Frequência") +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, color = "grey40"),
    axis.text.y = element_text(size = 20, color = "grey40"),
    axis.text.x = element_text(size = 20, color = "grey40"),
    axis.title.x = element_text(size = 15, color = "grey40"),
    axis.title.y = element_text(size = 15, color = "grey40"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank())

#Histograma MoCA
ggplot(dados_exp_outc, aes(x = MoCA_total_2)) +
  geom_histogram(bins = 30, alpha = 0.6, fill="orange2", color="grey50") +
  labs(title = "Distribuição de MoCA", x = "Pontuação total MoCA", y = "Frequência") +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, color = "grey40"),
      axis.text.y = element_text(size = 20, color = "grey40"),
    axis.text.x = element_text(size = 20, color = "grey40"),
    axis.title.x = element_text(size = 15, color = "grey40"),
    axis.title.y = element_text(size = 15, color = "grey40"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank())

#Gráfico dispersão idade*MMSE
ggplot(dados_exp_outc, aes(x = idade_2014, y = MMSE_total_2)) +
  geom_point(color="green4") +
  labs(title = "Gráfico de Dispersão MMSE", y = "MMSE_total", x = "Idade") +
  scale_x_continuous(limits = c(59, NA))+
  scale_y_continuous(limits = c(20, 30), breaks = seq(20, 30, 5),minor_breaks = seq(20,30,1))+
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, color = "grey40"),
    axis.text.y = element_text(size = 20, color = "grey40"),
    axis.text.x = element_text(size = 20, color = "grey40"),
    axis.title.x = element_text(size = 15, color = "grey40"),
    axis.title.y = element_text(size = 15, color = "grey40"),
    panel.grid.major = element_line(colour = "grey"),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_line(colour = "grey"))

#Gráfico dispersão idade*MoCA
ggplot(dados_exp_outc, aes(x = idade_2014, y = MoCA_total_2)) +
  geom_point(color="green4") +
  labs(title = "Gráfico de Dispersão MoCA", y = "MoCA_total", x = "Idade") +
  scale_x_continuous(limits = c(NA, 60)) +
  scale_y_continuous(limits = c(20, 30), breaks = seq(20, 30, 5),minor_breaks = seq(20,30,1))+
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, color = "grey40"),
    axis.text.y = element_text(size = 20, color = "grey40"),
    axis.text.x = element_text(size = 20, color = "grey40"),
    axis.title.x = element_text(size = 15, color = "grey40"),
    axis.title.y = element_text(size = 15, color = "grey40"),
    panel.grid.major = element_line(colour = "grey"),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_line(colour = "grey"))

#Tabela resumo Teste Preensão
dados_DIN <- dados_exp_outc %>%
  select(DIN1_DOM_2, DIN2_DOM_2, DIN3_DOM_2, DIN1_NDOM_2, DIN2_NDOM_2, DIN3_NDOM_2) %>%
  pivot_longer(cols = everything(), values_to = "Categoria", names_to = "Variável") #%>%
  #filter(!is.na(Categoria)) # Remover valores ausentes, se houver

tabela_freq_preensao <- dados_DIN %>%
  group_by(Variável, Categoria) %>%
  summarise(Frequência = n(), .groups = "drop") %>%
  mutate(Percentagem = round(Frequência / sum(Frequência) * 100, 2)) #%>%
  #arrange(desc(Frequência))
#print(tabela_freq_preensao)
#write.csv(tabela_freq_preensao, "tabela_freq_preensao.csv", row.names = FALSE)

tabela_preensao <- bind_rows(
  dados_exp_outc %>%
    summarise(
      "Variável" = c("DOM_1"),
      "Mínimo" = min(DIN1_DOM_2, na.rm = TRUE),
      "Máximo" = max(DIN1_DOM_2, na.rm = TRUE),
      "Mediana" = median(DIN1_DOM_2, na.rm=TRUE),
      "IQR" = IQR(DIN1_DOM_2, na.rm=TRUE),
      "Média" = round(mean(DIN1_DOM_2, na.rm = TRUE),2),
      "Desvio-padrão" = round(sd(DIN1_DOM_2, na.rm = TRUE),2)),
  dados_exp_outc %>% summarise(
    "Variável" = c("DOM_2"),
    "Mínimo" = min(DIN2_DOM_2, na.rm = TRUE),
    "Máximo" = max(DIN2_DOM_2, na.rm = TRUE),
    "Mediana" = median(DIN2_DOM_2, na.rm=TRUE),
    "IQR" = IQR(DIN2_DOM_2, na.rm=TRUE),
    "Média" = round(mean(DIN2_DOM_2, na.rm = TRUE),2),
    "Desvio-padrão" = round(sd(DIN2_DOM_2, na.rm = TRUE),2)),
  dados_exp_outc %>% summarise(
    "Variável" = c("DOM_3"),
    "Mínimo" = min(DIN3_DOM_2, na.rm = TRUE),
    "Máximo" = max(DIN3_DOM_2, na.rm = TRUE),
    "Mediana" = median(DIN3_DOM_2, na.rm=TRUE),
    "IQR" = IQR(DIN3_DOM_2, na.rm=TRUE),
    "Média" = round(mean(DIN3_DOM_2, na.rm = TRUE),2),
    "Desvio-padrão" = round(sd(DIN3_DOM_2, na.rm = TRUE),2)),
  dados_exp_outc %>% summarise(
    "Variável" = c("nDOM_1"),
    "Mínimo" = min(DIN1_NDOM_2, na.rm = TRUE),
    "Máximo" = max(DIN1_NDOM_2, na.rm = TRUE),
    "Mediana" = median(DIN1_NDOM_2, na.rm=TRUE),
    "IQR" = IQR(DIN1_NDOM_2, na.rm=TRUE),
    "Média" = round(mean(DIN1_NDOM_2, na.rm = TRUE),2),
    "Desvio-padrão" = round(sd(DIN1_NDOM_2, na.rm = TRUE),2)),
  dados_exp_outc %>% summarise(
    "Variável" = c("nDOM_2"),
    "Mínimo" = min(DIN2_NDOM_2, na.rm = TRUE),
    "Máximo" = max(DIN2_NDOM_2, na.rm = TRUE),
    "Mediana" = median(DIN2_NDOM_2, na.rm=TRUE),
    "IQR" = IQR(DIN2_NDOM_2, na.rm=TRUE),
    "Média" = round(mean(DIN2_NDOM_2, na.rm = TRUE),2),
    "Desvio-padrão" = round(sd(DIN2_NDOM_2, na.rm = TRUE),2)),
  dados_exp_outc %>% summarise(
    "Variável" = c("nDOM_3"),
    "Mínimo" = min(DIN3_NDOM_2, na.rm = TRUE),
    "Máximo" = max(DIN3_NDOM_2, na.rm = TRUE),
    "Mediana" = median(DIN3_NDOM_2, na.rm=TRUE),
    "IQR" = IQR(DIN3_NDOM_2, na.rm=TRUE),
    "Média" = round(mean(DIN3_NDOM_2, na.rm = TRUE),2),
    "Desvio-padrão" = round(sd(DIN3_NDOM_2, na.rm = TRUE),2)))
#write.csv(tabela_preensao, "tabela_preensao.csv", row.names = FALSE)
#preensao <- dados_exp_outc %>%
#  select(contains("DIN")) %>%
#  mutate(across(c(DIN1_NDOM_2, DIN2_NDOM_2, DIN3_NDOM_2), ~ if_else(DIN1_NDOM_2==0 & DIN2_NDOM_2==0 & DIN3_NDOM_2==0, NA_real_, .))) %>%
#  mutate(across(c(DIN1_DOM_2, DIN2_DOM_2, DIN3_DOM_2), ~ if_else(DIN1_DOM_2==0 & DIN2_DOM_2==0 & DIN3_DOM_2==0, NA_real_, .)))
#filter(if_any(contains("NDOM"), ~ . == 0))

#Histogramas força preensão palmar
D1D <- ggplot(dados_exp_outc)+
  geom_histogram(aes(x = DIN1_DOM_2), bins = 30, alpha = 0.5, fill = "#7FDC9B", color="grey50") +
  labs(x = "Força (Kg)", y = "Frequência") +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, color = "grey40"),
    axis.text.y = element_text(size = 20, color = "grey40"),
    axis.text.x = element_text(size = 20, color = "grey40"),
    axis.title.x = element_text(size = 15, color = "grey40"),
    axis.title.y = element_text(size = 15, color = "grey40"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank())

D2D <- ggplot(dados_exp_outc)+
  geom_histogram(aes(x = DIN2_DOM_2), bins = 30, alpha = 0.5, fill = "#FBBAB6", color="grey50" ) +
  labs(x = "Força (Kg)", y = "Frequência") +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, color = "grey40"),
    axis.text.y = element_text(size = 20, color = "grey40"),
    axis.text.x = element_text(size = 20, color = "grey40"),
    axis.title.x = element_text(size = 15, color = "grey40"),
    axis.title.y = element_text(size = 15, color = "grey40"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank())

D3D <- ggplot(dados_exp_outc)+
  geom_histogram(aes(x = DIN3_DOM_2), bins = 30, alpha = 0.5, fill = "#B0CDFF", color="grey50" ) +
  labs(x = "Força (Kg)", y = "Frequência") +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, color = "grey40"),
    axis.text.y = element_text(size = 20, color = "grey40"),
    axis.text.x = element_text(size = 20, color = "grey40"),
    axis.title.x = element_text(size = 15, color = "grey40"),
    axis.title.y = element_text(size = 15, color = "grey40"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank())

D1ND <- ggplot(dados_exp_outc)+
  geom_histogram(aes(x = DIN1_NDOM_2), bins = 30, alpha = 0.5, fill = "#7FDC9B", color="grey50") +
  labs(x = "Força (Kg)", y = "Frequência") +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, color = "grey40"),
    axis.text.y = element_text(size = 20, color = "grey40"),
    axis.text.x = element_text(size = 20, color = "grey40"),
    axis.title.x = element_text(size = 15, color = "grey40"),
    axis.title.y = element_text(size = 15, color = "grey40"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank())

D2ND <- ggplot(dados_exp_outc)+
  geom_histogram(aes(x = DIN2_NDOM_2), bins = 30, alpha = 0.5, fill = "#FBBAB6", color="grey50" ) +
  labs(x = "Força (Kg)", y = "Frequência") +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, color = "grey40"),
    axis.text.y = element_text(size = 20, color = "grey40"),
    axis.text.x = element_text(size = 20, color = "grey40"),
    axis.title.x = element_text(size = 15, color = "grey40"),
    axis.title.y = element_text(size = 15, color = "grey40"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank())

D3ND <- ggplot(dados_exp_outc)+
  geom_histogram(aes(x = DIN3_NDOM_2), bins = 30, alpha = 0.5, fill = "#B0CDFF", color="grey50" ) +
  labs(x = "Força (Kg)", y = "Frequência") +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, color = "grey40"),
    axis.text.y = element_text(size = 20, color = "grey40"),
    axis.text.x = element_text(size = 20, color = "grey40"),
    axis.title.x = element_text(size = 15, color = "grey40"),
    axis.title.y = element_text(size = 15, color = "grey40"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank())

mao_dom <- textGrob("Mão dominante", gp = gpar(fontsize = 20, col = "grey40", fontface = "bold"))
mao_ndom <- textGrob("Mão não dominante", gp = gpar(fontsize = 20, col = "grey40", fontface = "bold"))

ensaio <- arrangeGrob(
  textGrob("Ensaio 1", gp = gpar(fontsize = 20, col = "grey40")),
  textGrob("Ensaio 2", gp = gpar(fontsize = 20, col = "grey40")),
  textGrob("Ensaio 3", gp = gpar(fontsize = 20, col = "grey40")),
  ncol = 3)

grid.arrange(
  ensaio,
  mao_dom,
  arrangeGrob(D1D, D2D, D3D, ncol = 3),
  mao_ndom,
  arrangeGrob(D1ND, D2ND, D3ND, ncol = 3),
  nrow = 5,  # Four rows total
  heights = c(0.2, 0.2, 1, 0.2, 1))

#Teste caminhada
tabela_tcaminhada <- bind_rows(
  dados_exp_outc %>%
    summarise(
      "Variável" = c("Ensaio 1"),
      "Mínimo" = min(walk1_temp_2, na.rm = TRUE),
      "Máximo" = max(walk1_temp_2, na.rm = TRUE),
      "Mediana" = median(walk1_temp_2, na.rm=TRUE),
      "IQR" = IQR(walk1_temp_2, na.rm=TRUE),
      "Média" = round(mean(walk1_temp_2, na.rm = TRUE),2),
      "Desvio-padrão" = round(sd(walk1_temp_2, na.rm = TRUE),2)),
  dados_exp_outc %>% summarise(
    "Variável" = c("Ensaio 2"),
    "Mínimo" = min(walk2_temp_2, na.rm = TRUE),
    "Máximo" = max(walk2_temp_2, na.rm = TRUE),
    "Mediana" = median(walk2_temp_2, na.rm=TRUE),
    "IQR" = IQR(walk2_temp_2, na.rm=TRUE),
    "Média" = round(mean(walk2_temp_2, na.rm = TRUE),2),
    "Desvio-padrão" = round(sd(walk2_temp_2, na.rm = TRUE),2)))
#write.csv(tabela_tcaminhada, "tabela_tcaminhada.csv", row.names = FALSE)

#Histogramas teste caminhada
TC1 <- ggplot(dados_exp_outc)+
  geom_histogram(aes(x = DIN1_DOM_2), bins = 30, alpha = 0.5, fill = "#FBBAB6", color="grey50") +
  labs(title = "Ensaio 1", x = "Tempo (s)", y = "Frequência") +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, color = "grey40"),
    axis.text.y = element_text(size = 20, color = "grey40"),
    axis.text.x = element_text(size = 20, color = "grey40"),
    axis.title.x = element_text(size = 15, color = "grey40"),
    axis.title.y = element_text(size = 15, color = "grey40"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank())

TC2 <- ggplot(dados_exp_outc)+
  geom_histogram(aes(x = DIN2_DOM_2), bins = 30, alpha = 0.5, fill = "#7FDC9B", color="grey50" ) +
  labs(title = "Ensaio 2", x = "Tempo (s)", y = "Frequência") +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, color = "grey40"),
    axis.text.y = element_text(size = 20, color = "grey40"),
    axis.text.x = element_text(size = 20, color = "grey40"),
    axis.title.x = element_text(size = 15, color = "grey40"),
    axis.title.y = element_text(size = 15, color = "grey40"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank())

titulo <- textGrob("Teste de caminhada", gp = gpar(fontsize = 20, col = "grey40", fontface = "bold"))

grid.arrange(
  titulo,
  arrangeGrob(TC1, TC2, ncol = 2),
  heights = c(0.2, 1))