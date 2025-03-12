#C�digo Mariana Castro
# Tercis - exposição - NDVI

# Carregar pacotes
library(dplyr)
library(tidyr)
library(ggplot2)

# Transformar os dados para formato longo
dados_long_NDVI <- dados_todos %>%
  pivot_longer(cols = starts_with("w"),  # Seleciona todas as colunas que começam com "w"
               names_to = "wave_buffer",
               values_to = "NDVI") %>%
  separate(wave_buffer, into = c("wave", "ndvi", "mean", "buffer"), sep = "_", extra = "merge") %>%
  mutate(
    wave = as.numeric(sub("w", "", wave)),  # Remove "w" e converte para número
    buffer = paste0("buffer_", buffer)  # Mantém a identificação do buffer
  ) %>%
  select(ID1, wave, buffer, NDVI)  # Mantém apenas colunas relevantes

# Criar os tercis para cada wave e buffer separadamente
dados_long_NDVI <- dados_long_NDVI %>%
  group_by(wave, buffer) %>%
  mutate(tercil = ntile(NDVI, 3)) %>%
  ungroup()

# Criar a trajetória de cada participante para cada buffer
trajetorias_tercis_NDVI <- dados_long_NDVI %>%
  arrange(ID1, buffer, wave) %>%  # Certificar-se da ordenação correta
  group_by(ID1, buffer) %>%
  summarise(tercil_traj = paste(tercil, collapse = "-"), .groups = "drop") 

# Classificar os padrões de mudança ao longo das 6 waves
trajetorias_tercis_NDVI <- trajetorias_tercis_NDVI %>%
  mutate(
    categoria = case_when(
      tercil_traj == "1-1-1-1-1-1" ~ "Stable low NDVI",  
      tercil_traj == "2-2-2-2-2-2" ~ "Stable medium NDVI",  
      tercil_traj == "3-3-3-3-3-3" ~ "Stable high NDVI",  
      grepl("^1(-1)*-2(-2)*-3(-3)*$", tercil_traj) ~ "Ascending NDVI",
      grepl("^3(-3)*-2(-2)*-1(-1)*$", tercil_traj) ~ "Descending NDVI",
      grepl("1-3|3-1", tercil_traj) ~ "Fluctuating NDVI", # Exemplo de novo padrão
      TRUE ~ "Other pattern"
    )
  )

# Visualizar os resultados
print(trajetorias_tercis_NDVI)

library(writexl)
write_xlsx(trajetorias_tercis_NDVI, "trajetorias_tercis_NDVI.xlsx")

# Contar quantos participantes há em cada categoria por buffer
contagem_categorias_trajetorias_tercis_NDVI <- trajetorias_tercis_NDVI %>%
  group_by(buffer, categoria) %>%
  summarise(n_participantes = n(), .groups = "drop") %>%
  arrange(buffer, desc(n_participantes))  # Ordenar por buffer e quantidade

print(contagem_categorias_trajetorias_tercis_NDVI)

library(writexl)
write_xlsx(contagem_categorias_trajetorias_tercis_NDVI, "contagem_categorias_trajetorias_tercis_NDVI.xlsx")

# Contar quantos participantes há em cada trajetória por buffer
contagem_trajetorias_tercis_NDVI <- trajetorias_tercis_NDVI %>%
  group_by(buffer, tercil_traj) %>%
  summarise(n_participantes = n(), .groups = "drop") %>%
  arrange(buffer, desc(n_participantes))

print(contagem_trajetorias_tercis_NDVI)

library(writexl)
write_xlsx(contagem_trajetorias_tercis_NDVI, "contagem_trajetorias_tercis_NDVI.xlsx")

# Visualização: Gráfico de barras para cada buffer
ggplot(contagem_categorias_trajetorias_tercis_NDVI, aes(x = categoria, y = n_participantes, fill = categoria)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ buffer) +  # Um gráfico para cada buffer
  coord_flip() +  # Melhor visualização se houver muitas categorias
  labs(title = "NDVI Category Distribution by Buffer",
       x = "NDVI Category",
       y = "Number of Participants") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legenda desnecessária
