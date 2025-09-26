# Librerías
library(bibliometrix)
library(tidyverse)
library(ggsci)
library(treemapify)

theme_set(theme_light())

# Carga de fuentes 
base_scopus_sub <- convert2df(file = "fuentes/scopus_movilidad_subjetiva.bib", 
                dbsource = "scopus", 
                format = "bibtex")

base_scopus_mov <- convert2df(file = "fuentes/scopus_movilidad_social.bib", 
                               dbsource = "scopus", 
                               format = "bibtex")

base_scopus_sub <- base_scopus_sub %>% 
  mutate(search_type = "Movilidad subjetiva")
base_scopus_mov <- base_scopus_mov %>% 
  mutate(search_type = "Movilidad social")

base_scopus <- bind_rows(base_scopus_sub, base_scopus_mov)

base_scopus <- base_scopus %>%
  distinct(DI, .keep_all = TRUE)



results <- biblioAnalysis(base_scopus_mov, sep = ";")

S <- summary(object = results, k = 10, pause = FALSE)


# Publicaciones por año
base_scopus %>%
  filter(PY >= 2000) %>%
  group_by(PY, search_type) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = PY, y = n, fill = search_type)) +
  geom_col() +
  scale_fill_atlassian() +
  labs(title = "Publicaciones sobre movilidad social subjetiva por año",
       caption = "Fuente: elaboración propia en base SCOPUS") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = seq(2000, 2025, 5)) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank()
  )

ggsave("graficos/publicaciones_anio.png", width = 8, height = 5)

#Publicaciones por país
base_scopus_sub <- metaTagExtraction(base_scopus_sub, Field = "AU_CO", sep = ";")

pub_pais <- base_scopus_sub %>%
  filter(!is.na(AU_CO)) %>%
  separate_rows(AU_CO, sep = ";") %>%
  mutate(AU_CO = trimws(AU_CO))

pub_pais %>%
  count(AU_CO, sort = TRUE) %>% 
  top_n(10) %>% 
  ggplot(aes(area = n, fill = AU_CO, label = paste(AU_CO, n, sep = "\n"))) +
  geom_treemap() +
  geom_treemap_text(colour = "white", size = 20) +
  scale_fill_d3("category10") +
  labs(title = "Países con más publicaciones sobre movilidad subjetiva",
       caption = "Fuente: elaboración propia en base SCOPUS") +
  theme(legend.position = "none",
        plot.title = element_text(size = 16))
  
ggsave("graficos/publicaciones_pais.png", width = 8, height = 5)

base_scopus_mov <- metaTagExtraction(base_scopus_mov, Field = "AU_CO", sep = ";")

pub_pais <- base_scopus_mov %>%
  filter(!is.na(AU_CO)) %>%
  separate_rows(AU_CO, sep = ";") %>%
  mutate(AU_CO = trimws(AU_CO))

pub_pais %>%
  count(AU_CO, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(area = n, fill = AU_CO, label = paste(AU_CO, n, sep = "\n"))) +
  geom_treemap() +
  geom_treemap_text(colour = "white", size = 20) +
  scale_fill_d3("category10") +
  labs(title = "Países con más publicaciones sobre movilidad social",
       caption = "Fuente: elaboración propia en base SCOPUS") +
  theme(legend.position = "none",
        plot.title = element_text(size = 16))

ggsave("graficos/publicaciones_pais_mov.png", width = 8, height = 5)


# Palabras clave
keywords <- base_scopus_sub %>%
  filter(!is.na(DE)) %>%
  separate_rows(DE, sep = ";") %>%      # separa keywords múltiples
  mutate(DE = trimws(tolower(DE))) %>%  # limpio espacios y paso a minúsculas
  count(DE, sort = TRUE) %>% 
  filter(!DE %in% c("article", "study", "human", "social mobility"))


