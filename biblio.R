# Librerías
library(bibliometrix)
library(tidyverse)

# Carga de fuentes 
base_scopus <- convert2df(file = "fuentes/scopus_movilidad_subjetiva.bib", 
                dbsource = "scopus", 
                format = "bibtex")

results <- biblioAnalysis(base_scopus, sep = ";")

S <- summary(object = results, k = 10, pause = FALSE)


# Publicaciones por año
base_scopus %>%
  filter(PY >= 2000) %>%
  group_by(PY) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = PY, y = n)) +
  geom_col(fill = "lightblue") +
  labs(title = "Publicaciones por año",
       x = "Año de publicación",
       y = "Número de publicaciones") +
  theme_minimal()


#Publicaciones por país
base_scopus <- metaTagExtraction(base_scopus, Field = "AU_CO", sep = ";")


base_scopus %>%
  filter(PY >= 2000) %>%
  group_by(AU_CO) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  slice_head(n = 10) 



%>%
  ggplot(aes(x = reorder(C1, n), y = n)) +
  geom_col(fill = "lightgreen") +
  coord_flip() +
  labs(title = "Top 10 países por número de publicaciones",
       x = "País",
       y = "Número de publicaciones") +
  theme_minimal()
