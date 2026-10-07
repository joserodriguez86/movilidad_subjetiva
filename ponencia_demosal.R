# Cargar librerías necesarias --------------
pacman::p_load(tidyverse, haven, ggsci, jtools, nnet, gtsummary, huxtable,
               marginaleffects, survey, sjPlot, bibliometrix, treemapify, patchwork, ggtext, ggoxford)

theme_set(theme_light())

# Carga de fuentes ----------------
base_scopus_sub <- convert2df(file = "fuentes/scopus_movilidad_subjetiva_2026.bib", 
                              dbsource = "scopus", 
                              format = "bibtex")

base_scopus_mov <- convert2df(file = "fuentes/scopus_movilidad_social_2026.bib", 
                              dbsource = "scopus", 
                              format = "bibtex")

base_scopus_sub <- base_scopus_sub %>% 
  mutate(search_type = "Movilidad subjetiva") %>%
  distinct(DI, .keep_all = TRUE)

base_scopus_mov <- base_scopus_mov %>% 
  mutate(search_type = "Movilidad social") %>%
  distinct(DI, .keep_all = TRUE)

base_scopus <- bind_rows(base_scopus_sub, base_scopus_mov)

argentina2024 <- read_dta("fuentes/base2024_argentina.dta")

wvs <- readRDS("fuentes/WVS_Cross-National_Wave_7_rds_v6_0.rds")

load("fuentes/latinobarometro_select.RData")

cols <- pal_locuszoom()(3)   # Cambiá el "3" si hay más niveles
cols <- cols[c(3, 2, 1)]            # Reordená a gusto
cols5<- pal_locuszoom()(5) 
cols5 <- cols5[c(5, 4, 3, 2, 1)]

paises <- c("ARG", "BOL", "CHL", "URY", "BRA", "PER", "COL", "ECU", "VEN", "GTM", "MEX", "NIC")


# Variables -----------------

argentina2024 <- argentina2024 %>% 
  mutate(
    
    # Movilidad subjetiva
    movilidad_sub = ifelse(p15 < 4, p15, NA_real_),
    movilidad_sub = factor(
      movilidad_sub,
      levels = c(2, 1, 3),
      labels = c("Ascendente", "Reproducción", "Descendente")
    ),
    movilidad_sub2 = ifelse(movilidad_sub == "Descendente", 1, 0),
    
    # Cohorte de nacimiento
    cohorte = case_when(
      o3_1 < 25 ~ "2000",
      o3_1 >= 25 & o3_1 < 35 ~ "1990",
      o3_1 >= 35 & o3_1 < 45 ~ "1980",
      o3_1 >= 45 & o3_1 < 55 ~ "1970",
      o3_1 >= 55 & o3_1 < 65 ~ "1960",
      o3_1 >= 65 & o3_1 < 75 ~ "1950",
      o3_1 >= 75 ~ "1940"
    ) |> factor(),
    
    # Género
    genero = factor(genero, labels = c("Varón", "Mujer")),
    
    # Colapso de categoría ocupacional
    categoria = case_when(
      categoria_ocupacional == 1 ~ 1,
      categoria_ocupacional == 2 ~ 2,
      categoria_ocupacional >= 3 ~ 3
    ),
    
    # Clase social actual basada en ocupación
    clase_encuestado = case_when(
      categoria == 1 & o17 >= 3 & o17 < 7 ~ "Dueño de empresa grande/mediana o director",
      CIUO >= 1000 & CIUO < 2000 ~ "Dueño de empresa grande/mediana o director",
      (CIUO >= 2000 & CIUO < 2320) | (CIUO >= 2400 & CIUO < 3000) ~ "Profesional",
      CIUO >= 2320 & CIUO < 2400 & categoria == 1 & o17 < 3~ "Pequeño propietario",
      CIUO >= 2320 & CIUO < 2400 & categoria == 2 & (o24_1 == 1 | o24_2 == 1 | o24_3 == 1) ~ "Pequeño propietario",
      CIUO >= 2320 & CIUO < 2400 & categoria == 2 & (o24_1 != 1 & o24_2 != 1 & o24_3 != 1) ~ "Técnico/Administrativo",
      CIUO >= 2320 & CIUO < 2400 & categoria == 3 ~ "Técnico/Administrativo",
      categoria == 1 & o17 <= 2 &
        (  (CIUO >= 3000 & CIUO < 5211) |
            (CIUO >= 5220 & CIUO < 6300) |
            (CIUO >= 7000 & CIUO < 9000)
        ) ~ "Pequeño propietario",
      categoria == 2 & (o24_1 == 1 | o24_2 == 1 | o24_3 == 1) & 
        (
          (CIUO >= 3000 & CIUO < 5211) |
            (CIUO >= 5220 & CIUO < 6000) |
            (CIUO >= 6000 & CIUO < 6300) |
            (CIUO >= 7000 & CIUO < 9000)
        ) ~ "Pequeño propietario",
      categoria == 2 & (o24_1 != 1 & o24_2 != 1 & o24_3 != 1) & 
          (CIUO >= 3000 & CIUO < 5000)
           ~ "Técnico/Administrativo",
      categoria == 3 & (CIUO >= 3000 & CIUO < 5000)
      ~ "Técnico/Administrativo", 
      categoria == 2 & (o24_1 != 1 & o24_2 != 1 & o24_3 != 1) & 
        (
          (CIUO >= 5000 & CIUO < 5211) |
            (CIUO >= 5220 & CIUO < 6000) |
            (CIUO >= 6000 & CIUO < 6300) |
            (CIUO >= 7000 & CIUO < 9000)
        ) ~ "Trabajador manual calificado",
      categoria == 3 &
        (
          (CIUO >= 5000 & CIUO < 5200) |
            (CIUO >= 5221 & CIUO < 6000) |
            (CIUO >= 6000 & CIUO < 6300) |
            (CIUO >= 7000 & CIUO < 9000)
        ) ~ "Trabajador manual calificado",
      CIUO == 210 | CIUO == 310 ~ "Trabajador manual calificado",
      (CIUO >= 5200 & CIUO < 5221) |
        (CIUO >= 6300 & CIUO < 7000) |
        (CIUO >= 9000 & CIUO < 9998) ~ "Trabajador manual no calificado",
      TRUE ~ NA_character_
    ),
    
    clase_encuestado = factor(
      clase_encuestado,
      levels = c(
        "Dueño de empresa grande/mediana o director",
        "Profesional",
        "Pequeño propietario",
        "Técnico/Administrativo",
        "Trabajador manual calificado",
        "Trabajador manual no calificado"
      )
    ),
    
    clase_encuestado5 = fct_collapse(
      clase_encuestado,
      "Director–profesional" = c(
        "Dueño de empresa grande/mediana o director",
        "Profesional"
      ),
      "Pequeño propietario" = "Pequeño propietario",
      "Técnico–administrativo" = "Técnico/Administrativo",
      "Trabajador manual calificado" = "Trabajador manual calificado",
      "Trabajador manual no calificado" = "Trabajador manual no calificado"
    ),
    
    # Clase de origen (parental)
    clase_origen = case_when(
      o45 %in% c(1, 2) ~ "Dueño de empresa grande/mediana o director",
      o45 %in% c(3, 4) ~ "Profesional",
      o45 %in% c(6, 7) ~ "Pequeño propietario",
      o45 %in% c(5, 9) ~ "Técnico/Administrativo",
      o45 %in% c(8, 10, 11) ~ "Trabajador manual calificado",
      o45 %in% c(12:16) ~ "Trabajador manual no calificado",
      TRUE ~ NA_character_
    ),
    
    clase_origen = factor(
      clase_origen,
      levels = c(
        "Dueño de empresa grande/mediana o director",
        "Profesional",
        "Pequeño propietario",
        "Técnico/Administrativo",
        "Trabajador manual calificado",
        "Trabajador manual no calificado"
      )
    ),
    
    clase_origen5 = fct_collapse(
      clase_origen,
      "Director–profesional" = c(
        "Dueño de empresa grande/mediana o director",
        "Profesional"
      ),
      "Pequeño propietario" = "Pequeño propietario",
      "Técnico–administrativo" = "Técnico/Administrativo",
      "Trabajador manual calificado" = "Trabajador manual calificado",
      "Trabajador manual no calificado" = "Trabajador manual no calificado"
    ),
    # Movilidad objetiva
    movilidad_objetiva = case_when(
      clase_origen5 == clase_encuestado5 ~ "Reproducción social",
      clase_origen5 == "Director–profesional" &
        clase_encuestado5 == "Pequeño propietario"  ~ "Movilidad descendente corta",
      clase_origen5 == "Director–profesional" &
        clase_encuestado5 %in% c(
          "Técnico/Administrativo",
          "Trabajador manual calificado",
          "Trabajador manual no calificado"
        ) ~ "Movilidad descendente larga",
      clase_origen5 == "Pequeño propietario" &
        clase_encuestado5 == "Director–profesional" ~ "Movilidad ascendente corta",
      clase_origen5 == "Pequeño propietario" &
        clase_encuestado5 == "Técnico–administrativo" ~ "Movilidad descendente corta",
      clase_origen5 == "Pequeño propietario" &
        clase_encuestado5 %in% c(
          "Trabajador manual calificado",
          "Trabajador manual no calificado"
        ) ~ "Movilidad descendente larga",
      clase_origen5 == "Técnico–administrativo" &
        clase_encuestado5 == "Director–profesional" ~ "Movilidad ascendente larga",
      clase_origen5 == "Técnico–administrativo" &
        clase_encuestado5 == "Pequeño propietario" ~ "Movilidad ascendente corta",
      clase_origen5 == "Técnico–administrativo" &
        clase_encuestado5 == "Trabajador manual calificado" ~ "Movilidad descendente corta",
      clase_origen5 == "Técnico–administrativo" &
        clase_encuestado5 == "Trabajador manual no calificado" ~ "Movilidad descendente larga",
      clase_origen5 == "Trabajador manual calificado" &
        clase_encuestado5 %in% c(
          "Director–profesional",
          "Pequeño propietario"
        ) ~ "Movilidad ascendente larga",
      clase_origen5 == "Trabajador manual calificado" &
        clase_encuestado5 == "Técnico–administrativo" ~ "Movilidad ascendente corta",
      clase_origen5 == "Trabajador manual calificado" &
        clase_encuestado5 == "Trabajador manual no calificado" ~ "Movilidad descendente corta",
      clase_origen5 == "Trabajador manual no calificado" &
        clase_encuestado5 %in% c(
          "Director–profesional",
          "Técnico–administrativo",
          "Pequeño propietario"
        ) ~ "Movilidad ascendente larga",
      clase_origen5 == "Trabajador manual no calificado" &
        clase_encuestado5 == "Trabajador manual calificado" ~ "Movilidad ascendente corta",
      TRUE ~ NA_character_
    ),
    
    movilidad_objetiva = factor(
      movilidad_objetiva,
      levels = c(
        "Movilidad ascendente larga",
        "Movilidad ascendente corta",
        "Reproducción social",
        "Movilidad descendente corta",
        "Movilidad descendente larga"
      )
    ),
    
    movilidad_objetiva2 = case_when(
      movilidad_objetiva %in% c(
        "Movilidad ascendente larga",
        "Movilidad ascendente corta"
      ) ~ "Movilidad ascendente",
      movilidad_objetiva == "Reproducción social" ~ "Reproducción social",
      movilidad_objetiva %in% c(
        "Movilidad descendente corta",
        "Movilidad descendente larga"
      ) ~ "Movilidad descendente",
      TRUE ~ NA_character_
    ),
    
    movilidad_objetiva2 = factor(
      movilidad_objetiva2,
      levels = c(
        "Movilidad ascendente",
        "Reproducción social",
        "Movilidad descendente"
      )
    ),
    
    # Clase subjetiva
    clase_subjetiva = case_when(
      p14 == 2 ~ "Alto medio",
      p14 == 3 ~ "Medio",
      p14 == 4 ~ "Bajo medio",
      p14 == 5 ~ "Clase trabajadora",
      p14 == 6 ~ "Clase baja",
      TRUE ~ NA_character_
    ),
    
    clase_subjetiva = factor(
      clase_subjetiva,
      levels = c(
        "Alto medio",
        "Medio",
        "Bajo medio",
        "Clase trabajadora",
        "Clase baja"
      )
    ),
    
    # Explicaciones de movilidad
    explicacion_mov1 = case_when(
      p19_1 %in% c(1, 3, 4) ~ "Mérito / individual",
      p19_1 %in% c(2, 5, 6, 7) ~ "Estructural",
      TRUE ~ NA_character_
    ),
    
    explicacion_mov2 = case_when(
      p19_2 %in% c(1, 3, 4) ~ "Mérito / individual",
      p19_2 %in% c(2, 5, 6, 7) ~ "Estructural",
      TRUE ~ NA_character_
    ),
    
    explicacion_mov = case_when(
      explicacion_mov1 == "Mérito / individual" & explicacion_mov2 == "Mérito / individual" ~ "Mérito / individual",
      explicacion_mov1 == "Estructural" & explicacion_mov2 == "Estructural" ~ "Estructural",
      explicacion_mov1 != explicacion_mov2 ~ "Mixta",
      TRUE ~ NA_character_
    ),
    
    explicacion_mov = factor(
      explicacion_mov,
      levels = c("Mérito / individual", "Mixta", "Estructural")
    ),
    
    # Desigualdad percibida
    desigualdad = case_when(
      p1 == 1 ~ "Muy desigual",
      p1 == 2 ~ "Algo desigual",
      p1 %in% c(3, 4) ~ "Poco o nada desigual",
      TRUE ~ NA_character_
    ),
    
    desigualdad = factor(
      desigualdad,
      levels = c(
        "Muy desigual",
        "Algo desigual",
        "Poco o nada desigual"
      )
    ),
    
    desigualdad_rp = case_when(
      p16_3 == 1 ~ "Muy desigual",
      p16_3 == 2 ~ "Algo desigual",
      p16_3 %in% c(3, 4) ~ "Poco o nada desigual",
      TRUE ~ NA_character_
    ),
    
    desigualdad_rp = factor(
      desigualdad_rp,
      levels = c(
        "Muy desigual",
        "Algo desigual",
        "Poco o nada desigual"
      )
    ),
    
    desigualdad_clases = case_when(
      p16_4 == 1 ~ "Muy desigual",
      p16_4 == 2 ~ "Algo desigual",
      p16_4 %in% c(3, 4) ~ "Poco o nada desigual",
      TRUE ~ NA_character_
    ),
    
    desigualdad_clases = factor(
      desigualdad_clases,
      levels = c(
        "Muy desigual",
        "Algo desigual",
        "Poco o nada desigual"
      )
    )
  )


wvs <- wvs %>% 
  mutate(movilidad_sub = ifelse(Q56 > 0, Q56, NA_real_),
         movilidad_sub = factor(movilidad_sub, 
                                levels = c(1, 3, 2),
                                labels = c("Ascendente", "Reproducción", "Descendente")),
         cohorte = case_when(Q262 < 27 ~ "1990",
                             Q262 >=27 & Q262 < 37 ~ "1980",
                             Q262 >=37 & Q262 < 47 ~ "1970",
                             Q262 >=47 & Q262 < 57 ~ "1960",
                             Q262 >=57 & Q262 < 67 ~ "1950",
                             Q262 >=67 ~ "1940"))

latinobarometro <- latinobarometro %>% 
  filter(anio == 2020) %>% 
  mutate(movilidad_sub = escala_pobriq_personal - escala_pobriq_padres,
         movilidad_sub_f = case_when(
           movilidad_sub > 0 ~ "Ascendente",
           movilidad_sub == 0 ~ "Reproducción",
           movilidad_sub < 0 ~ "Descendente",
           TRUE ~ NA_character_
         ),
         movilidad_sub_f = factor(movilidad_sub_f, 
                                  levels = c("Ascendente", "Reproducción", "Descendente")),
         iso3 = countrycode::countrycode(pais,
                                         origin = "iso3n",
                                         destination = "iso3c"),
         cohorte = case_when(
           edad < 20 ~ "2000",
           edad >= 20 & edad < 30 ~ "1990",
           edad >= 30 & edad < 40 ~ "1980",
           edad >= 40 & edad < 50 ~ "1970",
           edad >= 50 & edad < 60 ~ "1960",
           edad >= 60 & edad < 70 ~ "1950",
           edad >= 70 ~ "1940"
         )
         
  )


# Análisis de la bibliografía --------------
base_scopus %>%
  filter(PY >= 2000 & PY < 2027) %>%
  group_by(PY, search_type) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = PY, y = n, fill = search_type)) +
  geom_col() +
  scale_fill_atlassian() +
  labs(title = "Publicaciones sobre la temática de la movilidad social por año",
       subtitle = "Términos específicos: movilidad social subjetiva; percepción de la movilidad social; \nmovilidad social percibida",
       caption = "Fuente: en base a SCOPUS (29/9/2026)") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = seq(2000, 2026, 2)) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    plot.subtitle = element_text(size = 10),
    legend.position = "bottom"
  )

ggsave("graficos/publicaciones_anio.png", width = 7, height = 4, dpi = 300)


base_scopus_sub <- metaTagExtraction(base_scopus_sub, Field = "AU_CO", sep = ";")

pub_pais <- base_scopus_sub %>%
  filter(!is.na(AU_CO)) %>%
  separate_rows(AU_CO, sep = ";") %>%
  mutate(AU_CO = trimws(AU_CO))

mov_sub <- pub_pais %>%
  count(AU_CO, sort = TRUE) %>% 
  slice_max(order_by = n, n = 10, with_ties = FALSE) %>% 
  ggplot(aes(area = n, fill = AU_CO, label = paste(AU_CO, n, sep = "\n"))) +
  geom_treemap() +
  geom_treemap_text(colour = "white", size = 20) +
  scale_fill_d3("category10") +
  labs(title = "Movilidad subjetiva") +
  theme(legend.position = "none")

base_scopus_mov <- metaTagExtraction(base_scopus_mov, Field = "AU_CO", sep = ";")

pub_pais <- base_scopus_mov %>%
  filter(!is.na(AU_CO)) %>%
  separate_rows(AU_CO, sep = ";") %>%
  mutate(AU_CO = trimws(AU_CO))

mov_soc <- pub_pais %>%
  count(AU_CO, sort = TRUE) %>%
  slice_max(order_by = n, n = 10, with_ties = FALSE) %>% 
  ggplot(aes(area = n, fill = AU_CO, label = paste(AU_CO, n, sep = "\n"))) +
  geom_treemap() +
  geom_treemap_text(colour = "white", size = 20) +
  scale_fill_d3("category10") +
  labs(title = "Movilidad social (general)") +
  theme(legend.position = "none")

mov_sub / mov_soc +
  plot_annotation(
    title = "Países con mayores publicaciones en la temática",
    caption = "Fuente: en base a SCOPUS (29/9/2026)",
    theme = theme(
      plot.title = element_text(size = 16)
    )
  ) 

ggsave("graficos/publicaciones_paises.png", width = 6, height = 4, dpi = 300)

# Ratio de publicaciones
ratio_anual <- base_scopus %>%
  filter(PY >= 2000 & PY < 2027) %>%
  group_by(PY, search_type) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(
    names_from = search_type,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(
    ratio = `Movilidad subjetiva` / `Movilidad social`,
    porcentaje = ratio * 100
  )

ratio_anual %>%
  ggplot(aes(x = PY, y = porcentaje)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Evolución de la literatura sobre movilidad social subjetiva",
    subtitle = "Publicaciones de movilidad subjetiva por cada 100 publicaciones \nde movilidad social",
    caption = "Fuente: elaboración propia en base a SCOPUS",
    x = NULL,
    y = NULL
  ) +
  scale_x_continuous(
    breaks = seq(2000, 2026, 2)
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"), breaks = seq(0, 12, 2))

ggsave("graficos/publicaciones_ratio.png", width = 6, height = 4, dpi = 300)


# Descriptivos ESAyPI---------------
theme_gtsummary_language("es", decimal.mark = ",", big.mark = ".")

argentina2024 %>%
  filter(!is.na(clase_origen), !is.na(clase_encuestado)) %>%
  svydesign(data = ., ids = ~ 1, weights = ~pondera_sin_elevar) %>%
  tbl_svysummary(include = c(movilidad_sub, movilidad_objetiva, clase_encuestado, 
                             clase_origen, clase_subjetiva, cohorte, genero,
                             explicacion_mov, desigualdad),
                 label = c(movilidad_sub ~ "Movilidad subjetiva",
                           movilidad_objetiva ~ "Movilidad objetiva",
                           clase_encuestado ~ "Clase social encuestado",
                           clase_origen ~ "Clase social de origen",
                           clase_subjetiva ~ "Clase social subjetiva",
                           cohorte ~ "Cohorte de nacimiento",
                           genero ~ "Género",
                           explicacion_mov ~ "Explicación de la movilidad",
                           desigualdad ~ "Desigualdad percibida"),
                 statistic = list(
                   all_continuous() ~ "{mean} ({sd})",
                   all_categorical() ~ "{n} ({p}%)"),
                 digits = list(all_categorical() ~ c(0, 1),
                               all_continuous() ~ c(0, 1)),
                 missing = "no")  %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "tablas/tabla_descriptiva.docx")

# Resultados ----------------
## Tendencias wvs -------------------

df_sum <- wvs %>%
  filter(B_COUNTRY_ALPHA %in% paises, !is.na(movilidad_sub)) %>%
  group_by(B_COUNTRY_ALPHA, movilidad_sub) %>%
  summarise(n = sum(W_WEIGHT, na.rm = TRUE), .groups = "drop_last") %>%
  group_by(B_COUNTRY_ALPHA) %>%
  mutate(prop = n / sum(n)) %>% 
  ungroup()

asc_order <- df_sum %>%
  group_by(B_COUNTRY_ALPHA) %>%
  summarise(prop_asc = sum(n[movilidad_sub == "Ascendente"], na.rm = TRUE) / sum(n, na.rm = TRUE)) %>%
  mutate(prop_asc = ifelse(is.na(prop_asc), 0, prop_asc))

df_plot <- df_sum %>%
  left_join(asc_order, by = "B_COUNTRY_ALPHA") %>%
  mutate(B_COUNTRY_ALPHA = fct_reorder(B_COUNTRY_ALPHA, prop_asc))


ggplot(df_plot, aes(x = B_COUNTRY_ALPHA, y = prop, fill = movilidad_sub)) +
  geom_col(position = "fill") +
  scale_fill_manual(values = cols) +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)), 
            position = position_fill(vjust = 0.5), color = "white", size = 3) +
  labs(title = "Percepción de movilidad social en América Latina 2017-2022",
       subtitle = "Países seleccionados. Pregunta directa.",
       caption = "Fuente: elaboración propia en base a WVS 7") +
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

ggsave("graficos/movilidad_sub_wvs.png", width = 6, height = 4, dpi = 300)


## Tendencias Latinobarometro ---------------
df_sum <- latinobarometro %>%
  filter(iso3 %in% paises, !is.na(movilidad_sub)) %>%
  group_by(iso3, movilidad_sub_f) %>%
  summarise(n = sum(wt, na.rm = TRUE), .groups = "drop_last") %>%
  group_by(iso3) %>%
  mutate(prop = n / sum(n)) %>% 
  ungroup()

asc_order <- df_sum %>%
  group_by(iso3) %>%
  summarise(prop_asc = sum(n[movilidad_sub_f == "Ascendente"], na.rm = TRUE) / sum(n, na.rm = TRUE)) %>%
  mutate(prop_asc = ifelse(is.na(prop_asc), 0, prop_asc))

df_plot <- df_sum %>%
  left_join(asc_order, by = "iso3") %>%
  mutate(iso3 = fct_reorder(iso3, prop_asc))

ggplot(df_plot, aes(x = iso3, y = prop, fill = movilidad_sub_f)) +
  geom_col(position = "fill") +
  scale_fill_manual(values = cols) +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)),
            position = position_fill(vjust = 0.5), color = "white", size = 3) +
  labs(title = "Percepción de movilidad social en América Latina 2020",
       subtitle = "Países seleccionados. Pregunta indirecta.",
       caption = "Fuente: elaboración propia en base a Latinobarómetro") +
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

ggsave("graficos/movilidad_sub_latinobarometro.png", width = 6, height = 4, dpi = 300)


## Cohorte Argentina ----
argentina2024 %>% 
  filter(!is.na(movilidad_sub)) %>%
  group_by(cohorte, movilidad_sub) %>%
  tally(pondera_sin_elevar) %>% 
  group_by(cohorte) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = cohorte, y = prop, fill = movilidad_sub)) +
  geom_col(position = "fill") +
  scale_fill_manual(values = cols) +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)),
            position = position_fill(vjust = 0.5), color = "white", size = 3) +
  labs(title = "Percepción de movilidad social en Argentina según cohorte, 2024.",
       caption = "Fuente: elaboración propia en base a ESAyPI 2024.") +
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

ggsave("graficos/movilidad_sub_cohorte_arg1.png", width = 6, height = 4, dpi = 300)


wvs %>% 
  filter(B_COUNTRY_ALPHA == "ARG", !is.na(movilidad_sub)) %>%
  group_by(cohorte, movilidad_sub) %>%
  summarise(n = sum(W_WEIGHT, na.rm = TRUE), .groups = "drop_last") %>%
  group_by(cohorte) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = cohorte, y = prop, fill = movilidad_sub)) +
  geom_col(position = "fill") +
  scale_fill_manual(values = cols) +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)),
            position = position_fill(vjust = 0.5), color = "white", size = 3) +
  labs(title = "Percepción de movilidad social en Argentina según cohorte, 2017.",
       caption = "Fuente: elaboración propia en base a WVS 7") +
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )


ggsave("graficos/movilidad_sub_cohorte_arg2.png", width = 7, height = 4, dpi = 300)

latinobarometro %>% 
  filter(iso3 == "ARG", !is.na(movilidad_sub_f)) %>% 
  group_by(cohorte, movilidad_sub_f) %>% 
  summarise(n = sum(wt, na.rm = TRUE), .groups = "drop_last") %>% 
  group_by(cohorte) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = cohorte, y = prop, fill = movilidad_sub_f)) +
  geom_col(position = "fill") +
  scale_fill_manual(values = cols) +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)),
            position = position_fill(vjust = 0.5), color = "white", size = 3) +
  labs(title = "Percepción de movilidad social en Argentina según cohorte, 2020.",
       caption = "Fuente: elaboración propia en base a Latinobarómetro") +
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

ggsave("graficos/movilidad_sub_cohorte_arg3.png", width = 7, height = 4, dpi = 300)


## Por clase social ------------
argentina2024 %>% 
  filter(!is.na(movilidad_sub), !is.na(clase_encuestado)) %>%
  group_by(clase_encuestado, movilidad_sub) %>%
  tally(pondera_sin_elevar) %>% 
  group_by(clase_encuestado) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = fct_rev(clase_encuestado), y = prop, fill = movilidad_sub)) +
  geom_col(position = "fill") +
  scale_fill_manual(values = cols) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)),
            position = position_fill(vjust = 0.5), color = "white", size = 3.5) +
  labs(title = "Percepción de movilidad social en Argentina según clase social objetiva, 2024.",
       caption = "Fuente: elaboración propia en base a ESAyPI 2024.") +
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.text = element_text(size = 10),
    plot.title = element_markdown()
  ) +
  coord_flip()

ggsave("graficos/movilidad_sub_clase_objetiva.png", width = 8, height = 5, dpi = 300)


argentina2024 %>% 
  filter(!is.na(movilidad_sub), !is.na(clase_subjetiva)) %>%
  group_by(clase_subjetiva, movilidad_sub) %>%
  tally(pondera_sin_elevar) %>% 
  group_by(clase_subjetiva) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = fct_rev(clase_subjetiva), y = prop, fill = movilidad_sub)) +
  geom_col(position = "fill") +
  scale_fill_manual(values = cols) +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)),
            position = position_fill(vjust = 0.5), color = "white", size = 3.5) +
  labs(title = "Percepción de movilidad social en Argentina según clase subjetiva, 2024.",
       caption = "Fuente: elaboración propia en base a ESAyPI 2024.") +
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.text = element_text(size = 10),
    plot.title = element_markdown()
  ) +
  coord_flip()

ggsave("graficos/movilidad_sub_clase_subjetiva.png", width = 8, height = 5, dpi = 300)



## Por movilidad objetiva---------
argentina2024 %>% 
  filter(!is.na(movilidad_sub), !is.na(movilidad_objetiva)) %>%
  group_by(movilidad_objetiva, movilidad_sub) %>%
  tally(pondera_sin_elevar) %>% 
  group_by(movilidad_objetiva) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = fct_rev(movilidad_objetiva), y = prop, fill = movilidad_sub)) +
  geom_col(position = "fill") +
  scale_fill_manual(values = cols) +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)),
            position = position_fill(vjust = 0.5), color = "white", size = 3.5) +
  labs(title = "Percepción de movilidad social en Argentina según trayectorias intergeneracionales, 2024.",
       caption = "Fuente: elaboración propia en base a ESAyPI 2024.") +
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_markdown(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.position = "bottom"
  ) +
  coord_flip()

ggsave("graficos/movilidad_sub_trayectorias.png", width = 8, height = 5, dpi = 300)


## Explicaciones de movilidad -------------
argentina2024 %>% 
  filter(!is.na(movilidad_sub), !is.na(explicacion_mov)) %>%
  group_by(explicacion_mov, movilidad_sub) %>%
  tally(pondera_sin_elevar) %>% 
  group_by(explicacion_mov) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = explicacion_mov, y = prop, fill = movilidad_sub)) +
  geom_col(position = "fill") +
  scale_fill_manual(values = cols) +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)),
            position = position_fill(vjust = 0.5), color = "white", size = 3.5) +
  labs(title = "Percepción de movilidad social en Argentina según tipo de justificación, 2024.",
       caption = "Fuente: elaboración propia en base a ESAyPI 2024.") +
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_markdown(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.position = "bottom"
  )

ggsave("graficos/movilidad_sub_explicaciones.png", width = 7, height = 5, dpi = 300)

argentina2024 %>% 
  filter(!is.na(movilidad_objetiva), !is.na(explicacion_mov)) %>%
  group_by(explicacion_mov, movilidad_objetiva) %>%
  tally(pondera_sin_elevar) %>% 
  group_by(explicacion_mov) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = explicacion_mov, y = prop, fill = movilidad_objetiva)) +
  geom_col(position = "fill") +
  scale_fill_manual(values = cols5) +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)),
            position = position_fill(vjust = 0.5), color = "white", size = 3.5) +
  labs(title = "Percepción de movilidad social en Argentina según tipo de justificación, 2024.",
       caption = "Fuente: elaboración propia en base a ESAyPI 2024.") +
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_markdown(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.position = "right"
  )

ggsave("graficos/movilidad_obj_explicaciones.png", width = 8, height = 5, dpi = 300)


## Determinantes de la movilidad -----------
argentina_reg <- argentina2024 %>% 
  filter(!is.na(clase_encuestado5), !is.na(clase_origen5)) %>% 
  mutate(
    movilidad_objetiva = fct_relevel(movilidad_objetiva, 
                                     "Movilidad descendente larga"),
    movilidad_objetiva2 = fct_relevel(movilidad_objetiva2, "Movilidad descendente"),
    movilidad_sub2 = factor(movilidad_sub2, levels = c(0, 1)),
    clase_subjetiva = fct_relevel(clase_subjetiva, "Clase baja"),
    explicacion_mov = fct_relevel(explicacion_mov, "Mérito / individual"),
    clase_encuestado5 = fct_relevel(clase_encuestado5, "Trabajador manual no calificado"),
    clase_origen5 = fct_relevel(clase_origen5, "Trabajador manual no calificado"),
    movilidad_sub = fct_relevel(movilidad_sub, "Reproducción"),
    desigualdad = fct_relevel(desigualdad, "Muy desigual"),
    desigualdad_rp = fct_relevel(desigualdad_rp, "Poco o nada desigual"),
    desigualdad_clases = fct_relevel(desigualdad_clases, "Poco o nada desigual"),
    cohorte = fct_relevel(cohorte, "2000")
  )

multinomial <- multinom(movilidad_sub ~ movilidad_objetiva2 + cohorte + genero + clase_subjetiva + explicacion_mov + desigualdad,
                        data = argentina_reg,
                        weights = pondera_sin_elevar,
                        trace = F,
                        model = TRUE)

tidied <- tidy(multinomial)
tidied$estimate <- exp(tidied$estimate)

models <- list()
models[["Ascendente"]] <- tidy_replace(multinomial, tidied[tidied$y.level == "Ascendente", ])
models[["Descendente"]] <- tidy_replace(multinomial, tidied[tidied$y.level == "Descendente", ])

# export_summs(models,
#              error_pos = "same",
#              model.names = c("Ascendente vs Reproducción", "Descendente vs Reproducción"),
#              stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.1),
#              scale = T,
#              robust = T)
# 
# DescTools::PseudoR2(multinomial, which = "McFadden")

etiquetas <- c("Movilidad objetiva: Ascendente", 
               "Movilidad objetiva: Reproducción",
               "Cohorte: 1940",
               "Cohorte: 1950",
               "Cohorte: 1960",
               "Cohorte: 1970",
               "Cohorte: 1980",
               "Cohorte: 1990",
               "Mujer",
               "Clase subjetiva: Clase media alta",
               "Clase subjetiva: Clase media",
               "Clase subjetiva: Clase media baja",
               "Clase subjetiva: Clase trabajadora",
               "Explicación mov.: Mixta",
               "Explicación mov.: Estructural",
               "Desigualdad percibida: Algo desigual",
               "Desigualdad percibida: Poco o nada desigual")

etiquetas_ajustadas <- str_wrap(etiquetas, width = 35)


plot_model(multinomial, 
           type = "est", 
           ci.lvl = .91,
           vline.color = "black",
           show.p = T,
           dot.size = 2,
           line.size = 0.5,
           axis.lim = c(.05, 15)
) +
  scale_color_atlassian() +
  scale_x_discrete(labels = rev(etiquetas_ajustadas)) +
  theme(
    axis.text.x = element_text(size = 10),
    strip.text = element_text(color = "black")
  ) +
  labs(
    title = "Determinantes de la movilidad subjetiva",
    subtitle = "Categoría de referencia: Reproducción social",
    caption = "Fuente: elaboración propia en base a ESAyPI 2024")

ggsave("graficos/determinantes_movilidad_subjetiva.png", width = 8, height = 6, dpi = 300)

