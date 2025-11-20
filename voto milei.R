argentina2024 <- argentina2024 %>% 
  mutate(voto1 = ifelse(p26 == 3, 1, 0),
         voto2 = ifelse(p27 == 2, 1, 0))


argentina2024 %>%
  group_by(genero, o3_1) %>%
  summarise(porcentaje_voto = weighted.mean(voto1, pondera, na.rm = TRUE)) %>% 
  ggplot(aes(x = o3_1, y = porcentaje_voto, color = genero)) +
  geom_point() +
  geom_smooth(method = "loess", se = T) +
  scale_x_continuous(breaks = seq(0, 90, by = 10)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_d3() +
  labs(x = "Edad", y = "Porcentaje de votantes", color = "Sexo",
       title = "Porcentaje de votantes de Milei en primera vuelta 2023")

ggsave("porcentaje_voto_mile1.png", width = 8, height = 5, dpi = 300)

argentina2024 %>%
  group_by(genero, o3_1) %>%
  summarise(porcentaje_voto = weighted.mean(voto2, pondera, na.rm = TRUE)) %>% 
  ggplot(aes(x = o3_1, y = porcentaje_voto, color = genero)) +
  geom_point() +
  geom_smooth(method = "loess", se = T) +
  scale_x_continuous(breaks = seq(0, 90, by = 10)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_d3() +
  labs(x = "Edad", y = "Porcentaje de votantes", color = "Sexo",
       title = "Porcentaje de votantes de Milei en segunda vuelta 2023")

ggsave("porcentaje_voto_mile2.png", width = 8, height = 5, dpi = 300)

argentina2024 %>%
  filter(voto1 == 1) %>% 
  ggplot(aes(x = o3_1, fill = genero, color = genero)) +
  geom_density(alpha = .5) +
  scale_x_continuous(breaks = seq(0, 90, by = 10)) +
  scale_fill_d3() +
  scale_color_d3() +
  labs(x = "Edad", y = "Densidad de votantes", fill = "Sexo", color = "Sexo",
       title = "Votantes de Milei en primera vuelta 2023")

ggsave("voto_mile1.png", width = 8, height = 5, dpi = 300)

argentina2024 %>%
  filter(voto2 == 1) %>% 
  ggplot(aes(x = o3_1, fill = genero, color = genero)) +
  geom_density(alpha = .5) +
  scale_x_continuous(breaks = seq(0, 90, by = 10)) +
  scale_fill_d3() +
  scale_color_d3() +
  labs(x = "Edad", y = "Densidad de votantes", fill = "Sexo", color = "Sexo",
       title = "Votantes de Milei en segunda vuelta 2023")
