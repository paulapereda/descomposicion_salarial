library(tidyverse)

## Brechas de ingresos laborales para años seleccionados (1990, 2004 y 2018). 
## Trabajadores de entre 25 y 59 años. 

readxl::read_xlsx("data/evolucion_brecha.xlsx") %>% 
  pivot_longer(- Sexo, names_to = "anio", values_to = "valor") %>% 
  pivot_wider(names_from = Sexo, values_from = valor) %>% 
  mutate(Brecha = abs(round((Mujer - Varón)/Varón, 2))) %>% 
  ggplot(aes(anio, Brecha, group = 1)) +
  geom_line(color = "#726A95", size = .8) +
  scale_y_continuous(limits = c(0, .45), 
                     breaks = seq(0, .45, by = .05), 
                     labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Evolución de la diferencia de ingresos laborales medios de las mujeres respecto a los varones",
       x = "",
       y = "") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1)) +
  ggsave("plots/evolucion_brecha.png", dpi = 550, width = 10, height = 5)

data <- readr::read_rds("data/df_2004.rds") %>% 
  select(bc_pesoan, bc_pe2, bc_pe3, bc_pobp, bc_ing_lab) %>% 
  transmute(exp_anio = bc_pesoan,
            sexo = case_when(
              bc_pe2 == 1 ~ "Varón",
              bc_pe2 == 2 ~ "Mujer"),
            sexo = factor(sexo, levels = c("Varón", "Mujer")),
            edad = bc_pe3, 
            cond_actividad = case_when(
              bc_pobp == 1	~ "Menores de 14 años",
              bc_pobp == 2	~ "Ocupados",
              bc_pobp == 3	~ "Desocupados buscan trabajo por primera vez",
              bc_pobp == 4	~ "Desocupados propiamente dichos",
              bc_pobp == 5	~ "Desocupados en seguro de paro",
              bc_pobp == 6	~ "Inactivo, realiza quehaceres del hogar",
              bc_pobp == 7	~ "Inactivo, estudiante",
              bc_pobp == 8	~ "Inactivo, rentista",
              bc_pobp == 9	~ "Inactivo, pensionista",
              bc_pobp == 10 ~ "Inactivo, jubilado",
              bc_pobp == 11 ~ "Inactivo, otro"),
            ingreso_laboral = bc_ing_lab) %>%
  filter(edad >= 18 | edad <= 59) %>% 
  filter(cond_actividad == "Ocupados") %>%
  group_by(sexo) %>%
  summarise(mean = round(sum(ingreso_laboral*exp_anio, na.rm = T)/sum(exp_anio, na.rm = T))) %>% 
  readr::write_csv("hola.csv")

# data <- readr::read_rds("data/df_2018.rds") %>% 
#   select(bc_pesoan, bc_pe2, bc_pe3, bc_area, bc_pobp, bc_ing_lab) %>% 
#   transmute(exp_anio = bc_pesoan,
#             sexo = case_when(
#               bc_pe2 == 1 ~ "Varón",
#               bc_pe2 == 2 ~ "Mujer"),
#             sexo = factor(sexo, levels = c("Varón", "Mujer")),
#             edad = bc_pe3, 
#             cond_actividad = case_when(
#               bc_pobp == 1	~ "Menores de 14 años",
#               bc_pobp == 2	~ "Ocupados",
#               bc_pobp == 3	~ "Desocupados buscan trabajo por primera vez",
#               bc_pobp == 4	~ "Desocupados propiamente dichos",
#               bc_pobp == 5	~ "Desocupados en seguro de paro",
#               bc_pobp == 6	~ "Inactivo, realiza quehaceres del hogar",
#               bc_pobp == 7	~ "Inactivo, estudiante",
#               bc_pobp == 8	~ "Inactivo, rentista",
#               bc_pobp == 9	~ "Inactivo, pensionista",
#               bc_pobp == 10 ~ "Inactivo, jubilado",
#               bc_pobp == 11 ~ "Inactivo, otro"),
#             ingreso_laboral = bc_ing_lab,
#             area = case_when(
#               bc_area == 1 ~ "Montevideo",
#               bc_area == 2 ~ "Interior > 5000",
#               bc_area == 3 ~ "Interior < 5000",
#               bc_area == 4 ~ "Rural disperso")) %>%
#   filter(area == "Montevideo" | area == "Interior > 5000") %>% 
#   filter(edad >= 18 | edad <= 59) %>% 
#   filter(cond_actividad == "Ocupados") %>%
#   group_by(sexo) %>%
#   summarise(mean = round(sum(ingreso_laboral*exp_anio, na.rm = T)/sum(exp_anio, na.rm = T))) %>% 
#   readr::write_csv("hola.csv")

id_90 <- readr::read_rds("data/df_1990.rds") %>% 
  select(bc_pesoan, bc_pe2, bc_pe3, bc_pobp, bc_tipo_ocup) %>% 
  transmute(exp_anio = bc_pesoan,
            sexo = case_when(
              bc_pe2 == 1 ~ "Varón",
              bc_pe2 == 2 ~ "Mujer"),
            sexo = factor(sexo, levels = c("Varón", "Mujer")),
            edad = bc_pe3, 
            cond_actividad = case_when(
              bc_pobp == 1	~ "Menores de 14 años",
              bc_pobp == 2	~ "Ocupados",
              bc_pobp == 3	~ "Desocupados buscan trabajo por primera vez",
              bc_pobp == 4	~ "Desocupados propiamente dichos",
              bc_pobp == 5	~ "Desocupados en seguro de paro",
              bc_pobp == 6	~ "Inactivo, realiza quehaceres del hogar",
              bc_pobp == 7	~ "Inactivo, estudiante",
              bc_pobp == 8	~ "Inactivo, rentista",
              bc_pobp == 9	~ "Inactivo, pensionista",
              bc_pobp == 10 ~ "Inactivo, jubilado",
              bc_pobp == 11 ~ "Inactivo, otro"),
            tipo_ocup = case_when(
              bc_tipo_ocup == 0 ~ "Fuerzas Armadas",	 	
              bc_tipo_ocup == 1	~ "Miembro de poder ejecutivo y cuerpos legislativos y personal directivo de la administración pública y de las empresas",	 	
              bc_tipo_ocup == 2	~ "Profesionales científicos e intelectuales",	 	
              bc_tipo_ocup == 3	~ "Técnicos y profesionales de nivel medio",	 	
              bc_tipo_ocup == 4	~ "Empleados de oficina",	 	
              bc_tipo_ocup == 5	~ "Trabajadores de los servicios y vendedores de comercios y mercado",	 	
              bc_tipo_ocup == 6	~ "Agricultores y trabajadores calificados agropecuarios y pesqueros",	 	
              bc_tipo_ocup == 7	~ "Oficiales, operarios y artesanos de artes mecánicas y de otros",	 	
              bc_tipo_ocup == 8	~ "Operadores y montadores de instalaciones y máquinas",	 	
              bc_tipo_ocup == 9	~ "Trabajadores no calificados")) %>%
  filter(edad >= 18 | edad <= 59) %>% 
  filter(cond_actividad == "Ocupados") %>%
  group_by(tipo_ocup) %>%
  summarise(id_90 = abs((sum(exp_anio[sexo == "Varón"])/sum(exp_anio) - sum(exp_anio[sexo == "Mujer"])/sum(exp_anio)/2))) 

id_00 <- readr::read_rds("data/df_2000.rds") %>% 
  select(bc_pesoan, bc_pe2, bc_pe3, bc_pobp, bc_tipo_ocup) %>% 
  transmute(exp_anio = bc_pesoan,
            sexo = case_when(
              bc_pe2 == 1 ~ "Varón",
              bc_pe2 == 2 ~ "Mujer"),
            sexo = factor(sexo, levels = c("Varón", "Mujer")),
            edad = bc_pe3, 
            cond_actividad = case_when(
              bc_pobp == 1	~ "Menores de 14 años",
              bc_pobp == 2	~ "Ocupados",
              bc_pobp == 3	~ "Desocupados buscan trabajo por primera vez",
              bc_pobp == 4	~ "Desocupados propiamente dichos",
              bc_pobp == 5	~ "Desocupados en seguro de paro",
              bc_pobp == 6	~ "Inactivo, realiza quehaceres del hogar",
              bc_pobp == 7	~ "Inactivo, estudiante",
              bc_pobp == 8	~ "Inactivo, rentista",
              bc_pobp == 9	~ "Inactivo, pensionista",
              bc_pobp == 10 ~ "Inactivo, jubilado",
              bc_pobp == 11 ~ "Inactivo, otro"),
            tipo_ocup = case_when(
              bc_tipo_ocup == 0 ~ "Fuerzas Armadas",	 	
              bc_tipo_ocup == 1	~ "Miembro de poder ejecutivo y cuerpos legislativos y personal directivo de la administración pública y de las empresas",	 	
              bc_tipo_ocup == 2	~ "Profesionales científicos e intelectuales",	 	
              bc_tipo_ocup == 3	~ "Técnicos y profesionales de nivel medio",	 	
              bc_tipo_ocup == 4	~ "Empleados de oficina",	 	
              bc_tipo_ocup == 5	~ "Trabajadores de los servicios y vendedores de comercios y mercado",	 	
              bc_tipo_ocup == 6	~ "Agricultores y trabajadores calificados agropecuarios y pesqueros",	 	
              bc_tipo_ocup == 7	~ "Oficiales, operarios y artesanos de artes mecánicas y de otros",	 	
              bc_tipo_ocup == 8	~ "Operadores y montadores de instalaciones y máquinas",	 	
              bc_tipo_ocup == 9	~ "Trabajadores no calificados")) %>%
  filter(edad >= 18 | edad <= 59) %>% 
  filter(cond_actividad == "Ocupados") %>%
  group_by(tipo_ocup) %>%
  summarise(id_00 = abs((sum(exp_anio[sexo == "Varón"])/sum(exp_anio) - sum(exp_anio[sexo == "Mujer"])/sum(exp_anio)/2))) 


id_06 <- readr::read_rds("data/df_2006.rds") %>% 
  select(bc_pesoan, bc_pe2, bc_pe3, bc_area, bc_pobp, bc_tipo_ocup) %>% 
  transmute(exp_anio = bc_pesoan,
            sexo = case_when(
              bc_pe2 == 1 ~ "Varón",
              bc_pe2 == 2 ~ "Mujer"),
            sexo = factor(sexo, levels = c("Varón", "Mujer")),
            edad = bc_pe3, 
            cond_actividad = case_when(
              bc_pobp == 1	~ "Menores de 14 años",
              bc_pobp == 2	~ "Ocupados",
              bc_pobp == 3	~ "Desocupados buscan trabajo por primera vez",
              bc_pobp == 4	~ "Desocupados propiamente dichos",
              bc_pobp == 5	~ "Desocupados en seguro de paro",
              bc_pobp == 6	~ "Inactivo, realiza quehaceres del hogar",
              bc_pobp == 7	~ "Inactivo, estudiante",
              bc_pobp == 8	~ "Inactivo, rentista",
              bc_pobp == 9	~ "Inactivo, pensionista",
              bc_pobp == 10 ~ "Inactivo, jubilado",
              bc_pobp == 11 ~ "Inactivo, otro"),
            tipo_ocup = case_when(
              bc_tipo_ocup == 0 ~ "Fuerzas Armadas",	 	
              bc_tipo_ocup == 1	~ "Miembro de poder ejecutivo y cuerpos legislativos y personal directivo de la administración pública y de las empresas",	 	
              bc_tipo_ocup == 2	~ "Profesionales científicos e intelectuales",	 	
              bc_tipo_ocup == 3	~ "Técnicos y profesionales de nivel medio",	 	
              bc_tipo_ocup == 4	~ "Empleados de oficina",	 	
              bc_tipo_ocup == 5	~ "Trabajadores de los servicios y vendedores de comercios y mercado",	 	
              bc_tipo_ocup == 6	~ "Agricultores y trabajadores calificados agropecuarios y pesqueros",	 	
              bc_tipo_ocup == 7	~ "Oficiales, operarios y artesanos de artes mecánicas y de otros",	 	
              bc_tipo_ocup == 8	~ "Operadores y montadores de instalaciones y máquinas",	 	
              bc_tipo_ocup == 9	~ "Trabajadores no calificados"),
            area = case_when(
              bc_area == 1 ~ "Montevideo",
              bc_area == 2 ~ "Interior > 5000",
              bc_area == 3 ~ "Interior < 5000",
              bc_area == 4 ~ "Rural disperso")) %>%
  filter(area == "Montevideo" | area == "Interior > 5000") %>%
  filter(edad >= 18 | edad <= 59) %>% 
  filter(cond_actividad == "Ocupados") %>%
  group_by(tipo_ocup) %>%
  summarise(id_06 = abs((sum(exp_anio[sexo == "Varón"])/sum(exp_anio) - sum(exp_anio[sexo == "Mujer"])/sum(exp_anio)/2))) 

id_10 <- readr::read_rds("data/df_2010.rds") %>% 
  select(bc_pesoan, bc_pe2, bc_pe3, bc_area, bc_pobp, bc_tipo_ocup) %>% 
  transmute(exp_anio = bc_pesoan,
            sexo = case_when(
              bc_pe2 == 1 ~ "Varón",
              bc_pe2 == 2 ~ "Mujer"),
            sexo = factor(sexo, levels = c("Varón", "Mujer")),
            edad = bc_pe3, 
            cond_actividad = case_when(
              bc_pobp == 1	~ "Menores de 14 años",
              bc_pobp == 2	~ "Ocupados",
              bc_pobp == 3	~ "Desocupados buscan trabajo por primera vez",
              bc_pobp == 4	~ "Desocupados propiamente dichos",
              bc_pobp == 5	~ "Desocupados en seguro de paro",
              bc_pobp == 6	~ "Inactivo, realiza quehaceres del hogar",
              bc_pobp == 7	~ "Inactivo, estudiante",
              bc_pobp == 8	~ "Inactivo, rentista",
              bc_pobp == 9	~ "Inactivo, pensionista",
              bc_pobp == 10 ~ "Inactivo, jubilado",
              bc_pobp == 11 ~ "Inactivo, otro"),
            tipo_ocup = case_when(
              bc_tipo_ocup == 0 ~ "Fuerzas Armadas",	 	
              bc_tipo_ocup == 1	~ "Miembro de poder ejecutivo y cuerpos legislativos y personal directivo de la administración pública y de las empresas",	 	
              bc_tipo_ocup == 2	~ "Profesionales científicos e intelectuales",	 	
              bc_tipo_ocup == 3	~ "Técnicos y profesionales de nivel medio",	 	
              bc_tipo_ocup == 4	~ "Empleados de oficina",	 	
              bc_tipo_ocup == 5	~ "Trabajadores de los servicios y vendedores de comercios y mercado",	 	
              bc_tipo_ocup == 6	~ "Agricultores y trabajadores calificados agropecuarios y pesqueros",	 	
              bc_tipo_ocup == 7	~ "Oficiales, operarios y artesanos de artes mecánicas y de otros",	 	
              bc_tipo_ocup == 8	~ "Operadores y montadores de instalaciones y máquinas",	 	
              bc_tipo_ocup == 9	~ "Trabajadores no calificados"),
            area = case_when(
              bc_area == 1 ~ "Montevideo",
              bc_area == 2 ~ "Interior > 5000",
              bc_area == 3 ~ "Interior < 5000",
              bc_area == 4 ~ "Rural disperso")) %>%
  filter(area == "Montevideo" | area == "Interior > 5000") %>%
  filter(edad >= 18 | edad <= 59) %>% 
  filter(cond_actividad == "Ocupados") %>%
  group_by(tipo_ocup) %>%
  summarise(id_10 = abs((sum(exp_anio[sexo == "Varón"])/sum(exp_anio) - sum(exp_anio[sexo == "Mujer"])/sum(exp_anio)/2))) 

id_18 <- readr::read_rds("data/df_2018.rds") %>% 
  select(bc_pesoan, bc_pe2, bc_pe3, bc_area, bc_pobp, bc_tipo_ocup) %>% 
  transmute(exp_anio = bc_pesoan,
            sexo = case_when(
              bc_pe2 == 1 ~ "Varón",
              bc_pe2 == 2 ~ "Mujer"),
            sexo = factor(sexo, levels = c("Varón", "Mujer")),
            edad = bc_pe3, 
            cond_actividad = case_when(
              bc_pobp == 1	~ "Menores de 14 años",
              bc_pobp == 2	~ "Ocupados",
              bc_pobp == 3	~ "Desocupados buscan trabajo por primera vez",
              bc_pobp == 4	~ "Desocupados propiamente dichos",
              bc_pobp == 5	~ "Desocupados en seguro de paro",
              bc_pobp == 6	~ "Inactivo, realiza quehaceres del hogar",
              bc_pobp == 7	~ "Inactivo, estudiante",
              bc_pobp == 8	~ "Inactivo, rentista",
              bc_pobp == 9	~ "Inactivo, pensionista",
              bc_pobp == 10 ~ "Inactivo, jubilado",
              bc_pobp == 11 ~ "Inactivo, otro"),
            tipo_ocup = case_when(
              bc_tipo_ocup == 0 ~ "Fuerzas Armadas",	 	
              bc_tipo_ocup == 1	~ "Miembro de poder ejecutivo y cuerpos legislativos y personal directivo de la administración pública y de las empresas",	 	
              bc_tipo_ocup == 2	~ "Profesionales científicos e intelectuales",	 	
              bc_tipo_ocup == 3	~ "Técnicos y profesionales de nivel medio",	 	
              bc_tipo_ocup == 4	~ "Empleados de oficina",	 	
              bc_tipo_ocup == 5	~ "Trabajadores de los servicios y vendedores de comercios y mercado",	 	
              bc_tipo_ocup == 6	~ "Agricultores y trabajadores calificados agropecuarios y pesqueros",	 	
              bc_tipo_ocup == 7	~ "Oficiales, operarios y artesanos de artes mecánicas y de otros",	 	
              bc_tipo_ocup == 8	~ "Operadores y montadores de instalaciones y máquinas",	 	
              bc_tipo_ocup == 9	~ "Trabajadores no calificados"),
            area = case_when(
              bc_area == 1 ~ "Montevideo",
              bc_area == 2 ~ "Interior > 5000",
              bc_area == 3 ~ "Interior < 5000",
              bc_area == 4 ~ "Rural disperso")) %>%
  filter(area == "Montevideo" | area == "Interior > 5000") %>%
  filter(edad >= 18 | edad <= 59) %>% 
  filter(cond_actividad == "Ocupados") %>%
  group_by(tipo_ocup) %>%
  summarise(id_18 = abs((sum(exp_anio[sexo == "Varón"])/sum(exp_anio) - sum(exp_anio[sexo == "Mujer"])/sum(exp_anio)/2))) 

indice_duncan <- id_90 %>% 
  left_join(id_00) %>% 
  left_join(id_06) %>% 
  left_join(id_10) %>% 
  left_join(id_18) %>% 
  mutate(id_90 = round(id_90, 3),
         id_00 = round(id_00, 3),
         id_06 = round(id_06, 3),
         id_10 = round(id_10, 3),
         id_18 = round(id_18, 3)) %>% 
  write_csv("data/id.csv")
