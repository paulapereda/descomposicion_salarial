library(tidyverse)
library(labelled)
library(haven)

aux_ciiu <- read_rds("data/aux_ciiu.rds")

df_18 <- read_rds("data/df_2018.rds") %>% 
  remove_labels() %>% 
  transmute(numero = bc_correlat, 
            nper = bc_nper,
            mes = bc_mes,
            ciiu = f72_2,
            depto = bc_dpto, 
            region = region_3, 
            exp_anio = bc_pesoan,
            sexo = bc_pe2, 
            edad = bc_pe3,
            parentesco = bc_pe4,
            estado_civil = bc_pe5,
            cond_actividad = pobpcoac,
            cat_ocupacion = bc_cat2,
            tamanio = f77, 
            ingreso_laboral = pt4,
            ciiu4 = f76_2,
            bc_rama,
            bc_tipo_ocup,
            bc_nivel,
            anios_educ = bc_edu,
            horas_trabajadas = f85 + f98,
            estred13,
            antiguedad = f88_2) %>% 
  left_join(aux_ciiu) %>% 
  mutate(montevideo = ifelse(depto == 1, T, F),
         area_metropolitana = ifelse(depto == 3 | depto == 16 | depto == 1, T, F),
         parentesco = case_when(
           parentesco == 1	~ "Jefe/a",
           parentesco == 2	~ "Esposo/a o compañero/a",
           parentesco == 3	~ "Hijo/a de ambos",
           parentesco == 4	~ "Hijo/a sólo del jefe",
           parentesco == 5	~ "Hijo/a sólo del esposo/a compañero/a",
           parentesco == 6	~ "Yerno/nuera",
           parentesco == 7	~ "Padre/madre",
           parentesco == 8	~ "Suegro/a",
           parentesco == 9	~ "Hermano/a",
           parentesco == 10 ~	"Cuñado/a",
           parentesco == 11 ~	"Nieto/a",
           parentesco == 12 ~	"Otro pariente",
           parentesco == 13 ~	"Otro no pariente",
           parentesco == 14 ~	"Servicio doméstico o familiar del mismo"
         ),
         estado_civil = case_when(
           estado_civil == 1	~ "Unión libre",
           estado_civil == 2	~ "Casado",
           estado_civil == 3	~ "Divorciado o separado",
           estado_civil == 4	~ "Viudo",
           estado_civil == 5	~ "Soltero",
         ),
         juntado = ifelse(estado_civil == "Unión libre" | estado_civil ==  "Casado", T, F),
         cond_actividad = case_when(
           cond_actividad == 1	~ "Menores de 14 años",
           cond_actividad == 2	~ "Ocupados",
           cond_actividad == 3	~ "Desocupados buscan trabajo por primera vez",
           cond_actividad == 4	~ "Desocupados propiamente dichos",
           cond_actividad == 5	~ "Desocupados en seguro de paro",
           cond_actividad == 6	~ "Inactivo, realiza quehaceres del hogar",
           cond_actividad == 7	~ "Inactivo, estudiante",
           cond_actividad == 8	~ "Inactivo, rentista",
           cond_actividad == 9	~ "Inactivo, pensionista",
           cond_actividad == 10 ~ "Inactivo, jubilado",
           cond_actividad == 11 ~ "Inactivo, otro"
         ),
         cat_ocupacion = case_when(
           cat_ocupacion == 1 ~ "Asalariado/a privado/a",
           cat_ocupacion == 2 ~ "Asalariado/a público/a",
           cat_ocupacion == 3 ~ "Miembro de cooperativa de producción o trabajo",
           cat_ocupacion == 4 ~ "Patrón/a" ,
           cat_ocupacion == 5 ~ "Cuenta propia sin local ni inversión",
           cat_ocupacion == 6 ~ "Cuenta propia con local o inversión",
           cat_ocupacion == 7 ~ "Miembro del hogar no remunerado",
           cat_ocupacion == 8 ~ "Trabajador/a de un programa social de empleo"
         ),
         sexo = case_when(
           sexo == 1 ~ "Varón",
           sexo == 2 ~ "Mujer"),
         mujer = ifelse(sexo == "Mujer", T, F),
         tamanio = case_when(
           tamanio == 1 ~ "Una persona",
           tamanio == 2 ~ "2 a 4 personas",
           tamanio == 3 ~ "5 a 9 personas",
           tamanio == 6 ~ "10 a 49 personas",
           tamanio == 7 ~ "10 a 49 personas",
           tamanio == 5 ~ "50 o más personas"
         ),
         bc_nivel = case_when(
           bc_nivel == 0 ~ "Sin instrucción",
           bc_nivel == 1 ~ "Primaria",
           bc_nivel == 2 ~ "Secundaria",
           bc_nivel == 3 ~ "Enseñanza técnica o UTU",
           bc_nivel == 4 ~ "Magisterio o Profesorado",
           bc_nivel == 5 ~ "Universidad o similar"
         ),
         bc_rama = case_when(
           bc_rama == 1 ~ "Agropecuaria y Minería",
           bc_rama == 2 ~ "Industria Manufactureras",
           bc_rama == 3 ~ "Electricidad, Gas y Agua",
           bc_rama == 4 ~ "Construcción",
           bc_rama == 5 ~ "Comercio, Restaurantes y Hoteles",
           bc_rama == 6 ~ "Transportes y Comunicaciones",
           bc_rama == 7 ~ "Servicios a empresas",
           bc_rama == 8 ~ "Servicios comunales, sociales y personales"
         ),
         bc_tipo_ocup = case_when(
           bc_tipo_ocup == 0	~ "Fuerzas Armadas",
           bc_tipo_ocup == 1	~ "Miembro de poder ejecutivo y cuerpos legislativos y personal directivo de la administración pública y de las empresas",
           bc_tipo_ocup == 2	~ "Profesionales científicos e intelectuales",
           bc_tipo_ocup == 3	~ "Técnicos y profesionales de nivel medio",
           bc_tipo_ocup == 4	~ "Empleados de oficina",
           bc_tipo_ocup == 5	~ "Trabajadores de los servicios y vendedores de comercios y mercados",
           bc_tipo_ocup == 6	~ "Agricultores y trabajadores calificados agropecuarios y pesqueros",
           bc_tipo_ocup == 7	~ "Oficiales, operarios y artesanos de artes mecánicas y de otros oficios",
           bc_tipo_ocup == 8	~ "Operadores y montadores de instalaciones y máquinas",
           bc_tipo_ocup == 9	~ "Trabajadores no calificados"
         )) %>% 
  group_by(numero) %>% 
  mutate(menores_18 = ifelse(any(edad, na.rm = T) < 18, T, F),
         hijos = ifelse(any(parentesco == "Hijo/a de ambos") |
                          any(parentesco == "Hijo/a sólo del esposo/a compañero/a"), T, F)) %>% 
  ungroup() %>% 
  mutate(hijos_menores_18 = ifelse(menores_18 == T & hijos == T, T, F)) %>% 
  write_rds("data/df_18_oaxaca.rds")

oaxaca_18 <- df_18 %>% 
  filter(edad >= 25 & edad <= 59) %>% 
  filter(cond_actividad == "Ocupados") %>% 
  mutate(lnwage = log(ingreso_laboral)) %>% 
  filter(!is.na(lnwage) & !is.nan(lnwage) & !is.infinite(lnwage)) 

write_rds(oaxaca_18, "data/oaxaca_18.rds")
haven::write_dta(oaxaca_18, "data/oaxaca_18.dta")
