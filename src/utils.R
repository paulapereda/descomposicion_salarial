load("~/descomposicion_salarial/data/ciiu.rda")

ciiu <- ciiu %>% 
  mutate(ciiu4 = str_extract(ciiu_4, "\\d{4}")) %>% 
  distinct(seccion, ciiu4) %>% 
  left_join(cargar_ciiu_seccion) %>% 
  write_rds("aux_ciiu.rds")

cargar_ciiu_seccion <- cargar_ciiu_seccion %>% rename(ciiu4 = ciiu_4)
