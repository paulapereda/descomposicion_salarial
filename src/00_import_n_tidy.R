install.packages("oaxaca")


ech_2019 <- haven::read_dta("data/p18.dta") %>% 
  readr::write_rds("data/df_2018.rds")
