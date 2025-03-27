
rm(list=ls())


setwd("/home/depp/chercheurs/ltouitou")


# 0 - Packages and file paths --------------------------------------------------


list_of_packages <- c(
  "tidyverse", 
  "fst",
  "arrow",
  "rstatix",
  "lmerTest"
)
invisible(lapply(list_of_packages, library, character.only = TRUE))
rm(list_of_packages)




conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflicts_prefer(lmerTest::lmer)


### Main repositories
perso <- file.path("/home/depp/chercheurs/ltouitou")
shared <- file.path("/home/depp/projets_inter/PSE-ecartsFG/PSE-ecartsFG-echanges")




### Data repositories
input <- file.path(shared, "Data", "Input")
intermediate <- file.path(shared, "Data", "Intermediate")
output <- file.path(shared, "Data", "Output")


# 1 - Load all data ------------------------------------------------------------


first_cohort <- readRDS(file.path(shared, "Data", "Output", "cohort_2018_cleanP_imp.rds"))
second_cohort <- readRDS(file.path(shared, "Data", "Output", "cohort_2019_cleanP_imp.rds"))
third_cohort <- readRDS(file.path(shared, "Data", "Output", "cohort_2020_cleanP_imp.rds"))
fourth_cohort <- readRDS(file.path(shared, "Data", "Output", "cohort_2021_cleanP_imp.rds"))

# 2 - Create tables ------------------------------------------------------------

minmax__first_cohort <- first_cohort %>%
  select(Sexe, matches("^(T1_|T2_|T3_)") & !matches(("(_z|_P|_Cut|_Language|Math)$"))) %>%
  pivot_longer(cols = matches("^(T1_|T2_|T3_)") , names_to = "Exercice", values_to = "Score") %>%
  group_by(Exercice) %>%
  summarise(Moyenne_initial = mean(Score, na.rm = T),
            Gender_gap_initial = mean(Score[Sexe == "Boys"], na.rm = T) - mean(Score[Sexe == "Girls"], na.rm = T),
            Sd_initial = sd(Score, na.rm = T),
            Min_initial = min(Score, na.rm = T),
            Max_initial = max(Score, na.rm = T)) %>%
  ungroup() %>%
  mutate(Exercice = paste0(Exercice, "_P"))

minmax__second_cohort <- second_cohort %>%
  select(Sexe, matches("^(T1_|T2_|T3_)") & !matches(("(_z|_P|_Cut|_Language|Math)$"))) %>%
  pivot_longer(cols = matches("^(T1_|T2_|T3_)") , names_to = "Exercice", values_to = "Score") %>%
  group_by(Exercice) %>%
  summarise(Moyenne_initial = mean(Score, na.rm = T),
            Gender_gap_initial = mean(Score[Sexe == "Boys"], na.rm = T) - mean(Score[Sexe == "Girls"], na.rm = T),
            Sd_initial = sd(Score, na.rm = T),
            Min_initial = min(Score, na.rm = T),
            Max_initial = max(Score, na.rm = T)) %>%
  ungroup() %>%
  mutate(Exercice = paste0(Exercice, "_P"))

minmax__third_cohort <- third_cohort %>%
  select(Sexe, matches("^(T1_|T2_|T3_)") & !matches(("(_z|_P|_Cut|_Language|Math)$"))) %>%
  pivot_longer(cols = matches("^(T1_|T2_|T3_)") , names_to = "Exercice", values_to = "Score") %>%
  group_by(Exercice) %>%
  summarise(Moyenne_initial = mean(Score, na.rm = T),
            Gender_gap_initial = mean(Score[Sexe == "Boys"], na.rm = T) - mean(Score[Sexe == "Girls"], na.rm = T),
            Sd_initial = sd(Score, na.rm = T),
            Min_initial = min(Score, na.rm = T),
            Max_initial = max(Score, na.rm = T)) %>%
  ungroup() %>%
  mutate(Exercice = paste0(Exercice, "_P"))

minmax__fourth_cohort <- fourth_cohort %>%
  select(Sexe, matches("^(T1_|T2_|T3_)") & !matches(("(_z|_P|_Cut|_Language|Math)$"))) %>%
  pivot_longer(cols = matches("^(T1_|T2_|T3_)") , names_to = "Exercice", values_to = "Score") %>%
  group_by(Exercice) %>%
  summarise(Moyenne_initial = mean(Score, na.rm = T),
            Gender_gap_initial = mean(Score[Sexe == "Boys"], na.rm = T) - mean(Score[Sexe == "Girls"], na.rm = T),
            Sd_initial = sd(Score, na.rm = T),
            Min_initial = min(Score, na.rm = T),
            Max_initial = max(Score, na.rm = T)) %>%
  ungroup() %>%
  mutate(Exercice = paste0(Exercice, "_P"))


first_c_table <- first_cohort %>%
  select(Sexe, matches("_P$")) %>%
  pivot_longer(cols = ends_with("_P"), names_to = "Exercice", values_to = "Score") %>%
  group_by(Exercice) %>%
  summarise(Moyenne = mean(Score, na.rm = T),
            Moyenne_garcon = mean(Score[Sexe == "Boys"], na.rm = T),
            Moyenne_fille = mean(Score[Sexe == "Girls"], na.rm = T),
            Gender_gap = mean(Score[Sexe == "Boys"], na.rm = T) - mean(Score[Sexe == "Girls"], na.rm = T),
            Sd = sd(Score, na.rm = T),
            .groups = "drop") %>%
  left_join(minmax__first_cohort, by = "Exercice")
  
second_c_table <- second_cohort %>%
  select(Sexe, matches("_P$")) %>%
  pivot_longer(cols = ends_with("_P"), names_to = "Exercice", values_to = "Score") %>%
  group_by(Exercice) %>%
  summarise(Moyenne = mean(Score, na.rm = T),
            Moyenne_garcon = mean(Score[Sexe == "Boys"], na.rm = T),
            Moyenne_fille = mean(Score[Sexe == "Girls"], na.rm = T),
            Gender_gap = mean(Score[Sexe == "Boys"], na.rm = T) - mean(Score[Sexe == "Girls"], na.rm = T),
            Sd = sd(Score, na.rm = T),
            .groups = "drop") %>%
  left_join(minmax__second_cohort, by = "Exercice")

third_c_table <- third_cohort %>%
  select(Sexe, matches("_P$")) %>%
  pivot_longer(cols = ends_with("_P"), names_to = "Exercice", values_to = "Score") %>%
  group_by(Exercice) %>%
  summarise(Moyenne = mean(Score, na.rm = T),
            Moyenne_garcon = mean(Score[Sexe == "Boys"], na.rm = T),
            Moyenne_fille = mean(Score[Sexe == "Girls"], na.rm = T),
            Gender_gap = mean(Score[Sexe == "Boys"], na.rm = T) - mean(Score[Sexe == "Girls"], na.rm = T),
            Sd = sd(Score, na.rm = T),
            .groups = "drop") %>%
  left_join(minmax__third_cohort, by = "Exercice")

fourth_c_table <- fourth_cohort %>%
  select(Sexe, matches("_P$")) %>%
  pivot_longer(cols = ends_with("_P"), names_to = "Exercice", values_to = "Score") %>%
  group_by(Exercice) %>%
  summarise(Moyenne = mean(Score, na.rm = T),
            Moyenne_garcon = mean(Score[Sexe == "Boys"], na.rm = T),
            Moyenne_fille = mean(Score[Sexe == "Girls"], na.rm = T),
            Gender_gap = mean(Score[Sexe == "Boys"], na.rm = T) - mean(Score[Sexe == "Girls"], na.rm = T),
            Sd = sd(Score, na.rm = T),
            .groups = "drop") %>%
  left_join(minmax__fourth_cohort, by = "Exercice")

write_csv2(first_c_table, 
           file.path("/home/depp/projets_inter/PSE-ecartsFG/PSE-ecartsFG-echanges/Export/Tables", "first_cohort_mean&gg_exercice.csv"))


write_csv2(second_c_table, 
           file.path("/home/depp/projets_inter/PSE-ecartsFG/PSE-ecartsFG-echanges/Export/Tables", "second_cohort_mean&gg_exercice.csv"))

write_csv2(third_c_table, 
           file.path("/home/depp/projets_inter/PSE-ecartsFG/PSE-ecartsFG-echanges/Export/Tables", "third_cohort_mean&gg_exercice.csv"))

write_csv2(fourth_c_table, 
           file.path("/home/depp/projets_inter/PSE-ecartsFG/PSE-ecartsFG-echanges/Export/Tables", "fourth_cohort_mean&gg_exercice.csv"))




