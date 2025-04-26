
# 0 - Packages and file paths --------------------------------------------------

list_of_packages <- c(
  "tidyverse", 
  "zoo", 
  "fst",
  "data.table"
)
invisible(lapply(list_of_packages, library, character.only = TRUE))
rm(list_of_packages)


conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("select", "dplyr")

### Main repositories
perso <- file.path("/home/depp/chercheurs/ltouitou")
shared <- file.path("/home/depp/projets_inter/PSE-ecartsFG/PSE-ecartsFG-echanges")


### Data repositories
input <- file.path(shared, "Data", "Input")
intermediate <- file.path(shared, "Data", "Intermediate")
output <- file.path(shared, "Data", "Output")
pour_pauline <- file.path(shared, "pour_pauline")


# 1 - Consolidation ------------------------------------------------------------

## (1) Loading the data --------------------------------------------------------

### September evaluations
for (elt in list.files(file.path(input, "Repères_CP-CE1"))) {
  if (str_detect(elt, "[0-9]{2}CPCE1_Donnees(CP|CE1)_crypt\\.rds")) {
    assign(
      paste(
        "reperes", 
        paste0("20", str_extract(elt, "^[0-9]{2}")), 
        str_extract(elt, "(?<=Donnees)(CP|CE1)(?=_crypt)"), 
        sep = "_"
      ), 
      readRDS(file.path(input, "Repères_CP-CE1", elt)) %>% 
        mutate(
          school_year = paste(
            paste0("20", str_extract(elt, "^[0-9]{2}")), 
            as.character(
              as.numeric(paste0("20", str_extract(elt, "^[0-9]{2}"))) + 1
            ), 
            sep = "-"
          ), 
          survey_wave = "sept"
        )
    )
    print(paste(elt, "has been loaded successfully."))
  }
}
rm(elt)


### January evaluations
for (elt in list.files(file.path(input, "Repères_CP-CE1"))) {
  if (str_detect(elt, "[0-9]{2}PECP_Donnees.*_crypt\\.rds")) {
    assign(
      paste(
        "reperes", 
        paste0("20", str_extract(elt, "^[0-9]{2}")), 
        "milieu_CP", 
        sep = "_"
      ), 
      readRDS(file.path(input, "Repères_CP-CE1", elt)) %>% 
        mutate(
          school_year = paste(
            as.character(
              as.numeric(paste0("20", str_extract(elt, "^[0-9]{2}"))) - 1
            ), 
            paste0("20", str_extract(elt, "^[0-9]{2}")), 
            sep = "-"
          ), 
          survey_wave = "janv"
        )
    )
    print(paste(elt, "has been loaded successfully."))
  }
}
rm(elt)

### Uniforming variable names and types
reperes_2018_CP <- reperes_2018_CP %>% 
  rename(ips = apae_IPS)
reperes_2023_milieu_CP <- reperes_2023_milieu_CP %>% 
  mutate(ips = as.double(ips))

### Assembling the data in cohorts
reperes_1819 <- bind_rows(
  reperes_2018_CP, reperes_2019_milieu_CP, reperes_2019_CE1
) %>%
  filter(!is.na(ine_crypt)) %>%
  mutate(grade_level = str_extract(domain_id, "^(CP|CE1)"))

reperes_1920 <- bind_rows(
  reperes_2019_CP, reperes_2020_milieu_CP, reperes_2020_CE1
)%>%
  filter(!is.na(ine_crypt)) %>%
mutate(grade_level = str_extract(domain_id, "^(CP|CE1)"))

reperes_2021 <- bind_rows(
  reperes_2020_CP, reperes_2021_milieu_CP, reperes_2021_CE1
)%>%
  filter(!is.na(ine_crypt)) %>%
mutate(grade_level = str_extract(domain_id, "^(CP|CE1)"))

reperes_2122 <- bind_rows(
  reperes_2021_CP, reperes_2022_milieu_CP, reperes_2022_CE1
)%>%
  filter(!is.na(ine_crypt)) %>%
  mutate(grade_level = str_extract(domain_id, "^(CP|CE1)"))

rm(
  reperes_2018_CP, reperes_2019_milieu_CP, reperes_2019_CE1, # 1st cohort
  reperes_2019_CP, reperes_2020_milieu_CP, reperes_2020_CE1, # 2nd cohort
  reperes_2020_CP, reperes_2021_milieu_CP, reperes_2021_CE1, # 3rd cohort
  reperes_2021_CP, reperes_2022_milieu_CP, reperes_2022_CE1,
  reperes_2022_CP, reperes_2023_milieu_CP)
gc()



list_bases <- list(reperes_1819 = reperes_1819, reperes_1920 = reperes_1920, 
                   reperes_2021 = reperes_2021, reperes_2122 = reperes_2122)

for (i in 1:length(list_bases)) {
  
  print(i)
  
  uai_cp  <- list_bases[[i]] %>%
    filter(grade_level == "CP", survey_wave == "sept") %>%
    select(CodeEleve         = ine_crypt,
           CodeEtablissement = UAI,
           sexe              = sex,
           birthday,
           strate           = strate2,
           IPS              = ips,
           CodeClasse       = class_room_id,
           grade_level) %>%
    group_by(CodeEleve, CodeEtablissement, sexe, birthday, strate, IPS, CodeClasse, grade_level) %>%
    # somme les nombre d'items evalues par etablissement
    mutate(
      n = n()
    ) %>%
    ungroup() %>%
    # garde l'etablissement dans lequel l'enfant a passe le plus d'epreuve
    group_by(CodeEleve) %>%
    slice_max(n, with_ties = F) %>%
    select(-n)
  
  assign(paste("uai_cp", names(list_bases[i]), sep = "_"), uai_cp)
  rm(uai_cp)
  
  uai_ce1  <- list_bases[[i]] %>%
    filter(grade_level == "CE1", survey_wave == "sept") %>%
    select(CodeEleve         = ine_crypt,
           CodeEtablissement = UAI,
           sexe              = sex,
           birthday,
           strate           = strate2,
           IPS              = ips,
           CodeClasse       = class_room_id,
           grade_level) %>%
    group_by(CodeEleve, CodeEtablissement, sexe, birthday, strate, IPS, CodeClasse, grade_level) %>%
    # somme les nombre d'items evalues par etablissement
    mutate(
      n = n()
    ) %>%
    ungroup() %>%
    # garde l'etablissement dans lequel l'enfant a passe le plus d'epreuve
    group_by(CodeEleve) %>%
    slice_max(n, with_ties = F) %>%
    select(-n)
  
  assign(paste("uai_ce1", names(list_bases[i]), sep = "_"), uai_ce1)
  rm(uai_ce1)

  }


# Remerge avec les scores associés ----------------------------------------

# cp
cp_1819  <- uai_cp_reperes_1819 %>%
  left_join(
    reperes_1819 %>% 
      select(CodeEleve = ine_crypt,
             grade_level, 
             domain_id,
             score_dom,
             survey_wave), by = c("CodeEleve", "grade_level")
  )

cp_1920  <- uai_cp_reperes_1920 %>%
  left_join(
    reperes_1920 %>% 
      select(CodeEleve = ine_crypt,
             grade_level, 
             domain_id,
             score_dom,
             survey_wave), by = c("CodeEleve", "grade_level")
  )

cp_2021 <- uai_cp_reperes_2021 %>%
  left_join(
    reperes_2021 %>% 
      select(CodeEleve = ine_crypt,
             grade_level, 
             domain_id,
             score_dom,
             survey_wave), by = c("CodeEleve", "grade_level")
  )

cp_2122 <- uai_cp_reperes_2122 %>%
  left_join(
    reperes_2122 %>% 
      select(CodeEleve = ine_crypt,
             grade_level, 
             domain_id,
             score_dom,
             survey_wave), by = c("CodeEleve", "grade_level")
  )

rm(uai_cp_reperes_1819, uai_cp_reperes_1920, uai_cp_reperes_2021, uai_cp_reperes_2122)

# ce1
ce1_1819  <- uai_ce1_reperes_1819 %>%
  left_join(
    reperes_1819 %>% 
      select(CodeEleve = ine_crypt,
             grade_level, 
             domain_id,
             score_dom,
             survey_wave, 
             domain_id,
             score_dom,
             survey_wave), by = c("CodeEleve", "grade_level")
  )

ce1_1920  <- uai_ce1_reperes_1920 %>%
  left_join(
    reperes_1920 %>% 
      select(CodeEleve = ine_crypt,
             grade_level, 
             domain_id,
             score_dom,
             survey_wave), by = c("CodeEleve", "grade_level")
  )

ce1_2021 <- uai_ce1_reperes_2021 %>%
  left_join(
    reperes_2021%>% 
      select(CodeEleve = ine_crypt,
             grade_level, 
             domain_id,
             score_dom,
             survey_wave), by = c("CodeEleve", "grade_level")
  )

ce1_2122 <- uai_ce1_reperes_2122 %>%
  left_join(
    reperes_2122 %>% 
      select(CodeEleve = ine_crypt,
             grade_level, 
             domain_id,
             score_dom,
             survey_wave), by = c("CodeEleve", "grade_level")
  )

rm(uai_ce1_reperes_1819, uai_ce1_reperes_1920, uai_ce1_reperes_2021, uai_ce1_reperes_2122)

# append bases
reperes_clean1_1819 <- bind_rows(cp_1819, ce1_1819)
reperes_clean1_1920 <- bind_rows(cp_1920, ce1_1920)
reperes_clean1_2021 <- bind_rows(cp_2021, ce1_2021)
reperes_clean1_2122 <- bind_rows(cp_2122, ce1_2122)


# verifier coherence sexe et birthday pour les eleves apres avoir append les bases

list_bases <- list(reperes_clean1_1819, reperes_clean1_1920, reperes_clean1_2021, reperes_clean1_2122)

for (i in 1:length(list_bases)) {
  
  # verif 1
  print(list_bases[[i]] %>% 
    group_by(
      grade_level, survey_wave
    ) %>%
    tally())
  
  # verif 2 (same sex par entre cp et ce1)
  print(list_bases[[i]] %>% 
          select(CodeEleve, grade_level,sexe) %>%
          group_by(
            CodeEleve, grade_level
          ) %>%
          distinct() %>%
          mutate(
            n_sex = n()
          ) %>%
          group_by(n_sex) %>%
          tally())
  
  # verif 3 (same birthday par entre cp et ce1)
  print(list_bases[[i]] %>% 
          select(CodeEleve, grade_level,sexe) %>%
          group_by(
            CodeEleve, grade_level
          ) %>%
          distinct() %>%
          mutate(
            n_birthday = n()
          ) %>%
          group_by(n_birthday) %>%
          tally())
}



# Cleaning scores ---------------------------------------------------------


list_bases <- list(reperes_clean1_1819, reperes_clean1_1920, reperes_clean1_2021, reperes_clean1_2122)
  
for (i in 1:length(list_bases)) {
  
  eleve_sexe <- list_bases[[i]] %>%
    group_by(CodeEleve) %>%
    summarise(
      sexe = dplyr::first(sexe, na_rm = T)
    )
  
  eleve_birthday <- list_bases[[i]] %>%
    group_by(CodeEleve) %>%
    summarise(
      birthday = dplyr::first(birthday, na_rm = T)
    )
  
  list_bases[[i]] <- list_bases[[i]] %>%
    select(-c(birthday,sexe)) %>%
    left_join(eleve_sexe, by = "CodeEleve") %>%
    left_join(eleve_birthday, by = "CodeEleve") %>%
    group_by_at(vars(-score_dom)) %>%
    summarise(score_dom = mean(score_dom))%>%
    ungroup()
    
}

# Pivot wider -------------------------------------------------------------
# attention, repartir des bases qui sont dans list_bases

for (i in 1:length(list_bases)) {
  
  a <- list_bases[[i]] %>%
    mutate(domain_id = case_when(grade_level == "CP" & survey_wave == "sept" ~ paste(domain_id, "Debut", sep = "_"),
                                 grade_level == "CP" & survey_wave == "janv" ~ paste(domain_id, "Mi", sep = "_"),
                                 TRUE ~ domain_id)) 
  
  a_cp_sept <- a %>%
    filter(grade_level == "CP", survey_wave == "sept") %>%
    pivot_wider(
      names_from = domain_id,
      values_from = score_dom
    )  %>%
    rename(
      strate_CP_sept = strate,
      IPS_CP_sept    = IPS,
      CodeEtablissementCP_sept = CodeEtablissement,
      CodeClasseCP_sept        = CodeClasse) %>%
    select(-c(grade_level, survey_wave))  
  
  a_cp_janv <- a %>%
    filter(grade_level == "CP", survey_wave == "janv") %>%
    pivot_wider(
      names_from = domain_id,
      values_from = score_dom
    ) %>%
    rename(
      strate_CP_janv = strate,
      IPS_CP_janv    = IPS,
      CodeEtablissementCP_janv = CodeEtablissement,
      CodeClasseCP_janv        = CodeClasse) %>%
    select(-c(grade_level, survey_wave))  
  
  a_ce1_sept <- a %>%
    filter(grade_level == "CE1", survey_wave == "sept") %>%
    pivot_wider(
      names_from = domain_id,
      values_from = score_dom
    ) %>%
    rename(
      strate_CE1 = strate,
      IPS_CE1    = IPS,
      CodeEtablissementCE1 = CodeEtablissement,
      CodeClasseCE1        = CodeClasse) %>%
    select(-c(grade_level, survey_wave))  
  
  list_bases[[i]] <- a_cp_sept %>%
    full_join(a_cp_janv, by = "CodeEleve") %>%
    full_join(a_ce1_sept, by = "CodeEleve") %>%
    mutate(
      CodeEtablissementCP =
        case_when(
          !is.na(CodeEtablissementCP_sept) ~ CodeEtablissementCP_sept,
          !is.na(CodeEtablissementCP_janv) ~ CodeEtablissementCP_janv,
          TRUE ~ NA
        ),
      CodeClasseCP =
        case_when(
          !is.na(CodeClasseCP_sept) ~ CodeClasseCP_sept,
          !is.na(CodeClasseCP_janv) ~ CodeClasseCP_janv,
          TRUE ~ NA
        ),
      strate_CP =
        case_when(
          !is.na(strate_CP_sept) ~ strate_CP_sept,
          !is.na(strate_CP_janv) ~ strate_CP_janv,
          TRUE ~ NA
        ),
      IPS_CP =
        case_when(
          !is.na(IPS_CP_sept) ~ IPS_CP_sept,
          !is.na(IPS_CP_janv) ~ IPS_CP_janv,
          TRUE ~ NA
        ),
      sexe =
        case_when(
          !is.na(sexe) ~ sexe,
          !is.na(sexe.x) ~ sexe.x,
          !is.na(sexe.y) ~ sexe.y,
          TRUE ~ NA
        ),
      birthday =
        case_when(
          !is.na(birthday) ~ birthday,
          !is.na(birthday.x) ~ birthday.x,
          !is.na(birthday.y) ~ birthday.y,
          TRUE ~ NA
        )
    ) %>%
    select(-c(sexe.x, sexe.y, birthday.x, birthday.y, CodeEtablissementCP_sept, CodeEtablissementCP_janv,
              CodeClasseCP_sept, CodeClasseCP_janv, strate_CP_sept, strate_CP_janv, 
              IPS_CP_sept, IPS_CP_janv))
  
  rm(a_cp_sept, a_cp_janv, a_ce1_sept)
  
  # write_rds(list_bases[[i]], 
  #   paste0(intermediate, '/', "reperes_preprocess_", i, ".rds"))
  
  write_rds(list_bases[[i]],
            paste0(pour_pauline, "/", "reperes_preprocess_", i, ".rds"))
  
  
}


# 0 - Packages and file paths --------------------------------------------------

list_of_packages <- c(
  "tidyverse", 
  "zoo", 
  "fst",
  "data.table"
)
invisible(lapply(list_of_packages, library, character.only = TRUE))
rm(list_of_packages)


conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("select", "dplyr")

### Main repositories
perso <- file.path("/home/depp/chercheurs/ltouitou")
shared <- file.path("/home/depp/projets_inter/PSE-ecartsFG/PSE-ecartsFG-echanges")


### Data repositories
input <- file.path(shared, "Data", "Input")
intermediate <- file.path(shared, "Data", "Intermediate")
output <- file.path(shared, "Data", "Output")

# 1 - Loading the data --------------------------------------------------------

### September evaluations
for (elt in list.files(file.path(input, "Repères_CP-CE1"))) {
  if (str_detect(elt, "[0-9]{2}CPCE1_Donnees(CP|CE1)_crypt\\.rds")) {
    assign(
      paste(
        "reperes", 
        paste0("20", str_extract(elt, "^[0-9]{2}")), 
        str_extract(elt, "(?<=Donnees)(CP|CE1)(?=_crypt)"), 
        sep = "_"
      ), 
      readRDS(file.path(input, "Repères_CP-CE1", elt)) %>% 
        mutate(
          school_year = paste(
            paste0("20", str_extract(elt, "^[0-9]{2}")), 
            as.character(
              as.numeric(paste0("20", str_extract(elt, "^[0-9]{2}"))) + 1
            ), 
            sep = "-"
          ), 
          survey_wave = "sept"
        )
    )
    print(paste(elt, "has been loaded successfully."))
  }
}
rm(elt)


### January evaluations
for (elt in list.files(file.path(input, "Repères_CP-CE1"))) {
  if (str_detect(elt, "[0-9]{2}PECP_Donnees.*_crypt\\.rds")) {
    assign(
      paste(
        "reperes", 
        paste0("20", str_extract(elt, "^[0-9]{2}")), 
        "milieu_CP", 
        sep = "_"
      ), 
      readRDS(file.path(input, "Repères_CP-CE1", elt)) %>% 
        mutate(
          school_year = paste(
            as.character(
              as.numeric(paste0("20", str_extract(elt, "^[0-9]{2}"))) - 1
            ), 
            paste0("20", str_extract(elt, "^[0-9]{2}")), 
            sep = "-"
          ), 
          survey_wave = "janv"
        )
    )
    print(paste(elt, "has been loaded successfully."))
  }
}
rm(elt)

### Uniforming variable names and types
reperes_2018_CP <- reperes_2018_CP %>% 
  rename(ips = apae_IPS)
reperes_2023_milieu_CP <- reperes_2023_milieu_CP %>% 
  mutate(ips = as.double(ips))

# 2 - Correcting class id for first cohort -------------------------------------

id_classe_2018 <- reperes_2019_milieu_CP %>%
  select(ID_Eleve = ine_crypt, ID_Etab_CP = UAI, ID_Classe_CP = classe ) %>%
  filter(!is.na(ID_Eleve),
         !is.na(ID_Classe_CP),
         !is.na(ID_Etab_CP)) %>%
  distinct() %>%
  group_by(ID_Eleve, ID_Etab_CP) %>%
  slice(1) %>%
  ungroup()

# 3 - Merge and save of file -----------------------------------------

data_2018 <- readRDS(paste0(intermediate, '/', "reperes_preprocess_2018_2019_P.rds"))

data_2018 <- data_2018 %>%
  left_join(id_classe_2018, by = c("CodeEleve" = "ID_Eleve", "CodeEtablissementCP" = "ID_Etab_CP")) %>%
  select(-CodeClasseCP) %>%
  rename("CodeClasseCP" = "ID_Classe_CP")

write_rds(data_2018, paste0(intermediate, '/', "reperes_preprocess_2018_2019_P.rds"))


# Paths Lola

```{r}

### Main repositories
perso <- file.path("/home/depp/chercheurs/ltouitou")
shared <- file.path("/home/depp/projets_inter/PSE-ecartsFG/PSE-ecartsFG-echanges")


### Data repositories
input <- file.path(shared, "Data", "Input")
intermediate <- file.path(shared, "Data", "Intermediate")
output <- file.path(shared, "Data", "Output")

``` 

# Cohort - Parameters

```{r}

# Parameters 
ANNEE_COHORTE = params$cohort_year #  2018 2019 2020 2021

# Age in month (with limits to detect outliers)
AGE_PRECOCE    = 69   # parameter used to classify a child being in advance, but not an outlier
AGE_NORMAL_MOY = 74   # helps to define subcategories for age
AGE_RETARD     = 80   # parameter used to classify a child being late, but not an outlier
AGE_MIN        = 51   # all children with age below this age are considered outliers
AGE_MAX        = 98   # all children with age above this age are considered outliers
AGE_NORM_BAS   = 57   # all children with age between this and AGE_PRECOCE are considered 1 year in advance
AGE_NORM_SUP   = 92   # all children with age between this and AGE_RETARD are considered 1 year late

PATH_IMAGE = "~/img/" # where to store output

# threshold on class size
SEUIL_INF_CLASS = 5    # as calculating heterogeneity and mixity in very small classes might be confusing
SEUIL_SUP_CLASS = 28   # all classes above are considered outliers
SEUIL_TAILLE    = 13   # Political measure on small classe put the threshold at 12 

TEST = FALSE # TRUE if the notebook is currently being tested and want to run "fast" the pipeline

```

# Load data

```{r}

if (ANNEE_COHORTE == 2018){
  data_depp_1 <- readRDS(paste0(intermediate, '/', "reperes_preprocess_2018_2019_P.rds"))
  } else if (ANNEE_COHORTE == 2019){
  data_depp_1 <-  readRDS(paste0(intermediate, '/', "reperes_preprocess_2019_2020_P.rds"))
  } else if (ANNEE_COHORTE == 2020){
  data_depp_1 <-  readRDS(paste0(intermediate, '/', "reperes_preprocess_2020_2021_P.rds"))
  } else if (ANNEE_COHORTE == 2021){
  data_depp_1 <- readRDS(paste0(intermediate, '/', "reperes_preprocess_2021_2022_P.rds"))
  } else {
  print("Error in ANNEE_COHORTE or data location")
  }
if(TEST){
  set.seed(7)
  data_depp_1 <- data_depp_1 %>%
    slice_sample(prop = 0.05)
}
```


# Libraries & Packages 

```{r}

library(dplyr)
library(ggplot2)
library(naniar)
library(reshape2) 
library(UpSetR) # for upset plot (equivalence of Venn diagram)
library(mice) # imputation
library(LambertW) # Z scores

select <- dplyr::select

knitr::opts_chunk$set(echo = TRUE)
```


# Detect empty raw if any, and warn.

```{r}

# indicator of rows with only NA
ind <- apply(data_depp_1, 1, function(x) all(is.na(x)))
if (sum(ind) > 0){
  print("WARNING:")
  print(sum(ind))
  print(" rows contain only NA. They are deleted")
  data_depp_1 <- data_depp_1[!ind,]
} else {
  print("GOOD: No rows with only NAs.")
}

```

Note that the raw data contains `r nrow(data_depp_1)` children.

# Check nature of the column

```{r}

sapply(data_depp_1, class)

if (ANNEE_COHORTE == 2018){
  
    data_depp_1$sexe              <- as.factor(as.character(data_depp_1$sexe))
    data_depp_1$strate_CP         <- as.factor(as.character(data_depp_1$strate_CP))
    data_depp_1$strate_CE1        <- as.factor(as.character(data_depp_1$strate_CE1))
    data_depp_1$IPS_CP            <- as.numeric(as.character(data_depp_1$IPS_CP))
    data_depp_1$IPS_CE1           <- as.numeric(as.character(data_depp_1$IPS_CE1))
    
  } else if (ANNEE_COHORTE == 2019){
    
    data_depp_1$sexe              <- as.factor(as.character(data_depp_1$sexe))
    data_depp_1$strate_CP         <- as.factor(as.character(data_depp_1$strate_CP))
    data_depp_1$strate_CE1        <- as.factor(as.character(data_depp_1$strate_CE1))
    data_depp_1$IPS_CP            <- as.numeric(as.character(data_depp_1$IPS_CP))
    data_depp_1$IPS_CE1           <- as.numeric(as.character(data_depp_1$IPS_CE1))
    
    
  } else if (ANNEE_COHORTE == 2020){
    data_depp_1$sexe              <- as.factor(as.character(data_depp_1$sexe))
    data_depp_1$strate_CP         <- as.factor(as.character(data_depp_1$strate_CP))
    data_depp_1$strate_CE1        <- as.factor(as.character(data_depp_1$strate_CE1))
    data_depp_1$IPS_CP            <- as.numeric(as.character(data_depp_1$IPS_CP))
    data_depp_1$IPS_CE1           <- as.numeric(as.character(data_depp_1$IPS_CE1))
  
  } else if (ANNEE_COHORTE == 2021){
    data_depp_1$sexe              <- as.factor(as.character(data_depp_1$sexe))
    data_depp_1$strate_CP         <- as.factor(as.character(data_depp_1$strate_CP))
    data_depp_1$strate_CE1        <- as.factor(as.character(data_depp_1$strate_CE1))
    data_depp_1$IPS_CP            <- as.numeric(as.character(data_depp_1$IPS_CP))
    data_depp_1$IPS_CE1           <- as.numeric(as.character(data_depp_1$IPS_CE1))
    
  }

sapply(data_depp_1, class)

```


# Renaming variables

This part renames columns, and takes into account the fact that data come from four cohort that contained different columns names and/or columns.

```{r echo=TRUE}

# Child and school characteristics

names(data_depp_1)[names(data_depp_1) == "strate_CE1"]            <- "Categ_Etab_CE1"
names(data_depp_1)[names(data_depp_1) == "sexe"]                 <- "Sexe"
names(data_depp_1)[names(data_depp_1) == "strate_CP"]            <- "Categ_Etab_CP"
names(data_depp_1)[names(data_depp_1) == "birthday"]             <- "Date_Naiss"
names(data_depp_1)[names(data_depp_1) == "CodeEleve"]            <- "ID_Eleve"
names(data_depp_1)[names(data_depp_1) == "CodeEtablissementCP"]  <- "ID_Etab_CP"
names(data_depp_1)[names(data_depp_1) == "CodeClasseCP"]         <- "ID_Classe_CP"
names(data_depp_1)[names(data_depp_1) == "IPS_CP"]               <- "IPS_Etab_CP"  


# Language - First Period

names(data_depp_1)[names(data_depp_1) == "CPFCOMO_Debut"] <- "T1_Comp_Mots"
names(data_depp_1)[names(data_depp_1) == "CPFCOPH_Debut"] <- "T1_Comp_Phra"
names(data_depp_1)[names(data_depp_1) == "CPFCOTE_Debut"] <- "T1_Comp_Text"
names(data_depp_1)[names(data_depp_1) == "CPFPHPH_Debut"] <- "T1_Manip_Phon"
names(data_depp_1)[names(data_depp_1) == "CPFPHSY_Debut"] <- "T1_Manip_Syll"
names(data_depp_1)[names(data_depp_1) == "CPFRLGP_Debut"] <- "T1_Conn_Lettres"
names(data_depp_1)[names(data_depp_1) == "CPFRLLE_Debut"] <- "T1_Recon_Ecritu_L"
names(data_depp_1)[names(data_depp_1) == "CPFRLSY_Debut"] <- "T1_Compa_Lettres"

# Math - First Period

names(data_depp_1)[names(data_depp_1) == "CPMETNOCOEN_Debut"] <- "T1_Ecri_Nombre"
names(data_depp_1)[names(data_depp_1) == "CPMETNOCORN_Debut"] <- "T1_Lire_Nombre"
names(data_depp_1)[names(data_depp_1) == "CPMETNOPR_Debut"]   <- "T1_Resoud_Pb"
names(data_depp_1)[names(data_depp_1) == "CPMUTNOCA_Debut"]   <- "T1_Denombrer"
names(data_depp_1)[names(data_depp_1) == "CPMUTNOGN_Debut"]   <- "T1_Compa_Nombre"
names(data_depp_1)[names(data_depp_1) == "CPMUTNOLN_Debut"]   <- "T1_Ligne_Num"


# Language - Second Period

names(data_depp_1)[names(data_depp_1) == "CPFCOPH_Mi"]       <- "T2_Comp_Phra"
names(data_depp_1)[names(data_depp_1) == "CPFLILVMO_Mi"]     <- "T2_Lire_Mots"
names(data_depp_1)[names(data_depp_1) == "CPFLILVTE_Mi"]     <- "T2_Lire_Text"
names(data_depp_1)[names(data_depp_1) == "CPFLOE_Mi"]        <- "T2_Ecri_Syll"
names(data_depp_1)[names(data_depp_1) == "CPFLOR_Mi"]        <- "T2_Ecri_Mots"
names(data_depp_1)[names(data_depp_1) == "CPFPHPH_Mi"]       <- "T2_Manip_Phon"
names(data_depp_1)[names(data_depp_1) == "CPFRLGP_Mi"]       <- "T2_Conn_Lettres"

if (ANNEE_COHORTE == 2019){
  names(data_depp_1)[names(data_depp_1) == "CPFLICP_Mi"]       <- "T2_Comp_Phra_Lu" # New variable Max 8/8 - understanding sentences read alone

} else if (ANNEE_COHORTE == 2020){
  names(data_depp_1)[names(data_depp_1) == "CPFLICP_Mi"]       <- "T2_Comp_Phra_Lu"  # new variable in 2021 
}


# Math - Second Period

names(data_depp_1)[names(data_depp_1) == "CPMNOCACOGN_Mi"]   <- "T2_Compa_Nombre"
names(data_depp_1)[names(data_depp_1) == "CPMNOCACOLN_Mi"]   <- "T2_Ligne_Num"
names(data_depp_1)[names(data_depp_1) == "CPMNOCANECLAD_Mi"] <- "T2_Addition"
names(data_depp_1)[names(data_depp_1) == "CPMNOCANECLSO_Mi"] <- "T2_Soustract"
names(data_depp_1)[names(data_depp_1) == "CPMNOCANOEN_Mi"]   <- "T2_Ecri_Nombre"
names(data_depp_1)[names(data_depp_1) == "CPMNOCARP_Mi"]     <- "T2_Resoud_Pb"


if (ANNEE_COHORTE == 2018){
  
  # Math - First Period
  names(data_depp_1)[names(data_depp_1) == "CPMEGAS_Debut"]     <- "T1_Assemblage"   # Modif Lola
  
  # Language - Third Period
  names(data_depp_1)[names(data_depp_1) == "CE1COMO"]        <- "T3_Comp_Mots"
  names(data_depp_1)[names(data_depp_1) == "CE1COPH"]        <- "T3_Comp_Phra"
  names(data_depp_1)[names(data_depp_1) == "CE1FLOE"]        <- "T3_Ecri_Syll"
  names(data_depp_1)[names(data_depp_1) == "CE1FLOR"]        <- "T3_Ecri_Mots"
  names(data_depp_1)[names(data_depp_1) == "CE1LICP"]        <- "T3_Comp_Phra_Lu"
  names(data_depp_1)[names(data_depp_1) == "CE1LICTEN"]      <- "T3_Comp_Text_Lu"
  names(data_depp_1)[names(data_depp_1) == "CE1LILVMO"]      <- "T3_Lire_Mots"
  names(data_depp_1)[names(data_depp_1) == "CE1LILVTE"]      <- "T3_Lire_Text"
  
  # Math - Third Period
  names(data_depp_1)[names(data_depp_1) == "CE1MEGAS"]       <- "T3_Assemblage"
  names(data_depp_1)[names(data_depp_1) == "CE1MNOCACOLN"]   <- "T3_Ligne_Num"
  names(data_depp_1)[names(data_depp_1) == "CE1MNOCANECLAD"] <- "T3_Addition"
  names(data_depp_1)[names(data_depp_1) == "CE1MNOCANECLSO"] <- "T3_Soustract"
  names(data_depp_1)[names(data_depp_1) == "CE1MNOCANECM"]   <- "T3_Calcul_Mental"
  names(data_depp_1)[names(data_depp_1) == "CE1MNOCANOEN"]   <- "T3_Ecri_Nombre"
  names(data_depp_1)[names(data_depp_1) == "CE1MNOCANORC"]   <- "T3_Lire_Nombre"
  names(data_depp_1)[names(data_depp_1) == "CE1MNOCANORP"]   <- "T3_Repres_Nb"
  names(data_depp_1)[names(data_depp_1) == "CE1MNOCARP"]     <- "T3_Resoud_Pb"
  
} else if (ANNEE_COHORTE == 2019) {
  
    # Math - First Period
  names(data_depp_1)[names(data_depp_1) == "CPMEGAS_Debut"]     <- "T1_Assemblage"   # Modif Lola
  
  # Language - Third Period
  names(data_depp_1)[names(data_depp_1) == "CE1COMO"]            <- "T3_Comp_Mots"
  names(data_depp_1)[names(data_depp_1) == "CE1COPH"]            <- "T3_Comp_Phra"
  names(data_depp_1)[names(data_depp_1) == "CE1FLOE"]            <- "T3_Ecri_Syll"
  names(data_depp_1)[names(data_depp_1) == "CE1FLOR"]            <- "T3_Ecri_Mots"
  names(data_depp_1)[names(data_depp_1) == "CE1LICP"]            <- "T3_Comp_Phra_Lu"
  names(data_depp_1)[names(data_depp_1) == "CE1LICTEN"]          <- "T3_Comp_Text_Lu"
  names(data_depp_1)[names(data_depp_1) == "CE1LILVMO"]          <- "T3_Lire_Mots"
  names(data_depp_1)[names(data_depp_1) == "CE1LILVTE"]          <- "T3_Lire_Text"
  
  # Math - Third Period  
  names(data_depp_1)[names(data_depp_1) == "CE1MEGAS"]       <- "T3_Assemblage"
  names(data_depp_1)[names(data_depp_1) == "CE1MNOCACOLN"]   <- "T3_Ligne_Num"
  names(data_depp_1)[names(data_depp_1) == "CE1MNOCANECLAD"] <- "T3_Addition"
  names(data_depp_1)[names(data_depp_1) == "CE1MNOCANECLSO"] <- "T3_Soustract"
  names(data_depp_1)[names(data_depp_1) == "CE1MNOCANECM"]   <- "T3_Calcul_Mental"
  names(data_depp_1)[names(data_depp_1) == "CE1MNOCANOEN"]   <- "T3_Ecri_Nombre"
  names(data_depp_1)[names(data_depp_1) == "CE1MNOCANORC"]   <- "T3_Lire_Nombre"
  names(data_depp_1)[names(data_depp_1) == "CE1MNOCANORP"]   <- "T3_Repres_Nb"
  names(data_depp_1)[names(data_depp_1) == "CE1MNOCARP"]     <- "T3_Resoud_Pb"
  
} else if (ANNEE_COHORTE == 2020) {    
  
  # Math - First Period
  names(data_depp_1)[names(data_depp_1) == "CPMEGAS_Debut"]     <- "T1_Assemblage"   # Modif Lola
  
  # Language - Third Period
  names(data_depp_1)[names(data_depp_1) == "CE1COMO"]            <- "T3_Comp_Mots"
  names(data_depp_1)[names(data_depp_1) == "CE1COPH"]            <- "T3_Comp_Phra"
  names(data_depp_1)[names(data_depp_1) == "CE1FLOE"]            <- "T3_Ecri_Syll"
  names(data_depp_1)[names(data_depp_1) == "CE1FLOR"]            <- "T3_Ecri_Mots"
  names(data_depp_1)[names(data_depp_1) == "CE1LICP"]            <- "T3_Comp_Phra_Lu"
  names(data_depp_1)[names(data_depp_1) == "CE1LICTEN"]          <- "T3_Comp_Text_Lu"
  names(data_depp_1)[names(data_depp_1) == "CE1LILVMO"]          <- "T3_Lire_Mots"
  names(data_depp_1)[names(data_depp_1) == "CE1LILVTE"]          <- "T3_Lire_Text"
  
  # Math - Third Period  
  names(data_depp_1)[names(data_depp_1) == "CE1MEGAS"]       <- "T3_Assemblage"
  names(data_depp_1)[names(data_depp_1) == "CE1MNOCACOLN"]   <- "T3_Ligne_Num"
  names(data_depp_1)[names(data_depp_1) == "CE1MNOCANECLAD"] <- "T3_Addition"
  names(data_depp_1)[names(data_depp_1) == "CE1MNOCANECLSO"] <- "T3_Soustract"
  names(data_depp_1)[names(data_depp_1) == "CE1MNOCANECM"]   <- "T3_Calcul_Mental"
  names(data_depp_1)[names(data_depp_1) == "CE1MNOCANOEN"]   <- "T3_Ecri_Nombre"
  names(data_depp_1)[names(data_depp_1) == "CE1MNOCANORC"]   <- "T3_Lire_Nombre"
  names(data_depp_1)[names(data_depp_1) == "CE1MNOCANORP"]   <- "T3_Repres_Nb"
  names(data_depp_1)[names(data_depp_1) == "CE1MNOCARP"]     <- "T3_Resoud_Pb"


} else if (ANNEE_COHORTE == 2021) {

# Language - First Period

names(data_depp_1)[names(data_depp_1) == "CPFCOMO_Debut"] <- "T1_Comp_Mots"
names(data_depp_1)[names(data_depp_1) == "CPFCOPH_Debut"] <- "T1_Comp_Phra"
names(data_depp_1)[names(data_depp_1) == "CPFCOTE_Debut"] <- "T1_Comp_Text"
names(data_depp_1)[names(data_depp_1) == "CPFPHPH_Debut"] <- "T1_Manip_Phon"
names(data_depp_1)[names(data_depp_1) == "CPFPHSY_Debut"] <- "T1_Manip_Syll"
names(data_depp_1)[names(data_depp_1) == "CPFRLGP_Debut"] <-"T1_Conn_Lettres"
names(data_depp_1)[names(data_depp_1) == "CPFRLLE_Debut"] <- "T1_Recon_Ecritu_L"
names(data_depp_1)[names(data_depp_1) == "CPFRLSY_Debut"] <- "T1_Compa_Lettres"



# Math - First Period
names(data_depp_1)[names(data_depp_1) == "CPMEGAS_Debut"]     <- "T1_Assemblage"   
names(data_depp_1)[names(data_depp_1) == "CPMETNOCOEN_Debut"] <- "T1_Ecri_Nombre"
names(data_depp_1)[names(data_depp_1) == "CPMETNOCORN_Debut"] <- "T1_Lire_Nombre"
names(data_depp_1)[names(data_depp_1) == "CPMETNOPR_Debut"]   <- "T1_Resoud_Pb"
names(data_depp_1)[names(data_depp_1) == "CPMUTNOCA_Debut"]   <- "T1_Denombrer"
names(data_depp_1)[names(data_depp_1) == "CPMUTNOGN_Debut"]   <- "T1_Compa_Nombre"
names(data_depp_1)[names(data_depp_1) == "CPMUTNOLN_Debut"]   <- "T1_Ligne_Num"


# Language - Second Period

names(data_depp_1)[names(data_depp_1) == "CPFLICP_Mi"]       <- "T2_Comp_Phra_Lu"
names(data_depp_1)[names(data_depp_1) == "CPFCOPH_Mi"]       <- "T2_Comp_Phra"
names(data_depp_1)[names(data_depp_1) == "CPFLILVMO_Mi"]     <- "T2_Lire_Mots"
names(data_depp_1)[names(data_depp_1) == "CPFLILVTE_Mi"]     <- "T2_Lire_Text"
names(data_depp_1)[names(data_depp_1) == "CPFLOE_Mi"]        <- "T2_Ecri_Syll"
names(data_depp_1)[names(data_depp_1) == "CPFLOR_Mi"]        <- "T2_Ecri_Mots"
names(data_depp_1)[names(data_depp_1) == "CPFPHPH_Mi"]       <- "T2_Manip_Phon"
names(data_depp_1)[names(data_depp_1) == "CPFRLGP_Mi"]       <- "T2_Conn_Lettres"


# Math - Second Period

names(data_depp_1)[names(data_depp_1) == "CPMNOCACOGN_Mi"]   <- "T2_Compa_Nombre"
names(data_depp_1)[names(data_depp_1) == "CPMNOCACOLN_Mi"]   <- "T2_Ligne_Num"
names(data_depp_1)[names(data_depp_1) == "CPMNOCANECLAD_Mi"] <- "T2_Addition"
names(data_depp_1)[names(data_depp_1) == "CPMNOCANECLSO_Mi"] <- "T2_Soustract"
names(data_depp_1)[names(data_depp_1) == "CPMNOCANOEN_Mi"]   <- "T2_Ecri_Nombre"
names(data_depp_1)[names(data_depp_1) == "CPMNOCARP_Mi"]     <- "T2_Resoud_Pb"

  # Language - Third Period
  names(data_depp_1)[names(data_depp_1) == "CE1COMO"]            <- "T3_Comp_Mots"
  names(data_depp_1)[names(data_depp_1) == "CE1COPH"]            <- "T3_Comp_Phra"
  names(data_depp_1)[names(data_depp_1) == "CE1FLOE"]            <- "T3_Ecri_Syll"
  names(data_depp_1)[names(data_depp_1) == "CE1FLOR"]            <- "T3_Ecri_Mots"
  names(data_depp_1)[names(data_depp_1) == "CE1LICP"]            <- "T3_Comp_Phra_Lu"
  names(data_depp_1)[names(data_depp_1) == "CE1LICTEN"]          <- "T3_Comp_Text_Lu"
  names(data_depp_1)[names(data_depp_1) == "CE1LILVMO"]          <- "T3_Lire_Mots"
  names(data_depp_1)[names(data_depp_1) == "CE1LILVTE"]          <- "T3_Lire_Text"
  
  # Math - Third Period  
  names(data_depp_1)[names(data_depp_1) == "CE1MEGAS"]       <- "T3_Assemblage"
  names(data_depp_1)[names(data_depp_1) == "CE1MNOCACOLG"]   <- "T3_Ligne_Num"
  names(data_depp_1)[names(data_depp_1) == "CE1MNOCANECLAD"] <- "T3_Addition"
  names(data_depp_1)[names(data_depp_1) == "CE1MNOCANECLSO"] <- "T3_Soustract"
  names(data_depp_1)[names(data_depp_1) == "CE1MNOCANECM"]   <- "T3_Calcul_Mental"
  names(data_depp_1)[names(data_depp_1) == "CE1MNOCANOEN"]   <- "T3_Ecri_Nombre"
  names(data_depp_1)[names(data_depp_1) == "CE1MNOCANORC"]   <- "T3_Lire_Nombre"
  names(data_depp_1)[names(data_depp_1) == "CE1MNOCARP"]     <- "T3_Resoud_Pb"

}


Lang_T1 <- c("T1_Comp_Mots",
             "T1_Comp_Phra",
             "T1_Comp_Text",
             "T1_Manip_Phon",
             "T1_Manip_Syll",
             "T1_Conn_Lettres",
             "T1_Recon_Ecritu_L",
             "T1_Compa_Lettres")

Math_T1 <- c("T1_Ecri_Nombre",
             "T1_Lire_Nombre",
             "T1_Resoud_Pb",
             "T1_Denombrer",
             "T1_Compa_Nombre",
             "T1_Ligne_Num")

Lang_T2 <- c("T2_Comp_Phra",
             "T2_Lire_Mots",
             "T2_Lire_Text",
             "T2_Ecri_Syll",
             "T2_Ecri_Mots",
             "T2_Manip_Phon",
             "T2_Conn_Lettres")

Math_T2 <- c("T2_Compa_Nombre",
             "T2_Ligne_Num",
             "T2_Addition",
             "T2_Soustract",
             "T2_Ecri_Nombre",
             "T2_Resoud_Pb")

Lang_T3 <- c("T3_Comp_Mots",
             "T3_Comp_Phra",
             "T3_Ecri_Syll",
             "T3_Ecri_Mots",
             "T3_Comp_Phra_Lu",
             "T3_Comp_Text_Lu",
             "T3_Lire_Mots",
             "T3_Lire_Text")

Math_T3 <- c("T3_Assemblage",
             "T3_Ligne_Num",
             "T3_Addition",
             "T3_Soustract",
             "T3_Calcul_Mental",
             "T3_Ecri_Nombre",
             "T3_Lire_Nombre",
             "T3_Resoud_Pb")


```

# Covariates to compare 

In order to compare similar tests between 2018, 2019, 2020 and 2021

```{r echo=TRUE}


# SIMILAR (n:37) = The following list contains 37 tests for which the exact same tests were evaluated for cohort 1 and cohort 2 with identical number of items per tests

same_notes <- c("T1_Comp_Mots", "T1_Comp_Phra", "T1_Manip_Phon", "T1_Manip_Syll", "T1_Conn_Lettres","T1_Recon_Ecritu_L",
                "T1_Ecri_Nombre", "T1_Lire_Nombre", "T1_Resoud_Pb", "T1_Denombrer","T1_Ligne_Num",
                "T2_Comp_Phra","T2_Lire_Mots", "T2_Lire_Text", "T2_Ecri_Syll", "T2_Ecri_Mots", "T2_Manip_Phon", "T2_Conn_Lettres",
                "T2_Compa_Nombre", "T2_Ligne_Num","T2_Ecri_Nombre", "T2_Resoud_Pb", 
                "T3_Comp_Mots", "T3_Comp_Phra",  "T3_Ecri_Syll", "T3_Ecri_Mots", "T3_Comp_Phra_Lu", "T3_Comp_Text_Lu",
                "T3_Lire_Mots", "T3_Lire_Text",
                "T3_Assemblage", "T3_Ligne_Num", "T3_Calcul_Mental", "T3_Ecri_Nombre", "T3_Lire_Nombre","T3_Repres_Nb", "T3_Resoud_Pb")

# QUASI SIMILAR (n:44) = the following "notes" list, includes similar skills evaluated for cohort 2018  and 2019 BUT changes have been made for the 7 following tests (not the same number of items inside between the  2 cohorts):  "T1_Comp_Text", "T1_Compa_Lettres" , "T1_Compa_Nombre", "T2_Addition" ,"T2_Soustract",  "T3_Addition" , "T3_Soustract"

notes <- c(Lang_T1, Math_T1, Lang_T2, Math_T2, Lang_T3, Math_T3) 

```




# Rename categories

```{r}
if(ANNEE_COHORTE == 2018){
data_depp_1$Categ_Etab_CP <- case_when(data_depp_1$Categ_Etab_CP == "Public Hors REP" ~ "Public",
                                       data_depp_1$Categ_Etab_CP == "Prive" ~ "Private",
                                       data_depp_1$Categ_Etab_CP == "REP" ~ "REP",
                                       data_depp_1$Categ_Etab_CP == "REP+" ~ "REP+")

data_depp_1$Categ_Etab_CP <- factor(data_depp_1$Categ_Etab_CP, 
                                 levels = c("Private","Public", "REP", "REP+"))

} else if (ANNEE_COHORTE == 2019){
data_depp_1$Categ_Etab_CP <- case_when(data_depp_1$Categ_Etab_CP == "Public Hors EP" ~ "Public",
                                       data_depp_1$Categ_Etab_CP == "Prive" ~ "Private",
                                       data_depp_1$Categ_Etab_CP == "REP" ~ "REP",
                                       data_depp_1$Categ_Etab_CP == "REP+" ~ "REP+")

data_depp_1$Categ_Etab_CP <- factor(data_depp_1$Categ_Etab_CP, 
                                 levels = c("Private","Public", "REP", "REP+"))

} else if (ANNEE_COHORTE == 2020){
data_depp_1$Categ_Etab_CP <- case_when(data_depp_1$Categ_Etab_CP == "Public Hors EP" ~ "Public",
                                       data_depp_1$Categ_Etab_CP == "Prive" ~ "Private",
                                       data_depp_1$Categ_Etab_CP == "REP" ~ "REP",
                                       data_depp_1$Categ_Etab_CP == "REP+" ~ "REP+")

data_depp_1$Categ_Etab_CP <- factor(data_depp_1$Categ_Etab_CP, 
                                 levels = c("Private","Public", "REP", "REP+"))


} else if (ANNEE_COHORTE == 2021){
  

data_depp_1$Categ_Etab_CP <- as.factor(data_depp_1$Categ_Etab_CP)

data_depp_1$Categ_Etab_CP <- case_when(data_depp_1$Categ_Etab_CP == "Public Hors EP" ~ "Public",
                                       data_depp_1$Categ_Etab_CP == "Prive" ~ "Private",
                                       data_depp_1$Categ_Etab_CP == "REP" ~ "REP",
                                       data_depp_1$Categ_Etab_CP == "REP+" ~ "REP+")


data_depp_1$Categ_Etab_CP <- factor(data_depp_1$Categ_Etab_CP, 
                                 levels = c("Private","Public", "REP", "REP+"))  
summary(data_depp_1$Categ_Etab_CP)
}


```


# Creation of age variable

Note that birth date is stored as YEAR-MONTH in the raw data frame, for example:

```{r}

print(data_depp_1$Date_Naiss[1:10])

```


```{r}
# Create year and month from the column Date_Naiss

year     <- as.integer(substring(data_depp_1$Date_Naiss, 1, 4)) 
month    <- as.integer(substring(data_depp_1$Date_Naiss, 6, 7))



# sanity checks (all months values are in the good range, and all years are with 4 digits after 2000)
if(!any(month %in% 1:12)){print('WARNING: some month values are out of range')} else {print("GOOD: month values are all coherent")}
if(!any(year %in% 2000:2021)){print('WARNING: some year values are out of range')} else {print("GOOD: year values are all coherent")}

# Create age

data_depp_1$Age_CP  <- (ANNEE_COHORTE - year)*12 + (9-month)
data_depp_1$Age_CP <- as.numeric(data_depp_1$Age_CP)
summary(data_depp_1$Age_CP)


# Flag normal age or not
data_depp_1$Age_categ <- case_when(data_depp_1$Age_CP >= AGE_MIN & data_depp_1$Age_CP < AGE_PRECOCE       ~ "Young", # 1 year ahead
                                AGE_PRECOCE <= data_depp_1$Age_CP & data_depp_1$Age_CP <= AGE_RETARD    ~ "Normal",
                                data_depp_1$Age_CP > AGE_RETARD & data_depp_1$Age_CP <= AGE_MAX         ~ "Late") # 1 year late
# reorder columns 
data_depp_1$Age_categ <- factor(data_depp_1$Age_categ, levels = c("Young", "Normal" , "Late"))
summary(data_depp_1$Age_categ)



```

# Replace outliers for age (below AGE_MIN or above AGE_MAX) by NA for Age.

Number of remaining observations before removal of age outliers is `r nrow(data_depp_1)`

```{r echo=TRUE}

data_depp_1$age_abnormality <- ifelse(data_depp_1$Age_CP < AGE_MIN |  data_depp_1$Age_CP > AGE_MAX, TRUE, FALSE) # mean +/- 1.5 year 
print(paste0("Number of abnormal ages (below AGE_MIN and above AGE_MAX): ", sum(data_depp_1$age_abnormality, na.rm = TRUE)))


# Replace Age outliers with NA 
  
data_depp_1$Age_CP <- ifelse(data_depp_1$age_abnormality == TRUE , NA, data_depp_1$Age_CP) 
nrow(data_depp_1[is.na(data_depp_1$Age_CP),]) 

# nb of obs with age outilers

nrow(data_depp_1[data_depp_1$age_abnormality,])

```

Number of observations with age outliers is `r nrow(data_depp_1[data_depp_1$age_abnormality,])`


# Analyzing missings values 

```{r echo=TRUE}

number_of_missing_values              <- n_miss(data_depp_1) # function equivalent to sum(is.na(data))
total_number_of_items                 <- nrow(data_depp_1)*ncol(data_depp_1)
percentage_of_missing_values          <- 100* number_of_missing_values / total_number_of_items
number_of_observations_with_NA        <- nrow(data_depp_1) - nrow(na.omit(data_depp_1)) 
percentage_of_row_with_at_least_1_NA  <- 100* number_of_observations_with_NA / nrow(data_depp_1)

print(paste0("The total number of missing values is ", number_of_missing_values, 
             " while the total of values is ", total_number_of_items, ". ",
             "This corresponds to a percentage of ", round(percentage_of_missing_values,3), "%. ",
             "The number of children (observation or row) with at least one missing value: ", number_of_observations_with_NA, ". ",
             "This corresponds to a percentage of ", round(percentage_of_row_with_at_least_1_NA,3), "%. "))


data_depp_non_imp_1 <- data_depp_1

```

Print the number of missing  values per column.

```{r}

sapply(data_depp_1, function(x) sum(is.na(x)))

```




# Tests Abnormalities : Analyzing 4 aberrant variables (T2-T3 Lire Mots, Text)

We observed that for these tests (T2_Lire_Mots, T2_Lire_Text, T3_Lire_Mots, and T3_Lire_Text) outliers were reported. 
We redefined these variables from 0 till maximum where most of the kids (> 97%) are, the remaining 3% were transformed as NA.

## T2_Lire_Mots

```{r echo=TRUE}

# T2_Lire_Mots : from max 30 to max 100

print("Number of values replaced by NA for T2_Lire_Mots :" )
print(nrow(data_depp_1[data_depp_1$T2_Lire_Mots > 100, ]))

data_depp_1$T2_Lire_Mots[data_depp_1$T2_Lire_Mots > 100] <- NA # we replace only this note when > 100 by NA, but not the student



```


## T2_Lire_Text

```{r}



print("Number of values replaced by NA for T2_Lire_Text: ")
print(nrow(data_depp_1[data_depp_1$T2_Lire_Text > 195, ]))
data_depp_1$T2_Lire_Text[data_depp_1$T2_Lire_Text > 195] <- NA


```


## T3_Lire_Mots

```{r}


print("Number of values replaced by NA for T3_Lire_Mots: ")
print(nrow(data_depp_1[data_depp_1$T3_Lire_Mots > 93, ]))
data_depp_1$T3_Lire_Mots[data_depp_1$T3_Lire_Mots > 93] <- NA



```


## T3_Lire_Text

```{r echo=TRUE}



print("Number of values replaced by NA for T3_Lire_Text: ")
print(nrow(data_depp_1[data_depp_1$T3_Lire_Text > 136, ]))
data_depp_1$T3_Lire_Text[data_depp_1$T3_Lire_Text > 136] <- NA



```


We suggested another cut, which we implemented and stored with "_cut" to distinguish it from the previous values.
This different cut matched the original min and max of the cognitive tests. But unfortunately, loads of data were outliers (i.e., above these maximum)

```{r}

data_depp_1$T2_Lire_Mots_Cut     <- as.numeric(ifelse(data_depp_1$T2_Lire_Mots > 30, NaN, data_depp_1$T2_Lire_Mots))
data_depp_1$T2_Lire_Text_Cut     <- as.numeric(ifelse(data_depp_1$T2_Lire_Text > 29, NaN, data_depp_1$T2_Lire_Text))
data_depp_1$T3_Lire_Mots_Cut     <- as.numeric(ifelse(data_depp_1$T3_Lire_Mots > 60, NaN, data_depp_1$T3_Lire_Mots))
data_depp_1$T3_Lire_Text_Cut     <- as.numeric(ifelse(data_depp_1$T3_Lire_Text > 102, NaN, data_depp_1$T3_Lire_Text))

Fluency_With_Cut <- c("T2_Lire_Mots_Cut", "T2_Lire_Text_Cut", "T3_Lire_Mots_Cut", "T3_Lire_Text_Cut") # fluency with hardcuts


```


# Identify when all scores are at 0 or NA at a specific time (T1, T2, or T3)

Some series are full with 0 even if the child is supposed to be present. 
For example when looking at whole years: several examples contain either 0's or NA's on the whole line in T1, but show good results in T2 and T3, and are not counted as absent in T1.


```{r echo=TRUE}

  data_depp_1$T1_FR_ZNA <- rowSums(is.na(data_depp_1[, Lang_T1]) | data_depp_1[,Lang_T1] == 0)
  data_depp_1$T1_MA_ZNA <- rowSums(is.na(data_depp_1[, Math_T1]) | data_depp_1[,Math_T1] == 0)
  data_depp_1$T2_FR_ZNA <- rowSums(is.na(data_depp_1[, Lang_T2]) | data_depp_1[,Lang_T2] == 0)
  data_depp_1$T2_MA_ZNA <- rowSums(is.na(data_depp_1[, Math_T2]) | data_depp_1[, Math_T2] == 0)
  data_depp_1$T3_FR_ZNA <- rowSums(is.na(data_depp_1[, Lang_T3]) | data_depp_1[, Lang_T3] == 0)
  data_depp_1$T3_MA_ZNA <- rowSums(is.na(data_depp_1[, Math_T3]) | data_depp_1[, Math_T3] == 0)


```
This allows to print some of the rows:



Number of children with series of missing values:

- T1 Language: `r nrow(data_depp_1[data_depp_1$T1_FR_ZNA > length(Lang_T1) - 2,])`
- T1 Maths: `r nrow(data_depp_1[data_depp_1$T1_MA_ZNA > length(Math_T1) - 2,])`

- T2 Language: `r nrow(data_depp_1[data_depp_1$T1_FR_ZNA > length(Lang_T2) - 2,])`
- T2 Maths: `r nrow(data_depp_1[data_depp_1$T1_MA_ZNA > length(Math_T2) - 2,])`

- T3 Language: `r nrow(data_depp_1[data_depp_1$T1_FR_ZNA > length(Lang_T3) - 2,])`
- T3 Maths: `r nrow(data_depp_1[data_depp_1$T1_MA_ZNA > length(Math_T3) - 2,])`

```{r}
  
data_depp_1$test_abnormalities_Lang_T1 <- ifelse(data_depp_1$T1_FR_ZNA > length(Lang_T1) - 3, 1, 0) 
data_depp_1$test_abnormalities_Math_T1 <- ifelse(data_depp_1$T1_MA_ZNA > length(Math_T1) - 2 , 1, 0) 
data_depp_1$test_abnormalities_T1      <- ifelse(data_depp_1$test_abnormalities_Lang_T1 == 1 & data_depp_1$test_abnormalities_Math_T1 == 1, 1, 0)
data_depp_1$test_abnormalities_Lang_T2 <- ifelse(data_depp_1$T2_FR_ZNA > length(Lang_T2) - 3, 1, 0) 
data_depp_1$test_abnormalities_Math_T2 <- ifelse(data_depp_1$T2_MA_ZNA > length(Math_T2) - 2 , 1, 0) 
data_depp_1$test_abnormalities_T2      <- ifelse(data_depp_1$test_abnormalities_Lang_T2 == 1 & data_depp_1$test_abnormalities_Math_T2 == 1, 1, 0)

data_depp_1$test_abnormalities_Lang_T3 <- ifelse(data_depp_1$T3_FR_ZNA > length(Lang_T3) - 3, 1, 0) 
data_depp_1$test_abnormalities_Math_T3 <- ifelse(data_depp_1$T3_MA_ZNA > length(Math_T3) - 3, 1, 0) 
data_depp_1$test_abnormalities_T3      <- ifelse(data_depp_1$test_abnormalities_Lang_T3 == 1 & data_depp_1$test_abnormalities_Math_T3 == 1, 1, 0)

data_depp_1$totally_ab <- ifelse((data_depp_1$test_abnormalities_T1 == 1) &
                                           (data_depp_1$test_abnormalities_T2 == 1) &
                                           (data_depp_1$test_abnormalities_T3 == 1), 1, 0)
table(data_depp_1$totally_ab)

```

Remove totally abnormal students.


```{r}

data_depp_1 <- data_depp_1[!data_depp_1$totally_ab,]
print("Size of dataframe after the removal of students with high abnormalities: ")
print(nrow(data_depp_1))

```

In cases with many 0s and NAs on a series of test on a period, the child is considered absent, the Os and NAs are replaced by NAs on the whole line. This allows to avoid considering that the child had 0 at this exam, but in fact was not present.

Example before replacing with NA:

```{r}

head(data_depp_1[data_depp_1$test_abnormalities_T1 == 1, Lang_T1])

```


```{r}

if (ANNEE_COHORTE == 2018){
  data_depp_1[data_depp_1$test_abnormalities_T1 == 1, c(Math_T1, Lang_T1)] <- NA
  data_depp_1[data_depp_1$test_abnormalities_T2 == 1, c(Math_T2, Lang_T2)] <- NA
  data_depp_1[data_depp_1$test_abnormalities_T3 == 1, c(Math_T1, Lang_T3)] <- NA
} else if (ANNEE_COHORTE == 2019){
  data_depp_1[data_depp_1$test_abnormalities_T1 == 1, c(Math_T1, Lang_T1)] <- NA
  data_depp_1[data_depp_1$test_abnormalities_T2 == 1, c(Math_T2, Lang_T2)] <- NA
  data_depp_1[data_depp_1$test_abnormalities_T3 == 1, c(Math_T1, Lang_T3)] <- NA
} else if (ANNEE_COHORTE == 2020){
  data_depp_1[data_depp_1$test_abnormalities_T1 == 1, c(Math_T1, Lang_T1)] <- NA
  data_depp_1[data_depp_1$test_abnormalities_T2 == 1, c(Math_T2, Lang_T2)] <- NA
  data_depp_1[data_depp_1$test_abnormalities_T3 == 1, c(Math_T1, Lang_T3)] <- NA
} else if (ANNEE_COHORTE == 2021){
  data_depp_1[data_depp_1$test_abnormalities_T1 == 1, c(Math_T1, Lang_T1)] <- NA
  data_depp_1[data_depp_1$test_abnormalities_T2 == 1, c(Math_T2, Lang_T2)] <- NA
  data_depp_1[data_depp_1$test_abnormalities_T3 == 1, c(Math_T1, Lang_T3)] <- NA
}

```

We checked that it replaced everything with NA:

```{r}

head(data_depp_1[data_depp_1$test_abnormalities_T1 == 1, Lang_T1])

```

# Creation of other variables

## ID_etab_class

We identified a bias : children belonging to different classes from 2 different schools and departments in France were assigned with an identical class ID. 

When only considering the class ID, it did not allow to properly distinguish children of the same class. Therefore, we decided to match school ID and class ID to securize the identification of children.

```{r}
# Creation of unique ID per class per concatenation of etab and class ids is needed to get a unique identifier

data_depp_1$ID_etab_class <- paste(data_depp_1$ID_Etab_CP, data_depp_1$ID_Classe_CP, sep = "_")
length(unique(data_depp_1$ID_etab_class))

```

In total, there are `r length(unique(data_depp_1$ID_etab_class))` number of classes.

## Class size

```{r}

# Creating "Class size" variable = "Taille_Classe"

Nb_Per_Class <- data_depp_1[, c("ID_etab_class", "ID_Eleve")] %>%
  group_by(ID_etab_class) %>%
  summarise(Taille_Classe = n())
data_depp_1 <- merge(Nb_Per_Class, data_depp_1, by = "ID_etab_class")

print(paste0("Number of observations with size above SEUIL_SUP_CLASS: ", nrow(Nb_Per_Class[(Nb_Per_Class$Taille_Classe > SEUIL_SUP_CLASS ),])))

print(paste0("Number of observations with size below SEUIL_INF_CLASS: ", nrow(Nb_Per_Class[(Nb_Per_Class$Taille_Classe < SEUIL_INF_CLASS ),])))

rm(Nb_Per_Class)

```

## Labels "extreme" class size 

Note that the class sizes above SEUIL_SUP and below SEUIL_INF are `r nrow(data_depp_1[(data_depp_1$Taille_Classe < SEUIL_INF_CLASS | data_depp_1$Taille_Classe > SEUIL_SUP_CLASS ), ])`.

```{r}

data_depp_1$class_size_abnormalities <- ifelse(data_depp_1$Taille_Classe > SEUIL_SUP_CLASS, TRUE, FALSE)

data_depp_1$class_size_models <- ifelse(data_depp_1$Taille_Classe < SEUIL_INF_CLASS | data_depp_1$Taille_Classe > SEUIL_SUP_CLASS, TRUE, FALSE)

```


## Creation of Class size in Categories : small < 13 vs. normal >= 13

Creation of a new variable "class size" (continuous) and "class size category" (categorical)
- small class < 13 children / class
- normal >= 13 children / class

```{r}

data_depp_1$Taille_Classe_Cat <- ifelse(data_depp_1$Taille_Classe < SEUIL_TAILLE, "small_class", "normal")
table(data_depp_1$Taille_Classe_Cat)

```


# Creation of Number of children per school

```{r}

Nb_Per_School <- data_depp_1[, c("ID_Etab_CP", "ID_Eleve")] %>%
  group_by(ID_Etab_CP) %>%
  summarise(Taille_Ecole = n())
data_depp_1 <- merge(Nb_Per_School, data_depp_1, by = "ID_Etab_CP")
rm(Nb_Per_School)

length(unique(data_depp_1$ID_Etab_CP))

```

There are n = `r length(unique(data_depp_1$ID_Etab_CP))` schools

# Identification of Classes with NA in Sexe and erase them

```{r}

# How many classes with NA for Gender

classes_without_Sexe <- unique(data_depp_1[is.na(data_depp_1$Sexe), c("ID_etab_class")]) 

print(classes_without_Sexe)

sum(is.na(data_depp_1$Sexe))

```


```{r}

# Erase classes with NA for Gender

data_depp_1 <- data_depp_1[ ! (data_depp_1$ID_etab_class %in% classes_without_Sexe) , ]

```

# Creation of Sexe variable

Sexe     : Boys or Girls
Sexe_Num : 0.5 or -0.5

```{r echo=TRUE}

# For 2019 => Sexe 1 = Boys , Sexe 2 = Girls

if(ANNEE_COHORTE == 2018){
  
data_depp_1$Sexe <- case_when(data_depp_1$Sexe == "2" ~ "Girls",
                              data_depp_1$Sexe == "1" ~ "Boys")
data_depp_1$Sexe_Num <- case_when(data_depp_1$Sexe == "Girls" ~ -0.5,
                                  data_depp_1$Sexe == "Boys" ~ 0.5)
data_depp_1$Sexe_Num <- as.numeric(data_depp_1$Sexe_Num)

} else if (ANNEE_COHORTE == 2019){
  
data_depp_1$Sexe <- case_when(data_depp_1$Sexe == "2" ~ "Girls",
                              data_depp_1$Sexe == "1" ~ "Boys")
data_depp_1$Sexe_Num <- case_when(data_depp_1$Sexe == "Girls" ~ -0.5,
                                  data_depp_1$Sexe == "Boys" ~ 0.5)
data_depp_1$Sexe_Num <- as.numeric(data_depp_1$Sexe_Num)

} else if (ANNEE_COHORTE == 2020){
  
data_depp_1$Sexe <- case_when(data_depp_1$Sexe == "2" ~ "Girls",
                              data_depp_1$Sexe == "1" ~ "Boys")

data_depp_1$Sexe_Num <- case_when(data_depp_1$Sexe == "Girls" ~ -0.5,
                                  data_depp_1$Sexe == "Boys" ~ 0.5)

data_depp_1$Sexe_Num <- as.numeric(data_depp_1$Sexe_Num)

} else if (ANNEE_COHORTE == 2021){
  
data_depp_1$Sexe <- case_when(data_depp_1$Sexe == "2" ~ "Girls",
                              data_depp_1$Sexe == "1" ~ "Boys")

data_depp_1$Sexe_Num <- case_when(data_depp_1$Sexe == "Girls" ~ -0.5,
                                  data_depp_1$Sexe == "Boys" ~ 0.5)

data_depp_1$Sexe_Num <- as.numeric(data_depp_1$Sexe_Num)
}

# Sexe to factor

data_depp_1$Sexe <- as.factor(data_depp_1$Sexe)

table(data_depp_1$Sexe)

```


# Delete intermediary columns

```{r}

cols.dont.want <- c("T1_FR_ZNA", "T2_FR_ZNA", "T3_FR_ZNA", "T1_MA_ZNA","T2_MA_ZNA", "T3_MA_ZNA") 
data_depp_1 <- data_depp_1[, ! names(data_depp_1) %in% cols.dont.want, drop = F]

```


# Imputation

Note that imputation is performed after outliers management.


```{r}

cols.dont.want.for.imputation <- c("Date_Naiss", 
                                   "ID_Etab_CP", 
                                   "ID_Eleve",
                                   "ID_Etab_CE1",
                                   "ID_etab_class", 
                                   "ID_Classe_CP",
                                   "ID_Classe_CE1",
                                   "Age_Cat", 
                                   "age_abnormality",
                                   "test_abnormalities_Math_T1", 
                                   "test_abnormalities_Lang_T1",
                                   "test_abnormalities_T1",
                                   "test_abnormalities_Lang_T2",
                                   "test_abnormalities_Math_T2",
                                   "test_abnormalities_T2",
                                  "test_abnormalities_Lang_T3",
                                  "test_abnormalities_Math_T3",
                                  "test_abnormalities_T3",
                                  "totally_ab",
                                  "Taille_Classe_Cat",
                                  "class_size_abnormalities",
                                  "class_size_models")
if (ANNEE_COHORTE == 2019){
  cols.dont.want.for.imputation <- c(cols.dont.want.for.imputation, 
                                     "CodeEtablissementMi",
                                     "IPS_Etab_T2",
                                     "ID_Classe_T2",
                                     "Categ_Etab_T2")
}

if (ANNEE_COHORTE == 2020){
  cols.dont.want.for.imputation <- c(cols.dont.want.for.imputation, 
                                     "strate_Mi",
                                     "ips_Mi",
                                     "CodeClasseMi",
                                     "CodeEtablissementMi")
}

if (ANNEE_COHORTE == 2021){
cols.dont.want <- c("T1_FR_ZNA", "T2_FR_ZNA", "T3_FR_ZNA", "T1_MA_ZNA","T2_MA_ZNA", "T3_MA_ZNA") 
data_depp_1 <- data_depp_1[, ! names(data_depp_1) %in% cols.dont.want, drop = F]


cols.dont.want.for.imputation <- c("Date_Naiss", 
                                   "ID_Etab_CP", 
                                   "ID_Eleve",
                                   "ID_Etab_ce1",
                                   "ID_etab_class", 
                                   "ID_Classe_CP",
                                   "ID_Classe_ce1",
                                   "Age_Cat", 
                                   "age_abnormality",
                                   "test_abnormalities_Math_T1", 
                                   "test_abnormalities_Lang_T1",
                                   "test_abnormalities_T1",
                                   "test_abnormalities_Lang_T2",
                                   "test_abnormalities_Math_T2",
                                   "test_abnormalities_T2",
                                  "test_abnormalities_Lang_T3",
                                  "test_abnormalities_Math_T3",
                                  "test_abnormalities_T3",
                                  "totally_ab",
                                  "Taille_Classe_Cat",
                                  "class_size_abnormalities",
                                  "class_size_models")

}


```


Before imputation, check that columns are in factor, numeric, or integer format, if not it does not work.

```{r}

sapply(data_depp_1[, !names(data_depp_1) %in% cols.dont.want.for.imputation], class)

```



```{r}

data_depp_imputed <- data_depp_1
mice_mice <- mice(data = data_depp_1[, !names(data_depp_1) %in% cols.dont.want.for.imputation], 
                  m = 1, 
                  method = "pmm",
                  seed=500, # reproducibility
                  remove.collinear = FALSE) # because the cut and non cut columns are quasi colinear
X.mice <- complete(mice_mice, 1)
data_depp_imputed[, !names(data_depp_imputed) %in% cols.dont.want.for.imputation] <- X.mice

```


```{r}

# now that some data are imputed, fill remaining columns that could benefit from it

data_depp_imputed$Sexe_Num <- ifelse(data_depp_imputed$Sexe == "Girls", -0.5, 0.5)


# Age

data_depp_imputed$age_abnormality <- ifelse(data_depp_imputed$Age_CP < AGE_MIN |  data_depp_imputed$Age_CP > AGE_MAX, TRUE, FALSE)

data_depp_imputed$Age_Cat <- case_when(data_depp_imputed$Age_CP >= AGE_MIN & data_depp_imputed$Age_CP < AGE_PRECOCE       ~ "Young",
                                AGE_PRECOCE <= data_depp_imputed$Age_CP & data_depp_imputed$Age_CP < AGE_NORMAL_MOY ~ "Normal inf",
                                AGE_NORMAL_MOY <= data_depp_imputed$Age_CP & data_depp_imputed$Age_CP <= AGE_RETARD ~ "Normal sup",
                                data_depp_imputed$Age_CP > AGE_RETARD & data_depp_imputed$Age_CP <= AGE_MAX         ~ "Late")
data_depp_imputed$Age_Cat <- factor(data_depp_imputed$Age_Cat, levels = c("Young", "Normal inf", "Normal sup", "Late"))


data_depp_imputed$Age_categ <- case_when(data_depp_imputed$Age_CP >= AGE_MIN & data_depp_imputed$Age_CP < AGE_PRECOCE ~ "Young", 
                                AGE_PRECOCE <= data_depp_imputed$Age_CP & data_depp_imputed$Age_CP <= AGE_RETARD    ~ "Normal",
                                data_depp_imputed$Age_CP > AGE_RETARD & data_depp_imputed$Age_CP <= AGE_MAX         ~ "Late") 

data_depp_imputed$Age_categ <- factor(data_depp_imputed$Age_categ, levels = c("Young", "Normal" , "Late"))

```

# Save data



```{r}

# save the imputed df
write_rds(data_depp_imputed, paste0(intermediate, "/", "cohort_", ANNEE_COHORTE, "_after_1_preprocess_imp.rds"))


```

```{r}

# Data

data_depp <- data_depp_imputed

```


# Correct IPS when taking the imputed data

When imputing, the imputation process imputed with several values of IPS per class. We corrected this by taking the IPS average.

```{r}
  IPS_summarized <- data_depp[, c("ID_etab_class", "IPS_Etab_CP")] %>% 
    group_by(ID_etab_class) %>%
     summarise_at(vars(IPS_Etab_CP), list(IPS_Etab_CP = mean, sd = sd)) 
  
  data_depp <- subset(data_depp, select = -IPS_Etab_CP)
  
  data_depp <- merge(data_depp, IPS_summarized, by = "ID_etab_class")
  
  rm(IPS_summarized)

```

# Normalize grades : Create notes in % of success of the test to normalize the data, new variables in Percent of success per test _P

Reminder of which variables are contained in each list - only the existing variables in every cohort were kept, so that the composite variables were comparable from one year to the other : 44 tests

Lang_T1 <- c("T1_Comp_Mots",
             "T1_Comp_Phra",
             "T1_Comp_Text",
             "T1_Manip_Phon",
             "T1_Manip_Syll",
             "T1_Conn_Lettres",
             "T1_Recon_Ecritu_L",
             "T1_Compa_Lettres")

Math_T1 <- c("T1_Ecri_Nombre",
             "T1_Lire_Nombre",
             "T1_Resoud_Pb",
             "T1_Denombrer",
             "T1_Compa_Nombre",
             "T1_Ligne_Num")

Lang_T2 <- c("T2_Comp_Phra",
             "T2_Lire_Mots",
             "T2_Lire_Text",
             "T2_Ecri_Syll",
             "T2_Ecri_Mots",
             "T2_Manip_Phon",
             "T2_Conn_Lettres")
             
Math_T2 <- c("T2_Compa_Nombre",
             "T2_Ligne_Num",
             "T2_Addition",
             "T2_Soustract",
             "T2_Ecri_Nombre",
             "T2_Resoud_Pb")
             
Lang_T3 <- c("T3_Comp_Mots",
             "T3_Comp_Phra",
             "T3_Ecri_Syll",
             "T3_Ecri_Mots",
             "T3_Comp_Phra_Lu",
             "T3_Comp_Text_Lu",
             "T3_Lire_Mots",
             "T3_Lire_Text")

Math_T3 <- c("T3_Assemblage",
             "T3_Ligne_Num",
             "T3_Addition",
             "T3_Soustract",
             "T3_Calcul_Mental",
             "T3_Ecri_Nombre",
             "T3_Lire_Nombre",
             "T3_Repres_Nb",
             "T3_Resoud_Pb")
             
Attention : in 2021, T3_Repres_Nb is missing.

same_notes : 37 identical items

"T1_Comp_Mots"      "T1_Comp_Phra"      "T1_Manip_Phon"     "T1_Manip_Syll"     "T1_Conn_Lettres"   "T1_Recon_Ecritu_L"
"T1_Ecri_Nombre"    "T1_Lire_Nombre"    "T1_Resoud_Pb"      "T1_Denombrer"      "T1_Ligne_Num"      "T2_Comp_Phra"     
"T2_Lire_Mots"      "T2_Lire_Text"      "T2_Ecri_Syll"      "T2_Ecri_Mots"      "T2_Manip_Phon"     "T2_Conn_Lettres"  
"T2_Compa_Nombre"   "T2_Ligne_Num"      "T2_Ecri_Nombre"    "T2_Resoud_Pb"      "T3_Comp_Mots"      "T3_Comp_Phra"     
"T3_Ecri_Syll"      "T3_Ecri_Mots"      "T3_Comp_Phra_Lu"   "T3_Comp_Text_Lu"   "T3_Lire_Mots"      "T3_Lire_Text"     
"T3_Assemblage"     "T3_Ligne_Num"      "T3_Calcul_Mental"  "T3_Ecri_Nombre"    "T3_Lire_Nombre"    "T3_Repres_Nb"     
"T3_Resoud_Pb"   

```{r}

if (ANNEE_COHORTE == 2018){
same_notes_P    <- paste(same_notes, "_P", sep="") # 37 notes
notes_P         <- paste(notes, "_P", sep="")    # 44 notes
Lang_T1_P       <- paste(Lang_T1, "_P", sep="")
Lang_T2_P       <- paste(Lang_T2, "_P", sep="")
Lang_T3_P       <- paste(Lang_T3, "_P", sep="")
Math_T1_P       <- paste(Math_T1, "_P", sep="")
Math_T2_P       <- paste(Math_T2, "_P", sep="")
Math_T3_P       <- paste(Math_T3, "_P", sep="")

# Creating variables _P as the percentage of success per variable ranging from 0 to 100.

 for (i in c(same_notes, notes)){
    
    data_depp[[paste0(i, "_P")]] <- (data_depp[[i]] - min(data_depp[, i], na.rm = TRUE)) / (max(data_depp[, i], na.rm = TRUE) - min(data_depp[, i], na.rm = TRUE))*100
    
 }
dim(data_depp)
}
if (ANNEE_COHORTE == 2019){
same_notes_P    <- paste(same_notes, "_P", sep="") # 37 notes
notes_P         <- paste(notes, "_P", sep="")    # 44 notes
Lang_T1_P       <- paste(Lang_T1, "_P", sep="")
Lang_T2_P       <- paste(Lang_T2, "_P", sep="")
Lang_T3_P       <- paste(Lang_T3, "_P", sep="")
Math_T1_P       <- paste(Math_T1, "_P", sep="")
Math_T2_P       <- paste(Math_T2, "_P", sep="")
Math_T3_P       <- paste(Math_T3, "_P", sep="")

# Creating variables _P as the percentage of success per variable ranging from 0 to 100.

 for (i in c(same_notes, notes)){
    
    data_depp[[paste0(i, "_P")]] <- (data_depp[[i]] - min(data_depp[, i], na.rm = TRUE)) / (max(data_depp[, i], na.rm = TRUE) - min(data_depp[, i], na.rm = TRUE))*100
    
 }
dim(data_depp)
}
if (ANNEE_COHORTE == 2020){
same_notes_P    <- paste(same_notes, "_P", sep="") # 37 notes
notes_P         <- paste(notes, "_P", sep="")    # 44 notes
Lang_T1_P       <- paste(Lang_T1, "_P", sep="")
Lang_T2_P       <- paste(Lang_T2, "_P", sep="")
Lang_T3_P       <- paste(Lang_T3, "_P", sep="")
Math_T1_P       <- paste(Math_T1, "_P", sep="")
Math_T2_P       <- paste(Math_T2, "_P", sep="")
Math_T3_P       <- paste(Math_T3, "_P", sep="")

# Creating variables _P as the percentage of success per variable ranging from 0 to 100.

 for (i in c(same_notes, notes)){
    
    data_depp[[paste0(i, "_P")]] <- (data_depp[[i]] - min(data_depp[, i], na.rm = TRUE)) / (max(data_depp[, i], na.rm = TRUE) - min(data_depp[, i], na.rm = TRUE))*100
    
 }
dim(data_depp)
}
if (ANNEE_COHORTE == 2021){
# same_notes_P    <- paste(same_notes, "_P", sep="") # 37 notes
# notes_P         <- paste(notes, "_P", sep="")    # 44 notes
Lang_T1_P       <- paste(Lang_T1, "_P", sep="")
Lang_T2_P       <- paste(Lang_T2, "_P", sep="")
Lang_T3_P       <- paste(Lang_T3, "_P", sep="")
Math_T1_P       <- paste(Math_T1, "_P", sep="")
Math_T2_P       <- paste(Math_T2, "_P", sep="")
Math_T3 <- c("T3_Assemblage",
             "T3_Ligne_Num",
             "T3_Addition",
             "T3_Soustract",
             "T3_Calcul_Mental",
             "T3_Ecri_Nombre",
             "T3_Lire_Nombre",
             "T3_Resoud_Pb")
Math_T3_P       <- paste(Math_T3, "_P", sep="")


# Creating variables _P as the percentage of success per variable ranging from 0 to 100.

 for (i in c(Lang_T1, Lang_T2, Lang_T3, Math_T1, Math_T2, Math_T3)){
    
    data_depp[[paste0(i, "_P")]] <- (data_depp[[i]] - min(data_depp[, i], na.rm = TRUE)) / (max(data_depp[, i], na.rm = TRUE) - min(data_depp[, i], na.rm = TRUE))*100
    
 }
dim(data_depp)
}

```

# Creating Composed variable made from variables in _P

Creation of new ultra-composed variables regarding language and maths at T1, T2, T3. 
We decided to gather all the raw data (in percent of success) as the mean of the exercise (language or maths), as follow: 

NB : no need to add "na.rm =  TRUE" as NA = 0 due to imputation

data_depp$T1_Language_bis <- apply(data_depp[ , c(Lang_T1_P)], 1, mean, na.rm = TRUE)

```{r}
  
  # Language at T1 without  T1_Reco_lettre_ecri so that it is comparable between 2018 and 2019

data_depp$T1_Language <- apply(data_depp[ , c(Lang_T1_P)], 1, mean, na.rm = TRUE)

  # Language at T2 without  T2_var so that it is comparable between 2018 and 2019

data_depp$T2_Language <- apply(data_depp[ , c(Lang_T2_P)], 1, mean, na.rm = TRUE)

data_depp$T3_Language <- apply(data_depp[ , c(Lang_T3_P)], 1, mean, na.rm = TRUE)

data_depp$T1_Math <- apply(data_depp[ , c(Math_T1_P)], 1, mean, na.rm = TRUE)

data_depp$T2_Math <- apply(data_depp[ , c(Math_T2_P)], 1, mean, na.rm = TRUE)
  
  # With data_depp_1$T3_Resoud_Pb_P for Regressions and Description

data_depp$T3_Math <- apply(data_depp[ , c(Math_T3_P)], 1, mean, na.rm = TRUE)

dim(data_depp)

```

## Intermediate classes table

```{r}
# Extract all classes ID
all_classes_ids <- unique(data_depp$ID_etab_class)

# Instantiate
classes <- data_depp[, c("ID_etab_class", "Categ_Etab_CP", "Taille_Classe", "Taille_Classe_Cat", "Taille_Ecole", "IPS_Etab_CP")] %>%
  group_by(ID_etab_class, Categ_Etab_CP, Taille_Classe, Taille_Classe_Cat, Taille_Ecole)
classes <- unique(classes)
# Sanity check
nrow(classes) ==  length(all_classes_ids)
```

# Creating Nb of boys and girls per class

selecting min of 30% of girls and boys per class

```{r}

# building a variable for number of boys per class

nmixity <- data_depp[, c("ID_etab_class", "Sexe_Num")]
nmixity$Sexe_Num <- ifelse(nmixity$Sexe_Num == -0.5, 0, 1)
nmixity <- nmixity %>%
  group_by(ID_etab_class) %>%
  summarise(n_boy = sum(Sexe_Num))

classes <- merge(classes, nmixity[ , c("ID_etab_class", "n_boy")], by = "ID_etab_class")

# building a variable for number of girls per class

nmixity <- data_depp[, c("ID_etab_class", "Sexe_Num")]
nmixity$Sexe_Num <- ifelse(nmixity$Sexe_Num == -0.5, 0, 1)
nmixity <- nmixity %>%
  group_by(ID_etab_class) %>%
  summarise(n_girls = sum(Sexe_Num == 0))

classes <- merge(classes, nmixity[ , c("ID_etab_class", "n_girls")], by = "ID_etab_class")

rm(nmixity)

# classes <- classes[classes$n_boy > 1 & classes$n_girls > 1, ] # selecting min 2 boys and 2 girls per class

classes$n_boy   <- as.numeric(classes$n_boy)
classes$n_girls <- as.numeric(classes$n_girls)

Classes_singlesex_boys <- classes[classes$n_girls == 0, ]
Classes_singlesex_girls <- classes[classes$n_boy == 0, ]

sum(Classes_singlesex_boys$Taille_Classe)
sum(Classes_singlesex_girls$Taille_Classe)

```


# Boy_proportion = Mixity : Create variable for class mixity

```{r}

# compute mixity

mixity <- data_depp[, c("ID_etab_class", "Sexe_Num")]
mixity$Sexe_Num <- ifelse(mixity$Sexe_Num == -0.5, 0, 1)
mixity <- mixity %>%
  group_by(ID_etab_class) %>%
  summarise(boy_proportion = sum(Sexe_Num)/n())

# merge mixity with classes

classes <- merge(classes, mixity [ , c("ID_etab_class", "boy_proportion")], by = "ID_etab_class")
classes$boy_proportion <- round(classes$boy_proportion,2)

# clean memory

rm(mixity)

# check number of new dimension

dim(classes)


```

# Merge back class infos in original dataset

```{r}

data_depp <- data_depp %>%
  left_join(classes %>% select(ID_etab_class, boy_proportion) %>% distinct() %>% group_by(ID_etab_class) %>% slice(1) %>% ungroup(), by = "ID_etab_class")

```

# Save data

```{r}
if (ANNEE_COHORTE == 2018){
write_rds(data_depp, paste0(intermediate, '/', "cohort_2018_after_2_preprocess_imp.rds"))
}
if (ANNEE_COHORTE == 2019){
write_rds(data_depp, paste0(intermediate, '/', "cohort_2019_after_2_preprocess_imp.rds"))
}
if (ANNEE_COHORTE == 2020){
write_rds(data_depp, paste0(intermediate, '/', "cohort_2020_after_2_preprocess_imp.rds"))
}
if (ANNEE_COHORTE == 2021){
write_rds(data_depp, paste0(intermediate, '/', "cohort_2021_after_2_preprocess_imp.rds"))
}


```
# SELECT only classes big enough > SEUIL_INF_CLASS and not too big < SEUIL_SUP_CLASS
# SELECT only classes with min 30% of girls and 30% of boys
# SELECT only students aged 69 to 80 months (included)

```{r}

dim(data_depp)

data_depp <- data_depp[data_depp$Taille_Classe < SEUIL_SUP_CLASS & data_depp$Taille_Classe > SEUIL_INF_CLASS,]

dim(data_depp)

data_depp <- data_depp[data_depp$boy_proportion <= 0.7 & data_depp$boy_proportion >= 0.3,]

dim(data_depp)

data_depp <- data_depp[data_depp$Age_CP <= AGE_RETARD & data_depp$Age_CP >= AGE_PRECOCE,]

dim(data_depp)

```

# Save data

```{r}
if (ANNEE_COHORTE == 2018){
write_rds(data_depp, paste0(intermediate, '/', "cohort_2018_after_3_preprocess_imp.rds"))
}
if (ANNEE_COHORTE == 2019){
write_rds(data_depp, paste0(intermediate, '/', "cohort_2019_after_3_preprocess_imp.rds"))
}
if (ANNEE_COHORTE == 2020){
write_rds(data_depp, paste0(intermediate, '/', "cohort_2020_after_3_preprocess_imp.rds"))
}
if (ANNEE_COHORTE == 2021){
write_rds(data_depp, paste0(intermediate, '/', "cohort_2021_after_3_preprocess_imp.rds"))
}


```

# Transform in Z score



```{r}
var_z <- as.data.frame(Gaussianize(data_depp[, c("T1_Math", "T2_Math", "T3_Math", "T1_Language",
                                                    "T2_Language", "T3_Language", "Age_CP", "IPS_Etab_CP", "boy_proportion",
                                                    "Taille_Classe")],
                                         type = c("s"), method = c("IGMM"), return.u = TRUE)) %>%
  rename_with(~ str_replace(., "\\.U$", "_z"))

data_depp <- bind_cols(data_depp, var_z)

```



# Save data

```{r}
if (ANNEE_COHORTE == 2018){
write_rds(data_depp, paste0(output, '/', "cohort_2018_cleanP_imp.rds"))
}
if (ANNEE_COHORTE == 2019){
write_rds(data_depp, paste0(intermediate, '/', "cohort_2019_cleanP_imp.rds"))
}
if (ANNEE_COHORTE == 2020){
write_rds(data_depp, paste0(intermediate, '/', "cohort_2020_cleanP_imp.rds"))
}
if (ANNEE_COHORTE == 2021){
write_rds(data_depp, paste0(intermediate, '/', "cohort_2021_cleanP_imp.rds"))
}


```


## Export table


# 0 - Packages and file paths --------------------------------------------------


list_of_packages <- c(
  "tidyverse", 
  "fst",
  "arrow",
  "rstatix"
)
invisible(lapply(list_of_packages, library, character.only = TRUE))
rm(list_of_packages)




conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("select", "dplyr")


### Main repositories
perso <- file.path("/home/depp/chercheurs/ltouitou")
shared <- file.path("/home/depp/projets_inter/PSE-ecartsFG/PSE-ecartsFG-echanges")




### Data repositories
input <- file.path(shared, "Data", "Input")
intermediate <- file.path(shared, "Data", "Intermediate")
output <- file.path(shared, "Data", "Output")


# 1 - Load all data -----------------------------------------------------------


first_cohort <- readRDS(file.path(shared, "Data", "Output", "cohort_2018_cleanP_imp.rds"))
second_cohort <- readRDS(file.path(shared, "Data", "Output", "cohort_2019_cleanP_imp.rds"))
third_cohort <- readRDS(file.path(shared, "Data", "Output", "cohort_2020_cleanP_imp.rds"))
fourth_cohort <- readRDS(file.path(shared, "Data", "Output", "cohort_2021_cleanP_imp.rds"))


schools <- read.fst(file.path(shared, "Data", "Intermediate", "schools_info.fst")) 


students <- read.fst(file.path(shared, "Data", "Intermediate", "students_info.fst")) %>%
  mutate(family_type_2 = case_when(family_type == "single parent family" & (lien1 == "10" | lien2 == "10") ~ "single mother family",
                                   family_type == "single parent family" & (lien1 == "20" | lien2 == "20") ~ "single father family",
                                   TRUE ~ family_type))

# 2 - Merge all data -----------------------------------------------------------

first_cohort_final <- first_cohort %>%
  left_join(schools %>%
              filter(year == "2018"), 
            by = c("ID_Etab_CP" = "school_id")) %>%
  mutate(school_pedagogy = case_when(is.na(school_pedagogy) ~ 0,
                                     TRUE ~ school_pedagogy),
         school_religion = case_when(is.na(school_religion) ~ 0,
                                     TRUE ~ school_religion),
         school_type_disagreg = paste(school_type, school_priority, sep = " ")) %>%
  left_join(students, by = c("ID_Eleve" = "ident_crypt"))

# Il est possible que le join ne soit pas one to one sur les élèves
# Je ne crois pas que Pauline ait fait spécialement attention à n'avoir qu'une
# obs par élève (ce n'était pas le cas dans nos données, mais j'avais corrigé)

second_cohort_final <- second_cohort %>%
  left_join(schools %>%
              filter(year == "2019"), 
            by = c("ID_Etab_CP" = "school_id")) %>%
  mutate(school_pedagogy = case_when(is.na(school_pedagogy) ~ 0,
                                     TRUE ~ school_pedagogy),
         school_religion = case_when(is.na(school_religion) ~ 0,
                                     TRUE ~ school_religion),
         school_type_disagreg = paste(school_type, school_priority, sep = " ") ) 

third_cohort_final <- third_cohort %>%
  left_join(schools %>%
              filter(year == "2020"), 
            by = c("ID_Etab_CP" = "school_id")) %>%
  mutate(school_pedagogy = case_when(is.na(school_pedagogy) ~ 0,
                                     TRUE ~ school_pedagogy),
         school_religion = case_when(is.na(school_religion) ~ 0,
                                     TRUE ~ school_religion),
         school_type_disagreg = paste(school_type, school_priority, sep = " ") ) 

fourth_cohort_final <- fourth_cohort %>%
  left_join(schools %>%
              filter(year == "2021"), 
            by = c("ID_Etab_CP" = "school_id")) %>%
  mutate(school_pedagogy = case_when(is.na(school_pedagogy) ~ 0,
                                     TRUE ~ school_pedagogy),
         school_religion = case_when(is.na(school_religion) ~ 0,
                                     TRUE ~ school_religion),
         school_type_disagreg = paste(school_type, school_priority, sep = " ") ) 

# 3 - Exporting statistics tables ----------------------------------------------

## Family background ----------------------------------------------------------

gg_familySES_bar_data <- first_cohort_final %>%
  select(ips_c_group, ID_Eleve, Sexe, T1_Math_z, T2_Math_z, T3_Math_z, T1_Language_z,
         T2_Language_z, T3_Language_z) %>%
  pivot_longer(cols = matches("^T1|T2|T3")) %>%
  mutate(subject = substr(start = 4, stop = 7, name),
         time = substr(start = 1, stop = 2, name)) %>%
  select(-name) %>%
  group_by(subject, time, ips_c_group) %>%
  summarize(gender_gap = mean(value[Sexe == "Boys"], na.rm = TRUE) - mean(value[Sexe == "Girls"], na.rm = TRUE),
            sd_boys = sd(value[Sexe == "Boys"], na.rm = TRUE),
            sd_girls = sd(value[Sexe == "Girls"], na.rm = TRUE),
            n_boys = sum(!is.na(value[Sexe == "Boys"])),
            n_girls = sum(!is.na(value[Sexe == "Girls"])),
            mean_girls = mean(value[Sexe == "Girls"], na.rm = T),
            mean_boys = mean(value[Sexe == "Boys"], na.rm = T),
            sd_pooled = sd(value, na.rm = T)
  )%>%
  mutate(
    cohen_d_manual = (mean_boys - mean_girls) / (sqrt(sd_boys^2 + sd_girls^2)/2), 
    se_gap = (sd_boys*sd_boys)/n_boys + (sd_girls*sd_girls)/n_girls,
    ci_gap_min = gender_gap - 1.96 * se_gap,
    ci_gap_max = gender_gap + 1.96 * se_gap)

write.csv(gg_familySES_bar_data, file.path(paste0(shared,"/Export/Tables/gg_familySES_bar_data.csv")), row.names = FALSE)

gg_familytype_bar_data <- first_cohort_final %>%
  select(family_type, ID_Eleve, Sexe, T1_Math_z, T2_Math_z, T3_Math_z, T1_Language_z,
         T2_Language_z, T3_Language_z) %>%
  pivot_longer(cols = matches("^T1|T2|T3")) %>%
  mutate(subject = substr(start = 4, stop = 7, name),
         time = substr(start = 1, stop = 2, name)) %>%
  select(-name) %>%
  group_by(subject, time, family_type) %>%
  summarize(gender_gap = mean(value[Sexe == "Boys"], na.rm = TRUE) - mean(value[Sexe == "Girls"], na.rm = TRUE),
            sd_boys = sd(value[Sexe == "Boys"], na.rm = TRUE),
            sd_girls = sd(value[Sexe == "Girls"], na.rm = TRUE),
            n_boys = sum(!is.na(value[Sexe == "Boys"])),
            n_girls = sum(!is.na(value[Sexe == "Girls"])),
            mean_girls = mean(value[Sexe == "Girls"], na.rm = T),
            mean_boys = mean(value[Sexe == "Boys"], na.rm = T),
            sd_pooled = sd(value, na.rm = T)
  )%>%
  mutate(
    cohen_d_manual = (mean_boys - mean_girls) / (sqrt(sd_boys^2 + sd_girls^2)/2), 
    se_gap = (sd_boys*sd_boys)/n_boys + (sd_girls*sd_girls)/n_girls,
    ci_gap_min = gender_gap - 1.96 * se_gap,
    ci_gap_max = gender_gap + 1.96 * se_gap)

write.csv(gg_familytype_bar_data, file.path(paste0(shared,"/Export/Tables/gg_familytype_bar_data.csv")), row.names = FALSE)

gg_familyprofession_bar_data <- first_cohort_final %>%
  mutate(pcs_famille = case_when(pcs_mere == 38 & pcs_pere == 38 ~ "Engineer parents",
                                 pcs_mere >= 50 & pcs_mere < 60 & pcs_pere >= 50 & pcs_pere < 60 ~ "Employee parents",
                                 (pcs_mere == 67 | pcs_mere == 68) & (pcs_pere == 67 | pcs_pere == 68) ~ "Unskilled worker parents",
                                 (pcs_mere == 34 | pcs_mere == 42) & (pcs_pere == 34 | pcs_pere == 42) ~ "Teacher parents",
                                 TRUE ~ "other")) %>%
  select(pcs_famille, ID_Eleve, Sexe, T1_Math_z, T2_Math_z, T3_Math_z, T1_Language_z,
         T2_Language_z, T3_Language_z) %>%
  pivot_longer(cols = matches("^T1|T2|T3")) %>%
  mutate(subject = substr(start = 4, stop = 7, name),
         time = substr(start = 1, stop = 2, name)) %>%
  select(-name) %>%
  group_by(subject, time, pcs_famille) %>%
  summarize(gender_gap = mean(value[Sexe == "Boys"], na.rm = TRUE) - mean(value[Sexe == "Girls"], na.rm = TRUE),
            sd_boys = sd(value[Sexe == "Boys"], na.rm = TRUE),
            sd_girls = sd(value[Sexe == "Girls"], na.rm = TRUE),
            n_boys = sum(!is.na(value[Sexe == "Boys"])),
            n_girls = sum(!is.na(value[Sexe == "Girls"])),
            mean_girls = mean(value[Sexe == "Girls"], na.rm = T),
            mean_boys = mean(value[Sexe == "Boys"], na.rm = T),
            sd_pooled = sd(value, na.rm = T)
  )%>%
  mutate(
    cohen_d_manual = (mean_boys - mean_girls) / (sqrt(sd_boys^2 + sd_girls^2)/2), 
    se_gap = (sd_boys*sd_boys)/n_boys + (sd_girls*sd_girls)/n_girls,
    ci_gap_min = gender_gap - 1.96 * se_gap,
    ci_gap_max = gender_gap + 1.96 * se_gap)

write.csv(gg_familyprofession_bar_data, file.path(paste0(shared,"/Export/Tables/gg_familyprofession_bar_data.csv")), row.names = FALSE)



# Carte -------------------------------------------------------------------

list_cohort <- list(
  "2018-2019" = first_cohort_final,
  "2019-2020" = second_cohort_final,
  "2020-2021" = third_cohort_final,
  "2021-2022" = fourth_cohort_final
)

for (cohort_name in names(list_cohort)) {
  
  print(cohort_name)
  cohort <- list_cohort[[cohort_name]]
  
  gg_departements_data <- cohort %>%
    #keep only student obs identified by departement
    filter(!is.na(school_departement)) %>%
    mutate(school_departement = gsub("^0+", "", school_departement)) %>%
    select(school_departement, ID_Eleve, Sexe, T1_Math_z, T2_Math_z, T3_Math_z, T1_Language_z,
           T2_Language_z, T3_Language_z) %>%
    pivot_longer(cols = matches("^T1|T2|T3")) %>%
    mutate(subject = substr(start = 4, stop = 7, name),
           time = substr(start = 1, stop = 2, name)) %>%
    select(-name) %>%
    group_by(subject, time, school_departement) %>%
    summarize(gender_gap = mean(value[Sexe == "Boys"], na.rm = TRUE) - mean(value[Sexe == "Girls"], na.rm = TRUE),
              sd_boys = sd(value[Sexe == "Boys"], na.rm = TRUE),
              sd_girls = sd(value[Sexe == "Girls"], na.rm = TRUE),
              n_boys = sum(!is.na(value[Sexe == "Boys"])),
              n_girls = sum(!is.na(value[Sexe == "Girls"])),
              mean_girls = mean(value[Sexe == "Girls"], na.rm = T),
              mean_boys = mean(value[Sexe == "Boys"], na.rm = T),
              sd_pooled = sd(value, na.rm = T)
    )%>%
    mutate(
      cohen_d_manual = (mean_boys - mean_girls) / (sqrt(sd_boys^2 + sd_girls^2)/2), 
      se_gap = (sd_boys*sd_boys)/n_boys + (sd_girls*sd_girls)/n_girls,
      ci_gap_min = gender_gap - 1.96 * se_gap,
      ci_gap_max = gender_gap + 1.96 * se_gap)
  
  write.csv(gg_departements_data, 
            file.path(paste0(shared, "/Export/Tables/gg_departements_data_", cohort_name, ".csv")), 
            row.names = FALSE)
  
}

# Type school -------------------------------------------------------------


list_cohort <- list(
  "2018-2019" = first_cohort_final,
  "2019-2020" = second_cohort_final,
  "2020-2021" = third_cohort_final,
  "2021-2022" = fourth_cohort_final
)

for (cohort_name in names(list_cohort)) {
  
  print(cohort_name)
  cohort <- list_cohort[[cohort_name]]
  
  gg_type_schools_disagreg <- cohort %>%
    select(school_type_disagreg, ID_Eleve, Sexe, T1_Math_z, T2_Math_z, T3_Math_z, T1_Language_z,
           T2_Language_z, T3_Language_z) %>%
    pivot_longer(cols = matches("^T1|T2|T3")) %>%
    mutate(subject = substr(start = 4, stop = 7, name),
           time = substr(start = 1, stop = 2, name)) %>%
    select(-name) %>%
    group_by(subject, time, school_type_disagreg) %>%
    summarize(gender_gap = mean(value[Sexe == "Boys"], na.rm = TRUE) - mean(value[Sexe == "Girls"], na.rm = TRUE),
              sd_boys = sd(value[Sexe == "Boys"], na.rm = TRUE),
              sd_girls = sd(value[Sexe == "Girls"], na.rm = TRUE),
              n_boys = sum(!is.na(value[Sexe == "Boys"])),
              n_girls = sum(!is.na(value[Sexe == "Girls"])),
              mean_girls = mean(value[Sexe == "Girls"], na.rm = T),
              mean_boys = mean(value[Sexe == "Boys"], na.rm = T),
              sd_pooled = sd(value, na.rm = T)
    )%>%
    mutate(
      cohen_d_manual = (mean_boys - mean_girls) / (sqrt(sd_boys^2 + sd_girls^2)/2), 
      se_gap = (sd_boys*sd_boys)/n_boys + (sd_girls*sd_girls)/n_girls,
      ci_gap_min = gender_gap - 1.96 * se_gap,
      ci_gap_max = gender_gap + 1.96 * se_gap)
  
  write.csv(gg_type_schools_disagreg, file.path(paste0(shared,"/Export/Tables/gg_type_schools_disagreg_", cohort_name, ".csv")), row.names = FALSE)
  
  gg_type_schools_agreg <- cohort %>%
    select(school_type, ID_Eleve, Sexe, T1_Math_z, T2_Math_z, T3_Math_z, T1_Language_z,
           T2_Language_z, T3_Language_z) %>%
    pivot_longer(cols = matches("^T1|T2|T3")) %>%
    mutate(subject = substr(start = 4, stop = 7, name),
           time = substr(start = 1, stop = 2, name)) %>%
    select(-name) %>%
    group_by(subject, time, school_type) %>%
    summarize(gender_gap = mean(value[Sexe == "Boys"], na.rm = TRUE) - mean(value[Sexe == "Girls"], na.rm = TRUE),
              sd_boys = sd(value[Sexe == "Boys"], na.rm = TRUE),
              sd_girls = sd(value[Sexe == "Girls"], na.rm = TRUE),
              n_boys = sum(!is.na(value[Sexe == "Boys"])),
              n_girls = sum(!is.na(value[Sexe == "Girls"])),
              mean_girls = mean(value[Sexe == "Girls"], na.rm = T),
              mean_boys = mean(value[Sexe == "Boys"], na.rm = T),
              sd_pooled = sd(value, na.rm = T)
    )%>%
    mutate(
      cohen_d_manual = (mean_boys - mean_girls) / (sqrt(sd_boys^2 + sd_girls^2)/2), 
      se_gap = (sd_boys*sd_boys)/n_boys + (sd_girls*sd_girls)/n_girls,
      ci_gap_min = gender_gap - 1.96 * se_gap,
      ci_gap_max = gender_gap + 1.96 * se_gap)
  
  write.csv(gg_type_schools_agreg, file.path(paste0(shared,"/Export/Tables/gg_type_schools_agreg_", cohort_name, ".csv")), row.names = FALSE)
  
  gg_type_schools_religious <- cohort %>%
    select(school_religion, ID_Eleve, Sexe, T1_Math_z, T2_Math_z, T3_Math_z, T1_Language_z,
           T2_Language_z, T3_Language_z) %>%
    pivot_longer(cols = matches("^T1|T2|T3")) %>%
    mutate(subject = substr(start = 4, stop = 7, name),
           time = substr(start = 1, stop = 2, name)) %>%
    select(-name) %>%
    group_by(subject, time, school_religion) %>%
    summarize(gender_gap = mean(value[Sexe == "Boys"], na.rm = TRUE) - mean(value[Sexe == "Girls"], na.rm = TRUE),
              sd_boys = sd(value[Sexe == "Boys"], na.rm = TRUE),
              sd_girls = sd(value[Sexe == "Girls"], na.rm = TRUE),
              n_boys = sum(!is.na(value[Sexe == "Boys"])),
              n_girls = sum(!is.na(value[Sexe == "Girls"])),
              mean_girls = mean(value[Sexe == "Girls"], na.rm = T),
              mean_boys = mean(value[Sexe == "Boys"], na.rm = T),
              sd_pooled = sd(value, na.rm = T)
    )%>%
    mutate(
      cohen_d_manual = (mean_boys - mean_girls) / (sqrt(sd_boys^2 + sd_girls^2)/2), 
      se_gap = (sd_boys*sd_boys)/n_boys + (sd_girls*sd_girls)/n_girls,
      ci_gap_min = gender_gap - 1.96 * se_gap,
      ci_gap_max = gender_gap + 1.96 * se_gap)
  
  write.csv(gg_type_schools_religious, file.path(paste0(shared,"/Export/Tables/gg_schools_religious_", cohort_name, ".csv")), row.names = FALSE)
  
  gg_type_school_pedagogy <- cohort %>%
    select(school_pedagogy, ID_Eleve, Sexe, T1_Math_z, T2_Math_z, T3_Math_z, T1_Language_z,
           T2_Language_z, T3_Language_z) %>%
    pivot_longer(cols = matches("^T1|T2|T3")) %>%
    mutate(subject = substr(start = 4, stop = 7, name),
           time = substr(start = 1, stop = 2, name)) %>%
    select(-name) %>%
    group_by(subject, time, school_pedagogy) %>%
    summarize(gender_gap = mean(value[Sexe == "Boys"], na.rm = TRUE) - mean(value[Sexe == "Girls"], na.rm = TRUE),
              sd_boys = sd(value[Sexe == "Boys"], na.rm = TRUE),
              sd_girls = sd(value[Sexe == "Girls"], na.rm = TRUE),
              n_boys = sum(!is.na(value[Sexe == "Boys"])),
              n_girls = sum(!is.na(value[Sexe == "Girls"])),
              mean_girls = mean(value[Sexe == "Girls"], na.rm = T),
              mean_boys = mean(value[Sexe == "Boys"], na.rm = T),
              sd_pooled = sd(value, na.rm = T)
    )%>%
    mutate(
      cohen_d_manual = (mean_boys - mean_girls) / (sqrt(sd_boys^2 + sd_girls^2)/2), 
      se_gap = (sd_boys*sd_boys)/n_boys + (sd_girls*sd_girls)/n_girls,
      ci_gap_min = gender_gap - 1.96 * se_gap,
      ci_gap_max = gender_gap + 1.96 * se_gap)
  
  write.csv(gg_type_school_pedagogy, file.path(paste0(shared,"/Export/Tables/gg_schools_pedagogy_", cohort_name, ".csv")), row.names = FALSE)
  
}
# Parents PCS -------------------------------------------------------------

gg_pere_pcs_bar_data <- first_cohort_final %>%
  filter(!is.na(pcs_pere)) %>% ### keep students that have a father
  select(pcs_pere, pcs_pere_shortname, pcs_pere_longname, ID_Eleve, Sexe, T1_Math_z, T2_Math_z, T3_Math_z, T1_Language_z,
         T2_Language_z, T3_Language_z) %>%
  pivot_longer(cols = matches("^T1|T2|T3")) %>%
  mutate(subject = substr(start = 4, stop = 7, name),
         time = substr(start = 1, stop = 2, name)) %>%
  select(-name) %>%
  group_by(subject, time, pcs_pere, pcs_pere_shortname, pcs_pere_longname) %>%
  summarize(gender_gap = mean(value[Sexe == "Boys"], na.rm = TRUE) - mean(value[Sexe == "Girls"], na.rm = TRUE),
            sd_boys = sd(value[Sexe == "Boys"], na.rm = TRUE),
            sd_girls = sd(value[Sexe == "Girls"], na.rm = TRUE),
            n_boys = sum(!is.na(value[Sexe == "Boys"])),
            n_girls = sum(!is.na(value[Sexe == "Girls"])),
            mean_girls = mean(value[Sexe == "Girls"], na.rm = T),
            mean_boys = mean(value[Sexe == "Boys"], na.rm = T),
            sd_pooled = sd(value, na.rm = T)
  )%>%
  mutate(
    cohen_d_manual = (mean_boys - mean_girls) / (sqrt(sd_boys^2 + sd_girls^2)/2),
    se_gap = (sd_boys*sd_boys)/n_boys + (sd_girls*sd_girls)/n_girls,
    ci_gap_min = gender_gap - 1.96 * se_gap,
    ci_gap_max = gender_gap + 1.96 * se_gap)

gg_mere_pcs_bar_data <- first_cohort_final %>%
  filter(!is.na(pcs_mere)) %>% ### keep students that have a father
  select(pcs_mere, pcs_mere_shortname, pcs_mere_longname, ID_Eleve, Sexe, T1_Math_z, T2_Math_z, T3_Math_z, T1_Language_z,
         T2_Language_z, T3_Language_z) %>%
  pivot_longer(cols = matches("^T1|T2|T3")) %>%
  mutate(subject = substr(start = 4, stop = 7, name),
         time = substr(start = 1, stop = 2, name)) %>%
  select(-name) %>%
  group_by(subject, time, pcs_mere, pcs_mere_shortname, pcs_mere_longname) %>%
  summarize(gender_gap = mean(value[Sexe == "Boys"], na.rm = TRUE) - mean(value[Sexe == "Girls"], na.rm = TRUE),
            sd_boys = sd(value[Sexe == "Boys"], na.rm = TRUE),
            sd_girls = sd(value[Sexe == "Girls"], na.rm = TRUE),
            n_boys = sum(!is.na(value[Sexe == "Boys"])),
            n_girls = sum(!is.na(value[Sexe == "Girls"])),
            mean_girls = mean(value[Sexe == "Girls"], na.rm = T),
            mean_boys = mean(value[Sexe == "Boys"], na.rm = T),
            sd_pooled = sd(value, na.rm = T)
  )%>%
  mutate(
    cohen_d_manual = (mean_boys - mean_girls) / (sqrt(sd_boys^2 + sd_girls^2)/2), 
    se_gap = (sd_boys*sd_boys)/n_boys + (sd_girls*sd_girls)/n_girls,
    ci_gap_min = gender_gap - 1.96 * se_gap,
    ci_gap_max = gender_gap + 1.96 * se_gap)

write.csv(gg_pere_pcs_bar_data, file.path(paste0(shared,"/Export/Tables/gg_pere_pcs_bar_data.csv")), row.names = FALSE)
write.csv(gg_mere_pcs_bar_data, file.path(paste0(shared,"/Export/Tables/gg_mere_pcs_bar_data.csv")), row.names = FALSE)

## Generate Figure

# 0 - Packages, file paths and figure parameters -------------------------------

packages <- c("tidyverse", "patchwork",
              "grDevices", "cowplot", "janitor", "showtext",
              "readxl", "sf", "sysfonts")

invisible(lapply(packages, library, character.only = TRUE))
rm(packages)


conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("select", "dplyr")


# 1 - Data repositories ---------------------------------------------------

# data <- file.path("~/Documents", "Article Nature", "Export_151124", "Export","Tables")
data <- file.path("C:/Users/j.sultan/Dropbox/Reperes_CP_CE1/analyse/Export DEPP/20241115/Tables")

#output_fig <- file.path("~/Documents", "Article Nature", "Figures")
output_fig <- file.path("C:/Users/j.sultan/Dropbox/Reperes_CP_CE1/Nature/Figures/svg")

# 2 - Import data ---------------------------------------------------------

files <- list.files(path = data, pattern = "*.csv", full.names = TRUE)
path_files = paste(data, files, sep = "/")

# loading files into list with explicit names

read_csv_clean <- function(file) (
  
  if (grepl("gg_", file) | grepl("prop_", file)) {
    df <- read_csv(file) %>%
      mutate(
        subject =
          case_when(
            subject == "Lang" ~ "Language",
            TRUE ~ "Mathematics"
          ),
        time = factor(time, levels = c("T1", "T2", "T3")),
        cohen_d_manual = (mean_boys - mean_girls)/sd_pooled
      )
  } else {
    df <- read_csv(file)
  }
  
)

myfiles = lapply(files, read_csv_clean)

files = str_extract(files, "(?<=/)[^/]+(?=\\.csv)")

myfiles <- setNames(myfiles, files)

rm(files, path_files)

# 3 - Add arial fonts -----------------------------------------------------

font_add("arial", regular = "arial.ttf", bold = "arialbd.ttf")
showtext_auto()

# 4 - Figure style parameters ---------------------------------------------

my_palette_ipp = hcl.colors(12, "viridis")

col_beg_cp = my_palette_ipp[2]
col_mid_cp = my_palette_ipp[6]
col_beg_ce = my_palette_ipp[10]

darker_blue_txt = "#002832"
dark_blue       = "#00596E"
dark_blue_grey  = "#4B6874"
orange          = "#FF7221"

### alpha and font size
alpha = 0.9
### change font size/spacing depending on output format
#text_size = 7*.pt ### saving as .png
#spacing = .pt/10
text_size =  8 ### saving as .pdf
spacing = 1

### figure size (in inches) 
figure_width_one_col  = 2.25
figure_width_two_col  = 4.75
figure_width_three_col = 7.25
### to be decided for height
figure_height_one_col = 10
figure_height_two_col = 10
figure_height_three_col = 10

### gg theme
gg_theme <- theme_classic() +
  theme(
    axis.line = element_line(colour = dark_blue,
                             size = 0.5, linetype = "solid"),
    panel.background = element_rect(fill = "white",
                                    colour = "white"),
    panel.grid.major.x = element_line(size = 0.1, linetype = "dotted",
                                      colour = dark_blue_grey),
    panel.grid.major.y = element_line(size = 0.1, linetype = "dotted",
                                      colour = dark_blue_grey),
    strip.background = element_rect(colour = dark_blue, fill = dark_blue),
    strip.text = element_text(colour = "white"),
    legend.position = "bottom",
    legend.text = element_text(size = text_size, colour = darker_blue_txt),
    text = element_text(size = text_size, colour = darker_blue_txt, family = "sans"),
    axis.text.x = element_text(size = text_size, colour = darker_blue_txt, vjust = +0.5, lineheight= spacing),
    axis.text.y = element_text(size = text_size, colour = darker_blue_txt, vjust = +1, lineheight= spacing),
    axis.title.x = element_text(size = text_size, colour = darker_blue_txt, vjust = -0.75, lineheight= spacing),
    axis.title.y = element_text(size = text_size, colour = darker_blue_txt, vjust = +3, lineheight= spacing),
    plot.title = element_text(colour = darker_blue_txt), #hjust = 0.5
    plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm")
  ) 

# use Arial in each graph
par(family = "arial")

# 5 - Figure 3 -----------------------------------------------------------------

### make subtables for figure

parental_occupation <- bind_rows(myfiles$gg_familySES_bar_data %>%
                                   filter(subject == "Mathematics") %>%
                                   filter(!is.na(ips_c_group)) %>%
                                   filter(ips_c_group == 1 | ips_c_group == 4) %>%
                                   mutate(ips_c_group = case_when(ips_c_group == 1 ~ "Very low SES\nparents",
                                                                  ips_c_group == 4 ~ "Very high SES\nparents")) %>%
                                   rename("cat" = "ips_c_group"),
                                 myfiles$gg_familyprofession_bar_data %>%
                                   filter(subject == "Mathematics") %>%
                                   filter(pcs_famille == "Engineer parents" | pcs_famille == "Teacher parents") %>%
                                   rename("cat" = "pcs_famille") %>%
                                   mutate(cat = case_when(cat == "Engineer parents" ~ "Both parents\nEngineers",
                                                          TRUE ~ "Both parents\nTeachers"))
) %>%
  mutate(cat = factor(cat, levels = c( "Very low SES\nparents", "Very high SES\nparents", "Both parents\nEngineers", "Both parents\nTeachers"))) %>%
  group_by(cat) %>%
  mutate(N = n_boys[time == "T1"] + n_girls[time == "T1"],
         label = paste("N =", N)) %>%
  ungroup()

family_structure <- myfiles$gg_familytype_bar_data %>%
  filter(subject == "Mathematics") %>%
  filter(!is.na(family_type_2)) %>%
  rename("cat" = "family_type_2") %>%
  filter(cat != "other family type") %>%
  mutate(cat = case_when(cat == "heteroparental family" ~ "Oppposite-sex\nparents",
                         cat == "single mother family" ~ "Single\nmothers",
                         cat == "single father family" ~ "Single\nfathers",
                         cat == "single sex parent family" ~ "Same-sex\nparents")) %>%
  mutate(cat = factor(cat, levels = c( "Oppposite-sex\nparents", "Same-sex\nparents", "Single\nmothers", "Single\nfathers"))) %>%
  group_by(cat) %>%
  mutate(N = n_boys[time == "T1"] + n_girls[time == "T1"],
         label = paste("N =", N)) %>%
  ungroup()


typeschool <- rbind(
  
  myfiles$`gg_type_schools_agreg_2018-2019` %>%
    filter(subject == "Mathematics") %>%
    rename("cat" = "school_type") %>%
    filter(!is.na(cat)) %>%
    mutate(cat = case_when(cat == "Privé sous contrat" ~ "Private",
                           TRUE ~ "Public")),
  
  myfiles$`gg_schools_pedagogy_2018-2019` %>%
    filter(subject == "Mathematics") %>%
    rename("cat" = "school_pedagogy") %>%
    filter(cat == 1) %>%
    mutate(cat = "Alternative\npedagogy"),
  
  myfiles$`gg_schools_religious_2018-2019` %>%
    filter(subject == "Mathematics") %>%
    rename("cat" = "school_religion") %>%
    filter(cat == 1) %>%
    mutate(cat = "Religious\nschool")
) %>%
  mutate(cat = factor(cat, levels = c("Public", "Private", "Alternative\npedagogy", "Religious\nschool"))) %>%
  group_by(cat) %>%
  mutate(N = n_boys[time == "T1"] + n_girls[time == "T1"],
         label = paste("N =", N)) %>%
  ungroup()



c <- ggplot(parental_occupation, aes(x = time, y = cohen_d_manual, fill = time)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.9, show.legend = FALSE) +
  facet_wrap(~ cat, nrow = 1) +
  scale_fill_manual(name = "", values = c("T1" = col_beg_cp,
                                          "T2" = col_mid_cp,
                                          "T3" = col_beg_ce)) +
  ylab("Math gender gap in Cohen's d\n(Boys - Girls)") +
  scale_y_continuous(breaks = seq(0, 0.35, 0.05), limits = c(-0.1, 0.4)) +
  scale_x_discrete(breaks = NULL) +
  xlab("") +
  geom_hline(yintercept = 0, color = dark_blue) +
  labs(title = "(a) Parental occupation") +
  gg_theme +
  geom_text(aes(x = 2, y = 0.4, label = label, group = cat),
            hjust= 0.5, vjust=1,
            inherit.aes = FALSE,
            size = text_size*(2/7),
            color = dark_blue) +
  theme(strip.text = element_text(size = text_size, lineheight = spacing)) ### facet labels text size and spacing

d <- ggplot(family_structure, aes(x = time, y = cohen_d_manual, fill = time)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.9, show.legend = FALSE) +
  facet_wrap(~ cat, nrow = 1) +
  scale_fill_manual(name = "", values = c("T1" = col_beg_cp,
                                          "T2" = col_mid_cp,
                                          "T3" = col_beg_ce)) +
  ylab("Math gender gap in Cohen's d\n(Boys - Girls)") +
  scale_y_continuous(breaks = seq(0, 0.35, 0.05), limits = c(-0.1, 0.4)) +
  scale_x_discrete(breaks = NULL) +
  xlab("") +
  geom_hline(yintercept = 0, color = dark_blue) +
  labs(title = "(b) Family composition") +
  gg_theme +
  geom_text(aes(x = 2, y = 0.4, label = label, group = cat),
            hjust= 0.5, vjust=1,
            inherit.aes = FALSE,
            size = text_size*(2/7),
            color = dark_blue) +
  theme(strip.text = element_text(size = text_size, lineheight = spacing)) ### facet labels text size and spacing

e <- ggplot(typeschool, aes(x = time, y = cohen_d_manual, fill = time)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.9, show.legend = TRUE) +
  facet_wrap(~ cat, nrow = 1) +
  scale_fill_manual(name = "", values = c("T1" = col_beg_cp,
                                          "T2" = col_mid_cp,
                                          "T3" = col_beg_ce)) +
  ylab("Math gender gap in Cohen's d\n(Boys - Girls)") +
  scale_y_continuous(breaks = seq(0, 0.35, 0.05), limits = c(-0.1, 0.4)) +
  scale_x_discrete(breaks = NULL) +
  xlab("") +
  geom_hline(yintercept = 0, color = dark_blue) +
  labs(title = "(c) Type of school") +
  gg_theme +
  geom_text(aes(x = 2, y = 0.4, label = label, group = cat),
            hjust= 0.5, vjust=1,
            inherit.aes = FALSE,
            size = text_size*(2/7),
            color = dark_blue) +
  theme(strip.text = element_text(size = text_size, lineheight = spacing)) ### facet labels text size and spacing

# grDevices::cairo_pdf(file.path(output_fig, "fig_3.pdf"),
#                      height = 5.5,
#                      width = 4.75)

grDevices::svg(file.path(output_fig, "fig_3.svg"),
               height = 5.5,
               width = 4.75)

(c / plot_spacer()/ d / plot_spacer() / e +
    plot_layout(height = c(1, -0.2, 1, -0.2, 1)) & theme(legend.position = "bottom", legend.margin = margin(-11, 0, 0, 0))) + 
  plot_layout(axis_titles = "collect")

dev.off()




names_pcs <- read_excel(
  file.path("C:/Users/j.sultan/Dropbox/Reperes_CP_CE1/data/pcs/description_parents_profession.xlsx")) %>%
  as_tibble() %>%
  select(pcs = `PCS 2020 (Insee's nomenclature)`, pcs_eng = `English translation - short version`)

## Fig. S7a. Father -------------------------------------------------------

pere_data <- myfiles$gg_pere_pcs_bar_data %>% filter(subject == "Mathematics") %>%
  filter(!str_detect(pcs_pere_longname, 'Retraité|Non renseignée|contingent|Sans profession|Etudiant|Clergé')) %>%
  left_join(names_pcs, by = c("pcs_pere" = "pcs")) %>%
  group_by(pcs_eng) %>%
  mutate(N = n_boys[time == "T1"] + n_girls[time == "T1"],
         label = paste("N =", N)) %>%
  ungroup()


# grDevices::cairo_pdf(file.path(output_fig, "gg_ma_father.pdf"),
#                      height = 10,
#                      width = figure_width_three_col)

grDevices::svg(file.path(output_fig, "gg_ma_father.svg"),
               height = 10,
               width = figure_width_three_col)

ggplot(pere_data,
       aes(x = time, y = cohen_d_manual, fill = time)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap( ~ pcs_eng, ncol = 6) + 
  scale_fill_manual(name = "", 
                    values = c("T1" = col_beg_cp,
                               "T2" = col_mid_cp,
                               "T3" = col_beg_ce)) +
  ylab("Math gender gap in Cohen's d\n(Boys - Girls)") +
  scale_x_discrete(breaks = NULL) +
  xlab("") +
  geom_text(aes(x = 2, y = 0.4, label = label, group = pcs_eng),
            hjust= 0.5, vjust=1,
            inherit.aes = FALSE,
            size = text_size*(2/7),
            color = dark_blue) +
  gg_theme +
  geom_hline(yintercept = 0, color = dark_blue) 

dev.off()

mere_data <- myfiles$gg_mere_pcs_bar_data %>% filter(subject == "Mathematics") %>%
  filter(!str_detect(pcs_mere_longname, 'Retraité|Non renseignée|contingent|Sans profession|Etudiant|Clergé')) %>%
  left_join(names_pcs, by = c("pcs_mere" = "pcs")) %>%
  #rename('pcs_eng' = 'lola_final_version_short') %>%
  group_by(pcs_eng) %>%
  mutate(N = n_boys[time == "T1"] + n_girls[time == "T1"],
         label = paste("N =", N)) %>%
  ungroup()


# grDevices::cairo_pdf(file.path(output_fig, "gg_ma_mother.pdf"),
#                      height = 10,
#                      width = figure_width_three_col)

grDevices::svg(file.path(output_fig, "gg_ma_mother.svg"),
               height = 10,
               width = figure_width_three_col)

ggplot(mere_data,
       aes(x = time, y = cohen_d_manual, fill = time)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap( ~ pcs_eng, ncol = 6) + 
  scale_fill_manual(name = "", 
                    values = c("T1" = col_beg_cp,
                               "T2" = col_mid_cp,
                               "T3" = col_beg_ce)) +
  ylab("Math gender gap in Cohen's d\n(Boys - Girls)") +
  scale_x_discrete(breaks = NULL) +
  geom_text(aes(x = 2, y = 0.4, label = label, group = pcs_eng),
            hjust= 0.5, vjust=1,
            inherit.aes = FALSE,
            size = text_size*(2/7),
            color = dark_blue) +
  xlab("") +
  gg_theme +
  geom_hline(yintercept = 0, color = dark_blue)


dev.off()


# Function to create decrochage data
create_decrochage <- function(data, year) {
  message("Processing decrochage data for year: ", year)
  data %>%
    filter(subject == "Mathematics" & time != "T2") %>%
    group_by(school_departement) %>%
    summarise(decrochage = cohen_d_manual[time == "T3"] - cohen_d_manual[time == "T1"], .groups = "drop") %>%
    select(school_departement, decrochage)
}

# List of input data and years
myfiles <- list(
  `2018-2019` = myfiles$`gg_departements_data_2018-2019`,
  `2019-2020` = myfiles$`gg_departements_data_2019-2020`,
  `2020-2021` = myfiles$`gg_departements_data_2020-2021`,
  `2021-2022` = myfiles$`gg_departements_data_2021-2022`
)

# Create decrochage data for all years
decrochage_departements <- map2(
  myfiles, names(myfiles), 
  ~ create_decrochage(.x, .y)
)

# Function to process departements
process_departments <- function(departement_data, year) {
  message("Processing department geometries for year: ", year)

  
  departements_carto <- st_read(file.path("C:/Users/j.sultan/Dropbox/Reperes_CP_CE1/data/geo/departements-20180101.shp")) %>%
    mutate(code_insee = ifelse(code_insee %in% c('69D', '69M'), '69', code_insee)) %>%
    st_transform(crs = 2154)
  
  # Merge with input data
  decrochage_dep_data <- departements_carto %>%
    mutate(code_insee = sub("^0", "", code_insee)) %>%
    left_join(departement_data, by = c("code_insee" = "school_departement"))
  
  # Function to modify geometry
  place_geometry <- function(geometry, position, scale = 1) {
    output_geometry <- (geometry - st_centroid(geometry)) * scale + st_centroid(geometry) +
      position
    st_crs(output_geometry) <- st_crs(geometry)
    return(output_geometry)
  }
  
  # Process metropolitan France
  fra <- decrochage_dep_data %>% filter(!code_insee %in% c(971, 972, 973, 974, 976))
  fra_bbox <- st_bbox(fra)
  
  # Process each DOM
  glp <- decrochage_dep_data %>% filter(code_insee == 971)
  guy <- decrochage_dep_data %>% filter(code_insee == 973)
  mtq <- decrochage_dep_data %>% filter(code_insee == 972)
  myt <- decrochage_dep_data %>% filter(code_insee == 976)
  reu <- decrochage_dep_data %>% filter(code_insee == 974)
  
  # Adjust DOM positions and scales
  mtq2 <- mtq %>%
    mutate(geometry = place_geometry(st_geometry(mtq), 
                                     c(fra_bbox$xmin - st_bbox(mtq)$xmin - 150000,
                                       fra_bbox$ymin - st_bbox(mtq)$ymin + 50000), 
                                     scale = 2.5))
  
  glp2 <- glp %>%
    mutate(geometry = place_geometry(st_geometry(glp), 
                                     c(fra_bbox$xmin - st_bbox(glp)$xmin - 150000,
                                       st_bbox(mtq2)$ymax - st_bbox(glp)$ymin + 1.25 * 130000 - 20000), 
                                     scale = 2.5))
  
  reu2 <- reu %>%
    mutate(geometry = place_geometry(st_geometry(reu), 
                                     c(fra_bbox$xmin - st_bbox(reu)$xmin - 200000,
                                       st_bbox(glp2)$ymax - st_bbox(reu)$ymin + 0.5 * 130000), 
                                     scale = 1.2))
  
  myt2 <- myt %>%
    mutate(geometry = place_geometry(st_geometry(myt), 
                                     c(fra_bbox$xmin - st_bbox(myt)$xmin - 150000,
                                       st_bbox(reu2)$ymax - st_bbox(myt)$ymin + 0.75 * 130000), 
                                     scale = 3))
  
  guy2 <- guy %>%
    mutate(geometry = place_geometry(st_geometry(guy), 
                                     c(fra_bbox$xmin - st_bbox(guy)$xmin - 400000,
                                       st_bbox(myt2)$ymax - st_bbox(guy)$ymin - 100000), 
                                     scale = 0.4))
  
  # Combine all adjusted geometries
  data_2 <- bind_rows(fra, guy2, mtq2, myt2, reu2, glp2)
  
  return(data_2)
}

# Apply processing function for all years
data_2_list <- map2(decrochage_departements, names(myfiles), process_departments)

# Naming outputs
names(data_2_list) <- c("data_2_18", "data_2_19", "data_2_20", "data_2_21")

# Access each year as data_2_list$data_2_18, data_2_list$data_2_19, etc.


create_plot <- function(data) {
  ggplot() +
    geom_sf(data = data, aes(geometry = geometry, fill = decrochage), 
            color = "white", size = 0.2) +
    coord_sf(datum = NA, expand = FALSE) + 
    theme_void() + 
    scale_fill_gradient(low = "#56B1F7", high = "#132B43", 
                        name = "Variation in Cohen's d", 
                        limits = c(0, 0.4)) +
    theme(plot.margin = unit(c(1,1,1,1), "cm"),
          legend.position = "bottom",
          text = element_text(size = text_size, colour = darker_blue_txt))
  
}

# Create individual plots

p = 20
f <- create_plot(data_2_list$data_2_18) + theme(legend.position = "none",
                                                plot.margin = margin(t = 0,  
                                                                     r = p,  
                                                                     b = p,
                                                                     l = 0),
                                                plot.title = element_text(hjust = 0.5, color = darker_blue_txt,
                                                                          margin = margin(b = p))) +
  labs(title = "2018")
g <- create_plot(data_2_list$data_2_19) + theme(legend.position = "none",
                                                plot.margin = margin(t = 0,  
                                                                     r = 0,  
                                                                     b = p,
                                                                     l = p),
                                                plot.title = element_text(hjust = 0.5, color = darker_blue_txt,
                                                                          margin = margin(b = p))) +
  labs(title = "2019")
h <- create_plot(data_2_list$data_2_20) + theme(legend.position = "none",
                                                plot.margin = margin(t = p,  
                                                                     r = p,  
                                                                     b = 0,
                                                                     l = 0),
                                                plot.title = element_text(hjust = 0.5, color = darker_blue_txt,
                                                                          margin = margin(b = p))) +
  labs(title = "2020")
i <- create_plot(data_2_list$data_2_21) + theme(legend.position = "none",
                                                plot.margin = margin(t = p,  
                                                                     r = 0,  
                                                                     b = 0,
                                                                     l = p),
                                                plot.title = element_text(hjust = 0.5, color = darker_blue_txt,
                                                                          margin = margin(b = p))) +
  labs(title = "2021")

# Extract the legend from one of the plots
legend_plot <- create_plot(data_2_list$data_2_18)
legend = cowplot::get_plot_component(legend_plot, 'guide-box-bottom', return_all = TRUE)

test <- plot_grid(f, g, ncol = 2, nrow = 1)
test2 <- plot_grid(h, i, ncol = 2, nrow = 1)

### pdf
# grDevices::cairo_pdf(file.path(output_fig, "ground_lost_departements.pdf"),
#                      height = 5.5*2,
#                      width = figure_width_two_col*2)

grDevices::svg(file.path(output_fig, "ground_lost_departements.svg"),
               height = 5.5*2,
               width = figure_width_two_col*2)


plot_grid(test, test2, legend, ncol = 1, rel_widths = c(100, 100, 0.1),
          axis = "tblr")

dev.off()

