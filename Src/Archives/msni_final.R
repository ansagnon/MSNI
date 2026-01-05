rm(list = ls())
library(devtools)
library(humind)
library(dplyr)
library(readxl)
library(writexl)
library(impactR.utils)
library(impactR4PHU)
library(analysistools)
# library(presentresults)
library(tidyr)
library(tidyverse)

#source("functions/internals.R")

# Fonction pour calculer les sommes avec apply

fun_sum <- function(x) {
  if (all(is.na(x))) {
    NA
  } else {
    sum(x, na.rm = TRUE)
  }
}



###### Chargement des bases de données
## Base principale

# raw_data_clean <- readxl::read_xlsx("output/data/ner_msna_clean_data_v2_2024-09-05_analysis.xlsx",sheet="raw_data_clean",col_names = TRUE) |> 
#   dplyr::mutate(admin1_d_statut_deplacement=paste(admin1,d_statut_deplacement,sep = "_"),
#                 admin2_d_statut_deplacement=paste(admin2,d_statut_deplacement,sep = "_"))
# 
# loop_data_clean <- readxl::read_xlsx("output/data/ner_msna_clean_data_v2_2024-09-05_analysis.xlsx",sheet="loop_data_clean",col_names = TRUE) |> 
#   dplyr::rename(uuid=submission_uuid)|> 
#   dplyr::mutate(admin1_d_statut_deplacement=paste(admin1,d_statut_deplacement,sep = "_"),
#                 admin2_d_statut_deplacement=paste(admin2,d_statut_deplacement,sep = "_"))

# 
# #For PIN CHECKING - ESSINAM
# raw_data_admin2 <- raw_data_clean |> 
#   dplyr::select(admin1,admin2,admin3) |> 
#   dplyr::distinct(admin2,.keep_all = TRUE)
# 
# 
# 
# liste_admin2 <- readxl::read_xlsx("input/sampling/liste_admin_name.xlsx", sheet="liste_dep")
# 
# raw_data_admin_label <- raw_data_admin2 |> 
#   dplyr::left_join(liste_admin2, by=c("admin1"="Region_PCODE","admin2"="departement_PCODE"))
# 
# ####
# 
# 
# raw_data_clean <- raw_data_clean %>%
#   dplyr::mutate(wash_eau_source_score_cluster = case_when(
#     wash_domestic_water_source %in% c("piped_dwelling", "piped_compound", "piped_neighbour") ~ 1,
#     wash_domestic_water_source %in% c("tap", "forage", "protected_well", "well_spring", "cart_tank", "kiosk", "bottled_water", "sachet_water") ~ 2,
#     wash_drinking_water_source %in% c("unprotected_well", "tank_truck") ~ 3,
#     wash_drinking_water_source %in% c("rainwater_collection", "unprotected_spring") ~ 4,
#     wash_drinking_water_source =="surface_water"~ 5,
#     TRUE ~ NA_real_
#   ))
# 
# 
# raw_data_clean <- raw_data_clean %>%
#   dplyr::mutate(wash_time_score_cluster = dplyr::case_when(
#     wash_domestic_water_source=="piped_dwelling" & wash_domestic_water_quantity=="never_n" | wash_domestic_water_time_yn=="water_on_premises" & wash_domestic_water_quantity=="never_n"~1,
#     wash_domestic_water_time=="under30min" & wash_domestic_water_quantity=="never_n"~2,
#     wash_domestic_water_time=="30min_1hr"  & wash_domestic_water_quantity %in% c("rarely","sometimes")~3,
#     wash_domestic_water_time=="more_than_1hr" & wash_domestic_water_quantity=="often_n"~4,
#     wash_domestic_water_time=="more_than_1hr" & wash_domestic_water_quantity=="always_n"~5,
#     TRUE~NA_real_)
#   )
# 
# raw_data_clean <- raw_data_clean %>%
#   dplyr::mutate(wash_latrine_score_cluster = case_when(
#     stringr::str_detect(wash_sanitation_facility,"flush_piped_sewer|flush_septic_tank|flush_pit_latrine|flush_open_drain|flush_other|flush_elsewhere|compost|pit_latrine_slab") & stringr::str_detect(wash_sanitation_facility_sharing_yn,"no")~1,
#     stringr::str_detect(wash_sanitation_facility,"flush_piped_sewer|flush_septic_tank|flush_pit_latrine|flush_open_drain|flush_other|flush_elsewhere|compost|pit_latrine_slab") & stringr::str_detect(wash_sanitation_facility_sharing_yn,"yes")~2,
#     stringr::str_detect(wash_sanitation_facility,"pit_latrine_wo_slab") & stringr::str_detect(wash_sanitation_facility_sharing_yn,"no")~3,
#     stringr::str_detect(wash_sanitation_facility,"pit_latrine_wo_slab|plastic_bag|bucket") & stringr::str_detect(wash_sanitation_facility_sharing_yn,"yes")~4,
#     wash_sanitation_facility=="none_installation"~5,
#     TRUE~NA_real_
#   ))
# raw_data_clean <- raw_data_clean %>%
#   dplyr::mutate(wash_savon_score_cluster = case_when(
#     stringr::str_detect(wash_handwashing_facility,"available|mobile_object") & stringr::str_detect(wash_handwashing_facility_observed_water,"water_available") & stringr::str_detect(wash_handwashing_facility_observed_soap,"soap_available")~1,
#     stringr::str_detect(wash_handwashing_facility,"available|mobile_object") & stringr::str_detect(wash_handwashing_facility_observed_water,"water_available") & stringr::str_detect(wash_handwashing_facility_observed_soap,"alternative_available")~2,
#     stringr::str_detect(wash_handwashing_facility,"available|mobile_object") & stringr::str_detect(wash_handwashing_facility_observed_water,"water_available") & stringr::str_detect(wash_handwashing_facility_observed_soap,"soap_not_available")~3,
#     stringr::str_detect(wash_handwashing_facility,"available|mobile_object") & stringr::str_detect(wash_handwashing_facility_observed_water,"water_available") & stringr::str_detect(wash_handwashing_facility_observed_soap,"soap_not_available|alternative_available")~4,
#     stringr::str_detect(wash_handwashing_facility,"none_place_wash|no_permission|other") & stringr::str_detect(wash_handwashing_facility_observed_soap,"soap_not_available")~5,
#     TRUE~NA_real_
#   ))
# 
# wash_severite_data <- raw_data_clean |> 
#   dplyr::select(wash_domestic_water_source,wash_domestic_water_quantity,wash_domestic_water_time_yn,wash_domestic_water_time,wash_time_score_cluster,
#                 )


















library(pacman)
pacman::p_load(humind)
openxlsx::getSheetNames("Input/msna_bfa_2025_sans_gps_2025-12-23.xlsx")
raw_data_clean <- openxlsx::read.xlsx(xlsxFile = "Input/msna_bfa_2025_sans_gps_2025-12-23.xlsx", sheet = "main")




#####################################   Sécurité alimentaire  ######################################
# raw_data_clean <- raw_data_clean |> 
#   dplyr::select(-c(fsl_hhs_nofoodhh_recoded,fsl_hhs_nofoodhh_freq_recoded,fsl_hhs_sleephungry_recoded,fsl_hhs_sleephungry_freq_recoded,
#                    fsl_hhs_alldaynight_recoded,fsl_hhs_alldaynight_freq_recoded))

## Ajout des phases

## Calcul des indicateurs de SECAL avec addindicators

raw_data_clean <- raw_data_clean %>% 
  dplyr::select(-dplyr::matches("hhmember|c_hh_member_list")) %>% 
  # dplyr::mutate(fsl_lcsi_stress1_cal = dplyr::case_when(fsl_lcsi_stress1 %in% c('yes','no_exhausted')~1,
  #                                                       TRUE~0),
  #               fsl_lcsi_stress2_cal = dplyr::case_when(fsl_lcsi_stress2 %in% c('yes','no_exhausted')~1,
  #                                                       TRUE~0),
  #               fsl_lcsi_stress3_cal = dplyr::case_when(fsl_lcsi_stress3 %in% c('yes','no_exhausted')~1,
  #                                                       TRUE~0),
  #               fsl_lcsi_stress4_cal = dplyr::case_when(fsl_lcsi_stress4 %in% c('yes','no_exhausted')~1,
  #                                                       TRUE~0),
  #               fsl_lcsi_crisis1_cal = dplyr::case_when(fsl_lcsi_crisis1 %in% c('yes','no_exhausted')~1,
  #                                                       TRUE~0),
  #               fsl_lcsi_crisis2_cal =  dplyr::case_when(fsl_lcsi_crisis2 %in% c('yes','no_exhausted')~1,
  #                                                        TRUE~0),
  #               fsl_lcsi_crisis3_cal =  dplyr::case_when(fsl_lcsi_crisis3 %in% c('yes','no_exhausted')~1,
  #                                                        TRUE~0),
  #               fsl_lcsi_emergency1_cal =  dplyr::case_when(fsl_lcsi_emergency1 %in% c('yes','no_exhausted')~1,
  #                                                           TRUE~0),
  #               fsl_lcsi_emergency2_cal =   dplyr::case_when(fsl_lcsi_emergency2 %in% c('yes','no_exhausted')~1,
  #                                                            TRUE~0),
  #               fsl_lcsi_emergency3_cal =  dplyr::case_when(fsl_lcsi_emergency3 %in% c('yes','no_exhausted')~1,
  #                                                           TRUE~0),
  #               # fsl_hhs_nofoodhh=dplyr::case_when(fsl_hhs_nofoodhh=="yes"~"yes",
  #               #                                   fsl_hhs_nofoodhh=="no"~"no",
  #               #                                   TRUE~NA_character_),
  #               # fsl_hhs_sleephungry=dplyr::case_when(fsl_hhs_sleephungry=="yes"~"yes",
  #               #                                      fsl_hhs_sleephungry=="no"~"no",
  #               #                                      TRUE~NA_character_),
  #               # fsl_hhs_alldaynight=dplyr::case_when(fsl_hhs_alldaynight=="yes"~"yes",
  #               #                                      fsl_hhs_alldaynight=="no"~"no",
  #               #                                      TRUE~NA_character_),
  #               # fsl_rcsi_lessquality=dplyr::if_else(dplyr::between(fsl_rcsi_lessquality,0,100),fsl_rcsi_lessquality,NA),
  #               # fsl_rcsi_borrow=dplyr::if_else(dplyr::between(fsl_rcsi_borrow,0,100),fsl_rcsi_borrow,NA),
  #               # fsl_rcsi_mealsize=dplyr::if_else(dplyr::between(fsl_rcsi_mealsize,0,100),fsl_rcsi_mealsize,NA),
  #               # fsl_rcsi_mealadult=dplyr::if_else(dplyr::between(fsl_rcsi_mealadult,0,100),fsl_rcsi_mealadult,NA),
  #               # fsl_rcsi_mealnb=dplyr::if_else(dplyr::between(fsl_rcsi_mealnb,0,100),fsl_rcsi_mealnb,NA)
  # ) %>%
  humind::add_hhs( 
    hhs_nofoodhh_1 = "fsl_hhs_nofoodhh",
    hhs_nofoodhh_1a = "fsl_hhs_nofoodhh_freq",
    hhs_sleephungry_2 = "fsl_hhs_sleephungry",
    hhs_sleephungry_2a = "fsl_hhs_sleephungry_freq",
    hhs_alldaynight_3 = "fsl_hhs_alldaynight",
    hhs_alldaynight_3a = "fsl_hhs_alldaynight_freq",
    yes_answer = "yes",
    no_answer = "no",
    rarely_answer = "rarely",
    sometimes_answer = "sometimes",
    often_answer = "often"
  ) %>% 
  # Ajout RCSI
  humind::add_rcsi(
    rCSILessQlty = "fsl_rcsi_lessquality",
    rCSIBorrow = "fsl_rcsi_borrow",
    rCSIMealSize = "fsl_rcsi_mealsize",
    rCSIMealAdult = "fsl_rcsi_mealadult",
    rCSIMealNb = "fsl_rcsi_mealnb",
    new_colname = "rcsi") %>%
  # FCS
  humind::add_fcs(
    cutoffs = "normal",
    fsl_fcs_cereal = "fsl_fcs_cereal",
    fsl_fcs_legumes = "fsl_fcs_legumes",
    fsl_fcs_veg = "fsl_fcs_veg",
    fsl_fcs_fruit = "fsl_fcs_fruit",
    fsl_fcs_meat = "fsl_fcs_meat",
    fsl_fcs_dairy = "fsl_fcs_dairy",
    fsl_fcs_sugar = "fsl_fcs_sugar",
    fsl_fcs_oil = "fsl_fcs_oil") %>% 
  # LCSI
  humind::add_lcsi(
    lcsi_stress_vars = c("fsl_lcsi_stress1", "fsl_lcsi_stress2", "fsl_lcsi_stress3", "fsl_lcsi_stress4"),
    lcsi_crisis_vars = c("fsl_lcsi_crisis1", "fsl_lcsi_crisis2", "fsl_lcsi_crisis3"),
    lcsi_emergency_vars = c("fsl_lcsi_emergency1", "fsl_lcsi_emergency2", "fsl_lcsi_emergency3"),
    yes_val = "yes",
    no_val = "no_had_no_need",
    exhausted_val = "no_exhausted",
    not_applicable_val = "not_applicable")





fsl_lcsi_stress <- c("fsl_lcsi_stress1_cal","fsl_lcsi_stress2_cal","fsl_lcsi_stress3_cal","fsl_lcsi_stress4_cal")
fsl_lcsi_crisis <-c("fsl_lcsi_crisis1_cal","fsl_lcsi_crisis2_cal","fsl_lcsi_crisis3_cal")
fsl_lcsi_emergency <-c("fsl_lcsi_emergency1_cal","fsl_lcsi_emergency2_cal","fsl_lcsi_emergency3_cal")


# Création de variables pour les checks

## Utilisation d'une stratégie de stress
raw_data_clean$fsl_lcsi_stress<-apply(raw_data_clean[,fsl_lcsi_stress],1,fun_sum)

## Utilisation d'une stratégie de crise
raw_data_clean$fsl_lcsi_crisis<-apply(raw_data_clean[,fsl_lcsi_crisis],1,fun_sum)

## Utilisation d'une stratégie d'urgence
raw_data_clean$fsl_lcsi_emergency<-apply(raw_data_clean[,fsl_lcsi_emergency],1,fun_sum)

# fsl_lcsi_cat,fsl_lcsi_cat_exhaust

raw_data_clean <- raw_data_clean |> 
  humind::add_lcsi(fsl_lcsi_stress1 = "fsl_lcsi_stress1",
                   fsl_lcsi_stress2 = "fsl_lcsi_stress2",
                   fsl_lcsi_stress3 = "fsl_lcsi_stress3",
                   fsl_lcsi_stress4 = "fsl_lcsi_stress4",
                   fsl_lcsi_crisis1 = "fsl_lcsi_crisis1",
                   fsl_lcsi_crisis2 = "fsl_lcsi_crisis2",
                   fsl_lcsi_crisis3 = "fsl_lcsi_crisis3",
                   fsl_lcsi_emergency1 = "fsl_lcsi_emergency1",
                   fsl_lcsi_emergency2 = "fsl_lcsi_emergency2",
                   fsl_lcsi_emergency3 = "fsl_lcsi_emergency3",
                   yes_val = "yes",
                   no_val = "no_had_no_need",
                   exhausted_val = "no_exhausted",
                   not_applicable_val = "not_applicable")






raw_data_clean <- raw_data_clean |> 
  humind::add_hhs(fsl_hhs_nofoodhh = "fsl_hhs_nofoodhh",
                  fsl_hhs_nofoodhh_freq = "fsl_hhs_nofoodhh_freq",
                  fsl_hhs_sleephungry = "fsl_hhs_sleephungry",
                  fsl_hhs_sleephungry_freq = "fsl_hhs_sleephungry_freq",
                  fsl_hhs_alldaynight = "fsl_hhs_alldaynight",
                  fsl_hhs_alldaynight_freq = "fsl_hhs_alldaynight_freq",
                  yes_answer = "yes",
                  no_answer = "no",
                  rarely_answer = "rarely",
                  sometimes_answer = "sometimes",
                  often_answer = "often"
  )



raw_data_clean <- raw_data_clean |> 
  humind::add_rcsi(fsl_rcsi_lessquality = "fsl_rcsi_lessquality",
                   fsl_rcsi_borrow = "fsl_rcsi_borrow",
                   fsl_rcsi_mealsize = "fsl_rcsi_mealsize",
                   fsl_rcsi_mealadult = "fsl_rcsi_mealadult",
                   fsl_rcsi_mealnb = "fsl_rcsi_mealnb")

raw_data_clean <-raw_data_clean |>
  impactR4PHU::add_fcm_phase(fcs_column_name = "fsl_fcs_cat",
                             rcsi_column_name = "rcsi_cat",
                             hhs_column_name = "hhs_cat_ipc",
                             fcs_categories_acceptable = "Acceptable",
                             fcs_categories_poor = "Poor",
                             fcs_categories_borderline = "Borderline",
                             rcsi_categories_low = "No to Low",
                             rcsi_categories_medium = "Medium",
                             rcsi_categories_high = "High",
                             hhs_categories_none = "None",
                             hhs_categories_little = "Little",
                             hhs_categories_moderate = "Moderate",
                             hhs_categories_severe = "Severe",
                             hhs_categories_very_severe = "Very Severe")



## Idicateur composite SECAL
raw_data_clean <-raw_data_clean |>
  humind::add_comp_foodsec(fc_phase = "fsl_fc_phase",
                           phase1 = "Phase 1 FC",
                           phase2 = "Phase 2 FC",
                           phase3 = "Phase 3 FC",
                           phase4 = "Phase 4 FC",
                           phase5 = "Phase 5 FC")



####################################        Santé                   #############################

## Individu ayant besoin d'accéder à des soins de santé - Binaire : Ok
loop_data_clean<-loop_data_clean |> 
  humind::add_loop_healthcare_needed_cat(ind_healthcare_needed = "health_ind_healthcare_needed",
                                         ind_healthcare_needed_no = "no",
                                         ind_healthcare_needed_yes = "yes",
                                         ind_healthcare_needed_dnk = "dnk",
                                         ind_healthcare_needed_pnta = "pnta",
                                         ind_healthcare_received = "health_ind_healthcare_received",
                                         ind_healthcare_received_no = "no",
                                         ind_healthcare_received_yes = "yes",
                                         ind_healthcare_received_dnk = "dnk",
                                         ind_healthcare_received_pnta = "pnta",
                                         wgq_dis = NULL,
                                         ind_age = "ind_age")
                                 

## Nombre de personnes qui n'ont pas eu besoin d'accéder aux soins de santé : Ok
raw_data_clean<-raw_data_clean |> 
  humind::add_loop_healthcare_needed_cat_to_main(loop_data_clean,
                                                 ind_healthcare_needed_no = "health_ind_healthcare_needed_no",
                                                 ind_healthcare_needed_yes_unmet = "health_ind_healthcare_needed_yes_unmet",
                                                 ind_healthcare_needed_yes_met = "health_ind_healthcare_needed_yes_met",
                                                 ind_healthcare_needed_no_wgq_dis = NULL,
                                                 ind_healthcare_needed_yes_unmet_wgq_dis = NULL,
                                                 ind_healthcare_needed_yes_met_wgq_dis = NULL,
                                                 id_col_main = "uuid",
                                                 id_col_loop = "uuid")
                                         


## Indicateur composite santé : Ok
raw_data_clean<-raw_data_clean |> 
  humind::add_comp_health(ind_healthcare_needed_no_n = "health_ind_healthcare_needed_no_n",
                          ind_healthcare_needed_yes_unmet_n = "health_ind_healthcare_needed_yes_unmet_n",
                          ind_healthcare_needed_yes_met_n = "health_ind_healthcare_needed_yes_met_n",
                          wgq_dis = FALSE,
                          ind_healthcare_needed_no_wgq_dis_n = NULL,
                          ind_healthcare_needed_yes_unmet_wgq_dis_n = NULL,
                          ind_healthcare_needed_yes_met_wgq_dis_n =NULL)
                  





####################################        Education                   #############################

## Individu en âge scolaire dans la boucle - Binaire
loop_data_clean<-loop_data_clean |> 
  humind::add_loop_edu_ind_age_corrected(raw_data_clean,
                                         id_col_loop = "uuid",
                                         id_col_main = "uuid",
                                         survey_start_date = "start",
                                         school_year_start_month = 10, 
                                         ind_age = "ind_age",
                                         schooling_start_age = 5,
                                         month = NULL)

                                 


## Nombre d'individus(somme des individus par uuid) en âge scolaire rappoté dans la base princicale à partir de la base boucle
raw_data_clean <- raw_data_clean |> 
  humind::add_loop_edu_ind_schooling_age_d_to_main(loop_data_clean,
                                                   ind_schooling_age_d = "edu_ind_schooling_age_d", 
                                                   id_col_main = "uuid",
                                                   id_col_loop = "uuid")

                                           


## Individu fréquentant l'école dans la boucle  - Binaire
loop_data_clean<-loop_data_clean |> 
  humind::add_loop_edu_access_d(ind_access = "edu_access",
                                yes = "yes",
                                no = "no",
                                pnta = "pnta",
                                dnk = "dnk",
                                ind_schooling_age_d = "edu_ind_schooling_age_d")
                        


## Ajout de la variable "edu_access" à la base principale
raw_data_clean <-raw_data_clean |> 
  humind::add_loop_edu_access_d_to_main(loop_data_clean,
                                        ind_access_d = "edu_ind_access_d",
                                        ind_no_access_d = "edu_ind_no_access_d", 
                                        id_col_main = "uuid",
                                        id_col_loop = "uuid")

                                


## Individu avec une barrière d'accès à l'école liée à la protection de l'enfance - Binaire
#Problèmes de barrière non prise en compte dans la fonction en attendant le retours de ROCIO("trop_jeune","abandon","exclu")

# A discuter des modalités à prendre en compte

loop_data_clean<-loop_data_clean |> 
  humind::add_loop_edu_barrier_protection_d(barriers = "edu_barrier",
                                            protection_issues = c("protection_at_school", "protection_travel_school",
                                                                  "child_work_home", "child_work_outside", "child_armed_group", "child_marriage",
                                                                  "ban", "enroll_lack_documentation", "discrimination_stig"),
                                            ind_schooling_age_d = "edu_ind_schooling_age_d")
                                    


## Ajout de la variable "edu_barrier" à la base principale
raw_data_clean <- raw_data_clean |> 
  humind::add_loop_edu_barrier_protection_d_to_main(loop_data_clean,
                                                    ind_barrier_protection_d = "edu_ind_barrier_protection_d",
                                                    id_col_main = "uuid",
                                                    id_col_loop = "uuid")
                                            




## Individu dont l'éducation a été perturbée 

loop_data_clean<-loop_data_clean |> 
  humind::add_loop_edu_disrupted_d(occupation = "edu_disrupted_occupation",
                                   hazards = "edu_disrupted_hazards",
                                   displaced = "edu_disrupted_displaced",
                                   teacher = "edu_disrupted_teacher",
                                   levels = c("yes", "no", "dnk", "pnta"),
                                   ind_schooling_age_d = "edu_ind_schooling_age_d")
                               

## Ajout des  modalités de perturbation de l'ecole  dans la base principale
raw_data_clean <-raw_data_clean |> 
  humind::add_loop_edu_disrupted_d_to_main(loop_data_clean,
                                           occupation_d = "edu_disrupted_occupation_d", 
                                           hazards_d = "edu_disrupted_hazards_d", 
                                           displaced_d = "edu_disrupted_displaced_d", 
                                           teacher_d = "edu_disrupted_teacher_d", 
                                           id_col_main = "uuid",
                                           id_col_loop = "uuid")
    

## Indicateur composite de l'education 
raw_data_clean <-raw_data_clean |> 
  humind::add_comp_edu(schooling_age_n = "edu_schooling_age_n",
                       no_access_n = "edu_no_access_n",
                       barrier_protection_n = "edu_barrier_protection_n",
                       occupation_n = "edu_disrupted_occupation_n",
                       hazards_n = "edu_disrupted_hazards_n",
                       displaced_n = "edu_disrupted_displaced_n",
                       teacher_n = "edu_disrupted_teacher_n")
                              


####################################        EHA                   #############################

#Catégorie de la source d'eau potable ok
raw_data_clean<-raw_data_clean |> 
  humind::add_drinking_water_source_cat(drinking_water_source = "wash_drinking_water_source",
                                        drinking_water_source_cat_improved = c("forage","piped_dwelling", "piped_compound","piped_neighbour", "tap", 
                                                                               "protected_well", "rainwater_collection", "tank_truck","well_spring", 
                                                                               "cart_tank", "kiosk","sachet_water"),
                                        drinking_water_source_cat_unimproved = c("unprotected_well", "unprotected_spring"),
                                        drinking_water_source_cat_surface_water = "surface_water",
                                        drinking_water_source_cat_undefined = c("dnk", "pnta", "other")
                                        )
                                              


## Catégorie du temps nécessaire pour aller chercher de l'eau potable ok
## Supression de tous les zéro de la variable "wash_drinking_water_time_int" car la "add_drinking_water_time_cat" n'autorise pas de 0

# raw_data_clean<-raw_data_clean %>% 
#   mutate(wash_drinking_water_time_int=case_when(wash_drinking_water_time_int==0~NA_integer_,
#                                                 TRUE~wash_drinking_water_time_int))


table(raw_data_clean$wash_drinking_water_time_int, useNA = 'ifany')


raw_data_clean<-raw_data_clean |> 
  humind::add_drinking_water_time_cat(drinking_water_time_yn = "wash_drinking_water_time_yn",
                                      water_on_premises = "water_on_premises",
                                      number_minutes = "number_minutes",
                                      dnk = "dnk",
                                      undefined = "pnta",
                                      drinking_water_time_int = "wash_drinking_water_time_int",
                                      max = 600,
                                      drinking_water_time_sl = "wash_drinking_water_time_sl",
                                      sl_under_30_min = "under30min",
                                      sl_30min_1hr = "30min_1hr",
                                      sl_more_than_1hr = "more_than_1hr",
                                      sl_undefined = c("dnk", "pnta"),
                                      drinking_water_source = "wash_drinking_water_source",
                                      skipped_drinking_water_source_premises = "piped_dwelling",
                                      skipped_drinking_water_source_undefined = c("dnk", "pnta"))


table(raw_data_clean$wash_drinking_water_time_cat, useNA = 'ifany')

## Seuil du temps nécessaire pour aller chercher de l'eau potable (seuil à 30 minutes)



#saveRDS(raw_data_clean,"raw_data_clean.RDS")
raw_data_clean <- raw_data_clean |> 
  humind::add_drinking_water_time_threshold_cat(drinking_water_time_30min_cat = "wash_drinking_water_time_cat",
                                                drinking_water_time_30min_cat_premises = "premises",
                                                drinking_water_time_30min_cat_under_30min = c("under30min"),
                                                drinking_water_time_30min_cat_above_30min = c("30min_1hr","more_than_1hr"),
                                                drinking_water_time_30min_cat_undefined = "undefined")



# ## Catégorie de l’échelle JMP – Qualité de l’eau pour boire
raw_data_clean<-raw_data_clean |>
  humind::add_drinking_water_quality_jmp_cat(drinking_water_source_cat = "wash_drinking_water_source_cat",
                                             drinking_water_source_cat_improved = "improved",
                                             drinking_water_source_cat_unimproved = "unimproved",
                                             drinking_water_source_cat_surface_water = "surface_water",
                                             drinking_water_source_cat_undefined = "undefined",
                                             drinking_water_time_30min_cat = "wash_drinking_water_time_30min_cat",
                                             drinking_water_time_30min_cat_premises = "premises",
                                             drinking_water_time_30min_cat_under_30min = "under_30min",
                                             drinking_water_time_30min_cat_above_30min = "above_30min",
                                             drinking_water_time_30min_cat_undefined = "undefined")


                                                   

##############################################        Assainissement                   ##################
#Catégorie du type d'installation sanitaire ok
raw_data_clean<-raw_data_clean |> 
  humind::add_sanitation_facility_cat(sanitation_facility = "wash_sanitation_facility",
                                      improved = c("flush_piped_sewer", "flush_septic_tank", "flush_pit_latrine",
                                                   "flush_dnk_where", "pit_latrine_slab", "twin_pit_latrine_slab",
                                                   "ventilated_pit_latrine_slab", "container", "compost","flush_other"),
                                      unimproved = c("flush_open_drain", "flush_elsewhere", "pit_latrine_wo_slab", "bucket",
                                                     "hanging_toilet", "plastic_bag"),
                                      none = "none_installation",
                                      undefined = c("other", "dnk", "pnta"))

                              
# Nous avons: flush_other=   Chasse d'eau vers un autre endroit (A)                                  

## Statut de partage de l'installation sanitaire ok 
raw_data_clean<-raw_data_clean |> 
  humind::add_sharing_sanitation_facility_cat(sharing_sanitation_facility = "wash_sanitation_facility_sharing_yn",
                                              yes = "yes",
                                              no = "no",
                                              undefined = c("dnk", "pnta"),
                                              sanitation_facility = "wash_sanitation_facility",
                                              skipped_sanitation_facility = NULL)
                                      
                                                    

## Catégorie du nombre de personnes partageant l'installation sanitaire ok
raw_data_clean<-raw_data_clean |> 
  humind::add_sharing_sanitation_facility_n_ind(sharing_sanitation_facility_cat = "wash_sharing_sanitation_facility_cat",
                                                sharing_sanitation_facility_cat_shared = "shared",
                                                sharing_sanitation_facility_cat_not_shared = "not_shared",
                                                sharing_sanitation_facility_cat_not_applicable = "not_applicable",
                                                sharing_sanitation_facility_cat_undefined = "undefined",
                                                sanitation_facility_sharing_n = "wash_sanitation_facility_sharing_n",
                                                hh_size = "hh_size",
                                                weight = "weights")
                                        
                                                      

## Catégorie de l’échelle JMP – Latrines
raw_data_clean<-raw_data_clean |> 
  humind::add_sanitation_facility_jmp_cat(sanitation_facility_cat = "wash_sanitation_facility_cat",
                                          sanitation_facility_cat_improved = "improved",
                                          sanitation_facility_cat_unimproved = "unimproved",
                                          sanitation_facility_cat_none = "none",
                                          sanitation_facility_cat_undefined = "undefined",
                                          sharing_sanitation_facility_cat = "wash_sharing_sanitation_facility_cat",
                                          sharing_sanitation_facility_cat_shared = "shared",
                                          sharing_sanitation_facility_cat_not_shared = "not_shared",
                                          sharing_sanitation_facility_cat_not_applicable = "not_applicable",
                                          sharing_sanitation_facility_cat_undefined = "undefined")
                                  
                                                
## Catégorie de l’échelle JMP – Station de lavage des mains
#Ajustement 
raw_data_clean <-raw_data_clean %>%  
  dplyr::mutate(wash_handwashing_facility=case_when(wash_handwashing_facility=="available"|wash_handwashing_facility=="mobile_object"~"available",
                                                    TRUE~wash_handwashing_facility))
                                                                                

raw_data_clean <-raw_data_clean |> 
  humind::add_handwashing_facility_cat(survey_modality = "survey_modality",
                                       survey_modality_in_person = c("in_person"),
                                       survey_modality_remote = c("remote"),
                                       facility = "wash_handwashing_facility",
                                       facility_yes = "available",                       #Normalement il devrait y'avoir "mobile_object" dans ce vecteur mais la fonction n'accepte qu'un element d"ou la rasion pourquoi j'ai merger en haut "available" et"mobile_object" pour avoir""available" 
                                       facility_no = "none_place_wash",
                                       facility_no_permission = "no_permission",
                                       facility_undefined = "other",
                                       facility_observed_water = "wash_handwashing_facility_observed_water",
                                       facility_observed_water_yes = "water_available",
                                       facility_observed_water_no = "water_not_available",
                                       facility_observed_soap = "wash_handwashing_facility_observed_soap",
                                       facility_observed_soap_yes = "soap_available",
                                       facility_observed_soap_no = "soap_not_available",
                                       facility_observed_soap_alternative = "alternative_available",
                                       facility_reported = "wash_handwashing_facility_reported",
                                       facility_reported_yes = c("available","fixed_dwelling","mobile"),
                                       facility_reported_no = c("none_place_wash"),
                                       facility_reported_undefined = c("other", "dnk", "pnta"),
                                       facility_reported_no_permission_soap = "wash_soap_observed",
                                       facility_reported_no_permission_soap_yes = c("yes_soap_shown", "yes_soap_not_shown"),
                                       facility_reported_no_permission_soap_no = "no",
                                       facility_reported_no_permission_soap_undefined = c("dnk", "pnta"),
                                       facility_reported_no_permission_soap_type = "wash_soap_observed_type",
                                       facility_reported_no_permission_soap_type_yes = c("soap", "detergent"),
                                       facility_reported_no_permission_soap_type_no = "ash_mud_sand",
                                       facility_reported_no_permission_soap_type_undefined = c("other", "dnk", "pnta"),
                                       facility_reported_remote_soap = "wash_soap_reported",
                                       facility_reported_remote_soap_yes = "yes",
                                       facility_reported_remote_soap_no = "no",
                                       facility_reported_remote_soap_undefined = c("dnk", "pnta","no_other_produits"), # j'ai ajouté "no_other_produits"
                                       facility_reported_remote_soap_type = "wash_soap_reported_type",
                                       facility_reported_remote_soap_type_yes = c("soap", "detergent"),
                                       facility_reported_remote_soap_type_no = c("ash_mud_sand"),
                                       facility_reported_remote_soap_type_undefined = c("other", "dnk", "pnta"))
                               
                                              

#correction de la majuscule en minscule dans la la varaible 
## indicateur composite EHA ok
raw_data_clean<-raw_data_clean |>
  humind::add_comp_wash(setting = "setting",
                        setting_camp = "camp",
                        setting_urban = "urban",
                        setting_rural = "rural",
                        drinking_water_quantity = "wash_drinking_water_quantity",
                        drinking_water_quantity_always  = "always_n",
                        drinking_water_quantity_often  = "often_n",
                        drinking_water_quantity_sometimes = "sometimes",
                        drinking_water_quantity_rarely = "rarely",
                        drinking_water_quantity_never = "never_n",
                        drinking_water_quantity_dnk = "dnk",
                        drinking_water_quantity_pnta = "pnta",
                        drinking_water_quality_jmp_cat = "wash_drinking_water_quality_jmp_cat",
                        drinking_water_quality_jmp_cat_surface_water   = "surface_water",
                        drinking_water_quality_jmp_cat_unimproved = "unimproved",
                        drinking_water_quality_jmp_cat_limited = "limited",
                        drinking_water_quality_jmp_cat_basic = "basic",
                        drinking_water_quality_jmp_cat_safely_managed = "safely_managed",
                        drinking_water_quality_jmp_cat_undefined = "undefined",
                        sanitation_facility_jmp_cat = "wash_sanitation_facility_jmp_cat",
                        sanitation_facility_jmp_cat_open_defecation = "open_defecation",
                        sanitation_facility_jmp_cat_unimproved = "unimproved",
                        sanitation_facility_jmp_cat_limited = "limited",
                        sanitation_facility_jmp_cat_basic = "basic",
                        sanitation_facility_jmp_cat_safely_managed = "safely_managed",
                        sanitation_facility_jmp_cat_undefined = "undefined",
                        sanitation_facility_cat = "wash_sanitation_facility_cat",
                        sanitation_facility_cat_none = "none",
                        sanitation_facility_cat_unimproved = "unimproved",
                        sanitation_facility_cat_improved = "improved",
                        sanitation_facility_cat_undefined = "undefined",
                        sanitation_facility_n_ind = "wash_sharing_sanitation_facility_n_ind",
                        sanitation_facility_n_ind_50_and_above = "50_and_above",
                        sanitation_facility_n_ind_20_to_49 = "20_to_49",
                        sanitation_facility_n_ind_19_and_below = "19_and_below",
                        handwashing_facility_jmp_cat = "wash_handwashing_facility_jmp_cat",
                        handwashing_facility_jmp_cat_no_facility  = "no_facility",
                        handwashing_facility_jmp_cat_limited = "limited",
                        handwashing_facility_jmp_cat_basic = "basic",
                        handwashing_facility_jmp_cat_undefined = "undefined")



#########################################################  ABNA ###################################################

## Catégorie du type d'abri : Ok
raw_data_clean<-raw_data_clean |> 
  humind::add_shelter_type_cat(shelter_type = "snfi_shelter_type",
                               sl_none = "none_snfi",
                               sl_collective_center = "collective_center",
                               sl_undefined = "pnta",
                               shelter_type_individual = "snfi_shelter_type_individual",
                               adequate = c("hut", "transitional_shelter", "durabel_shelter","nomad_tent"),
                               inadequate = c("collective_center","unfinished_building","straw_housing","traditional_tent", "emergency_shelter_plastic",
                                              "emergency_shelter_cotton","other_emergency_shelter"),
                               undefined = c("pnta", "other", "dnk"))
                       
                                     


## Nombre de problèmes de logement: Ok
raw_data_clean<-raw_data_clean |> 
  humind::add_shelter_issue_cat(shelter_issue = "snfi_shelter_issue",
                                none = "none_pb",
                                issues = c("lack_privacy", "lack_space_lackr", "temperature_hot", "temperature_cold", 
                                           "ventilation", "leak","lock", "lack_lighting","difficulty_move"),
                                undefined = c("dnk", "pnta"), 
                                other=c("other"),
                                sep = "_")

                        



## Risque lié aux modalités d'occupation: Ok
raw_data_clean<-raw_data_clean |> 
  humind::add_occupancy_cat(occupancy = "hlp_occupancy",
                            high_risk = c("no_agreement"),
                            medium_risk = c("rented", "hosted_free"),
                            low_risk = c("ownership"),
                            undefined = c("dnk", "pnta", "other"))
                    
                                  


## Nombre de tâches domestiques ne pouvant être effectuées : Ok
raw_data_clean<-raw_data_clean |> 
  humind::add_fds_cannot_cat(fds_cooking = "snfi_fds_cooking",
                             fds_cooking_cannot = "no_cannot",
                             fds_cooking_can_issues = "yes_issues",
                             fds_cooking_can_no_issues = "yes_no_issues",
                             fds_cooking_no_need = "no_no_need",
                             fds_cooking_undefined = c("pnta", "dnk"),
                             fds_sleeping = "snfi_fds_sleeping",
                             fds_sleeping_cannot = "no_cannot",
                             fds_sleeping_can_issues = "yes_issues",
                             fds_sleeping_can_no_issues = "yes_no_issues",
                             fds_sleeping_undefined = c("pnta", "dnk"),
                             fds_storing = "snfi_fds_storing",
                             fds_storing_cannot = "no_cannot",
                             fds_storing_can_issues = "yes_issues",
                             fds_storing_can_no_issues = "yes_no_issues",
                             fds_storing_undefined = c("pnta", "dnk"),
                             fds_personal_hygiene = "snfi_fds_personal_hygiene",
                             fds_personal_hygiene_cannot = "no_cannot",
                             fds_personal_hygiene_can_issues = "yes_issues",
                             fds_personal_hygiene_can_no_issues = "yes_no_issues",
                             fds_personal_hygiene_undefined = c("pnta", "dnk"),
                             lighting_source = "energy_lighting_source",
                             lighting_source_none = "none_fire",
                             lighting_source_undefined = c("pnta", "dnk"))

                    
# SNFI sectoral composite - add score and dummy for in need

###Indicateur composite  ABNA
raw_data_clean<- raw_data_clean |> 
  humind::add_comp_snfi()

# raw_data_clean<- raw_data_clean |> 
#   humind::add_comp_snfi(shelter_type_cat = "snfi_shelter_type_cat",
#                         shelter_type_cat_none = "none",
#                         shelter_type_cat_inadequate = "inadequate",
#                         shelter_type_cat_adequate = "adequate",
#                         shelter_type_cat_undefined = "undefined",
#                         shelter_issue_cat = "snfi_shelter_issue_cat",
#                         shelter_issue_cat_7_to_8 = "7_to_8",
#                         shelter_issue_cat_4_to_6 = "4_to_6",
#                         shelter_issue_cat_1_to_3 = "1_to_3",
#                         shelter_issue_cat_none = "none",
#                         shelter_issue_cat_undefined = "undefined",
#                         shelter_issue_cat_other = "other",
#                         occupancy_cat = "hlp_occupancy_cat",
#                         occupancy_cat_high_risk = "high_risk",
#                         occupancy_cat_medium_risk = "medium_risk",
#                         occupancy_cat_low_risk = "low_risk",
#                         occupancy_cat_undefined = "undefined",
#                         fds_cannot_cat = "snfi_fds_cannot_cat",
#                         fds_cannot_cat_4_to_5 = "4_to_5_tasks",
#                         fds_cannot_cat_2_to_3 = "2_to_3_tasks",
#                         fds_cannot_cat_1 = "1_task",
#                         fds_cannot_cat_none = "none",
#                         fds_cannot_cat_undefined = "undefined")


#########################################################  Protection  ###################################################
##Reconstitution de la varibale "prot_child_sep" à partir de ces deux variables désagréés :"prot_child_sep_boys"et "prot_child_sep_girls"
#reprise de calcul de la somme des enfants séparés
raw_data_clean <- raw_data_clean %>% 
  dplyr::mutate(prot_child_sep_n= rowSums(select(., prot_child_sep_boys_n, prot_child_sep_girls_n), na.rm = TRUE))

#Vérification que "dnk" et "pnta" ou l'inverse ne sont pas sélectionnés simultanément dans la meme enquête pour la variable "prot_child_sep_boys" et "prot_child_sep_girls"
raw_data_clean <- raw_data_clean |> 
  dplyr::mutate(verif_dnk_pnta=case_when(prot_child_sep_boys=="dnk" & prot_child_sep_girls=="dnk"~1,
                                         prot_child_sep_boys=="dnk" & prot_child_sep_girls=="pnta"~1,
                                         prot_child_sep_boys=="pnta" & prot_child_sep_girls=="pnta"~1,
                                         prot_child_sep_boys=="pnta" & prot_child_sep_girls=="dnk"~1,
                                         TRUE ~ 0))
                                                                     
sum(raw_data_clean$verif_dnk_pnta)

#création de la variable "prot_child_sep" à partir de la somme des enfants séparés
raw_data_clean <- raw_data_clean |> 
  dplyr::mutate(prot_child_sep=case_when(prot_child_sep_n>0 ~"yes",
                                         prot_child_sep_n==0~"no",
                                         TRUE ~ NA_character_))
                                                                     



## Catégorie des raisons pour lesquelles les enfants sont séparés de leur famille ok
#raison de separation des enfants non prise en compte dans la fonction "parent"
raw_data_clean<-raw_data_clean |> 
  humind::add_child_sep_cat(child_sep = "prot_child_sep",
                    child_sep_yes = "yes",
                    child_sep_no = "no",
                    child_sep_undefined = c("pnta", "dnk"),
                    child_sep_reason = "prot_child_sep_reason",
                    child_sep_reason_non_severe = c("left_study"),
                    child_sep_reason_severe = c("left_employment", "left_married"),
                    child_sep_reason_very_severe = c("left_armed_groups", "kidnapped", "separated_displacement","stayed_in_origin", "missing", "detained"),
                    child_sep_reason_undefined = c("other", "dnk", "pnta"),
                    sep = "_")
                                  




## Indicateur composite de protection ok 
raw_data_clean<-raw_data_clean |> 
  humind::add_comp_prot(child_sep_cat = "prot_child_sep_cat",
                        child_sep_cat_none = "none",
                        child_sep_cat_very_severe = "at_least_one_very_severe",
                        child_sep_cat_severe = "at_least_one_severe",
                        child_sep_cat_non_severe = "at_least_one_non_severe",
                        child_sep_cat_undefined = "undefined",
                        concern_freq_cope = "prot_concern_freq_cope",
                        concern_freq_displaced = "prot_concern_freq_displaced",
                        concern_hh_freq_kidnapping = "prot_concern_hh_freq_kidnapping",
                        concern_hh_freq_discrimination = "prot_concern_hh_freq_discrimination",
                        concern_always = "toujours",
                        concern_several_times = "plusieurs_fois",
                        concern_once_or_twice = "une_deux_fois",
                        concern_never = "jamais",
                        concern_dnk = "dnk",
                        concern_pnta = "pnta")
                                      


#aap_received_assistance ok
raw_data_clean<-raw_data_clean |> 
  humind::add_received_assistance(received_assistance_12m = "aap_received_assistance_12m",
                                  yes = "yes",
                                  no = "no",
                                  undefined = c("dnk", "pnta"),
                                  received_assistance_date = "aap_received_assistance_date",
                                  date_past_30d = "past_30d",
                                  date_1_3_months = "1_3_months",
                                  date_4_6_months = "4_6_months",
                                  date_7_12_months = "7_12_months",
                                  date_undefined = c("dnk", "pnta")
                                  )

#add_income_source_prop ok

raw_data_clean<-raw_data_clean |> 
  humind::add_income_source_prop(income_souce_salaried_n = "cm_income_source_salaried_n",
                                 income_source_casual_n = "cm_income_source_casual_n",
                                 income_source_own_business_n = "cm_income_source_own_business_n",
                                 income_source_own_production_n = "cm_income_source_own_production_n",
                                 income_source_social_benefits_n = "cm_income_source_social_benefits_n",
                                 income_source_rent_n = "cm_income_source_rent_n",
                                 income_source_remittances_n = "cm_income_source_remittances_n",
                                 income_source_assistance_n = "cm_income_source_assistance_n",
                                 income_source_support_friends_n = "cm_income_source_support_friends_n",
                                 income_source_donation_n = "cm_income_source_donation_n",
                                 income_source_other_n = "cm_income_source_other_n")
                                               


# cm_income_source_top1,cm_income_source_top2, cm_income_source_top3 ok
raw_data_clean <- raw_data_clean |> 
  humind::add_income_source_rank(emergency = c("cm_income_source_assistance_n", "cm_income_source_support_friends_n", "cm_income_source_donation_n"),
                                 unstable = c ("cm_income_source_casual_n", "cm_income_source_social_benefits_n", "cm_income_source_rent_n", "cm_income_source_remittances_n"),
                                 stable = c("cm_income_source_salaried_n", "cm_income_source_own_business_n", "cm_income_source_own_production_n"),
                                 other = "cm_income_source_other_n",
                                 id_col = "uuid")
  


raw_data_clean <- raw_data_clean |> 
  humind::add_age_cat(age_col="resp_age",
                      breaks = c(0, 18, 60, 120),
                      labels = NULL,
                      int_undefined = c(-999, 999),
                      char_undefined = "undefined",
                      new_colname = NULL)
  

raw_data_clean <- raw_data_clean |> 
  humind::add_age_cat(age_col="c_age_hoh",
                      breaks = c(0, 18, 60, 120),
                      labels = NULL,
                      int_undefined = c(-999, 999),
                      char_undefined = "undefined",
                      new_colname = NULL)


raw_data_clean <- raw_data_clean |> 
  humind::add_age_18_cat(age_col="c_age_hoh",
                         int_undefined = c(-999, 999),
                         char_undefined = "undefined",
                         new_colname = NULL)

# ind_age_cat,   ind_age_18_cat,  ind_age_18_cat_d

loop_data_clean <- loop_data_clean |> 
  humind::add_age_cat(age_col = "ind_age",
                      breaks = c(seq(0, 115, by = 5), 120),
                      labels = NULL,
                      int_undefined = c(-999, 999),
                      char_undefined = "undefined",
                      new_colname = NULL)

loop_data_clean <- loop_data_clean |> 
  humind::add_age_18_cat(age_col = "ind_age",
                         int_undefined = c(-999, 999),
                         char_undefined = "undefined",
                         new_colname = NULL)
  



raw_data_clean <- raw_data_clean |> 
  dplyr::mutate(aap_priority_challenge.lack_access=aap_priority_challenge_lack_access,
                aap_priority_challenge.food_access=aap_priority_challenge_food_access,
                aap_priority_challenge.shelter_housing_lack=aap_priority_challenge_shelter_housing_lack,
                aap_priority_challenge.wash_lack=aap_priority_challenge_wash_lack,
                aap_priority_challenge.hygiene_snfi=aap_priority_challenge_hygiene_snfi,
                aap_priority_challenge.non_hygiene_nfis_lack=aap_priority_challenge_non_hygiene_nfis_lack,
                aap_priority_challenge.livelihoods_support_employment_lack=aap_priority_challenge_livelihoods_support_employment_lack,
                aap_priority_challenge.healthcare_lack=aap_priority_challenge_healthcare_lack,
                aap_priority_challenge.psychosocial_felling=aap_priority_challenge_psychosocial_felling,
                aap_priority_challenge.protection_safety_lack=aap_priority_challenge_protection_safety_lack,
                aap_priority_challenge.no_education_children=aap_priority_challenge_no_education_children,
                aap_priority_challenge.legal_documentation=aap_priority_challenge_legal_documentation,
                aap_priority_challenge.lack_mechanism_info=aap_priority_challenge_lack_mechanism_info,
                aap_priority_challenge.telecomunication=aap_priority_challenge_telecomunication,
                aap_priority_challenge.law_justice=aap_priority_challenge_law_justice,
                aap_priority_challenge.women_not_safe=aap_priority_challenge_women_not_safe,
                aap_priority_challenge.repay_debt=aap_priority_challenge_repay_debt,
                aap_priority_challenge.ins_access_to_energy=aap_priority_challenge_ins_access_to_energy,
                aap_priority_challenge.none=aap_priority_challenge_none,
                aap_priority_challenge.other=aap_priority_challenge_other,
                aap_priority_challenge.dnk=aap_priority_challenge_dnk,
                aap_priority_challenge.pnta=aap_priority_challenge_pnta,
                aap_preferred_modality.do_not_want_assistance=aap_preferred_modality_do_not_want_assistance,
                aap_preferred_modality.in_kind_food=aap_preferred_modality_in_kind_food,
                aap_preferred_modality.in_kind_nfis=aap_preferred_modality_in_kind_nfis,
                aap_preferred_modality.physical_cash=aap_preferred_modality_physical_cash,
                aap_preferred_modality.cash_bank_transfer=aap_preferred_modality_cash_bank_transfer,
                aap_preferred_modality.cash_via_prepaid_cards=aap_preferred_modality_cash_via_prepaid_cards,
                aap_preferred_modality.vouchers=aap_preferred_modality_vouchers,
                aap_preferred_modality.services=aap_preferred_modality_services,
                aap_preferred_modality.construction_infrastructures=aap_preferred_modality_construction_infrastructures,
                aap_preferred_modality.shelter_housing_lack=aap_preferred_modality_shelter_housing_lack,
                aap_preferred_modality.other=aap_preferred_modality_other,
                aap_preferred_modality.dnk=aap_preferred_modality_dnk,
                aap_preferred_modality.pnta=aap_preferred_modality_pnta)
                

##Exportation des résultats


###########################################           MSNI               ###########################
## calcul de MSNI
raw_data_clean<-raw_data_clean |> 
  humind::add_msni(comp_foodsec_score = "comp_foodsec_score",
                   comp_snfi_score = "comp_snfi_score",
                   comp_wash_score = "comp_wash_score",
                   comp_prot_score = "comp_prot_score",
                   comp_health_score = "comp_health_score",
                   comp_edu_score = "comp_edu_score",
                   comp_foodsec_in_need = "comp_foodsec_in_need",
                   comp_snfi_in_need = "comp_snfi_in_need",
                   comp_wash_in_need = "comp_wash_in_need",
                   comp_prot_in_need = "comp_prot_in_need",
                   comp_health_in_need = "comp_health_in_need",
                   comp_edu_in_need = "comp_edu_in_need")

table(raw_data_clean$comp_foodsec_score,useNA = 'ifany')
table(raw_data_clean$comp_wash_score,useNA = 'ifany')

raw_data_clean_to_listing <- raw_data_clean |> 
  dplyr::select(uuid,c_age_hoh_cat,c_gender_hoh,setting)

loop_data_clean<-loop_data_clean |> 
  dplyr::left_join(raw_data_clean_to_listing,by="uuid")



#  Pour la note méthodologique
# proportions_age_chef <- prop.table(table(raw_data_clean$c_age_hoh, useNA = "always")) * 100
# proportions_age_chef
# 
# proportions_d_statut <- prop.table(table(raw_data_clean$d_statut_deplacement, useNA = "always")) * 100
# proportions_d_statut
# 
# proportions_sante <- prop.table(table(raw_data_clean$comp_health_score, useNA = "always")) * 100
# proportions_sante
# 
# 
# proportions_msni_score <- prop.table(table(raw_data_clean$msni_score, useNA = "always")) * 100
# proportions_msni_score



survey <- impactR::import_xlsx("input/kobo_tool/NER_MSNA_2024_tool_final.xlsx",sheet="survey") %>% 
  janitor::remove_empty(which = "rows")

survey_1<-survey |> 
  dplyr::rename(label=label_francais_fr)

survey <- survey |>  
  impactR::split_survey(
    col_to_split = "type",
    into = c("type", "list_name"),
    sep = " ",
    fill = "right") %>% 
  dplyr::rename(label=label_francais_fr)




# 2.2. Chargement de la feuille choices du questionnaire

choices <- impactR::import_xlsx("input/kobo_tool/NER_MSNA_2024_tool_final.xlsx",sheet="choices") %>% 
           tibble::as_tibble() %>% 
           dplyr::rename(label=label_francais_fr)


#################################### Analysis Main Data with manual List of Analysis ####################################

# design for analysis
my_design <- srvyr::as_survey_design(raw_data_clean,strata = stratum, weights = weights)
my_loop_design <- srvyr::as_survey_design(loop_data_clean,strata = stratum, weights = weights)


####################### DAP ###########################

manual_loa_sd <- readxl::read_excel("input/dap_analyse/loa.xlsx", sheet = "loa")
manual_loop_loa_sd <- read_excel("input/dap_analyse/loa.xlsx", sheet = "loa_demo")
info_loa_to_merge <- read_excel("input/dap_analyse/loa.xlsx", sheet = "info_loa_to_merge")

####################### sans desag ###########################

my_results_SD <- create_analysis(my_design, loa = manual_loa_sd, sm_separator = ".")
analysis_admin0_res<-my_results_SD[["results_table"]]


analysis_admin0_loop <- create_analysis(my_loop_design, loa = manual_loop_loa_sd, sm_separator = ".")
analysis_admin0_loop_res<-analysis_admin0_loop[["results_table"]]


analyse_msni <- dplyr::bind_rows(analysis_admin0_res,analysis_admin0_loop_res)|> 
  dplyr::filter(stat!="NaN")


# analyse_finale_x_group <- analyse_msni %>%
#   create_table_variable_x_group(analysis_key = "analysis_key")


# analyse_msni<-analyse_msni |> 
#   dplyr::mutate(analysis_var_rec=dplyr::if_else(analysis_type!="ratio",analysis_var,analysis_key))

analyse_msni_indicator<-analyse_msni |> 
  dplyr::left_join(info_loa_to_merge,by=c("analysis_var"))




presentresults_MSNA2024_dictionary <- presentresults::create_label_dictionary(survey_1,choices,label_column = 'label')


label_results_msni_indicator <- add_label_columns_to_results_table(
  results_table = analyse_msni_indicator,
  dictionary = presentresults_MSNA2024_dictionary
)

label_results_msni_indicator <- label_results_msni_indicator |> 
  dplyr::mutate(group_var=dplyr::if_else(is.na(group_var),"admin0",group_var))


analyse<-label_results_msni_indicator |> 
  dplyr::mutate(group_name=dplyr::case_when(group_var=="admin0"~"Population générale",
                                            group_var=="d_statut_deplacement"~"Population par statut de déplacement",
                                            group_var=="admin1"~"Population générale par région",
                                            group_var=="admin1_d_statut_deplacement"~"Population par statut de déplacement et par région",
                                            group_var=="admin2"~"Population générale par département",
                                            group_var=="admin2_d_statut_deplacement"~"Population par statut de déplacement et par département",
                                            group_var=="c_age_hoh_cat"~"Age du chef de ménage",
                                            group_var=="c_gender_hoh"~"Sexe du chef de ménage"),
                group=group_var)



analysis_admin0 <- analyse|> filter(group_var=="admin0")
analysis_admin1 <- analyse|> filter(group_var=="admin1")
analysis_admin2 <- analyse|> filter(group_var=="admin2")

analysis <- analyse|> mutate(region = case_when(group == "admin1" ~ label_group_var_value),
                             departement = case_when(group == "admin2" ~ label_group_var_value),
                             statut_deplacement = case_when(group == "d_statut_deplacement" ~ label_group_var_value),
                             statut_region = case_when(group == "admin1_d_statut_deplacement" ~ group_var_value),
                             statut_departement = case_when(group == "admin2_d_statut_deplacement" ~ group_var_value),
                             pas_desagregation = case_when(is.na(group) ~ label_group_var_value))


analysis <- analysis|> separate(statut_region, c("region_s", "statut_deplacement_r"),extra = "warn")|> 
  separate(statut_departement, c("departement_s", "statut_deplacement_d"),extra = "warn")

analysis <- analysis|> mutate(statut_deplacement_d = recode(statut_deplacement_d,
                                                            "non" = "Population non déplacée",
                                                            "pdi" = "Population déplacée interne",
                                                            "refugie" = "Population réfugiée",
                                                            "retourne" = "Population retournée"),
                              statut_deplacement_r = recode(statut_deplacement_r,
                                                            "non" = "Population non déplacée",
                                                            "pdi" = "Population déplacée interne",
                                                            "refugie" = "Population réfugiée",
                                                            "retourne" = "Population retournée"))

#analysis <- analysis|> filter(!is.na(stat))



#ajouter population generale a chaque desagregation pour les tableaux de pivote


# 1
analysis_admin0_statut_deplacement <- analysis_admin0|> mutate(statut_deplacement = group_name)



#ajouter population generale desagregee par region et departement pour les tableaux pivotes


# 2

analysis_admin1_statut_region <- analysis_admin1|> mutate(statut_deplacement_r = group_name,
                                                          region_s = label_group_var_value)





# 3

analysis_admin2_statut_departement <- analysis_admin2|> mutate(statut_deplacement_d = group_name,
                                                               departement_s = label_group_var_value)




analyse_final_combine <- dplyr::bind_rows(analysis,
                                          analysis_admin0_statut_deplacement,
                                          analysis_admin1_statut_region,
                                          analysis_admin2_statut_departement)

analyse_final_combine <- analyse_final_combine|> filter(!is.na(stat))




analyse_final_combine <- analyse_final_combine|> mutate(region_s = recode(region_s,
                                                                          "NER001"="Agadez",
                                                                          "NER002" = "Diffa",
                                                                          "NER003" = "Dosso",
                                                                          "NER004" = "Maradi",
                                                                          "NER005" = "Tahoua",
                                                                          "NER006" = "Tillabéri",
                                                                          "NER007" = "Zinder",
                                                                          "NER008" = "Niamey"),
                                                        departement_s = recode(departement_s,
                                                                               "NER006001"="Abala",
                                                                               "NER005001"="Abalak",
                                                                               "NER001001"="Aderbissinat",
                                                                               "NER004001"="Aguié",
                                                                               "NER001002"="Arlit",
                                                                               "NER006002"="Ayerou",
                                                                               "NER005002"="Bagaroua",
                                                                               "NER006003"="Balleyara",
                                                                               "NER006004"="Banibangou",
                                                                               "NER006005"="Bankilaré",
                                                                               "NER007001"="Belbedji",
                                                                               "NER004002"="Bermo",
                                                                               "NER001003"="Bilma",
                                                                               "NER005003"="Birni N'Konni",
                                                                               "NER003001"="Boboye",
                                                                               "NER002001"="Bosso",
                                                                               "NER005004"="Bouza",
                                                                               "NER004003"="Dakoro",
                                                                               "NER007002"="Damagaram Takaya",
                                                                               "NER002002"="Diffa",
                                                                               "NER003002"="Dioundiou",
                                                                               "NER003003"="Dogondoutchi",
                                                                               "NER003004"="Dosso",
                                                                               "NER007003"="Dungass",
                                                                               "NER003005"="Falmey",
                                                                               "NER006006"="Filingué",
                                                                               "NER003006"="Gaya",
                                                                               "NER004004"="Gazaoua",
                                                                               "NER006007"="Gothèye",
                                                                               "NER002003"="Goudoumaria",
                                                                               "NER007004"="Gouré",
                                                                               "NER004005"="Guidan Roumdji",
                                                                               "NER001004"="Iferouane",
                                                                               "NER005005"="Illéla",
                                                                               "NER001005"="Ingall",
                                                                               "NER007005"="Kantché",
                                                                               "NER005006"="Keita",
                                                                               "NER006008"="Kollo",
                                                                               "NER003007"="Loga",
                                                                               "NER005007"="Madaoua",
                                                                               "NER004006"="Madarounfa",
                                                                               "NER007006"="Magaria",
                                                                               "NER002004"="Maïné Soroa",
                                                                               "NER005008"="Malbaza",
                                                                               "NER004008"="Mayahi",
                                                                               "NER007007"="Mirriah",
                                                                               "NER002005"="N'Gourti",
                                                                               "NER002006"="N'Guigmi",
                                                                               "NER006009"="Ouallam",
                                                                               "NER006010"="Say",
                                                                               "NER005009"="Tahoua",
                                                                               "NER007008"="Takeita",
                                                                               "NER007009"="Tanout",
                                                                               "NER005010"="Tassara",
                                                                               "NER005011"="Tchintabaraden",
                                                                               "NER001006"="Tchirozerine",
                                                                               "NER006011"="Téra",
                                                                               "NER007010"="Tesker",
                                                                               "NER004009"="Tessaoua",
                                                                               "NER003008"="Tibiri",
                                                                               "NER006012"="Tillabéri",
                                                                               "NER005012"="Tillia",
                                                                               "NER006013"="Torodi",
                                                                               "NER004007"="Ville de Maradi",
                                                                               "NER008001"="Ville de Niamey",
                                                                               "NER005013"="Ville de Tahoua",
                                                                               "NER007011"="Ville de Zinder"
                                                        ))

analyse_final_combine$region_pour_departement <- NA
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Abala"]<-"Tillabéri"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Abalak"]<-"Tahoua"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Aderbissinat"]<-"Agadez"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Aguié"]<-"Maradi"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Arlit"]<-"Agadez"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Ayerou"]<-"Tillabéri"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Bagaroua"]<-"Tahoua"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Balleyara"]<-"Tillabéri"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Banibangou"]<-"Tillabéri"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Bankilaré"]<-"Tillabéri"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Belbedji"]<-"Zinder"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Bermo"]<-"Maradi"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Bilma"]<-"Agadez"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Birni N'Konni"]<-"Tahoua"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Boboye"]<-"Dosso"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Bosso"]<-"Diffa"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Bouza"]<-"Tahoua"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Dakoro"]<-"Maradi"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Damagaram Takaya"]<-"Zinder"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Diffa"]<-"Diffa"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Dioundiou"]<-"Dosso"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Dogondoutchi"]<-"Dosso"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Dosso"]<-"Dosso"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Dungass"]<-"Zinder"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Falmey"]<-"Dosso"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Filingué"]<-"Tillabéri"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Gaya"]<-"Dosso"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Gazaoua"]<-"Maradi"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Gothèye"]<-"Tillabéri"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Goudoumaria"]<-"Diffa"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Gouré"]<-"Zinder"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Guidan Roumdji"]<-"Maradi"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Iferouane"]<-"Agadez"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Illéla"]<-"Tahoua"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Ingall"]<-"Agadez"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Kantché"]<-"Zinder"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Keita"]<-"Tahoua"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Kollo"]<-"Tillabéri"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Loga"]<-"Dosso"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Madaoua"]<-"Tahoua"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Madarounfa"]<-"Maradi"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Magaria"]<-"Zinder"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Maïné Soroa"]<-"Diffa"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Malbaza"]<-"Tahoua"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Mayahi"]<-"Maradi"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Mirriah"]<-"Zinder"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="N'Gourti"]<-"Diffa"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="N'Guigmi"]<-"Diffa"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Ouallam"]<-"Tillabéri"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Say"]<-"Tillabéri"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Tahoua"]<-"Tahoua"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Takeita"]<-"Zinder"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Tanout"]<-"Zinder"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Tassara"]<-"Tahoua"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Tchintabaraden"]<-"Tahoua"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Tchirozerine"]<-"Agadez"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Téra"]<-"Tillabéri"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Tesker"]<-"Zinder"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Tessaoua"]<-"Maradi"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Tibiri"]<-"Dosso"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Tillabéri"]<-"Tillabéri"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Tillia"]<-"Tahoua"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Torodi"]<-"Tillabéri"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Ville de Maradi"]<-"Maradi"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Ville de Niamey"]<-"Niamey"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Ville de Tahoua"]<-"Tahoua"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Ville de Zinder"]<-"Zinder"



writexl::write_xlsx(analyse_final_combine,paste0("output/MSNI/ner_msna_msni_",lubridate::today(),".xlsx"))




####################### DAP Bulletin MSNI###########################

manual_loa_sd <- read_excel("input/dap_analyse/msni_bulletin_loa.xlsx", sheet = "basic")
manual_loop_loa_sd <- read_excel("input/dap_analyse/msni_bulletin_loa.xlsx", sheet = "topup-indiv")

info_msni_to_analysis <- read_excel("input/dap_analyse/msni_bulletin_loa.xlsx", sheet = "info_msni_to_analysis")


####################### sans desag ###########################

my_results_SD <- create_analysis(my_design, loa = manual_loa_sd, sm_separator = ".")
analysis_admin0_res<-my_results_SD[["results_table"]]


analysis_admin0_loop <- create_analysis(my_loop_design, loa = manual_loop_loa_sd, sm_separator = ".")
analysis_admin0_loop_res<-analysis_admin0_loop[["results_table"]]

# Bind
analyse_msni_bulletin <- dplyr::bind_rows(analysis_admin0_res,analysis_admin0_loop_res)|> 
  dplyr::filter(stat!="NaN")



analyse_msni_bulletin<-analyse_msni_bulletin |> 
  dplyr::left_join(info_msni_to_analysis,by="analysis_var")
# Labellisation des résultats




presentresults_MSNA2024_dictionary <- presentresults::create_label_dictionary(survey_1,choices,label_column = 'label')


label_results_msni_bulletin <- add_label_columns_to_results_table(
  results_table = analyse_msni_bulletin,
  dictionary = presentresults_MSNA2024_dictionary
)

label_results_msni_bulletin <- label_results_msni_bulletin |> 
  dplyr::mutate(group_var=dplyr::if_else(is.na(group_var),"admin0",group_var))


analyse<-label_results_msni_bulletin |> 
  dplyr::mutate(group_name=dplyr::case_when(group_var=="admin0"~"Population générale",
                                            group_var=="d_statut_deplacement"~"Population par statut de déplacement",
                                            group_var=="admin1"~"Population générale par région",
                                            group_var=="admin1_d_statut_deplacement"~"Population par statut de déplacement et par région",
                                            group_var=="admin2"~"Population générale par département",
                                            group_var=="admin2_d_statut_deplacement"~"Population par statut de déplacement et par département",
                                            group_var=="c_age_hoh_cat"~"Age du chef de ménage",
                                            group_var=="c_gender_hoh"~"Sexe du chef de ménage"),
                group=group_var)



analysis_admin0 <- analyse|> filter(group_var=="admin0")
analysis_admin1 <- analyse|> filter(group_var=="admin1")
analysis_admin2 <- analyse|> filter(group_var=="admin2")

analysis <- analyse|> mutate(region = case_when(group == "admin1" ~ label_group_var_value),
                             departement = case_when(group == "admin2" ~ label_group_var_value),
                             statut_deplacement = case_when(group == "d_statut_deplacement" ~ label_group_var_value),
                             statut_region = case_when(group == "admin1_d_statut_deplacement" ~ group_var_value),
                             statut_departement = case_when(group == "admin2_d_statut_deplacement" ~ group_var_value),
                             pas_desagregation = case_when(is.na(group) ~ label_group_var_value))


analysis <- analysis|> separate(statut_region, c("region_s", "statut_deplacement_r"),extra = "warn")|> 
  separate(statut_departement, c("departement_s", "statut_deplacement_d"),extra = "warn")

analysis <- analysis|> mutate(statut_deplacement_d = recode(statut_deplacement_d,
                                                            "non" = "Population non déplacée",
                                                            "pdi" = "Population déplacée interne",
                                                            "refugie" = "Population réfugiée",
                                                            "retourne" = "Population retournée"),
                              statut_deplacement_r = recode(statut_deplacement_r,
                                                            "non" = "Population non déplacée",
                                                            "pdi" = "Population déplacée interne",
                                                            "refugie" = "Population réfugiée",
                                                            "retourne" = "Population retournée"))

#analysis <- analysis|> filter(!is.na(stat))



#ajouter population generale a chaque desagregation pour les tableaux de pivote


# 1
analysis_admin0_statut_deplacement <- analysis_admin0|> mutate(statut_deplacement = group_name)



#ajouter population generale desagregee par region et departement pour les tableaux pivotes


# 2

analysis_admin1_statut_region <- analysis_admin1|> mutate(statut_deplacement_r = group_name,
                                                          region_s = label_group_var_value)





# 3

analysis_admin2_statut_departement <- analysis_admin2|> mutate(statut_deplacement_d = group_name,
                                                               departement_s = label_group_var_value)




analyse_final_combine <- dplyr::bind_rows(analysis,
                                          analysis_admin0_statut_deplacement,
                                          analysis_admin1_statut_region,
                                          analysis_admin2_statut_departement)

analyse_final_combine <- analyse_final_combine|> filter(!is.na(stat))




analyse_final_combine <- analyse_final_combine|> mutate(region_s = recode(region_s,
                                                                          "NER001"="Agadez",
                                                                          "NER002" = "Diffa",
                                                                          "NER003" = "Dosso",
                                                                          "NER004" = "Maradi",
                                                                          "NER005" = "Tahoua",
                                                                          "NER006" = "Tillabéri",
                                                                          "NER007" = "Zinder",
                                                                          "NER008" = "Niamey"),
                                                        departement_s = recode(departement_s,
                                                                               "NER006001"="Abala",
                                                                               "NER005001"="Abalak",
                                                                               "NER001001"="Aderbissinat",
                                                                               "NER004001"="Aguié",
                                                                               "NER001002"="Arlit",
                                                                               "NER006002"="Ayerou",
                                                                               "NER005002"="Bagaroua",
                                                                               "NER006003"="Balleyara",
                                                                               "NER006004"="Banibangou",
                                                                               "NER006005"="Bankilaré",
                                                                               "NER007001"="Belbedji",
                                                                               "NER004002"="Bermo",
                                                                               "NER001003"="Bilma",
                                                                               "NER005003"="Birni N'Konni",
                                                                               "NER003001"="Boboye",
                                                                               "NER002001"="Bosso",
                                                                               "NER005004"="Bouza",
                                                                               "NER004003"="Dakoro",
                                                                               "NER007002"="Damagaram Takaya",
                                                                               "NER002002"="Diffa",
                                                                               "NER003002"="Dioundiou",
                                                                               "NER003003"="Dogondoutchi",
                                                                               "NER003004"="Dosso",
                                                                               "NER007003"="Dungass",
                                                                               "NER003005"="Falmey",
                                                                               "NER006006"="Filingué",
                                                                               "NER003006"="Gaya",
                                                                               "NER004004"="Gazaoua",
                                                                               "NER006007"="Gothèye",
                                                                               "NER002003"="Goudoumaria",
                                                                               "NER007004"="Gouré",
                                                                               "NER004005"="Guidan Roumdji",
                                                                               "NER001004"="Iferouane",
                                                                               "NER005005"="Illéla",
                                                                               "NER001005"="Ingall",
                                                                               "NER007005"="Kantché",
                                                                               "NER005006"="Keita",
                                                                               "NER006008"="Kollo",
                                                                               "NER003007"="Loga",
                                                                               "NER005007"="Madaoua",
                                                                               "NER004006"="Madarounfa",
                                                                               "NER007006"="Magaria",
                                                                               "NER002004"="Maïné Soroa",
                                                                               "NER005008"="Malbaza",
                                                                               "NER004008"="Mayahi",
                                                                               "NER007007"="Mirriah",
                                                                               "NER002005"="N'Gourti",
                                                                               "NER002006"="N'Guigmi",
                                                                               "NER006009"="Ouallam",
                                                                               "NER006010"="Say",
                                                                               "NER005009"="Tahoua",
                                                                               "NER007008"="Takeita",
                                                                               "NER007009"="Tanout",
                                                                               "NER005010"="Tassara",
                                                                               "NER005011"="Tchintabaraden",
                                                                               "NER001006"="Tchirozerine",
                                                                               "NER006011"="Téra",
                                                                               "NER007010"="Tesker",
                                                                               "NER004009"="Tessaoua",
                                                                               "NER003008"="Tibiri",
                                                                               "NER006012"="Tillabéri",
                                                                               "NER005012"="Tillia",
                                                                               "NER006013"="Torodi",
                                                                               "NER004007"="Ville de Maradi",
                                                                               "NER008001"="Ville de Niamey",
                                                                               "NER005013"="Ville de Tahoua",
                                                                               "NER007011"="Ville de Zinder"
                                                        ))

analyse_final_combine$region_pour_departement <- NA
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Abala"]<-"Tillabéri"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Abalak"]<-"Tahoua"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Aderbissinat"]<-"Agadez"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Aguié"]<-"Maradi"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Arlit"]<-"Agadez"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Ayerou"]<-"Tillabéri"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Bagaroua"]<-"Tahoua"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Balleyara"]<-"Tillabéri"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Banibangou"]<-"Tillabéri"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Bankilaré"]<-"Tillabéri"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Belbedji"]<-"Zinder"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Bermo"]<-"Maradi"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Bilma"]<-"Agadez"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Birni N'Konni"]<-"Tahoua"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Boboye"]<-"Dosso"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Bosso"]<-"Diffa"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Bouza"]<-"Tahoua"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Dakoro"]<-"Maradi"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Damagaram Takaya"]<-"Zinder"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Diffa"]<-"Diffa"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Dioundiou"]<-"Dosso"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Dogondoutchi"]<-"Dosso"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Dosso"]<-"Dosso"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Dungass"]<-"Zinder"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Falmey"]<-"Dosso"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Filingué"]<-"Tillabéri"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Gaya"]<-"Dosso"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Gazaoua"]<-"Maradi"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Gothèye"]<-"Tillabéri"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Goudoumaria"]<-"Diffa"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Gouré"]<-"Zinder"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Guidan Roumdji"]<-"Maradi"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Iferouane"]<-"Agadez"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Illéla"]<-"Tahoua"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Ingall"]<-"Agadez"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Kantché"]<-"Zinder"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Keita"]<-"Tahoua"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Kollo"]<-"Tillabéri"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Loga"]<-"Dosso"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Madaoua"]<-"Tahoua"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Madarounfa"]<-"Maradi"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Magaria"]<-"Zinder"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Maïné Soroa"]<-"Diffa"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Malbaza"]<-"Tahoua"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Mayahi"]<-"Maradi"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Mirriah"]<-"Zinder"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="N'Gourti"]<-"Diffa"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="N'Guigmi"]<-"Diffa"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Ouallam"]<-"Tillabéri"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Say"]<-"Tillabéri"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Tahoua"]<-"Tahoua"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Takeita"]<-"Zinder"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Tanout"]<-"Zinder"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Tassara"]<-"Tahoua"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Tchintabaraden"]<-"Tahoua"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Tchirozerine"]<-"Agadez"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Téra"]<-"Tillabéri"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Tesker"]<-"Zinder"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Tessaoua"]<-"Maradi"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Tibiri"]<-"Dosso"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Tillabéri"]<-"Tillabéri"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Tillia"]<-"Tahoua"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Torodi"]<-"Tillabéri"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Ville de Maradi"]<-"Maradi"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Ville de Niamey"]<-"Niamey"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Ville de Tahoua"]<-"Tahoua"
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Ville de Zinder"]<-"Zinder"

# Exportation
writexl::write_xlsx(analyse_final_combine,paste0("output/MSNI/ner_msna_msni_bulletin_",lubridate::today(),".xlsx"))
