# Préparation de l'environnement de travail -------

## Remove ls()
rm(list = ls())

## Scientific Penality
getOption(x = "scipen")
options(scipen = "999")

## Timezone
Sys.setenv(tz = "africa/ouagadougou" )
Sys.timezone()


## Gestion des packages
pacman::p_load(
        "devtools",
        "humind",
        "openxlsx",
        "tidyverse",
        "impactR4PHU",
        "impactR.utils",        
        "analysistools"
        #"presentresults"
)



###### Chargement des bases de données
openxlsx::getSheetNames("Input/msna_bfa_2025_sans_gps_2025-12-23.xlsx")

# Chargement de la base de données ---------------------------------------
raw_data_clean <- openxlsx::read.xlsx(xlsxFile = "Input/msna_bfa_2025_sans_gps_2025-12-23.xlsx", sheet = "main") %>% 
                  rename(
                        # _______________________________________ household hunger scale _______________________________________
                        	
                        # The name of the column "In the past 30 days, was there ever no food to eat of any kind in your house because of lack of resources to get food?". 
                        fsl_hhs_nofoodhh = aucun_aliment,
                        fsl_hhs_nofoodhh_freq	= freq_aucun_aliment,
                        
                        # The name of the column "In the past 30 days, did you or any household member go to sleep at night hungry because there was not enough food?".
                        fsl_hhs_sleephungry = dormir_affame ,
                        fsl_hhs_sleephungry_freq = freq_dormir_affame ,
                        
                        # The name of the column "In the past 30 days, did you or any household member go a whole day and night without eating anything at all because there was not enough food?"
                        fsl_hhs_alldaynight = pas_assez_nourriture ,
                        fsl_hhs_alldaynight_freq = freq_pas_assez_nourriture,
                        
                        
                        
                        # _______________________________________ household CSI Score(rcsi) _______________________________________
                        
                        # During the last 7 days, were there days when your household had to rely on less preferred and less expensive food to cope with a lack of food or money to buy it?
                        fsl_rcsi_lessquality = jr_moins_preferer ,
                        	
                        # During the last 7 days, were there days when your household had to borrow food or rely on help from a relative or friend to cope with a lack of food or money to buy it?
                        fsl_rcsi_borrow = jr_emprunt_nourriture,
                        
                        # During the last 7 days, were there days when your household had to limit portion size of meals at meal times to cope with a lack of food or money to buy it?
                        fsl_rcsi_mealsize = jr_diminu_quantite,
                        
                        # During the last 7 days, were there days when your household had to restrict consumption by adults in order for small children to eat to cope with a lack of food or money to buy it?
                        fsl_rcsi_mealadult = jr_rest_consommation ,
                        
                        # During the last 7 days, were there days when your household had to reduce number of meals eaten in a day to cope with a lack of food or money to buy it?
                        fsl_rcsi_mealnb = jr_nb_repas ,
                        
                        # _______________________________________ add_fcs _______________________________________          
                        
                        
                        # number of days cereals were consumed
                        fsl_fcs_cereal  = nbjr_conso_cereal ,
                        # number of days legumes were consumed
                        fsl_fcs_legumes  = nbjr_conso_noix,
                        # number of days vegetables were consumed
                        fsl_fcs_veg  = nbjr_conso_legume,
                        # number of days fruits were consumed
                        fsl_fcs_fruit  = nbjr_conso_fruit,
                        # number of days meat/fish were consumed
                        fsl_fcs_meat  = nbjr_conso_viande,
                        # number of days dairy were consumed
                        fsl_fcs_dairy = nbjr_conso_lait,
                        # number of days sugar was consumed
                        fsl_fcs_sugar = nbjr_conso_sucre,
                        # number of days oil were consumed
                        fsl_fcs_oil = nbjr_conso_huile,
                        
                        
                        # _______________________________________ add_lcsi _______________________________________
                        
                        
                        # the name of the variable that indicates the first stress LCSI strategy
                        fsl_lcsi_stress1 = "isa_vente_actif_nproductif",
                        # the name of the variable that indicates the second stress LCSI strategy
                        fsl_lcsi_stress2 = "isa_vente_animal",
                        # the name of the variable that indicates the third stress LCSI strategy
                        fsl_lcsi_stress3 = "isa_depense_epargne",
                        # the name of the variable that indicates the fourth stress LCSI strategy
                        fsl_lcsi_stress4 = "isa_emprunt_argent",
                        
                        # the name of the variable that indicates the first crisis LCSI strategy
                        fsl_lcsi_crisis1 = "isa_vente_actif_productif",
                        # the name of the variable that indicates the first crisis LCSI strategy
                        fsl_lcsi_crisis2 = "isa_depense_reduite",
                        # the name of the variable that indicates the first crisis LCSI strategy
                        fsl_lcsi_crisis3 = "isa_consom_semence",
                        
                        # the name of the variable that indicates the first emergency LCSI strategy
                        fsl_lcsi_emergency1 = "isa_vente_animal_femel",
                        # the name of the variable that indicates the first emergency LCSI strategy
                        fsl_lcsi_emergency2 = "isa_mendie",
                        # the name of the variable that indicates the first emergency LCSI strategy
                        fsl_lcsi_emergency3 = "isa_activite_risque",   
                        
                        # _______________________________________ ABNA _______________________________________                       
                        
                        snfi_shelter_type = a_1_situation_hb_menage ,
                        snfi_shelter_type_individual = a_1_type_abri_menage ,
                        snfi_shelter_issue = a_1_probleme_abris , # Select Multiple snfi_shelter_issue
                        snfi_shelter_damage = a_1_abris_dommage,
                        hlp_occupancy =  mod_occupation ,
                        hlp_risk_eviction = risk_expul_menag ,
                        
                        # Nombre de tâches domestiques
                        snfi_fds_cooking = a_1_mbr_menage_cuisine ,
                        snfi_fds_sleeping = a_1_mbr_menage_dormir ,
                        snfi_fds_storing = a_2_stockage_eau_nour ,
                        energy_lighting_source = a_3_source_eclairage ,
                        
                        # _______________________________________ PROTECTION _______________________________________
                        
                        prot_needs_3_movement = probleme_de_securite ,
                        prot_needs_2_social = diff_menaces ,
                        prot_needs_2_activities = diff_acces_ressources ,
                        
                        prot_needs_1_justice = diff_acces_justice ,
                        prot_needs_1_services = diff_sante_educ,
                        
                        # _______________________________________ EHA ______________________________________
                        
                        wash_drinking_water_quantity = h_1_eau_manque ,
                        wash_drinking_water_source = h_1_principal_source_eau,
                        wash_drinking_water_time_int = h_1_eau_temp_collecte_estime,
                        wash_sanitation_facility = h_2_type_latrine ,
                        wash_sanitation_facility_sharing_yn	= h_2_partage_latrine_avec_person_hors_menage ,
                        wash_sanitation_facility_sharing_n =h_2_nb_personnes_utilisant_latrine
                  )


loop_listing <- openxlsx::read.xlsx(xlsxFile = "Input/msna_bfa_2025_sans_gps_2025-12-23.xlsx", sheet = "loop_listing")
loop_sante_nutri_educ <- openxlsx::read.xlsx(xlsxFile = "Input/msna_bfa_2025_sans_gps_2025-12-23.xlsx", sheet = "loop_sante_nutri_educ") %>% 
                         left_join(y = loop_listing, by = c("uuid", "person_id"), keep = FALSE)



loop_data_clean <- loop_sante_nutri_educ %>% 
                   left_join(y = raw_data_clean,by = c("uuid")) %>% 
                   rename(
                        # _______________________________________ SANTE _______________________________________
                        health_ind_healthcare_needed = sante_pb ,
                        health_ind_healthcare_received = sante_obtention_soin ,
                        
                        # _______________________________________ EDUCATION ______________________________________
                        edu_access = e_enfant_scolarise_formel, 
                        edu_barrier =	e_raison_pas_educ_formel                        
                   )


# ---------------------------------- Sécurité alimentaire ---------------------------------------

## Ajout des phases

## Calcul des indicateurs de SECAL avec addindicators

#### ---------------------------------- household hunger scale ------------------------------------

raw_data_clean <- raw_data_clean |>
                  mutate(
                      fsl_hhs_nofoodhh = dplyr::case_when(fsl_hhs_nofoodhh=="oui"~"oui", fsl_hhs_nofoodhh=="non"~"non", TRUE~NA_character_),
                      fsl_hhs_sleephungry = dplyr::case_when(fsl_hhs_sleephungry=="oui"~"oui", fsl_hhs_sleephungry=="non"~"non",  TRUE~NA_character_),
                      fsl_hhs_alldaynight = dplyr::case_when(fsl_hhs_alldaynight=="oui"~"oui", fsl_hhs_alldaynight=="non"~"non", TRUE~NA_character_)
                  ) |>
                  humind::add_hhs(
                          fsl_hhs_nofoodhh = "fsl_hhs_nofoodhh",
                          fsl_hhs_nofoodhh_freq = "fsl_hhs_nofoodhh_freq",
                          fsl_hhs_sleephungry = "fsl_hhs_sleephungry",
                          fsl_hhs_sleephungry_freq = "fsl_hhs_sleephungry_freq",
                          fsl_hhs_alldaynight = "fsl_hhs_alldaynight",
                          fsl_hhs_alldaynight_freq = "fsl_hhs_alldaynight_freq",
                          yes_answer = "oui",
                          no_answer = "non",
                          rarely_answer = "rarement",
                          sometimes_answer = "parfois",
                          often_answer = "souvent"
                  )
tail(names(raw_data_clean), 12)
# View( raw_data_clean %>% select(fsl_hhs_nofoodhh, fsl_hhs_nofoodhh_recoded, fsl_hhs_nofoodhh_freq, fsl_hhs_nofoodhh_freq_recoded, fsl_hhs_sleephungry, fsl_hhs_sleephungry_recoded,
#                           fsl_hhs_sleephungry_freq, fsl_hhs_sleephungry_freq_recoded, fsl_hhs_alldaynight, fsl_hhs_alldaynight_recoded, fsl_hhs_alldaynight_freq,
#                           fsl_hhs_alldaynight_freq_recoded, fsl_hhs_comp1, fsl_hhs_comp2, fsl_hhs_comp3, fsl_hhs_score, fsl_hhs_cat_ipc, fsl_hhs_cat
#                           )
# )

#### ---------------------------------- reduced Household CSI Score(rcsi) ------------------------------------

raw_data_clean <- raw_data_clean |>
                  mutate(
                      fsl_rcsi_lessquality = dplyr::if_else(dplyr::between(fsl_rcsi_lessquality,0,100),fsl_rcsi_lessquality,NA),
                      fsl_rcsi_borrow = dplyr::if_else(dplyr::between(fsl_rcsi_borrow,0,100),fsl_rcsi_borrow,NA),
                      fsl_rcsi_mealsize = dplyr::if_else(dplyr::between(fsl_rcsi_mealsize,0,100),fsl_rcsi_mealsize,NA),
                      fsl_rcsi_mealadult = dplyr::if_else(dplyr::between(fsl_rcsi_mealadult,0,100),fsl_rcsi_mealadult,NA),
                      fsl_rcsi_mealnb = dplyr::if_else(dplyr::between(fsl_rcsi_mealnb,0,100),fsl_rcsi_mealnb,NA)
                  ) |> 
                  humind::add_rcsi(
                      fsl_rcsi_lessquality = "fsl_rcsi_lessquality",
                      fsl_rcsi_borrow = "fsl_rcsi_borrow",
                      fsl_rcsi_mealsize = "fsl_rcsi_mealsize",
                      fsl_rcsi_mealadult = "fsl_rcsi_mealadult",
                      fsl_rcsi_mealnb = "fsl_rcsi_mealnb"
                  )

tail(names(raw_data_clean), 6)
# View( raw_data_clean %>% select(  tail(names(raw_data_clean), 6) ) )



#### ---------------------------------- add_fcs ------------------------------------

raw_data_clean <- raw_data_clean |>
                  add_fcs(
                      cutoffs = "normal", # A verifier
                      fsl_fcs_cereal = "fsl_fcs_cereal",
                      fsl_fcs_legumes = "fsl_fcs_legumes",
                      fsl_fcs_veg = "fsl_fcs_veg",
                      fsl_fcs_fruit = "fsl_fcs_fruit",
                      fsl_fcs_meat = "fsl_fcs_meat",
                      fsl_fcs_dairy = "fsl_fcs_dairy",
                      fsl_fcs_sugar = "fsl_fcs_sugar",
                      fsl_fcs_oil = "fsl_fcs_oil"
                  )

tail(names(raw_data_clean), 10)
# View(raw_data_clean %>% select(tail(names(raw_data_clean), 10)))




#### ---------------------------------- Livelihood Coping Strategy Index (LCSI) -----------------------

raw_data_clean <- raw_data_clean |>
                  mutate(
                      fsl_lcsi_stress1_cal = dplyr::case_when(fsl_lcsi_stress1 %in% c('oui','non_epuise')~1, TRUE~0),
                      fsl_lcsi_stress2_cal = dplyr::case_when(fsl_lcsi_stress2 %in% c('oui','non_epuise')~1,  TRUE~0),
                      fsl_lcsi_stress3_cal = dplyr::case_when(fsl_lcsi_stress3 %in% c('oui','non_epuise')~1,  TRUE~0),
                      fsl_lcsi_stress4_cal = dplyr::case_when(fsl_lcsi_stress4 %in% c('oui','non_epuise')~1,  TRUE~0),
                      fsl_lcsi_crisis1_cal = dplyr::case_when(fsl_lcsi_crisis1 %in% c('oui','non_epuise')~1,  TRUE~0),
                      fsl_lcsi_crisis2_cal = dplyr::case_when(fsl_lcsi_crisis2 %in% c('oui','non_epuise')~1, TRUE~0),
                      fsl_lcsi_crisis3_cal = dplyr::case_when(fsl_lcsi_crisis3 %in% c('oui','non_epuise')~1, TRUE~0),
                      fsl_lcsi_emergency1_cal = dplyr::case_when(fsl_lcsi_emergency1 %in% c('oui','non_epuise')~1, TRUE~0),
                      fsl_lcsi_emergency2_cal = dplyr::case_when(fsl_lcsi_emergency2 %in% c('oui','non_epuise')~1, TRUE~0),
                      fsl_lcsi_emergency3_cal = dplyr::case_when(fsl_lcsi_emergency3 %in% c('oui','non_epuise')~1, TRUE~0),
                  ) |>
                  add_lcsi(
                      fsl_lcsi_stress1 = "fsl_lcsi_stress1",
                      fsl_lcsi_stress2 = "fsl_lcsi_stress2",
                      fsl_lcsi_stress3 = "fsl_lcsi_stress3",
                      fsl_lcsi_stress4 = "fsl_lcsi_stress4",
                      fsl_lcsi_crisis1 = "fsl_lcsi_crisis1",
                      fsl_lcsi_crisis2 = "fsl_lcsi_crisis2",
                      fsl_lcsi_crisis3 = "fsl_lcsi_crisis3",
                      fsl_lcsi_emergency1 = "fsl_lcsi_emergency1",
                      fsl_lcsi_emergency2 = "fsl_lcsi_emergency2",
                      fsl_lcsi_emergency3 = "fsl_lcsi_emergency3",
                      yes_val = "oui", # yes
                      no_val = "non_vendu", # no_had_no_need
                      exhausted_val = "non_epuise", # no_exhausted
                      not_applicable_val = "non_pertinent" # not_applicable
                  )


# View(raw_data_clean %>% 
#     select(fsl_lcsi_stress1, fsl_lcsi_stress1_cal, fsl_lcsi_stress2, fsl_lcsi_stress2_cal, fsl_lcsi_stress3, fsl_lcsi_stress3_cal,  
#            fsl_lcsi_stress4, fsl_lcsi_stress4_cal, fsl_lcsi_crisis1, fsl_lcsi_crisis1_cal, fsl_lcsi_crisis2, fsl_lcsi_crisis2_cal,
#            fsl_lcsi_crisis3, fsl_lcsi_crisis3_cal, fsl_lcsi_emergency1, fsl_lcsi_emergency1_cal, fsl_lcsi_emergency2, fsl_lcsi_emergency2_cal,
#            fsl_lcsi_emergency3, fsl_lcsi_emergency3_cal, fsl_lcsi_stress_yes, fsl_lcsi_crisis_yes, fsl_lcsi_emergency_yes, fsl_lcsi_stress_exhaust, 
#            fsl_lcsi_crisis_exhaust, fsl_lcsi_stress, fsl_lcsi_crisis, fsl_lcsi_emergency, fsl_lcsi_cat_yes, fsl_lcsi_cat_exhaust, fsl_lcsi_cat )
# )




raw_data_clean <-raw_data_clean |>
  humind::add_fcm_phase(fcs_column_name = "fcs_classe",
                             rcsi_column_name = "rcsi_classe",
                             hhs_column_name = "hhs_classe",
                             # ------------------------ unique(raw_data_clean$fcs_classe)
                             fcs_categories_acceptable = "Acceptable",
                             fcs_categories_poor = "Faible",
                             fcs_categories_borderline = "Limite",
                             # ------------------------ unique(raw_data_clean$rcsi_classe)
                             rcsi_categories_low = "Phase 1 - Durable",
                             rcsi_categories_medium = "Phase 2 - Sous pression",
                             rcsi_categories_high = "Phase 3 - Crise/Urgence",
                             # ------------------------ unique(raw_data_clean$hhs_classe)
                             hhs_categories_none = "None",
                             hhs_categories_little = "Peu ou pas de faim",
                             hhs_categories_moderate = "Faim modérée",
                             hhs_categories_severe = "Faim sévère",
                             hhs_categories_very_severe = "Very Severe"
                        )
tail(names(raw_data_clean), 2)
# View( raw_data_clean %>% select(tail(names(raw_data_clean), 2)))


## Indicateur composite SECAL
raw_data_clean <-raw_data_clean |>
  # impactR4PHU::add_comp_foodsec(fc_phase = "fsl_fc_phase",fc_phase_levels = c("Phase 1 FC", "Phase 2 FC", "Phase 3 FC", "Phase 4 FC","Phase 5 FC")) 
  humind::add_comp_foodsec( fc_phase = "fsl_fc_phase",
                    phase1 = "Phase 1 FC",
                    phase2 = "Phase 2 FC",
                    phase3 = "Phase 3 FC",
                    phase4 = "Phase 4 FC",
                    phase5 = "Phase 5 FC"
)


# View( raw_data_clean %>% select(fsl_fc_phase, comp_foodsec_score, comp_foodsec_in_need,  comp_foodsec_in_severe_need ))














# ----------------------------------    Santé   ---------------------------------



#### Individu ayant besoin d'accéder à des soins de santé - Binaire    ---------------------------------

# unique(loop_data_clean$health_ind_healthcare_needed)
# unique(loop_data_clean$health_ind_healthcare_received)

loop_data_clean <- loop_data_clean |> 
                   humind::add_loop_healthcare_needed_cat(
                          ind_healthcare_needed = "health_ind_healthcare_needed",
                          ind_healthcare_needed_no = "non",
                          ind_healthcare_needed_yes = "oui",
                          ind_healthcare_needed_dnk = "nsp",
                          ind_healthcare_needed_pnta = "pnpr",
                          # _________________________
                          ind_healthcare_received = "health_ind_healthcare_received",
                          ind_healthcare_received_no = "non",
                          ind_healthcare_received_yes = "oui",
                          ind_healthcare_received_dnk = "nsp",
                          ind_healthcare_received_pnta = "pnpr",
                          ind_age = "c_age_du_membre" # Verifier que la var c_age_du_membre dans kobo mesure The name ....
                                                      # The name of the variable that indicates the age of the individual.
                    )
                      

#### Nombre de personnes qui n'ont pas eu besoin d'accéder aux soins de santé    ---------------------------------
raw_data_clean <- raw_data_clean |>
                  humind::add_loop_healthcare_needed_cat_to_main(
                      loop = loop_data_clean, 
                      ind_healthcare_needed_no = "health_ind_healthcare_needed_no",
                      ind_healthcare_needed_yes_unmet = "health_ind_healthcare_needed_yes_unmet",
                      ind_healthcare_needed_yes_met = "health_ind_healthcare_needed_yes_met",
                      id_col_main = "uuid",
                      id_col_loop = "uuid"
                    )
 
# View(loop_data_clean %>% select(health_ind_healthcare_needed, health_ind_healthcare_needed_no, health_ind_healthcare_needed_yes_unmet, health_ind_healthcare_needed_yes_met, 
#                           health_ind_healthcare_needed_cat, health_ind_healthcare_received, health_ind_healthcare_received_d
#                           ) )                      
                                


#### Indicateur composite santé    ---------------------------------
raw_data_clean <- raw_data_clean |> 
                  humind::add_comp_health(
                                      ind_healthcare_needed_no_n = "health_ind_healthcare_needed_no_n",
                                      ind_healthcare_needed_yes_unmet_n = "health_ind_healthcare_needed_yes_unmet_n",
                                      ind_healthcare_needed_yes_met_n = "health_ind_healthcare_needed_yes_met_n"
                        )


# View(raw_data_clean %>% select(health_ind_healthcare_needed_no_n, health_ind_healthcare_needed_yes_unmet_n, health_ind_healthcare_needed_yes_met_n, comp_health_score, comp_health_in_need, comp_health_in_severe_need) )










# ----------------------------------    ABNA    ---------------------------------

#### Catégorie du type d'abri ---------------------------------------------


# unique(raw_data_clean$snfi_shelter_type)
# unique(raw_data_clean$snfi_shelter_type_individual)

raw_data_clean <- raw_data_clean |>
                  humind::add_shelter_type_cat(
                      # Type hébergement
                      shelter_type = "snfi_shelter_type",
                      sl_none = "pas_abris", # none
                      sl_collective_center =  c("abri_collectif", "heb_par_ami_famille", "heb_aumoins_un_other_menage"), # collective_center
                      sl_undefined = "pnpr", # pnta
                      
                      # Type d'abris abri_individuel
                      shelter_type_individual = "snfi_shelter_type_individual", # 
                      adequate = c("maison_solide", "appartement"), # "house", "apartment"
                      inadequate = c("abris_urgence", "bat_inacheve", "tente", "abris_urgence_pignon", "hangars_depots_ferme", "abris_fortune", "abri_transitionnel"), # "makeshift", "unfinished_building", "tent"
                      undefined = c("autre", "nsp", "pnpr") # "pnta", "other", "dnk"
                  )
    
# View( raw_data_clean %>% select(snfi_shelter_type, snfi_shelter_type_individual, snfi_shelter_type_cat))                                   
                            







#### Nombre de problèmes de logement  ---------------------------------------------

# Alignement des noms de variables kobo avec les noms de variables attendus par le script du siege
new_names <- str_replace_all(string = raw_data_clean %>% select(starts_with("a_1_probleme_abris_", ignore.case = TRUE)) %>% names(),
                         pattern = "a_1_probleme_abris_", 
                         replacement = "snfi_shelter_issue_"
                         )
old_names <- names(raw_data_clean %>% select(starts_with("a_1_probleme_abris_", ignore.case = TRUE ))) 
raw_data_clean <- raw_data_clean %>% rename_cols(new_cols = new_names, old_cols = old_names)


# unique(raw_data_clean$snfi_shelter_issue)
raw_data_clean <- raw_data_clean |>                         
                  humind::add_shelter_issue_cat(
                    shelter_issue = "snfi_shelter_issue",
                    none = "pas_de_pb_notable", #none
                    issues = c("mnq_dintimite_a_linterieur", "mnq_despace_a_linterieur", "trop_chaud_ou_froid_a_linterieur", "ventilation_limitee", "presence_germe_pathogene", 
                               "fuite_en_cas_de_pluie", "imp_de_ferme_abris", "manque_eclairage_a_linterieur", "manque_eclairage_a_lexterieur", "mnq_declairage_a_lext_lint", 
                               "degat_cause_par_inondation", "deplacemnt_difficile_a_lint_lext", "manque_espace_a_linterieur"),
                    undefined = c("nsp", "pnpr"), # "dnk", "pnta"
                    other = c("autre"), # "other"
                    sep = "_"
                  )

# View(raw_data_clean %>% select(snfi_shelter_issue, new_names, snfi_shelter_issue_n, snfi_shelter_issue_cat ) )




#### Risque lié aux modalités d'occupation ------------------------------------------




                                  
# unique(raw_data_clean$hlp_occupancy)
# unique(raw_data_clean$hlp_risk_eviction)

raw_data_clean <- raw_data_clean |> 
                  humind::add_occupancy_cat(
                          occupancy = "hlp_occupancy", #  mod_occupation
                          occupancy_high_risk = c("occupation_illegale"), # no_agreement
                          occupancy_medium_risk = c("en_location", "heberge_gratuitemnt"), # "rented", "hosted_free"
                          occupancy_low_risk = c("proprio"), # ownership
                          occupancy_undefined = c("nsp", "pnpr", "autre"), # "dnk", "pnta", "other"
                          
                          eviction = "hlp_risk_eviction", #  risk_expul_menag
                          eviction_high_risk = "oui", # yes
                          eviction_low_risk = "non", # no
                    eviction_undefined = c("nsp", "pnpr") # "dnk", "pnta"
                  )


# View(raw_data_clean %>% select(hlp_occupancy, hlp_risk_eviction, hlp_occupancy_cat, hlp_risk_eviction, hlp_occupancy_cat, hlp_eviction_cat, hlp_tenure_security ))




#### Nombre de tâches domestiques ne pouvant être effectuées    ---------------------------------

unique(raw_data_clean$energy_lighting_source)


raw_data_clean<-raw_data_clean |> 
                humind::add_fds_cannot_cat(
                        fds_cooking = "snfi_fds_cooking", # a_1_mbr_menage_cuisine
                        fds_cooking_can = "non", # no
                        fds_cooking_cannot = "oui", # yes
                        fds_cooking_no_need = "non_pas_besoin_cuisiner", # no_need
                        fds_cooking_undefined = "nsp", # pnta
                        
                        fds_sleeping = "snfi_fds_sleeping", # a_1_mbr_menage_dormir
                        fds_sleeping_can = "oui",
                        fds_sleeping_cannot = "non",
                        fds_sleeping_undefined = "nsp",
                        
                        fds_storing = "snfi_fds_storing",  # a_2_stockage_eau_nour
                        fds_storing_cannot = "non",
                        fds_storing_can = c("oui"), # c("yes_issues", "yes_no_issues")
                        fds_storing_undefined = "pnpr",
                        
                        lighting_source = "energy_lighting_source", #  a_3_source_eclairage
                        lighting_source_none = "aucune_source",
                        lighting_source_undefined = c("pnpr", "nsp")
                )

# View(raw_data_clean %>% select(snfi_fds_cooking, snfi_fds_sleeping, snfi_fds_storing, energy_lighting_source, snfi_fds_cannot_n, snfi_fds_cannot_cat )  )


#### Add shelter dammage Cat    ---------------------------------

# Alignement des noms de variables kobo avec les noms de variables attendus par le script du siege
new_names <- str_replace_all(string = raw_data_clean %>% select(starts_with("a_1_abris_dommage_", ignore.case = TRUE)) %>% names(),
                         pattern = "a_1_abris_dommage_", 
                         replacement = "snfi_shelter_damage_"
                         )
old_names <- names(raw_data_clean %>% select(starts_with("a_1_abris_dommage_", ignore.case = TRUE ))) 
raw_data_clean <- raw_data_clean %>% rename_cols(new_cols = new_names, old_cols = old_names)



raw_data_clean <- raw_data_clean |> 
                  humind::add_shelter_damage_cat(
                    sep = "_",
                    snfi_shelter_damage = "snfi_shelter_damage",
                    none = "aucun",
                    minor = "dommage_mineur",
                    major = "dommage_important",
                    damage_windows_doors = "dommage_fenetre",
                    damage_floors = "dommage_sol",
                    damage_walls = "dommage_murs",
                    total_collapse = "effondrement",
                    other = "autre",
                    dnk = "nsp",
                    pnta = "pnpr"
                  )


# View(raw_data_clean %>% select(snfi_shelter_damage, snfi_shelter_damage_cat )  )








#### Add SNFI Sectoral Composite Score and Need Indicators -----------------


raw_data_clean<-raw_data_clean |> 
                humind::add_comp_snfi(
                        shelter_type_cat = "snfi_shelter_type_cat",
                        shelter_type_cat_none = "none",
                        shelter_type_cat_inadequate = "inadequate",
                        shelter_type_cat_adequate = "adequate",
                        shelter_type_cat_undefined = "undefined",
                        shelter_issue_cat = "snfi_shelter_issue_cat",
                        shelter_issue_cat_1_to_3 = "1_to_3",
                        shelter_issue_cat_4_to_7 = "4_to_7",
                        shelter_issue_cat_8_to_11 = "8_to_11",
                        shelter_issue_cat_none = "none",
                        shelter_issue_cat_undefined = "undefined",
                        shelter_issue_cat_other = "other",
                        tenure_security_cat = "hlp_occupancy_cat",
                        tenure_security_cat_high_risk = "high_risk",
                        tenure_security_cat_medium_risk = "medium_risk",
                        tenure_security_cat_low_risk = "low_risk",
                        tenure_security_cat_undefined = "undefined",
                        fds_cannot_cat = "snfi_fds_cannot_cat",
                        fds_cannot_cat_4 = "4_tasks",
                        fds_cannot_cat_2_to_3 = "2_to_3_tasks",
                        fds_cannot_cat_1 = "1_task",
                        fds_cannot_cat_none = "none",
                        fds_cannot_cat_undefined = "undefined",
                        shelter_damage = FALSE,
                        shelter_damage_cat = "snfi_shelter_damage_cat",
                        shelter_damage_cat_none = "none",
                        shelter_damage_cat_damaged = "damaged",
                        shelter_damage_cat_part = "part",
                        shelter_damage_cat_total = "total",
                        shelter_damage_cat_undefined = "undefined"
                )

# View(raw_data_clean %>% select(snfi_shelter_type_cat, snfi_shelter_issue_cat, hlp_occupancy_cat, energy_lighting_source, snfi_fds_cannot_n, snfi_fds_cannot_cat,
#                                snfi_shelter_damage_cat, comp_snfi_score_shelter_type_cat, comp_snfi_score_shelter_issue_cat, comp_snfi_score_tenure_security_cat,
#                                comp_snfi_score_fds_cannot_cat, comp_snfi_score, comp_snfi_in_need, comp_snfi_in_severe_need
#                                )  )

























#    ---------------------------------  Protection   ---------------------------------

#### - Catégorie des raisons pour lesquelles les enfants sont séparés de leur famille
#raison de separation des enfants non prise en compte dans la fonction "parent"

#### - Capacité à se déplacer et à accéder à des lieux publics ---------------------------------------------------

# Alignement des noms de variables kobo avec les noms de variables attendus par le script du siege
new_names <- str_replace_all(string = raw_data_clean %>% select(starts_with("probleme_de_securite_", ignore.case = TRUE)) %>% names(),
                         pattern = "probleme_de_securite_", 
                         replacement = "prot_needs_3_movement_"
                         )
old_names <- names(raw_data_clean %>% select(starts_with("probleme_de_securite_", ignore.case = TRUE ))) 
raw_data_clean <- raw_data_clean %>% rename_cols(new_cols = new_names, old_cols = old_names)


raw_data_clean <- raw_data_clean |> 
                  humind::add_prot_score_movement(
                      sep = "_",
                      prot_needs_3_movement = "prot_needs_3_movement", #   probleme_de_securite
                      no_changes_feel_unsafe = "non_pas_de_changement", #  no_changes_feel_unsafe
                      no_safety_concerns = "non_aucun_probleme", #  no_safety_concerns
                      women_girls_avoid_places = "evite_endroit_femme_fille", #  women_girls_avoid_places
                      men_boys_avoid_places = "evite_endroit_homme_garcon", #  men_boys_avoid_places
                      women_girls_avoid_night = "evite_sortie_nocture_femme_fille", #  women_girls_avoid_night
                      men_boys_avoid_night = "evite_sortie_nocture_homme_garcon", #  men_boys_avoid_night
                      girls_boys_avoid_school = "evite_chemin_ecole_fille_garcon", #  girls_boys_avoid_school
                      different_routes = "emprunt_itteneraire_diff", #  different_routes
                      avoid_markets = "evite_rendre_sur_marche", #  avoid_markets
                      avoid_public_offices = "evite_lieu_public", #  avoid_public_offices
                      avoid_fields = "evite_champs_paturage", #  avoid_fields
                      other_safety_measures = "use_of_other_safety_way", #  other_safety_measures
                      dnk = "nsp", #  dnk
                      pnta = "pnpr", #  pnta
                      .keep_weighted = FALSE
                  )


# View(raw_data_clean %>% select(new_names, comp_prot_score_prot_needs_3, comp_prot_score_movement))











#### - Capacité à participer à des pratiques et activités sécurisées -------------------------------------------
# Alignement des noms de variables kobo avec les noms de variables attendus par le script du siege
# prot_needs_2_activities
new_names <- str_replace_all(string = raw_data_clean %>% select(starts_with("diff_acces_ressources_", ignore.case = TRUE)) %>% names(),
                             pattern = "diff_acces_ressources_", 
                             replacement = "prot_needs_2_activities_"
                         )
old_names <- names(raw_data_clean %>% select(starts_with("diff_acces_ressources_", ignore.case = TRUE ))) 
raw_data_clean <- raw_data_clean %>% rename_cols(new_cols = new_names, old_cols = old_names)


# Alignement des noms de variables kobo avec les noms de variables attendus par le script du siege
# prot_needs_2_social
new_names <- str_replace_all(string = raw_data_clean %>% select(starts_with("diff_menaces_", ignore.case = TRUE)) %>% names(),
                             pattern = "diff_menaces_", 
                             replacement = "prot_needs_2_social_"
                         )
old_names <- names(raw_data_clean %>% select(starts_with("diff_menaces_", ignore.case = TRUE ))) 
raw_data_clean <- raw_data_clean %>% rename_cols(new_cols = new_names, old_cols = old_names)



raw_data_clean <- raw_data_clean |> 
                  humind::add_prot_score_practices(
                          sep = "_",
                          prot_needs_2_activities = "prot_needs_2_activities", # diff_acces_ressources
                          yes_work = "oui_affect_capacite_travail", #  yes_work
                          yes_livelihood = "oui_affect_acce_moyen_subsistance", #  yes_livelihood
                          yes_safety = "oui_affect_capacite_securit",  # yes_safety
                          yes_farm = "oui_affect_capacite_cultiver", #  yes_farm
                          yes_water = "oui_affect_collecte_eau", #  yes_water
                          yes_other_activities = "oui_affect_autres_activite", # yes_other_activities
                          yes_free_choices = "oui_affect_choix_libre", #  yes_free_choices
                          #_____________________
                          prot_needs_2_social = "prot_needs_2_social", # diff_menaces
                          yes_visiting_family = "oui_visite_mbr_famille", # yes_visiting_family
                          yes_visiting_friends = "oui_visite_amis", # yes_visiting_friends
                          yes_community_events = "oui_assist_evenement", #  yes_community_events
                          yes_joining_groups = "oui_rejoindre_groupe_public", # yes_joining_groups
                          yes_other_social = "oui_participe_actv_sociales", #  yes_other_social
                          yes_child_recreation = "oui_participe_actv_recreatives", # yes_child_recreation
                          yes_decision_making = "oui_participe_organes_elections", #  yes_decision_making
                          no = "non", # no
                          dnk = "nsp", # dnk
                          pnta = "pnpr", # pnta
                          .keep_weighted = FALSE
                  )


View(raw_data_clean %>% select(prot_needs_2_activities, prot_needs_2_social, comp_prot_score_prot_needs_2_activities, 
                               comp_prot_score_prot_needs_2_social, comp_prot_score_practices
                               ))









# Capacité à avoir accès à des droits et services

# Alignement des noms de variables kobo avec les noms de variables attendus par le script du siege
## prot_needs_1_services = diff_sante_educ
new_names <- str_replace_all(string = raw_data_clean %>% select(starts_with("diff_sante_educ_", ignore.case = TRUE)) %>% names(),
                             pattern = "diff_sante_educ_", 
                             replacement = "prot_needs_1_services_"
                         )
old_names <- names(raw_data_clean %>% select(starts_with("diff_sante_educ_", ignore.case = TRUE ))) 
raw_data_clean <- raw_data_clean %>% rename_cols(new_cols = new_names, old_cols = old_names)



# Alignement des noms de variables kobo avec les noms de variables attendus par le script du siege
## prot_needs_1_justice = diff_acces_justice
new_names <- str_replace_all(string = raw_data_clean %>% select(starts_with("diff_acces_justice_", ignore.case = TRUE)) %>% names(),
                             pattern = "diff_acces_justice_", 
                             replacement = "prot_needs_1_justice_"
                         )
old_names <- names(raw_data_clean %>% select(starts_with("diff_acces_justice_", ignore.case = TRUE ))) 
raw_data_clean <- raw_data_clean %>% rename_cols(new_cols = new_names, old_cols = old_names)


raw_data_clean <- raw_data_clean |> 
                  humind::add_prot_score_rights(
                          sep = "_",
                          prot_needs_1_services = "prot_needs_1_services", # diff_sante_educ
                          yes_healthcare = "oui_acce_centre_sante", # yes_healthcare
                          yes_schools = "oui_acces_ecoles", # yes_schools
                          yes_gov_services = "oui_acces_services_gouvernementaux",# yes_gov_services
                          yes_other_services = "oui_acces_autres_services", # yes_other_services
                          # ________________________
                          prot_needs_1_justice = "prot_needs_1_justice", # diff_acces_justice
                          yes_identity_documents = "oui_acces_doc_etat_civil", # yes_identity_documents
                          yes_counselling_legal = "oui_conseil_individuel", # yes_counselling_legal
                          yes_property_docs = "oui_acces_doc_sur_lgt", # yes_property_docs
                          yes_gov_services_justice = "oui_acces_services_gouvernementaux", # yes_gov_services
                          yes_other_services_justice = "oui_acces_autres_services", # yes_other_services
                          no = "non", # no
                          dnk = "nsp", # dnk
                          pnta = "pnpr", # pnta
                          .keep_weighted = FALSE                    
                  )

View(raw_data_clean %>% select(prot_needs_1_services, comp_prot_score_prot_needs_1_services, prot_needs_1_justice, 
                               comp_prot_score_prot_needs_1_justice, comp_prot_score_rights
                               )) #comp_prot_score_needs_1



## Indicateur composite de protection
# raw_data_clean<-raw_data_clean |>
#   humind::add_comp_prot(child_sep_cat = "prot_child_sep_cat",
#                         child_sep_cat_levels = c("none", "at_least_one_very_severe", "at_least_one_severe","at_least_one_non_severe", "undefined"),
#                         concern_freq_cope = "prot_concern_freq_cope",
#                         concern_freq_displaced = "prot_concern_freq_displaced",
#                         concern_hh_freq_kidnapping = "prot_concern_hh_freq_kidnapping",
#                         concern_hh_freq_discrimination = "prot_concern_hh_freq_discrimination",
#                         concern_levels = c("toujours", "plusieurs_fois", "une_deux_fois", "jamais", "dnk", "pnta")
#                         )
 




























# ----------------------------------    EHA    ---------------------------------
#### ----------------------------------    EAU    ---------------------------------


######--------------------------------  Quantité d'eau   --------------------------------

# 1- Ajouter une variable comp_wash_score_drinking_water_quantity attribuant les niveaux 1 à 4+ 
# (numérique 1 à 5) dans l'ordre suivant : Jamais (0 fois), Rarement (1-2 fois),
# Parfois (3-10 fois), Souvent (11-20 fois), Presque toujours (plus de 20 fois). 
#_____________________________ NEW VERSION_____________________________
# Add a variable comp_wash_score_drinking_water_quantity assigning levels 1 to 4+ (numeric 1 to 5) 
# in the following order: Never (0 times), Rarely (1–2 times), Sometimes (3–10 times), Often (11-20 times), 
# Always (more than 20 times). 

raw_data_clean <- raw_data_clean %>% 
                  mutate(comp_wash_score_drinking_water_quantity = case_when(
                      wash_drinking_water_quantity == "presque" ~ 5,
                      wash_drinking_water_quantity == "souvent" ~ 4,
                      wash_drinking_water_quantity == "parfois" ~ 3,
                      wash_drinking_water_quantity == "rarement" ~ 2,
                      wash_drinking_water_quantity == "jamais" ~ 1,
                      T ~ NA)
                  ) 

# View( raw_data_clean %>% select( wash_drinking_water_quantity, comp_wash_score_drinking_water_quantity) )






######     --------------------------------  Catégorie de la source d'eau potable     ---------------------------------
raw_data_clean <- raw_data_clean |> 
                  humind::add_drinking_water_source_cat(
                  drinking_water_source = "wash_drinking_water_source",
                  drinking_water_source_cat_improved = c("eau_robinet", # piped_dwelling
                                                         "eau_concession", # piped_compound
                                                         "eau_concession_voisin", # piped_neighbour
                                                         "robinet_public", # tap
                                                         "puits_forage", # borehole
                                                         "puit_amenage", # protected_well
                                                         "source_protege", # protected_spring
                                                         "eau_pluie", # rainwater_collection
                                                         "eau_camion", # tank_truck
                                                         "baril", # cart_tank
                                                         "kiosque_eau", # kiosk
                                                         "eau_bouteille", # bottled_water
                                                         "eau_sachet"  # sachet_water
                                                         ),
                  drinking_water_source_cat_unimproved = c("puit_traditionnel", # unprotected_well
                                                           "source_non_protege" # unprotected_spring
                                                           ),
                  drinking_water_source_cat_surface_water = "cours_eau",# surface_water
                  drinking_water_source_cat_undefined = c("autre", "nsp", "pnpr") # "dnk", "pnta", "other"
                )

# View(raw_data_clean %>% select(wash_drinking_water_source, wash_drinking_water_source_cat))



######     --------------------------------  Catégorie du temps nécessaire pour aller chercher de l'eau potable --------------------------------  

## Suppression de tous les zéro de la variable "wash_drinking_water_time_int" car la "add_drinking_water_time_cat" n'autorise pas de 0
raw_data_clean <- raw_data_clean %>% 
                  mutate(wash_drinking_water_time_int=case_when(wash_drinking_water_time_int==0~NA_integer_, TRUE~wash_drinking_water_time_int))


# add_drinking_water_time_cat() prend les arguments suivants :
# wash_drinking_water_time_yn | Component column: Time to fetch water, scoping question.
# water_on_premises	: Character vector of responses codes for water on premises.
# number_minutes : Character vector of responses codes for number of minutes.


# wash_drinking_water_time_int | Component column: Time to fetch water, integer.
# max	Integer, the maximum value for the time to fetch water.


# wash_drinking_water_source | Component column: Water source types.
# drinking_water_source_cat_improved	| Response code for improved water source.
# drinking_water_source_cat_unimproved | Response code for unimproved water source.
# drinking_water_source_cat_surface_water	| Response code for surface water source.
# drinking_water_source_cat_undefined	| Response code for undefined water source.


raw_data_clean <- raw_data_clean %>% 
                  mutate(total_time_get_water = ifelse(is.na(h_1_eau_temp_collecte), 0, h_1_eau_temp_collecte) + ifelse(is.na(h_1_eau_temp_attente_et_collecte), 0, h_1_eau_temp_attente_et_collecte)) %>% 
                  mutate(
                    # wash_drinking_water_source_cat = case_when(
                    #   h_1_principal_source_eau %in% c("poste_eau_autonome", "eau_robinet", "eau_concession", "eau_concession_voisin", "robinet_public", "forage",
                    #                                   "puit_amenage", "source_protege", "eau_sachet", "eau_camion", "eau_tricycle", "baril", "kiosque_eau","eau_bouteille") ~ "amelioree",
                    #   h_1_principal_source_eau %in% c("puit_traditionnel", "source_non_protege", "eau_pluie") ~ "non_amelioree",
                    #   h_1_principal_source_eau == "cours_eau" ~ "cours_eau",
                    #   T ~ NA_character_
                    # ),
                    wash_drinking_water_time_cat = case_when(
                          wash_drinking_water_source %in%	c("eau_robinet", "eau_concession") ~ "on_premises",
                          total_time_get_water < 30 ~ "under_30min",
                          total_time_get_water >= 30 | wash_drinking_water_time_int %in% c("plus_30min_moins_1h", "1heure_ou_plus") | h_1_eau_temp_attente_et_collecte_estime %in% c("plus_30min_moins_1h", "1heure_ou_plus") ~ "above_30min",
                          T ~ NA_character_
                      
                        )
                    )


# View(raw_data_clean %>% select(wash_drinking_water_source, total_time_get_water, wash_drinking_water_time_int, wash_drinking_water_time_cat))


#######  --------------------------------  Seuil du temps nécessaire pour aller chercher de l'eau potable (seuil à 30 minutes)  --------------------------------  
# raw_data_clean<-raw_data_clean |> 
#                 humind::add_drinking_water_time_threshold_cat(
#                       drinking_water_time_30min_cat = "wash_drinking_water_time_cat",
#                       drinking_water_time_30min_cat_premises = "premises",
#                       drinking_water_time_30min_cat_under_30min = c("under_30_min"),
#                       drinking_water_time_30min_cat_above_30min = c("30min_1hr", "more_than_1hr"),
#                       drinking_water_time_30min_cat_undefined = "undefined"
#                 )

# unique(raw_data_clean$wash_drinking_water_time_cat)

raw_data_clean <- raw_data_clean %>% 
                  add_drinking_water_time_threshold_cat(
                        drinking_water_time_30min_cat = "wash_drinking_water_time_cat",
                        drinking_water_time_30min_cat_premises = "on_premises", # premises
                        drinking_water_time_30min_cat_under_30min = c("under_30min"), # under_30_min
                        drinking_water_time_30min_cat_above_30min = c("above_30min"), # 30min_1hr # more_than_1hr
                        drinking_water_time_30min_cat_undefined = "undefined" # undefined
                  )


#######  --------------------------------  Catégorie de l’échelle JMP – Qualité de l’eau pour boire -------------------------------- 
# raw_data_clean<-raw_data_clean |> 
#   humind::add_drinking_water_quality_jmp_cat(drinking_water_source_cat = "wash_drinking_water_source_cat",
#                                              drinking_water_source_cat_levels = c("improved", "unimproved", "surface_water","undefined"),
#                                              drinking_water_time_30min_cat = "wash_drinking_water_time_30min_cat",
#                                              drinking_water_time_30min_cat_levels = c("premises", "under_30min", "above_30min","undefined"))


raw_data_clean<-raw_data_clean |>                                      
                add_drinking_water_quality_jmp_cat(
                  drinking_water_source_cat = "wash_drinking_water_source_cat",
                  drinking_water_source_cat_improved = "improved",
                  drinking_water_source_cat_unimproved = "unimproved",
                  drinking_water_source_cat_surface_water = "surface_water",
                  drinking_water_source_cat_undefined = "undefined",
                  #_____________________________________________
                  drinking_water_time_30min_cat = "wash_drinking_water_time_30min_cat",
                  drinking_water_time_30min_cat_premises = "premises",
                  drinking_water_time_30min_cat_under_30min = "under_30min",
                  drinking_water_time_30min_cat_above_30min = "above_30min",
                  drinking_water_time_30min_cat_undefined = "undefined"
                )

View(raw_data_clean %>%  select(wash_drinking_water_source_cat, wash_drinking_water_time_30min_cat, wash_drinking_water_quality_jmp_cat))                                                 















#### ----------------------------------ASSAINISSEMENT---------------------------------

#######  --------------------------------   Catégorie du type d'installation sanitaire -------------------------------- 

# unique(raw_data_clean$wash_sanitation_facility)

raw_data_clean<-raw_data_clean |> 
                humind::add_sanitation_facility_cat(
                sanitation_facility = "wash_sanitation_facility",
                improved = c("toil_chasse_eau_manuelle_ou_mecanic_egout_canalise", # flush_piped_sewer
                             "toil_chasse_eau_manuelle_ou_mecanic_fosse_septique", # flush_septic_tank
                             "toil_chasse_eau_manuelle_ou_mecanic_latrin_fosse",  # flush_pit_latrine
                             # "", # flush_dnk_where 
                             # "", # pit_latrine_slab
                             # "", # twin_pit_latrine_slab
                             # "", # ventilated_pit_latrine_slab
                             # "", # container
                             # "" # compost
                             "lat_tradi_avec_dalle",
                             "lat_sanplat_ameliore",
                             "lat_vip",
                             "toil_amovible_avc_conteneur",
                             "lat_vip_double_fosse_avc_dalle",
                             "lat_ecosan_avc_dalle" 
                          ),
                unimproved = c("toil_chasse_eau_manuelle_ou_mecanic_egout_ouvert", # flush_open_drain
                               "lat_tradi_sans_dalle", # pit_latrine_wo_slab
                               "utiliser_seau_ou_recipient" # bucket
                               # "", # flush_elsewhere
                               # "", # hanging_toilet
                               # ""  # plastic_bag
                               ),
                none = "defec_air_libre", # none
                undefined = c("autre", "nsp", "pnpr") # "other", "dnk", "pnta"
              )
            

#######  -------------------------------- Statut de partage de l'installation sanitaire --------------------------------

unique(raw_data_clean$wash_sanitation_facility_sharing_yn)

raw_data_clean<-raw_data_clean |> 
  humind::add_sharing_sanitation_facility_cat(sharing_sanitation_facility = "wash_sanitation_facility_sharing_yn",
                                              yes = "oui",
                                              no = "non",
                                              undefined = c("nsp", "pnpr"),
                                              sanitation_facility = "wash_sanitation_facility",skipped_sanitation_facility = NULL)
                                      
                                                    

#######  -------------------------------- Catégorie du nombre de personnes partageant l'installation sanitaire  --------------------------------

# unique(raw_data_clean$wash_sharing_sanitation_facility_cat)
# *****************************************************************
# *****************************************************************
# ****************************************************
raw_data_clean<-raw_data_clean |> 
                humind::add_sharing_sanitation_facility_n_ind(
                    sharing_sanitation_facility_cat = "wash_sharing_sanitation_facility_cat",
                    sharing_sanitation_facility_cat_shared = "shared",
                    sharing_sanitation_facility_cat_not_shared = "not_shared",
                    sharing_sanitation_facility_cat_not_applicable = "not_applicable",
                    sharing_sanitation_facility_cat_undefined = "undefined",
                    sanitation_facility_sharing_n = "wash_sanitation_facility_sharing_n",
                    hh_size = "hh_size", # Column name for household size. c_taille_hh
                    weight = "weight" # Column name for survey weights.
              )
# *****************************************************************
# *****************************************************************
# ****************************************************
                                        
                                                      

#######  -------------------------------- Catégorie de échelle JMP – Latrines   --------------------------------
raw_data_clean<-raw_data_clean |> 
                  humind::add_sanitation_facility_jmp_cat(
                  sanitation_facility_cat = "wash_sanitation_facility_cat",
                  sanitation_facility_cat_improved = "improved",
                  sanitation_facility_cat_unimproved = "unimproved",
                  sanitation_facility_cat_none = "none",
                  sanitation_facility_cat_undefined = "undefined",
                  sharing_sanitation_facility_cat = "wash_sharing_sanitation_facility_cat",
                  sharing_sanitation_facility_cat_shared = "shared",
                  sharing_sanitation_facility_cat_not_shared = "not_shared",
                  sharing_sanitation_facility_cat_not_applicable = "not_applicable",
                  sharing_sanitation_facility_cat_undefined = "undefined"
                )
                 

View(raw_data_clean %>% select(wash_sanitation_facility_cat, wash_sharing_sanitation_facility_cat, 
                          #wash_sharing_sanitation_n_ind,
                          # wash_sanitization_jmp_cat 
                          ))
                 
                                                
#######  -------------------------------- Catégorie de l’échelle JMP – Station de lavage des mains  -------------------------------- 
#Ajustement 
raw_data_clean <-raw_data_clean %>%  
  dplyr::mutate(wash_handwashing_facility=case_when(wash_handwashing_facility=="available"|wash_handwashing_facility=="mobile_object"~"available",
                                                    TRUE~wash_handwashing_facility))
                                                                                
# *****************************************************************
# *****************************************************************
# *****************************************************************
raw_data_clean <- raw_data_clean |> 
                  humind::add_handwashing_facility_cat(
                        survey_modality = "survey_modality",
                        survey_modality_in_person = c("in_person"),
                        survey_modality_remote = c("remote"),
                        facility = "wash_handwashing_facility",
                        facility_yes = c("available_fixed_in_dwelling", "available_fixed_in_plot", "available_fixed_or_mobile"),
                        facility_no = "none",
                        facility_no_permission = "no_permission",
                        facility_undefined = c("other", "pnta"),
                        facility_observed_water = "wash_handwashing_facility_observed_water",
                        facility_observed_water_yes = "water_available",
                        facility_observed_water_no = c("water_not_available"),
                        facility_observed_soap = "wash_soap_observed",
                        facility_observed_soap_yes = "yes_soap_shown",
                        facility_observed_soap_no = c("no"),
                        facility_observed_soap_undefined = c("other", "pnta"),
                        facility_reported = "wash_handwashing_facility_reported",
                        facility_reported_yes = c("fixed_dwelling", "fixed_yard", "mobile"),
                        facility_reported_no = c("none"),
                        facility_reported_undefined = c("other", "dnk", "pnta"),
                        facility_reported_water = "wash_handwashing_facility_water_reported",
                        facility_reported_water_yes = "yes",
                        facility_reported_water_no = c("no"),
                        facility_reported_water_undefined = c("dnk", "pnta"),
                        facility_reported_soap = "wash_soap_reported",
                        facility_reported_soap_yes = "yes",
                        facility_reported_soap_no = c("no"),
                        facility_reported_soap_undefined = c("dnk", "pnta"),
                        soap_type_observed = "wash_soap_observed_type",
                        soap_type_observed_yes = c("soap", "detergent"),
                        soap_type_observed_no = c("ash_mud_sand"),
                        soap_type_observed_undefined = c("dnk", "pnta", "other"),
                        soap_type_reported = "wash_soap_reported_type",
                        soap_type_reported_yes = c("soap", "detergent"),
                        soap_type_reported_no = c("ash_mud_sand"),
                        soap_type_reported_undefined = c("dnk", "pnta", "other")
                )
# *****************************************************************
# *****************************************************************
# ****************************************************                               
                                              

#correction de la majuscule en minscule dans la la varaible 
## indicateur composite EHA
raw_data_clean <- raw_data_clean |> 
                  add_comp_wash(
                    setting = "setting",
                    setting_camp = "camp",
                    setting_urban = "urban",
                    setting_rural = "rural",
                    #____________________________________________
                    drinking_water_quantity = "wash_hwise_drink",
                    drinking_water_quantity_always = "always",
                    drinking_water_quantity_often = "often",
                    drinking_water_quantity_sometimes = "sometimes",
                    drinking_water_quantity_rarely = "rarely",
                    drinking_water_quantity_never = "never",
                    drinking_water_quantity_dnk = "dnk",
                    drinking_water_quantity_pnta = "pnta",
                    #____________________________________________
                    drinking_water_quality_jmp_cat = "wash_drinking_water_quality_jmp_cat",
                    drinking_water_quality_jmp_cat_surface_water = "surface_water",
                    drinking_water_quality_jmp_cat_unimproved = "unimproved",
                    drinking_water_quality_jmp_cat_limited = "limited",
                    drinking_water_quality_jmp_cat_basic = "basic",
                    drinking_water_quality_jmp_cat_safely_managed = "safely_managed",
                    drinking_water_quality_jmp_cat_undefined = "undefined",
                    #____________________________________________
                    sanitation_facility_jmp_cat = "wash_sanitation_facility_jmp_cat",
                    sanitation_facility_jmp_cat_open_defecation = "open_defecation",
                    sanitation_facility_jmp_cat_unimproved = "unimproved",
                    sanitation_facility_jmp_cat_limited = "limited",
                    sanitation_facility_jmp_cat_basic = "basic",
                    sanitation_facility_jmp_cat_safely_managed = "safely_managed",
                    sanitation_facility_jmp_cat_undefined = "undefined",
                    #____________________________________________
                    sanitation_facility_cat = "wash_sanitation_facility_cat",
                    sanitation_facility_cat_none = "none",
                    sanitation_facility_cat_unimproved = "unimproved",
                    sanitation_facility_cat_improved = "improved",
                    sanitation_facility_cat_undefined = "undefined",
                    #____________________________________________
                    sanitation_facility_n_ind = "wash_sharing_sanitation_facility_n_ind",
                    sanitation_facility_n_ind_50_and_above = "50_and_above",
                    sanitation_facility_n_ind_20_to_49 = "20_to_49",
                    sanitation_facility_n_ind_19_and_below = "19_and_below",
                    #____________________________________________
                    sharing_sanitation_facility_cat = "wash_sharing_sanitation_facility_cat",
                    sharing_sanitation_facility_cat_shared = "shared",
                    sharing_sanitation_facility_cat_not_shared = "not_shared",
                    sharing_sanitation_facility_cat_not_applicable = "not_applicable",
                    sharing_sanitation_facility_cat_undefined = "undefined",
                    #____________________________________________
                    handwashing_facility_jmp_cat = "wash_handwashing_facility_jmp_cat",
                    handwashing_facility_jmp_cat_no_facility = "no_facility",
                    handwashing_facility_jmp_cat_limited = "limited",
                    handwashing_facility_jmp_cat_basic = "basic",
                    handwashing_facility_jmp_cat_undefined = "undefined"
                  )










                                     


# ----------------------------------    Education    ---------------------------------

#### Individu en âge scolaire dans la boucle - Binaire
loop_data_clean <- loop_data_clean |> 
                   humind::add_loop_edu_ind_age_corrected(
                      raw_data_clean,
                      id_col_loop = "uuid",
                      id_col_main = "uuid",
                      survey_start_date = "start",
                      school_year_start_month = 10,
                      ind_age = "c_age_du_membre", # ind_age A VERIFIER
                      month = NULL,
                      schooling_start_age = 6
                   )




#### Nombre d'individus(somme des individus par uuid) en âge scolaire rappoté dans la base princicale à partir de la base boucle

raw_data_clean <- raw_data_clean |> 
                  add_loop_edu_ind_schooling_age_d_to_main(
                    loop_data_clean ,
                    ind_schooling_age_d = "edu_ind_schooling_age_d",
                    id_col_main = "uuid",
                    id_col_loop = "uuid"
                  )

# View(loop_data_clean %>% select(c_age_du_membre, edu_ind_schooling_age_d, edu_ind_age_corrected))
                                         


#### Individu fréquentant l'école dans la boucle  - Binaire

unique(loop_data_clean$edu_access)
loop_data_clean <-  loop_data_clean |> 
                    humind::add_loop_edu_access_d(
                        ind_access = "edu_access",
                        yes = "oui",
                        no = "non",
                        pnta = "pnpr",
                        dnk = "nsp",
                        ind_schooling_age_d = "edu_ind_schooling_age_d"
                   )

                       


## Ajout de la variable "edu_access" à la base principale
raw_data_clean <- raw_data_clean |> 
                  humind::add_loop_edu_access_d_to_main(loop_data_clean,
                                                        ind_access_d = "edu_ind_access_d",
                                                        ind_no_access_d = "edu_ind_no_access_d", 
                                                        id_col_main = "uuid",
                                                        id_col_loop = "uuid")

                                

## Individu avec une barrière d'accès à l'école liée à la protection de l'enfance - Binaire
#Problèmes de barrière non prise en compte dans la fonction en attendant le retours de ROCIO("trop_jeune","abandon","exclu")

# unique(loop_data_clean$edu_ind_schooling_age_d)
loop_data_clean <- loop_data_clean |> 
                      humind::add_loop_edu_barrier_protection_d(
                      barriers = "edu_barrier",
                      protection_issues = c("risque_protection", # protection_at_school
                                            "risque_protection_trajet", # protection_travel_school
                                            "travail_domestiq", # child_work_home
                                            "participation_agr", # child_work_outside
                                            "enft_recrute", # child_armed_group
                                            "mariage", # child_marriage
                                            "interdiction", # ban
                                            "child_pregnancy", # grossesse
                                            "manq_documentation", # enroll_lack_documentation
                                            "discrimination", # discrimination
                                            "child_graduated" # enfant_diplome
                                            ),
                      ind_schooling_age_d = "edu_ind_schooling_age_d"
                  )
                                    


## Ajout de la variable "edu_barrier" à la base principale
raw_data_clean <- raw_data_clean |> 
                  humind::add_loop_edu_barrier_protection_d_to_main(loop_data_clean,
                                                                    ind_barrier_protection_d = "edu_ind_barrier_protection_d",
                                                                    id_col_main = "uuid",
                                                                    id_col_loop = "uuid")
                                 

## Individu dont l'éducation a été perturbée 
unique(loop_data_clean$edu_ind_barrier_protection_d)

loop_data_clean<- loop_data_clean |> 
                  humind::add_loop_edu_disrupted_d(
                          attack = NULL, # edu_disrupted_attack
                          hazards = "e_alea", # edu_disrupted_hazards
                          displaced = "e_ecole_abris", # edu_disrupted_displaced
                          teacher = "e_absence_enseignant", # edu_disrupted_teacher
                          levels = c("oui", "non", "nsp", "pnpr"), # "yes", "no", "dnk", "pnta"
                          ind_schooling_age_d = "edu_ind_schooling_age_d"
                )
                                               

## Ajout des  modalités de perturbation de école  dans la base principale
raw_data_clean <- raw_data_clean |> 
                  humind::add_loop_edu_disrupted_d_to_main(loop_data_clean,
                    attack_d = "edu_disrupted_attack_d", # edu_disrupted_attack_d
                    hazards_d = "e_alea_d", # edu_disrupted_hazards_d
                    displaced_d = "e_ecole_abris_d", # edu_disrupted_displaced_d
                    teacher_d = "e_absence_enseignant_d", # edu_disrupted_teacher_d
                    id_col_main = "uuid",
                    id_col_loop = "uuid"
                )

## Indicateur composite de l'education 
raw_data_clean <- raw_data_clean |> 
                  humind::add_comp_edu(schooling_age_n = "edu_schooling_age_n",
                                       no_access_n = "edu_no_access_n",
                                       barrier_protection_n = "edu_barrier_protection_n",
                                       occupation_n = "edu_disrupted_occupation_n",
                                       hazards_n = "edu_disrupted_hazards_n",
                                       displaced_n = "edu_disrupted_displaced_n",
                                       teacher_n = "edu_disrupted_teacher_n"
                                       )
                                              









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
                                 comp_edu_in_need = "comp_edu_in_need"
                                 )
                                 


#aap_received_assistance
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

#add_income_source_prop

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
                                 income_source_other_n = "cm_income_source_other_n"
  )
                                               


# cm_income_source_top1,cm_income_source_top2, cm_income_source_top3
raw_data_clean <- raw_data_clean |> 
  humind::add_income_source_rank(emergency = c("cm_income_source_assistance_n", "cm_income_source_support_friends_n", "cm_income_source_donation_n"),
                                 unstable = c ("cm_income_source_casual_n", "cm_income_source_social_benefits_n", "cm_income_source_rent_n", "cm_income_source_remittances_n"),
                                 stable = c("cm_income_source_salaried_n", "cm_income_source_own_business_n", "cm_income_source_own_production_n"),
                                 other = "cm_income_source_other_n",
                                 id_col = "uuid")
  


raw_data_clean <- raw_data_clean |> 
  humind::add_age_cat(age_col="resp_age",
                      breaks = c(seq(0, 115, by = 5), 120),
                      labels = NULL,
                      int_undefined = c(-999, 999),
                      char_undefined = "undefined",
                      new_colname = NULL)
  

raw_data_clean <- raw_data_clean |> 
  humind::add_age_cat(age_col="c_age_hoh",
                      breaks = c(seq(0, 115, by = 5), 120),
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
                         new_colname = NULL
  )
  
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

# fsl_hhs_score
# fsl_hhs_cat
# fsl_hhs_cat_ipc

raw_data_clean <- raw_data_clean |> 
  dplyr::select(-c(fsl_hhs_nofoodhh_recoded,fsl_hhs_nofoodhh_freq_recoded,fsl_hhs_sleephungry_recoded,fsl_hhs_sleephungry_freq_recoded,
                  fsl_hhs_alldaynight_recoded,fsl_hhs_alldaynight_freq_recoded))


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

# fsl_rcsi_score
# fsl_rcsi_cat

raw_data_clean <- raw_data_clean |> 
  humind::add_rcsi(fsl_rcsi_lessquality = "fsl_rcsi_lessquality",
                   fsl_rcsi_borrow = "fsl_rcsi_borrow",
                   fsl_rcsi_mealsize = "fsl_rcsi_mealsize",
                   fsl_rcsi_mealadult = "fsl_rcsi_mealadult",
                   fsl_rcsi_mealnb = "fsl_rcsi_mealnb")


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
                
raw_data_clean_to_listing <- raw_data_clean |> 
  dplyr::select(uuid,c_age_hoh_cat,c_gender_hoh,setting)

loop_data_clean<-loop_data_clean |> 
  dplyr::left_join(raw_data_clean_to_listing,by="uuid")
##Exportation des résultats

writexl::write_xlsx(x=list(
  raw_data_clean,
  loop_data_clean),
  path = paste0("output/MSNI/ner_msna_data_msni_",lubridate::today(),".xlsx"))

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


analyse_msni <- dplyr::bind_rows(analysis_admin0_res,analysis_admin0_loop_res) |> 
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




analyse_final_combine <- analyse_final_combine|> mutate(region_s = recode(),
                                                        departement_s = recode())

analyse_final_combine$region_pour_departement <- NA
analyse_final_combine$region_pour_departement[analyse_final_combine$departement_s =="Ville de Zinder"]<-"Zinder"

# Exportation
writexl::write_xlsx(analyse_final_combine,paste0("output/MSNI/ner_msna_msni_bulletin_",lubridate::today(),".xlsx"))














































