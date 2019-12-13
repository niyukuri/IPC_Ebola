

scorecard_evaluation_1st_cleaning <- function(data_score = data_score){
  
  # 1. Data on IPC evaluation -------------------
  
  ipc_data <- data_score[, c(112:195)]
  
  # rename columns
  names_ipc_data <- c( "pci_comp_1_presence_pf_equipe_pci_o", # -
                       "pci_comp_1_pf_comite_pci",
                       "pci_comp_1_pf_tdr_pci",
                       "pci_comp_1_pf_temps_taches_pci",
                       "pci_comp_1_pf_aucune_reponse",
                       
                       "pci_comp_2_triage_o", # -
                       "pci_comp_2_triage_thermoflash",
                       "pci_comp_2_triage_fiche_registre",
                       "pci_comp_2_triage_usage_correct_fiche_registre",
                       "pci_comp_2_triage_aucune_reponse",
                       
                       "pci_comp_3_zone_isolement_description_o", # - 
                       "pci_comp_3_zone_isolement_exist", # -
                       "pci_comp_3_zone_isolement_bien_identifie",
                       "pci_comp_3_zone_isolement_toilette",
                       "pci_comp_3_zone_isolement_comprent_lavage_main_epi",
                       "pci_comp_3_zone_isolement_aucune_reponse",
                       
                       "pci_comp_4_station_lavage_o", # - 
                       "pci_comp_4_station_lavage_et_eau_produits",
                       "pci_comp_4_station_lavage_personnel_pratique",
                       "pci_comp_4_station_lavage_affiches_techniques_hygiene",
                       "pci_comp_4_station_lavage_aucune_reponse",
                       
                       "pci_comp_5_epi_acces_pour_personel_connaissance_usage_o", # - 
                       "pci_comp_5_epi_acces_pour_personel_tout_moment_qte_suff_kits", # ok
                       "pci_comp_5_epi_acces_pour_personel_connaissance_usage_affichages", # ok
                       "pci_comp_5_epi_acces_pour_personel_connaissance_usage_etapes", # ok
                       "pci_comp_5_epi_acces_pour_personel_connaissance_usage_aucune_repose", # ok
                       
                       "pci_comp_5_epi_usage_standard_precaution_ebola_etapes", # - 
                       "pci_comp_5_epi_usage_standard_precaution_ebola_etapes_gants", # -
                       "pci_comp_5_epi_usage_standard_precaution_ebola_etapes_masque", # -
                       "pci_comp_5_epi_usage_standard_precaution_ebola_etapes_blouse", # -
                       "pci_comp_5_epi_usage_standard_precaution_ebola_etapes_tyvek", # -
                       "pci_comp_5_epi_usage_standard_precaution_ebola_etapes_ecran_facial", # -
                       "pci_comp_5_epi_usage_standard_precaution_ebola_etapes_tablier", # -
                       "pci_comp_5_epi_usage_standard_precaution_ebola_etapes_bottes", # -
                       
                       "pci_comp_6_dechets_tries_o", # - 
                       "pci_comp_6_dechets_tries_poubelles_affiches",
                       "pci_comp_6_dechets_tries_contenants",
                       "pci_comp_6_dechets_tries_dechets_dispose_types",
                       "pci_comp_6_dechets_tries_aucune_reponse",
                       
                       "pci_comp_7_dechets_bio_elimines_o", # - 
                       "pci_comp_7_dechets_bio_personnel_porte_epi",
                       "pci_comp_7_dechets_bio_incinerateur",
                       "pci_comp_7_dechets_bio_fosse_placenta",
                       "pci_comp_7_dechets_bio_aucune_reponse",
                       
                       "pci_comp_8_personnel_forme_pci_cette_annee_o", # - 
                       "pci_comp_8_personnel_forme_pci_precautions_standards",
                       "pci_comp_8_personnel_forme_pci_enregistre_person_forme",
                       "pci_comp_8_personnel_forme_pci_person_formation_continue",
                       "pci_comp_8_personnel_forme_pci_aucune_reponse",
                       
                       "pci_comp_9_systeme_gestion_cas_suspects_mve_o", # -
                       "pci_comp_9_systeme_gestion_cas_suspects_mve_numero_alerte_visible",
                       "pci_comp_9_systeme_gestion_cas_suspects_mve_depister_patient_hospitalises",
                       "pci_comp_9_systeme_gestion_cas_suspects_mve_patients_transfer_isolement_alerte",
                       "pci_comp_9_systeme_gestion_cas_suspects_mve_aucune_reponse",
                       
                       "pci_comp_10_personnel_kce_sterilisation_o", # - 
                       "pci_comp_10_materiel_sterilisation_mater_dispo",
                       "pci_comp_10_personnel_kce_sterilisation_sop",
                       "pci_comp_10_personnel_effectuant_sterilisation_forme",
                       "pci_comp_10_sterilisation_aucune_reponse",
                       
                       "pci_comp_11_mesure_nettoyage_desinfection_o", # -
                       "pci_comp_11_nettoyage_desinfection_sop",
                       "pci_comp_11_personnel_effectuant_nettoyage_desinfection_forme",
                       "pci_comp_11_personnel_effectuant_nettoyage_desinfection_porte_epi",
                       "pci_comp_11_nettoyage_desinfection_aucune_reponse",
                       
                       "pci_comp_12_prise_charge_suivi_personnel_expose_ebola_o", # - 
                       "pci_comp_12_prise_charge_suivi_personnel_expose_ebola_protocole",
                       "pci_comp_12_prise_charge_suivi_personnel_expose_ebola_clairement_definie_et_assuree",
                       "pci_comp_12_prise_charge_suivi_personnel_expose_ebola_alerte_equipe_intervention",
                       "pci_comp_12_prise_charge_suivi_personnel_expose_ebola_aucune_reponse",
                       
                       
                       "pci_commenatires",
                       
                       "score_c_1",
                       "score_c_2",
                       "score_c_3",
                       "score_c_4",
                       "score_c_5",
                       "score_c_6",
                       "score_c_7",
                       "score_c_8",
                       "score_c_9",
                       "score_c_10",
                       "score_c_11",
                       "score_c_12",
                       
                       "score_pci",
                       
                       "score_pci_100")
  
  
  names(ipc_data) <- names_ipc_data
  
  
  # remove unecessary columns
  
  remove_columns_ipc_data <- c("pci_comp_1_presence_pf_equipe_pci_o", 
                               "pci_comp_2_triage_o", 
                               "pci_comp_3_zone_isolement_description_o", 
                               "pci_comp_4_station_lavage_o", 
                               "pci_comp_5_epi_acces_pour_personel_connaissance_usage_o", 
                               "pci_comp_6_dechets_tries_o", 
                               "pci_comp_7_dechets_bio_elimines_o", 
                               "pci_comp_8_personnel_forme_pci_cette_annee_o", 
                               "pci_comp_9_systeme_gestion_cas_suspects_mve_o",  
                               "pci_comp_10_personnel_kce_sterilisation_o", 
                               "pci_comp_11_mesure_nettoyage_desinfection_o", 
                               "pci_comp_12_prise_charge_suivi_personnel_expose_ebola_o",
                               
                               
                               "pci_comp_3_zone_isolement_exist",
                               
                               "pci_comp_5_epi_usage_standard_precaution_ebola_etapes", # - 
                               "pci_comp_5_epi_usage_standard_precaution_ebola_etapes_gants",
                               "pci_comp_5_epi_usage_standard_precaution_ebola_etapes_masque",
                               "pci_comp_5_epi_usage_standard_precaution_ebola_etapes_blouse",
                               "pci_comp_5_epi_usage_standard_precaution_ebola_etapes_tyvek",
                               "pci_comp_5_epi_usage_standard_precaution_ebola_etapes_ecran_facial",
                               "pci_comp_5_epi_usage_standard_precaution_ebola_etapes_tablier",
                               "pci_comp_5_epi_usage_standard_precaution_ebola_etapes_bottes",
                               
                               "pci_commenatires")
  
  
  ipc_data_1 <- dplyr::select(ipc_data, -c(remove_columns_ipc_data))
  
  
  # Change scores per each criteria in numeric values not characters
  
  ipc_data_1$pci_comp_1_pf_comite_pci <- as.numeric(ipc_data_1$pci_comp_1_pf_comite_pci)
  ipc_data_1$pci_comp_1_pf_tdr_pci <-  as.numeric(ipc_data_1$pci_comp_1_pf_tdr_pci)
  ipc_data_1$pci_comp_1_pf_temps_taches_pci <-   as.numeric(ipc_data_1$pci_comp_1_pf_temps_taches_pci)
  
  ipc_data_1$pci_comp_2_triage_thermoflash  <- as.numeric(ipc_data_1$pci_comp_2_triage_thermoflash) 
  ipc_data_1$pci_comp_2_triage_fiche_registre  <-   as.numeric(ipc_data_1$pci_comp_2_triage_fiche_registre) 
  ipc_data_1$pci_comp_2_triage_usage_correct_fiche_registre <-  as.numeric(ipc_data_1$pci_comp_2_triage_usage_correct_fiche_registre)
  
  ipc_data_1$pci_comp_3_zone_isolement_bien_identifie  <- as.numeric(ipc_data_1$pci_comp_3_zone_isolement_bien_identifie)
  ipc_data_1$pci_comp_3_zone_isolement_toilette <-   as.numeric(ipc_data_1$pci_comp_3_zone_isolement_toilette)
  ipc_data_1$pci_comp_3_zone_isolement_comprent_lavage_main_epi <-  as.numeric(ipc_data_1$pci_comp_3_zone_isolement_comprent_lavage_main_epi)
  
  ipc_data_1$pci_comp_4_station_lavage_et_eau_produits <- as.numeric(ipc_data_1$pci_comp_4_station_lavage_et_eau_produits)
  ipc_data_1$pci_comp_4_station_lavage_personnel_pratique <-   as.numeric(ipc_data_1$pci_comp_4_station_lavage_personnel_pratique)
  ipc_data_1$pci_comp_4_station_lavage_affiches_techniques_hygiene <-  as.numeric(ipc_data_1$pci_comp_4_station_lavage_affiches_techniques_hygiene)
  
  ipc_data_1$pci_comp_5_epi_acces_pour_personel_tout_moment_qte_suff_kits  <-  as.numeric(ipc_data_1$pci_comp_5_epi_acces_pour_personel_tout_moment_qte_suff_kits) 
  ipc_data_1$pci_comp_5_epi_acces_pour_personel_connaissance_usage_affichages  <-  as.numeric(ipc_data_1$pci_comp_5_epi_acces_pour_personel_connaissance_usage_affichages) 
  ipc_data_1$pci_comp_5_epi_acces_pour_personel_connaissance_usage_etapes <-  as.numeric(ipc_data_1$pci_comp_5_epi_acces_pour_personel_connaissance_usage_etapes)
  
  ipc_data_1$pci_comp_6_dechets_tries_poubelles_affiches <- as.numeric(ipc_data_1$pci_comp_6_dechets_tries_poubelles_affiches) 
  ipc_data_1$pci_comp_6_dechets_tries_contenants  <-  as.numeric(ipc_data_1$pci_comp_6_dechets_tries_contenants) 
  ipc_data_1$pci_comp_6_dechets_tries_dechets_dispose_types <-  as.numeric(ipc_data_1$pci_comp_6_dechets_tries_dechets_dispose_types)
  
  ipc_data_1$pci_comp_7_dechets_bio_personnel_porte_epi <- as.numeric(ipc_data_1$pci_comp_7_dechets_bio_personnel_porte_epi) 
  ipc_data_1$pci_comp_7_dechets_bio_incinerateur  <-  as.numeric(ipc_data_1$pci_comp_7_dechets_bio_incinerateur) 
  ipc_data_1$pci_comp_7_dechets_bio_fosse_placenta <-  as.numeric(ipc_data_1$pci_comp_7_dechets_bio_fosse_placenta)
  
  ipc_data_1$pci_comp_8_personnel_forme_pci_precautions_standards  <- as.numeric(ipc_data_1$pci_comp_8_personnel_forme_pci_precautions_standards) 
  ipc_data_1$pci_comp_8_personnel_forme_pci_enregistre_person_forme  <-  as.numeric(ipc_data_1$pci_comp_8_personnel_forme_pci_enregistre_person_forme) 
  ipc_data_1$pci_comp_8_personnel_forme_pci_person_formation_continue <-  as.numeric(ipc_data_1$pci_comp_8_personnel_forme_pci_person_formation_continue)
  
  ipc_data_1$pci_comp_9_systeme_gestion_cas_suspects_mve_numero_alerte_visible <- as.numeric(ipc_data_1$pci_comp_9_systeme_gestion_cas_suspects_mve_numero_alerte_visible) 
  ipc_data_1$pci_comp_9_systeme_gestion_cas_suspects_mve_depister_patient_hospitalises  <-  as.numeric(ipc_data_1$pci_comp_9_systeme_gestion_cas_suspects_mve_depister_patient_hospitalises) 
  ipc_data_1$pci_comp_9_systeme_gestion_cas_suspects_mve_patients_transfer_isolement_alerte <- as.numeric(ipc_data_1$pci_comp_9_systeme_gestion_cas_suspects_mve_patients_transfer_isolement_alerte)
  
  ipc_data_1$pci_comp_10_materiel_sterilisation_mater_dispo <- as.numeric(ipc_data_1$pci_comp_10_materiel_sterilisation_mater_dispo) 
  ipc_data_1$pci_comp_10_personnel_kce_sterilisation_sop  <- as.numeric(ipc_data_1$pci_comp_10_personnel_kce_sterilisation_sop) 
  ipc_data_1$pci_comp_10_personnel_effectuant_sterilisation_forme <- as.numeric(ipc_data_1$pci_comp_10_personnel_effectuant_sterilisation_forme)
  
  ipc_data_1$pci_comp_11_nettoyage_desinfection_sop <- as.numeric(ipc_data_1$pci_comp_11_nettoyage_desinfection_sop) 
  ipc_data_1$pci_comp_11_personnel_effectuant_nettoyage_desinfection_forme  <-   as.numeric(ipc_data_1$pci_comp_11_personnel_effectuant_nettoyage_desinfection_forme) 
  ipc_data_1$pci_comp_11_personnel_effectuant_nettoyage_desinfection_porte_epi <-   as.numeric(ipc_data_1$pci_comp_11_personnel_effectuant_nettoyage_desinfection_porte_epi)
  
  ipc_data_1$pci_comp_12_prise_charge_suivi_personnel_expose_ebola_protocole  <- as.numeric(ipc_data_1$pci_comp_12_prise_charge_suivi_personnel_expose_ebola_protocole) 
  ipc_data_1$pci_comp_12_prise_charge_suivi_personnel_expose_ebola_clairement_definie_et_assuree  <-  as.numeric(ipc_data_1$pci_comp_12_prise_charge_suivi_personnel_expose_ebola_clairement_definie_et_assuree) 
  ipc_data_1$pci_comp_12_prise_charge_suivi_personnel_expose_ebola_alerte_equipe_intervention <- as.numeric(ipc_data_1$pci_comp_12_prise_charge_suivi_personnel_expose_ebola_alerte_equipe_intervention)
  
  
  
  # Sum scores per each IPC component 
  
  ipc_data_1$score_c_1 <- as.numeric(ipc_data_1$pci_comp_1_pf_comite_pci) +
    as.numeric(ipc_data_1$pci_comp_1_pf_tdr_pci) +
    as.numeric(ipc_data_1$pci_comp_1_pf_temps_taches_pci)
  
  ipc_data_1$score_c_2 <- as.numeric(ipc_data_1$pci_comp_2_triage_thermoflash) +
    as.numeric(ipc_data_1$pci_comp_2_triage_fiche_registre) +
    as.numeric(ipc_data_1$pci_comp_2_triage_usage_correct_fiche_registre)
  
  ipc_data_1$score_c_3 <- as.numeric(ipc_data_1$pci_comp_3_zone_isolement_bien_identifie) +
    as.numeric(ipc_data_1$pci_comp_3_zone_isolement_toilette) +
    as.numeric(ipc_data_1$pci_comp_3_zone_isolement_comprent_lavage_main_epi)
  
  ipc_data_1$score_c_4 <- as.numeric(ipc_data_1$pci_comp_4_station_lavage_et_eau_produits) +
    as.numeric(ipc_data_1$pci_comp_4_station_lavage_personnel_pratique) +
    as.numeric(ipc_data_1$pci_comp_4_station_lavage_affiches_techniques_hygiene)
  
  ipc_data_1$score_c_5 <- as.numeric(ipc_data_1$pci_comp_5_epi_acces_pour_personel_tout_moment_qte_suff_kits) +
    as.numeric(ipc_data_1$pci_comp_5_epi_acces_pour_personel_connaissance_usage_affichages) +
    as.numeric(ipc_data_1$pci_comp_5_epi_acces_pour_personel_connaissance_usage_etapes)
  
  ipc_data_1$score_c_6 <- as.numeric(ipc_data_1$pci_comp_6_dechets_tries_poubelles_affiches) +
    as.numeric(ipc_data_1$pci_comp_6_dechets_tries_contenants) +
    as.numeric(ipc_data_1$pci_comp_6_dechets_tries_dechets_dispose_types)
  
  ipc_data_1$score_c_7 <- as.numeric(ipc_data_1$pci_comp_7_dechets_bio_personnel_porte_epi) +
    as.numeric(ipc_data_1$pci_comp_7_dechets_bio_incinerateur) +
    as.numeric(ipc_data_1$pci_comp_7_dechets_bio_fosse_placenta)
  
  ipc_data_1$score_c_8 <- as.numeric(ipc_data_1$pci_comp_8_personnel_forme_pci_precautions_standards) +
    as.numeric(ipc_data_1$pci_comp_8_personnel_forme_pci_enregistre_person_forme) +
    as.numeric(ipc_data_1$pci_comp_8_personnel_forme_pci_person_formation_continue)
  
  ipc_data_1$score_c_9 <- as.numeric(ipc_data_1$pci_comp_9_systeme_gestion_cas_suspects_mve_numero_alerte_visible) +
    as.numeric(ipc_data_1$pci_comp_9_systeme_gestion_cas_suspects_mve_depister_patient_hospitalises) +
    as.numeric(ipc_data_1$pci_comp_9_systeme_gestion_cas_suspects_mve_patients_transfer_isolement_alerte)
  
  ipc_data_1$score_c_10 <- as.numeric(ipc_data_1$pci_comp_10_materiel_sterilisation_mater_dispo) +
    as.numeric(ipc_data_1$pci_comp_10_personnel_kce_sterilisation_sop) +
    as.numeric(ipc_data_1$pci_comp_10_personnel_effectuant_sterilisation_forme)
  
  ipc_data_1$score_c_11 <- as.numeric(ipc_data_1$pci_comp_11_nettoyage_desinfection_sop) +
    as.numeric(ipc_data_1$pci_comp_11_personnel_effectuant_nettoyage_desinfection_forme) +
    as.numeric(ipc_data_1$pci_comp_11_personnel_effectuant_nettoyage_desinfection_porte_epi)
  
  ipc_data_1$score_c_12 <- as.numeric(ipc_data_1$pci_comp_12_prise_charge_suivi_personnel_expose_ebola_protocole) +
    as.numeric(ipc_data_1$pci_comp_12_prise_charge_suivi_personnel_expose_ebola_clairement_definie_et_assuree) +
    as.numeric(ipc_data_1$pci_comp_12_prise_charge_suivi_personnel_expose_ebola_alerte_equipe_intervention)
  
  
  # Total score
  
  ipc_data_1$score_pci <- rowSums(ipc_data_1[,c("score_c_1", "score_c_2",
                                                "score_c_3", "score_c_4",
                                                "score_c_5", "score_c_6",
                                                "score_c_7", "score_c_8",
                                                "score_c_9", "score_c_10",
                                                "score_c_11", "score_c_12")], na.rm=TRUE)
  
  
  
  # Percentage of score
  
  ipc_data_1$score_pci_100 <- round((ipc_data_1$score_pci / 36) * 100, digits = 2)
  
  
  
  
  
  # 2. Data on FOSA location and partners ------------------
  
  data_fosa <- data_score[, c(16:109, 319:323)] # fosa et gelolocalisation
  
  
  
  names_data_fosa <- c("province", 
                       "zone_de_sante",
                       "aire_de_sante",
                       "nom_fosa",
                       "autre_nom_fosa",
                       "type_structure_de_sante",
                       "autre_type_structure_de_sante", # *
                       "categorie",
                       "secteur",
                       "autre_secteur", # *
                       
                       "nbre_lits_total",
                       "nbre_lits_occupes",
                       
                       "appuyee_par_partenaire",
                       
                       "nbre_partenaires",
                       "noms_partnaires",
                       "partenaire_adeco",
                       "partenaire_alima",
                       "partenaire_cac",
                       "partenaire_care_international",
                       "partenaire_caritas",
                       "partenaire_caritas_butembo",
                       "partenaire_cbca",
                       "partenaire_cdc_africa",
                       "partenaire_ceprossan",
                       "partenaire_cordaid",
                       "partenaire_famamundi",
                       "partenaire_fardc",
                       "partenaire_ima",
                       "partenaire_imc",
                       "partenaire_imt_anvers",
                       "partenaire_inrb",
                       "partenaire_institut_pasteur_de_dakar_ipd", 
                       "partenaire_irc",
                       "partenaire_irc_montpelier",
                       "partenaire_jica",
                       "partenaire_medair",
                       "partenaire_mercy_corps",
                       "partenaire_ministere_de_la_sante",
                       "partenaire_monusco",
                       "partenaire_mouvement_croix_rouge",
                       "partenaire_msf",
                       "partenaire_nih",
                       "partenaire_oim",
                       "partenaire_oms",
                       "partenaire_ong_musaka",
                       "partenaire_oxfam",
                       "partenaire_pam",
                       "partenaire_pap_rdc",
                       "partenaire_pdss",
                       "partenaire_pnc",
                       "partenaire_ppsp",
                       "partenaire_predict_metabiota",
                       "partenaire_premier_secours",
                       "partenaire_protection_civile",
                       "partenaire_racoj",
                       "partenaire_rb_fondation",
                       "partenaire_save_the_children",
                       "partenaire_sfcg",
                       "partenaire_sos_eau_et_foret",
                       "partenaire_tearfund",
                       "partenaire_ucla_drc",
                       "partenaire_umir",
                       "partenaire_unesco",
                       "partenaire_unfpa",
                       "partenaire_unhas",
                       "partenaire_unicef",
                       "partenaire_usamrid",
                       "partenaire_world_vision",
                       "partenaire_autre", 
                       "autre_partenaire", # *
                       
                       "type_support",
                       "type_support_gouvernance",
                       "type_support_surveillance",
                       "type_support_fourniture_medicale_et_intrants",
                       "type_support_appui_personnel_de_sante",
                       "type_support_formation_personnel_de_sante",
                       "type_support_prestation_de_service_de_sante",
                       "type_support_autre",
                       "autre_type_support", # *
                       
                       "lister_nbre_staff_categories",
                       "medecin_generaliste",
                       "chirurgien",
                       "gyneco_obst",
                       "interniste",
                       "pediatre",
                       "sage_femme",
                       "infirmier",
                       "technicien_lab",
                       "technicien_imagerie",
                       "pharmacien",
                       "assistant_pharmacie",
                       
                       "personnel_forme_pci",
                       "personnes_vaccines",
                       "autre_type_categories", # -
                       
                       "coordonnees_fosa", # -
                       "coordonnees_latitude_fosa",
                       "coordonnees_longitude_fosa",
                       "coordonnees_altitude_fosa",
                       "coordonnees_fosa_precision")
  
  
  
  names(data_fosa) <- names_data_fosa
  
  
  # Formatting coordonnees
  
  # longitude
  data_fosa$coordonnees_longitude_fosa <- sub("_", ".", data_fosa$coordonnees_longitude_fosa)
  
  data_fosa$coordonnees_longitude_fosa <- as.numeric(data_fosa$coordonnees_longitude_fosa)
  
  
  # latitude
  data_fosa$coordonnees_latitude_fosa <- sub("_", ".", data_fosa$coordonnees_latitude_fosa)
  
  data_fosa$coordonnees_latitude_fosa <- as.numeric(data_fosa$coordonnees_latitude_fosa)
  
  
  # altitude
  data_fosa$coordonnees_altitude_fosa <- sub("_", ".", data_fosa$coordonnees_altitude_fosa)
  
  data_fosa$coordonnees_altitude_fosa <- as.numeric(data_fosa$coordonnees_altitude_fosa)
  
  
  
  
  remove_columns_data_fosa <- c("autre_type_categories",
                                "coordonnees_fosa",
                                "coordonnees_fosa_precision")
  
  
  data_fosa_1 <- dplyr::select(data_fosa, -c(remove_columns_data_fosa))
  
  
  
  # Numeric values for number of partners
  
  data_fosa_1$nbre_partenaires <- as.numeric(data_fosa_1$nbre_partenaires)
  
  
  
  # 3. Data on the questionnaire responder and inteviewer ---------------
  
  data_questionnaire <- data_score[, c(1, 4, 328:330, 13, 14, 8, 9, 11, 12)] # fosa et gelolocalisation
  
  
  name_data_questionnaire <- c("date_enquete", 
                               "device_enquete", 
                               "submission_date",
                               "validation_status", 
                               "index", 
                               "nom_enqueteur", 
                               "numero_phone_enqueteur",
                               "nom_du_repondant", 
                               "fonction_personne_repondant", 
                               "numero_phone_repondant",
                               "email_repondant")
  
  
  names(data_questionnaire) <- name_data_questionnaire
  
  
  # Date formatting
  
  data_questionnaire$date_enquete <- parsedate::parse_date(data_questionnaire$date_enquete)
  data_questionnaire$submission_date <- parsedate::parse_date(data_questionnaire$submission_date)
  
  
  data_questionnaire$date_enquete <- lubridate::as_date(data_questionnaire$date_enquete)
  data_questionnaire$submission_date <- lubridate::as_date(data_questionnaire$submission_date)
  
  
  
  # 4. Combine all data together -------------
  
  
  data_scorecard_clean <- cbind(data_questionnaire, data_fosa_1, ipc_data_1)
  
  
  
  
  # 5. Cleaning ---------
  
  
  # 5.1. Remove data with Health Zone being NA 
  
  index_hz <- which(is.na(data_scorecard_clean$zone_de_sante))
  
  if(length(index_hz) >= 1){
    data_scorecard_clean_df <- data_scorecard_clean[-c(index_hz),]
  }else{
    data_scorecard_clean_df <- data_scorecard_clean
  }
  
  
  
  # 5.2. Dealing with "autre" : autre_fosa, autre_type_structure_de_sante
  
  # we bring the name which is in autre_fosa to fosa
  
  data_scorecard_clean_df_fosa <-  data_scorecard_clean_df
  
  index_autre_fosa <- which(data_scorecard_clean_df_fosa$nom_fosa=='autre_fosa')
  
  data_scorecard_clean_df_fosa_autre <- data_scorecard_clean_df_fosa[c(index_autre_fosa),]
  
  data_scorecard_clean_df_fosa_with_fosa_name <- data_scorecard_clean_df_fosa[-c(index_autre_fosa),]
  
  data_scorecard_clean_df_fosa_autre$nom_fosa <- data_scorecard_clean_df_fosa_autre$autre_nom_fosa
  
  data_nziza <- rbind(data_scorecard_clean_df_fosa_with_fosa_name, data_scorecard_clean_df_fosa_autre) 
  
  
  index_fosa <- which(is.na(data_nziza$nom_fosa)) 
  
  if(length(index_fosa) >= 1){
    data_nziza <- data_nziza[-c(index_fosa),]
  }else{
    data_nziza <- data_nziza
  }
  
  
  
  
  # 5.3. Define variable for partner presence or not
  
  data_nziza$presence_partenaire <- rep(NA, nrow(data_nziza))
  
  for(i in 1:nrow(data_nziza)){
    
    if(is.na(data_nziza$nbre_partenaires[i])){
      
      data_nziza$presence_partenaire[i] <- "NO"
      
    }else{
      
      data_nziza$presence_partenaire[i] <- "OUI"
      
    }
    
  }
  
  
  
  # Partners
  
  index_na_part <- which(is.na(data_nziza$appuyee_par_partenaire))
  
  for(j in 1:length(index_na_part)){
    
    data_nziza$appuyee_par_partenaire[index_na_part[j]] <- "non"
    
  }
  
  
  # Renaming type of FOSA
  
  data_nziza$type_structure_de_sante[data_nziza$type_structure_de_sante=="centre_de_sante_de_reference"] <- "CS_ref" # --
  data_nziza$type_structure_de_sante[data_nziza$type_structure_de_sante=="hopital_general_de_reference"] <- "HG_ref" # --
  data_nziza$type_structure_de_sante[data_nziza$type_structure_de_sante=="tradi_praticien_tradi_moderne"] <- "Tradi_mod"
  data_nziza$type_structure_de_sante[data_nziza$type_structure_de_sante=="poste_de_sante"] <- "PS"
  data_nziza$type_structure_de_sante[data_nziza$type_structure_de_sante=="centre_de_sante"] <- "CS"
  data_nziza$type_structure_de_sante[data_nziza$type_structure_de_sante=="centre_hospitalier"] <- "CH"
  data_nziza$type_structure_de_sante[data_nziza$type_structure_de_sante=="dispensaire"] <- "Disp."
  data_nziza$type_structure_de_sante[data_nziza$type_structure_de_sante=="clinique"] <- "Clin."
  
  
  # 5.4. Define unique name for FOSa due to the fact there are cases 
  # where you find same name of a FOSA in different Aires de Sante
  
  data_nziza$nom_fosa_new <- rep(NA, nrow(data_nziza))
  
  for(j in 1:nrow(data_nziza)){
    
    data_nziza$nom_fosa_new[j] <- paste0(data_nziza$nom_fosa[j],"_", data_nziza$zone_de_sante[j], "_", data_nziza$aire_de_sante[j])
    
  }
  
  
  return(data_nziza)
  
  
}
