
# Ebola Response Team in DRC
# WHO & MoH
# IPC Interventions

# Objective: Get IPC gaps after evaluation FOSA
# Data source: Scorecard
# External scripts: 2 utility functions

# Author: David Niyukuri (niyukuri@aims.ac.za & niyukurud@who.int)
# With support: Landry Kabego (megaglk3@gmail.com)



# After evaluating Healthcare Faclities in different locations, we need to pull the gaps
# to set-up a operational action plan (plan d'action operationel)
# The outputs are: operational action plan, and the frequencies of gaps per Health Zone

# The scorecard database has details on Healthcare Facility identification, and scores on the twelves IPC components
# each with 3 criteria


# suppressMessages(library(readxl))
suppressMessages(library(dplyr))
suppressMessages(library(lubridate))
suppressMessages(library(linelist))



# read scripts from Utility_Functions folder

path_to_scripts <- here::here("Utility_Functions")
scripts_files <- dir(path_to_scripts, pattern = ".R$", full.names=TRUE)
for (file in scripts_files) source(file)



# path to raw data: you change accordingly to your data folder

path_to_data <- here::here("data", "scorecard-2019-12-13-17-30-48.xlsx") 


# get data file

data_kobo <- rio::import(path_to_data,
                         guess_max = 1e4) %>%
  clean_data(guess_dates = FALSE) %>%
  as_tibble()


# Note: we have two forms to colelct the IPC data: baseline and scorecard



# data with IPC scorecard form
data_scorecard <- dplyr::filter(data_kobo, data_kobo$veuillez_selectionner_le_questionnaire=="ipc_scorecard")

# data with baseline form
data_baseline <- dplyr::filter(data_kobo, data_kobo$veuillez_selectionner_le_questionnaire=="baseline")


# Data for IPC score analysis: the combination of the data from the two forms
data_score <- rbind(data_scorecard, data_baseline)



# scorecard_evaluation_1st_cleaning script

data_nziza <- scorecard_evaluation_1st_cleaning(data_score = data_score)


# Consider recent evaluation ONLY


data_nziza_descriptive_recent <- recent_evaluation_only_fosa(data_hz = data_nziza)


# Partner status

data_nziza_descriptive_recent$partner_status <- rep(NA, nrow(data_nziza_descriptive_recent))

for(i in 1:nrow(data_nziza_descriptive_recent)){
  
  if(is.na(data_nziza_descriptive_recent$nbre_partenaires[i])){
    
    data_nziza_descriptive_recent$partner_status[i] <- "no"
    
  }else{
    
    data_nziza_descriptive_recent$partner_status[i] <- "yes"
    
  }
  
}




# Action plan automation -----------------

criteres_pci <- c("Critere 1: Point focal hygiène ou Comité hygiène/PCI en place",
                  "Critere 2: Triage en place",
                  "Critere 3: Identification d’une zone d’isolement/attente",
                  "Critere 4: Lavage des mains/Stations pour l’hygiène des mains",
                  "Critere 5: Disponibilité et usage des Équipements de Protection Individuel (EPI)",
                  "Critere 6: Tri des déchets",
                  "Critere 7: Élimination des déchets",
                  "Critere 8: Formation du personnel",
                  "Critere 9: Alerte des cas suspect intra-hospitalier (au niveau des FOSA)",
                  "Critere 10: Stérilisation",
                  "Critere 11: Bio-nettoyage de l’environnement du patient",
                  "Critere 12: Exposition d’un agent de santé au virus Ébola")



gaps <- c("L’établissement n'a pas de PF ou un Comité d’hygiène/PCI avec responsabilité, imputabilité et autorité", 
          "TDR n'est pas disponible et le PF ne les connais pas bien",
          "Le PF n'a pas du temps alloué pour effectuer ses tâches en PCI",
          
          "Température et symptômes de MVE ne sont pas vérifiés correctement et/ou Thermo flash non fonctionnel",
          "Fiche de triage et/ou registre non disponibles",
          "Non utilisation ou utilisation incorrecte de la fiche et du registre de triage",
          
          "Zone d'isolement non identifiée ou mauvais positionnement de la zone",
          "Pas de Latrines/toilettes dédiées dans la zone d'isolement ou pas de présence d’un bassin de lit/ urinoir",
          "L'un des éléments suivant manque dans l'espace d'isolation : une station de lavage des mains, des fournitures (EPI, un lit, bassin/ urinoir, etc.), une zone pour mettre les EPI et une zone pour enlever les EPI)",
          
          "Pas d'Eau propre + savon et/ou; Solution hydro-alcoolique et/ou; Solution d’eau chlorée 0.05%",
          "Le personnel n'est pas capable d’effectuer l’hygiène des mains correctement (selon technique OMS)",
          "Pas de posters sur les différentes techniques d’hygiène des mains au niveau de chaque station de LVM",
          
          "Les EPI ne sont pas accessible au personnel en tout moment et/ou en quantité suffisante dans la salle d’habillage",
          "Pas de posters pour les précautions standards et MVE sur comment mettre et enlever les EPI",
          "Le personnel n'est pas en mesure de mettre et retirer les EPI (précautions standards et MVE) en suivant correctement l’ensemble des étapes",
          
          "Pas des poubelles étanches, couvertes et étiquetées (infectieux ou non-infectieux) ni les affiches sur la gestion des déchets dans tous ou certains points de service aux patients",
          "Les contenants pour objets piquants/ tranchants ne sont pas disponibles à tous les points d'utilisation",
          "Les déchets ne sont pas triés selon les types de déchets (e.g. indiqué par des couleurs) : Infectieux, non-infectieux, piquants/tranchants",
          
          "Le personnel ne porte pas les EPI appropriés (gants en latex ou en nitrile, gants de ménage, lunettes de protection, bottes en caoutchouc, tabliers et masques de protection) lors de la manipulation des déchets",
          "Les déchets ne sont pas brûlés sur place dans un incinérateur et/ou il n'existe pas un système existe pour leur transport dans un autre endroit approprié",
          "Pas de fosse à placenta ou déchet organique",
          
          "Une partie ou tout le personnel n'a pas été formé sur les précautions standards, les précautions additionnelles (périodes pratiques et théoriques) et un accent sur Ébola et/ou fièvres hémorragiques dans les derniers 6 mois",
          "Pas de registre tenu contenant le nom des prestataires de soin qui ont reçu la formation, la date, le type de formation et l’organisme qui a donné la formation.",
          "Le prestataire de soin ne reçoit pas de formation continue à travers la supervision sur place",
          
          "Le numéro d'aletre n'est pas connu et n'est pas visible",
          "Les patients hospitalisés ne sont pas dépistés",
          "Une fois identifiés, les cas suspects ne sont pas déplacés vers la zone d'isolement/de transit et/ ou une alerte n'est pas  déclenchée",
          
          "Matériel de stérilisation n'est pas disponible tel que Autoclave, poupinel et accessoires nécessaires à la stérilisation, EPI)",
          "SOP sur la stérilisation n'est pas disponible",
          "Le personnel effectuant la stérilisation n'est pas formé",
          
          "SOP  sur le bio-nettoyage/desinfection n'est pas disponible",
          "Le personnel effectuant le nettoyage et la désinfection n'a pas été formé",
          "Le personnel de nettoyage ne  porte pas les EPI appropriés (gants en latex ou en nitrile, lunettes de protection, bottes en caoutchouc, tabliers et masques de protection)",
          
          "Pas de protocole d’évaluation et prise en charge en cas d’exposition (incluant un registre, les outils d’évaluation, communication, etc.)",
          "La prise en charge du personnel de santé exposé n'est pas clairement définie et assurée",
          "L’équipe d’investigation n'est pas alertée afin de proceder à l'inverstigation lorsqu'un agent de santé est exposé")





action_plan <- c("Identifier un point focal ou Comité d'hygiène PCI",
                 "Donner les TDR au point focal PCI et les lui expliquer",
                 "S'assurer que le point focal en place a du temps alloué pour cette activité",
                 
                 "Briefer le personnel sur la prise de température et doter un thermo flash fonctionnel",
                 "Rendre disponible les fiches et régistre de triage",
                 "Briefer le personnel sur l'utilisation de la fiche et régistre de triage",
                 
                 "Identifier une zone d'isolement appropriée/ assigner un partenaire à la construction de la zone d'isolement",
                 "S'assurer de la présence des latrines/toilettes et/ou bassin de lit dans la zone d'isolement",
                 "Doter: station de lavage des mains, des fournitures (EPI, un lit, bassin/urinoir, etc.), une zone pour",
                 
                 "Doter: une station de lavage de mains à tous les points de soins",
                 "Briefer le personnel sur la technique de lavage des mains (selon OMS)",
                 "Placer les posters sur les différentes techniques d'hygiène des mains au niveau de chaque station de lavage des mains",
                 
                 "Doter un kit PCI afin de rendre accessible les EPI",
                 "Placer les posters sur les précautions standards et MVE sur le port et retrait des EPI",
                 "Briefer le personnel sur le port et retrait des EPI",
                 
                 "Placer des poubelles étanches couvertes et étiquetées (infectieux, non-infectieux), placer des affiches sur la gestion des déchets dans tous les points de services aux patients",
                 "Rendre disponibles les contenants pour objets piquants/ tranchants ne sont pas disponibles à tous les points d'utilisation",
                 "Briefer sur le trie des déchets: infectieux, non-infectieux, piquants/tranchants",
                 
                 "Briefer le personnel sur le port d'EPI appropriés lors de la manipulation des déchets",
                 "Doter la FOSA en bruleurs ou incinérateur; ou mettre en place un système de transport de déchets dans un endroit approprié",
                 "Mettre en place un fosse à placenta ou déchet organique",
                 
                 "Identifier le personnel qui n'a pas été formé en précautions standards et additionnelles et programmer la formation",
                 "Doter un registre qui servira d'enregistrer tous les personnels formés, la date de formation, le type de formation et l'organisme qui a formé",
                 "S'assurer que les prestataires de soins reçoivent une formation continue à travers la supervision formative sur place",
                 
                 "Afficher le numéro d'alerte dans les lieux de soins et s'assurer que tout prestataire de soin en prend connaissance",
                 "Briefer le personnel sur le depistage des patients hospitalisés, rendre disponible les SOP sur la surveillance intra-hospitalière",
                 "Briefer sur l'isolement des cas suspects après identification et déclanchement de l'alerte",
                 
                 "Doter la FOSA en materiel de stérilisation et EPI",
                 "Rendre disponible les SOP sur la stérilisation et s'assurer que le personnel le comprend",
                 "Programmer la formation du personnel sur la stérilisation",
                 
                 "Rendre disponible les SOP sur le bio-nettoyage de l'environnement",
                 "Programmer la formation  du personnel sur le bi-néttoyage",
                 "Briefer le personnel de nettoyage sur le port d'EPI appropriés lors du bio-nettoyage",
                 
                 "Mettre en place un protocole d'évaluation et de prise en charge en cas d'exposition d'un personnel de soin (incluant un registre, les outils d'évaluation, communication,etc)",
                 "Définir clairement la prise en charge du personnel de santé exposé",
                 "Mettre en place un système d'alerte afin que l'équipe d'investigation puisse proceder à l'investigation lorsqu'un agent de santé est exposé")




pci_criteres <- c("L’établissement a un PF ou un Comité d’hygiène/PCI avec responsabilité, imputabilité et autorité",
                  "TDR disponible et le PF les connais bien",
                  "Le PF a du temps alloué pour effectuer ses tâches en PCI",
                  
                  "Température et symptômes de MVE sont vérifiés correctement - Thermo flash fonctionnel",
                  "Fiche de triage et registre disponibles",
                  "Utilisation correcte de la fiche et du registre de triage",
                  
                  "Zone bien identifiée « Isolement » et à l’écart des autres unités/service",
                  "Latrines / toilettes dédiées dans la zone d'isolement ou présence d’un bassin de lit/ urinoir",
                  "L'espace d'isolation comprend: une station de lavage des mains, des fournitures (EPI, un lit, bassin/ urinoir, etc.), une zone pour mettre les EPI et une zone pour enlever les EPI)",
                  
                  "Devrait inclure (Eau propre + savon et/ou; Solution hydro-alcoolique et/ou; Solution d’eau chlorée 0.05% [si les deux précédents ne sont pas disponible]",
                  "Le personnel est capable d’effectuer l’hygiène des mains correctement (selon technique OMS)",
                  "Présence de posters sur les différentes techniques d’hygiène des mains au niveau de chaque station de LVM",
                  
                  
                  "EPI accessible au personnel en tout moment et en quantité suffisante dans la salle d’habillage",
                  "Présence de posters (précautions standards et MVE) MSP/0MS sur comment mettre et enlever les EPI",
                  "Le personnel est en mesure de mettre et retirer les EPI (précautions standards et MVE) en suivant correctement l’ensemble des étapes",
                  
                  "Des poubelles étanches, couvertes et étiquetées (infectieux ou non-infectieux) et les affiches sur la gestion des déchets sont disponibles dans tous les points de service aux patients)",
                  "Des contenants pour objets piquants/ tranchants sont disponibles à tous les points d'utilisation",
                  "Les déchets sont triés selon les types de déchets (e.g. indiqué par des couleurs) : Infectieux, non-infectieux, piquants/tranchants",
                  
                  "Le personnel porte les EPI appropriés (gants en latex ou en nitrile, gants de ménage, lunettes de protection, bottes en caoutchouc, tabliers et masques de protection) lors de la manipulation des déchets",
                  "Les déchets sont brûlé sur place dans un incinérateur ou un système existe pour leur transport dans un autre endroit approprié",
                  "Une fosse à placenta ou déchet organique est présent lorsque requis",
                  
                  "L’ensemble du personnel a été formé au minimum sur les précautions standards, les précautions additionnelles (périodes pratiques et théoriques) et un accent sur Ébola et/ou fièvres hémorragiques dans les derniers 6 mois",
                  "Un registre est tenu contenant le nom des prestataires de soin qui ont reçu la formation, la date, le type de formation et l’organisme qui a donné la formation.",
                  "Le prestataire de soin reçoit une formation continue à travers la supervision sur place",
                  
                  "Un numéro d’alerte est connu et visible",
                  "Les patients hospitalisés sont dépistés au moins deux fois par jour pour identifier les cas suspects",
                  "Une fois identifiés, les cas suspects sont déplacés vers la zone d'isolement/de transit et une alerte est déclenchée",
                  
                  "Matériel de stérilisation disponible tel que Autoclave, poupinel et accessoires nécessaires à la stérilisation, EPI)",
                  "SOP disponible sur comment effectuer la stérilisation du matériels/équipements",
                  "Le personnel effectuant la stérilisation a été formé",
                  
                  "SOP disponible sur comment effectuer le nettoyage /désinfection lorsqu’il y a des liquides corporels ou déversements de sang et le nettoyage et décontamination du matériel réutilisable",
                  "Le personnel effectuant le nettoyage et la désinfection a été formé",
                  "Le personnel de nettoyage porte les EPI appropriés (gants en latex ou en nitrile, lunettes de protection, bottes en caoutchouc, tabliers et masques de protection)",
                  
                  "Un protocole d’évaluation et prise en charge en cas d’exposition est en place (incluant un registre, les outils d’évaluation, communication, etc.)",
                  "La prise en charge du personnel de santé exposé est clairement définie et assurée",
                  "L’équipe d’investigation est alertée et procède à l’investigation lorsqu’un agent de santé est exposé")


# 
# d = c(k_1, k_2, k_3, k_4, k_5, k_6, 
#       k_7, k_8, k_9, k_10, k_11, k_12, 
#       k_13, k_14, k_15, k_16, k_17, k_18, 
#       k_19, k_20, k_21, k_22, k_23, k_24, 
#       k_25, k_26, k_27, k_28, k_29, k_30, 
#       k_31, k_32, k_33, k_34, k_35, k_36)




dat_i <- data_nziza_descriptive_recent  


dat_i_keep <- dat_i[, c(13:15, 19, 20, 25, 26, 1, 6,7, 8:10, 169)]



dat_pao_hf <- data.frame(matrix(ncol = 15, nrow = 0)) # for all HF in one HZ


colnames(dat_pao_hf) <- c("zone_de_sante", "aire_de_sante", "nom_fosa",                        
                          "categorie", "secteur", "date_enquete",                    
                          "nom_enqueteur", "numero_phone_enqueteur", "nom_du_repondant",                
                          "fonction_personne_repondant", "numero_phone_repondant", "score_pci_100",                   
                          "Gaps identifies", "Plan d'Action", "Critere qui etait non applicable")




for(i in 1:nrow(dat_i)){
  
  gaps_vector <- vector()
  action_vector <- vector()
  commentaires_epi <- vector() # pci_criteres
  criteres_pci_num <- vector()
  
  
  dat_pao_hf_i <- data.frame(matrix(ncol = 15, nrow = 0))  # for one HF 
  
  
  colnames(dat_pao_hf_i) <- c("zone_de_sante", "aire_de_sante", "nom_fosa",                        
                              "categorie", "secteur", "date_enquete",                    
                              "nom_enqueteur", "numero_phone_enqueteur", "nom_du_repondant",                
                              "fonction_personne_repondant", "numero_phone_repondant", "score_pci_100",                   
                              "Gaps identifies", "Plan d'Action", "Critere qui etait non applicable")
  
  dat_i_keep_i <- dat_i_keep[i,]
  
  
  k_1 <- as.numeric(dat_i$pci_comp_1_pf_comite_pci[i])
  
  
  if(is.na(k_1)){
    gaps_vector <- c(gaps_vector, NA)
    action_vector <- c(action_vector, NA)
    commentaires_epi <- c(commentaires_epi, pci_criteres[1])
    criteres_pci_num <- c(criteres_pci_num, criteres_pci[1])
  }else{
    if(k_1==0){
      
      gaps_vector <- c(gaps_vector, gaps[1])
      action_vector <- c(action_vector, action_plan[1])
      commentaires_epi <- c(commentaires_epi, NA)
      criteres_pci_num <- c(criteres_pci_num, criteres_pci[1])
      
    }
  }
  
  
  
  k_2 <- as.numeric(dat_i$pci_comp_1_pf_tdr_pci[i])
  
  if(is.na(k_2)){
    gaps_vector <- c(gaps_vector, NA)
    action_vector <- c(action_vector, NA)
    commentaires_epi <- c(commentaires_epi, pci_criteres[2])
    criteres_pci_num <- c(criteres_pci_num, criteres_pci[1])
  }else{
    if(k_2==0){
      
      gaps_vector <- c(gaps_vector, gaps[2])
      action_vector <- c(action_vector, action_plan[2])
      commentaires_epi <- c(commentaires_epi, NA)
      criteres_pci_num <- c(criteres_pci_num, criteres_pci[1])
      
    }
  }
  
  
  k_3 <- as.numeric(dat_i$pci_comp_1_pf_temps_taches_pci[i])
  
  if(is.na(k_3)){
    gaps_vector <- c(gaps_vector, NA)
    action_vector <- c(action_vector, NA)
    commentaires_epi <- c(commentaires_epi, pci_criteres[3])
    criteres_pci_num <- c(criteres_pci_num, criteres_pci[1])
  }else{
    if(k_3==0){
      
      gaps_vector <- c(gaps_vector, gaps[3])
      action_vector <- c(action_vector, action_plan[3])
      commentaires_epi <- c(commentaires_epi, NA)
      criteres_pci_num <- c(criteres_pci_num, criteres_pci[1])
      
    }
  }
  
  
  
  k_4 <- as.numeric(dat_i$pci_comp_2_triage_thermoflash[i])
  
  if(is.na(k_4)){
    gaps_vector <- c(gaps_vector, NA)
    action_vector <- c(action_vector, NA)
    commentaires_epi <- c(commentaires_epi, pci_criteres[4])
    criteres_pci_num <- c(criteres_pci_num, criteres_pci[2])
  }else{
    if(k_4==0){
      
      gaps_vector <- c(gaps_vector, gaps[4])
      action_vector <- c(action_vector, action_plan[4])
      commentaires_epi <- c(commentaires_epi, NA)
      criteres_pci_num <- c(criteres_pci_num, criteres_pci[2])
      
    }
  }
  
  
  
  k_5 <- as.numeric(dat_i$pci_comp_2_triage_fiche_registre[i])
  
  if(is.na(k_5)){
    gaps_vector <- c(gaps_vector, NA)
    action_vector <- c(action_vector, NA)
    commentaires_epi <- c(commentaires_epi, pci_criteres[5])
    criteres_pci_num <- c(criteres_pci_num, criteres_pci[2])
  }else{
    if(k_5==0){
      
      gaps_vector <- c(gaps_vector, gaps[5])
      action_vector <- c(action_vector, action_plan[5])
      commentaires_epi <- c(commentaires_epi, NA)
      criteres_pci_num <- c(criteres_pci_num, criteres_pci[2])
      
    }
  }
  
  
  
  k_6 <- as.numeric(dat_i$pci_comp_2_triage_usage_correct_fiche_registre[i])
  
  if(is.na(k_6)){
    gaps_vector <- c(gaps_vector, NA)
    action_vector <- c(action_vector, NA)
    commentaires_epi <- c(commentaires_epi, pci_criteres[6])
    criteres_pci_num <- c(criteres_pci_num, criteres_pci[2])
  }else{
    if(k_6==0){
      
      gaps_vector <- c(gaps_vector, gaps[6])
      action_vector <- c(action_vector, action_plan[6])
      commentaires_epi <- c(commentaires_epi, NA)
      criteres_pci_num <- c(criteres_pci_num, criteres_pci[2])
      
    }
  }
  
  
  
  k_7 <- as.numeric(dat_i$pci_comp_3_zone_isolement_bien_identifie[i])
  
  if(is.na(k_7)){
    gaps_vector <- c(gaps_vector, NA)
    action_vector <- c(action_vector, NA)
    commentaires_epi <- c(commentaires_epi, pci_criteres[7])
    criteres_pci_num <- c(criteres_pci_num, criteres_pci[3])
  }else{
    if(k_7==0){
      
      gaps_vector <- c(gaps_vector, gaps[7])
      action_vector <- c(action_vector, action_plan[7])
      commentaires_epi <- c(commentaires_epi, NA)
      criteres_pci_num <- c(criteres_pci_num, criteres_pci[3])
      
    }
  }
  
  
  k_8 <- as.numeric(dat_i$pci_comp_3_zone_isolement_toilette[i])
  
  if(is.na(k_8)){
    gaps_vector <- c(gaps_vector, NA)
    action_vector <- c(action_vector, NA)
    commentaires_epi <- c(commentaires_epi, pci_criteres[8])
    criteres_pci_num <- c(criteres_pci_num, criteres_pci[3])
  }else{
    if(k_8==0){
      
      gaps_vector <- c(gaps_vector, gaps[8])
      action_vector <- c(action_vector, action_plan[8])
      commentaires_epi <- c(commentaires_epi, NA)
      criteres_pci_num <- c(criteres_pci_num, criteres_pci[3])
      
    }
  }
  
  
  
  k_9 <- as.numeric(dat_i$pci_comp_3_zone_isolement_comprent_lavage_main_epi[i])
  
  if(is.na(k_9)){
    gaps_vector <- c(gaps_vector, NA)
    action_vector <- c(action_vector, NA)
    commentaires_epi <- c(commentaires_epi, pci_criteres[9])
    criteres_pci_num <- c(criteres_pci_num, criteres_pci[3])
  }else{
    if(k_9==0){
      
      gaps_vector <- c(gaps_vector, gaps[9])
      action_vector <- c(action_vector, action_plan[9])
      commentaires_epi <- c(commentaires_epi, NA)
      criteres_pci_num <- c(criteres_pci_num, criteres_pci[3])
      
    }
  }
  
  
  
  k_10 <- as.numeric(dat_i$pci_comp_4_station_lavage_et_eau_produits[i])
  
  if(is.na(k_10)){
    gaps_vector <- c(gaps_vector, NA)
    action_vector <- c(action_vector, NA)
    commentaires_epi <- c(commentaires_epi, pci_criteres[10])
    criteres_pci_num <- c(criteres_pci_num, criteres_pci[4])
  }else{
    if(k_10==0){
      
      gaps_vector <- c(gaps_vector, gaps[10])
      action_vector <- c(action_vector, action_plan[10])
      commentaires_epi <- c(commentaires_epi, NA)
      criteres_pci_num <- c(criteres_pci_num, criteres_pci[4])
      
    }
  }
  
  
  
  k_11 <- as.numeric(dat_i$pci_comp_4_station_lavage_personnel_pratique[i])
  
  if(is.na(k_11)){
    gaps_vector <- c(gaps_vector, NA)
    action_vector <- c(action_vector, NA)
    commentaires_epi <- c(commentaires_epi, pci_criteres[11])
    criteres_pci_num <- c(criteres_pci_num, criteres_pci[4])
  }else{
    if(k_11==0){
      
      gaps_vector <- c(gaps_vector, gaps[11])
      action_vector <- c(action_vector, action_plan[11])
      commentaires_epi <- c(commentaires_epi, NA)
      criteres_pci_num <- c(criteres_pci_num, criteres_pci[4])
      
    }
  }
  
  
  
  k_12 <- as.numeric(dat_i$pci_comp_4_station_lavage_affiches_techniques_hygiene[i])
  
  if(is.na(k_12)){
    gaps_vector <- c(gaps_vector, NA)
    action_vector <- c(action_vector, NA)
    commentaires_epi <- c(commentaires_epi, pci_criteres[12])
    criteres_pci_num <- c(criteres_pci_num, criteres_pci[4])
  }else{
    if(k_12==0){
      
      gaps_vector <- c(gaps_vector, gaps[12])
      action_vector <- c(action_vector, action_plan[12])
      commentaires_epi <- c(commentaires_epi, NA)
      criteres_pci_num <- c(criteres_pci_num, criteres_pci[4])
      
    }
  }
  
  
  
  k_13 <- as.numeric(dat_i$pci_comp_5_epi_acces_pour_personel_tout_moment_qte_suff_kits[i])
  
  if(is.na(k_13)){
    gaps_vector <- c(gaps_vector, NA)
    action_vector <- c(action_vector, NA)
    commentaires_epi <- c(commentaires_epi, pci_criteres[13])
    criteres_pci_num <- c(criteres_pci_num, criteres_pci[5])
  }else{
    if(k_13==0){
      
      gaps_vector <- c(gaps_vector, gaps[13])
      action_vector <- c(action_vector, action_plan[13])
      commentaires_epi <- c(commentaires_epi, NA)
      criteres_pci_num <- c(criteres_pci_num, criteres_pci[5])
      
    }
  }
  
  
  
  k_14 <- as.numeric(dat_i$pci_comp_5_epi_acces_pour_personel_connaissance_usage_affichages[i])
  
  if(is.na(k_14)){
    gaps_vector <- c(gaps_vector, NA)
    action_vector <- c(action_vector, NA)
    commentaires_epi <- c(commentaires_epi, pci_criteres[14])
    criteres_pci_num <- c(criteres_pci_num, criteres_pci[5])
  }else{
    if(k_14==0){
      
      gaps_vector <- c(gaps_vector, gaps[14])
      action_vector <- c(action_vector, action_plan[14])
      commentaires_epi <- c(commentaires_epi, NA)
      criteres_pci_num <- c(criteres_pci_num, criteres_pci[5])
      
    }
  }
  
  
  
  k_15 <- as.numeric(dat_i$pci_comp_5_epi_acces_pour_personel_connaissance_usage_etapes[i])
  
  if(is.na(k_15)){
    gaps_vector <- c(gaps_vector, NA)
    action_vector <- c(action_vector, NA)
    commentaires_epi <- c(commentaires_epi, pci_criteres[15])
    criteres_pci_num <- c(criteres_pci_num, criteres_pci[5])
  }else{
    if(k_15==0){
      
      gaps_vector <- c(gaps_vector, gaps[15])
      action_vector <- c(action_vector, action_plan[15])
      commentaires_epi <- c(commentaires_epi, NA)
      criteres_pci_num <- c(criteres_pci_num, criteres_pci[5])
      
    }
  }
  
  
  
  k_16 <- as.numeric(dat_i$pci_comp_6_dechets_tries_poubelles_affiches[i])
  
  if(is.na(k_16)){
    gaps_vector <- c(gaps_vector, NA)
    action_vector <- c(action_vector, NA)
    commentaires_epi <- c(commentaires_epi, pci_criteres[16])
    criteres_pci_num <- c(criteres_pci_num, criteres_pci[6])
  }else{
    if(k_16==0){
      
      gaps_vector <- c(gaps_vector, gaps[16])
      action_vector <- c(action_vector, action_plan[16])
      commentaires_epi <- c(commentaires_epi, NA)
      criteres_pci_num <- c(criteres_pci_num, criteres_pci[6])
      
    }
  }
  
  
  k_17 <- as.numeric(dat_i$pci_comp_6_dechets_tries_contenants[i])
  
  if(is.na(k_17)){
    gaps_vector <- c(gaps_vector, NA)
    action_vector <- c(action_vector, NA)
    commentaires_epi <- c(commentaires_epi, pci_criteres[17])
    criteres_pci_num <- c(criteres_pci_num, criteres_pci[6])
  }else{
    if(k_17==0){
      
      gaps_vector <- c(gaps_vector, gaps[17])
      action_vector <- c(action_vector, action_plan[17])
      commentaires_epi <- c(commentaires_epi, NA)
      criteres_pci_num <- c(criteres_pci_num, criteres_pci[6])
      
    }
  }
  
  
  k_18 <- as.numeric(dat_i$pci_comp_6_dechets_tries_dechets_dispose_types[i])
  
  if(is.na(k_18)){
    gaps_vector <- c(gaps_vector, NA)
    action_vector <- c(action_vector, NA)
    commentaires_epi <- c(commentaires_epi, pci_criteres[18])
    criteres_pci_num <- c(criteres_pci_num, criteres_pci[6])
  }else{
    if(k_18==0){
      
      gaps_vector <- c(gaps_vector, gaps[18])
      action_vector <- c(action_vector, action_plan[18])
      commentaires_epi <- c(commentaires_epi, NA)
      criteres_pci_num <- c(criteres_pci_num, criteres_pci[6])
      
    }
  }
  
  
  k_19 <- as.numeric(dat_i$pci_comp_7_dechets_bio_personnel_porte_epi[i])
  
  if(is.na(k_19)){
    gaps_vector <- c(gaps_vector, NA)
    action_vector <- c(action_vector, NA)
    commentaires_epi <- c(commentaires_epi, pci_criteres[19])
    criteres_pci_num <- c(criteres_pci_num, criteres_pci[7])
  }else{
    if(k_19==0){
      
      gaps_vector <- c(gaps_vector, gaps[19])
      action_vector <- c(action_vector, action_plan[19])
      commentaires_epi <- c(commentaires_epi, NA)
      criteres_pci_num <- c(criteres_pci_num, criteres_pci[7])
      
    }
  }
  
  
  
  k_20 <- as.numeric(dat_i$pci_comp_7_dechets_bio_incinerateur[i])
  
  if(is.na(k_20)){
    gaps_vector <- c(gaps_vector, NA)
    action_vector <- c(action_vector, NA)
    commentaires_epi <- c(commentaires_epi, pci_criteres[20])
    criteres_pci_num <- c(criteres_pci_num, criteres_pci[7])
  }else{
    if(k_20==0){
      
      gaps_vector <- c(gaps_vector, gaps[20])
      action_vector <- c(action_vector, action_plan[20])
      commentaires_epi <- c(commentaires_epi, NA)
      criteres_pci_num <- c(criteres_pci_num, criteres_pci[7])
      
    }
  }
  
  
  k_21 <- as.numeric(dat_i$pci_comp_7_dechets_bio_fosse_placenta[i])
  
  if(is.na(k_21)){
    gaps_vector <- c(gaps_vector, NA)
    action_vector <- c(action_vector, NA)
    commentaires_epi <- c(commentaires_epi, pci_criteres[21])
    criteres_pci_num <- c(criteres_pci_num, criteres_pci[7])
  }else{
    if(k_21==0){
      
      gaps_vector <- c(gaps_vector, gaps[21])
      action_vector <- c(action_vector, action_plan[21])
      commentaires_epi <- c(commentaires_epi, NA)
      criteres_pci_num <- c(criteres_pci_num, criteres_pci[7])
      
    }
  }
  
  
  
  k_22 <- as.numeric(dat_i$pci_comp_8_personnel_forme_pci_precautions_standards[i])
  
  if(is.na(k_22)){
    gaps_vector <- c(gaps_vector, NA)
    action_vector <- c(action_vector, NA)
    commentaires_epi <- c(commentaires_epi, pci_criteres[22])
    criteres_pci_num <- c(criteres_pci_num, criteres_pci[8])
  }else{
    if(k_22==0){
      
      gaps_vector <- c(gaps_vector, gaps[22])
      action_vector <- c(action_vector, action_plan[22])
      commentaires_epi <- c(commentaires_epi, NA)
      criteres_pci_num <- c(criteres_pci_num, criteres_pci[8])
      
    }
  }
  
  
  
  k_23 <- as.numeric(dat_i$pci_comp_8_personnel_forme_pci_enregistre_person_forme[i])
  
  if(is.na(k_23)){
    gaps_vector <- c(gaps_vector, NA)
    action_vector <- c(action_vector, NA)
    commentaires_epi <- c(commentaires_epi, pci_criteres[23])
    criteres_pci_num <- c(criteres_pci_num, criteres_pci[8])
  }else{
    if(k_23==0){
      
      gaps_vector <- c(gaps_vector, gaps[23])
      action_vector <- c(action_vector, action_plan[23])
      commentaires_epi <- c(commentaires_epi, NA)
      criteres_pci_num <- c(criteres_pci_num, criteres_pci[8])
      
    }
  }
  
  
  
  k_24 <- as.numeric(dat_i$pci_comp_8_personnel_forme_pci_person_formation_continue[i])
  
  if(is.na(k_24)){
    gaps_vector <- c(gaps_vector, NA)
    action_vector <- c(action_vector, NA)
    commentaires_epi <- c(commentaires_epi, pci_criteres[24])
    criteres_pci_num <- c(criteres_pci_num, criteres_pci[8])
  }else{
    if(k_24==0){
      
      gaps_vector <- c(gaps_vector, gaps[24])
      action_vector <- c(action_vector, action_plan[24])
      commentaires_epi <- c(commentaires_epi, NA)
      criteres_pci_num <- c(criteres_pci_num, criteres_pci[8])
      
    }
  }
  
  
  
  k_25 <- as.numeric(dat_i$pci_comp_9_systeme_gestion_cas_suspects_mve_numero_alerte_visible[i])
  
  if(is.na(k_25)){
    gaps_vector <- c(gaps_vector, NA)
    action_vector <- c(action_vector, NA)
    commentaires_epi <- c(commentaires_epi, pci_criteres[25])
    criteres_pci_num <- c(criteres_pci_num, criteres_pci[9])
  }else{
    if(k_25==0){
      
      gaps_vector <- c(gaps_vector, gaps[25])
      action_vector <- c(action_vector, action_plan[25])
      commentaires_epi <- c(commentaires_epi, NA)
      criteres_pci_num <- c(criteres_pci_num, criteres_pci[9])
      
    }
  }
  
  
  
  k_26 <- as.numeric(dat_i$pci_comp_9_systeme_gestion_cas_suspects_mve_depister_patient_hospitalises[i])
  
  if(is.na(k_26)){
    gaps_vector <- c(gaps_vector, NA)
    action_vector <- c(action_vector, NA)
    commentaires_epi <- c(commentaires_epi, pci_criteres[26])
    criteres_pci_num <- c(criteres_pci_num, criteres_pci[9])
  }else{
    if(k_26==0){
      
      gaps_vector <- c(gaps_vector, gaps[26])
      action_vector <- c(action_vector, action_plan[26])
      commentaires_epi <- c(commentaires_epi, NA)
      criteres_pci_num <- c(criteres_pci_num, criteres_pci[9])
      
    }
  }
  
  
  
  k_27 <- as.numeric(dat_i$pci_comp_9_systeme_gestion_cas_suspects_mve_patients_transfer_isolement_alerte[i])
  
  if(is.na(k_27)){
    gaps_vector <- c(gaps_vector, NA)
    action_vector <- c(action_vector, NA)
    commentaires_epi <- c(commentaires_epi, pci_criteres[27])
    criteres_pci_num <- c(criteres_pci_num, criteres_pci[9])
  }else{
    if(k_27==0){
      
      gaps_vector <- c(gaps_vector, gaps[27])
      action_vector <- c(action_vector, action_plan[27])
      commentaires_epi <- c(commentaires_epi, NA)
      criteres_pci_num <- c(criteres_pci_num, criteres_pci[9])
      
    }
  }
  
  
  k_28 <- as.numeric(dat_i$pci_comp_10_materiel_sterilisation_mater_dispo[i])
  
  if(is.na(k_28)){
    gaps_vector <- c(gaps_vector, NA)
    action_vector <- c(action_vector, NA)
    commentaires_epi <- c(commentaires_epi, pci_criteres[28])
    criteres_pci_num <- c(criteres_pci_num, criteres_pci[10])
  }else{
    if(k_28==0){
      
      gaps_vector <- c(gaps_vector, gaps[28])
      action_vector <- c(action_vector, action_plan[28])
      commentaires_epi <- c(commentaires_epi, NA)
      criteres_pci_num <- c(criteres_pci_num, criteres_pci[10])
      
    }
  }
  
  
  
  k_29 <- as.numeric(dat_i$pci_comp_10_personnel_kce_sterilisation_sop[i])
  
  if(is.na(k_29)){
    gaps_vector <- c(gaps_vector, NA)
    action_vector <- c(action_vector, NA)
    commentaires_epi <- c(commentaires_epi, pci_criteres[29])
    criteres_pci_num <- c(criteres_pci_num, criteres_pci[10])
  }else{
    if(k_29==0){
      
      gaps_vector <- c(gaps_vector, gaps[29])
      action_vector <- c(action_vector, action_plan[29])
      commentaires_epi <- c(commentaires_epi, NA)
      criteres_pci_num <- c(criteres_pci_num, criteres_pci[10])
      
    }
  }
  
  
  k_30 <- as.numeric(dat_i$pci_comp_10_personnel_effectuant_sterilisation_forme[i])
  
  if(is.na(k_30)){
    gaps_vector <- c(gaps_vector, NA)
    action_vector <- c(action_vector, NA)
    commentaires_epi <- c(commentaires_epi, pci_criteres[30])
    criteres_pci_num <- c(criteres_pci_num, criteres_pci[10])
  }else{
    if(k_30==0){
      
      gaps_vector <- c(gaps_vector, gaps[30])
      action_vector <- c(action_vector, action_plan[30])
      commentaires_epi <- c(commentaires_epi, NA)
      criteres_pci_num <- c(criteres_pci_num, criteres_pci[10])
      
    }
  }
  
  
  k_31 <- as.numeric(dat_i$pci_comp_11_nettoyage_desinfection_sop[i])
  
  if(is.na(k_31)){
    gaps_vector <- c(gaps_vector, NA)
    action_vector <- c(action_vector, NA)
    commentaires_epi <- c(commentaires_epi, pci_criteres[31])
    criteres_pci_num <- c(criteres_pci_num, criteres_pci[11])
  }else{
    if(k_31==0){
      
      gaps_vector <- c(gaps_vector, gaps[31])
      action_vector <- c(action_vector, action_plan[31])
      commentaires_epi <- c(commentaires_epi, NA)
      criteres_pci_num <- c(criteres_pci_num, criteres_pci[11])
      
    }
  }
  
  #
  as.numeric(k_32 <- dat_i$pci_comp_11_personnel_effectuant_nettoyage_desinfection_forme[i])
  
  if(is.na(k_32)){
    gaps_vector <- c(gaps_vector, NA)
    action_vector <- c(action_vector, NA)
    commentaires_epi <- c(commentaires_epi, pci_criteres[32])
    criteres_pci_num <- c(criteres_pci_num, criteres_pci[11])
  }else{
    if(k_32==0){
      
      gaps_vector <- c(gaps_vector, gaps[32])
      action_vector <- c(action_vector, action_plan[32])
      commentaires_epi <- c(commentaires_epi, NA)
      criteres_pci_num <- c(criteres_pci_num, criteres_pci[11])
    }
  }
  
  
  k_33 <- as.numeric(dat_i$pci_comp_11_personnel_effectuant_nettoyage_desinfection_porte_epi[i])
  
  if(is.na(k_33)){
    gaps_vector <- c(gaps_vector, NA)
    action_vector <- c(action_vector, NA)
    commentaires_epi <- c(commentaires_epi, pci_criteres[33])
    criteres_pci_num <- c(criteres_pci_num, criteres_pci[11])
  }else{
    if(k_33==0){
      
      gaps_vector <- c(gaps_vector, gaps[33])
      action_vector <- c(action_vector, action_plan[33])
      commentaires_epi <- c(commentaires_epi, NA)
      criteres_pci_num <- c(criteres_pci_num, criteres_pci[11])
      
    }
  }
  
  
  
  k_34 <- as.numeric(dat_i$pci_comp_12_prise_charge_suivi_personnel_expose_ebola_protocole[i])
  
  if(is.na(k_34)){
    gaps_vector <- c(gaps_vector, NA)
    action_vector <- c(action_vector, NA)
    commentaires_epi <- c(commentaires_epi, pci_criteres[34])
    criteres_pci_num <- c(criteres_pci_num, criteres_pci[12])
  }else{
    if(k_34==0){
      
      gaps_vector <- c(gaps_vector, gaps[34])
      action_vector <- c(action_vector, action_plan[34])
      commentaires_epi <- c(commentaires_epi, NA)
      criteres_pci_num <- c(criteres_pci_num, criteres_pci[12])
      
    }
  }
  
  
  k_35 <- as.numeric(dat_i$pci_comp_12_prise_charge_suivi_personnel_expose_ebola_clairement_definie_et_assuree[i])
  
  if(is.na(k_35)){
    gaps_vector <- c(gaps_vector, NA)
    action_vector <- c(action_vector, NA)
    commentaires_epi <- c(commentaires_epi, pci_criteres[35])
    criteres_pci_num <- c(criteres_pci_num, criteres_pci[12])
  }else{
    if(k_35==0){
      
      gaps_vector <- c(gaps_vector, gaps[35])
      action_vector <- c(action_vector, action_plan[35])
      commentaires_epi <- c(commentaires_epi, NA)
      criteres_pci_num <- c(criteres_pci_num, criteres_pci[12])
      
    }
  }
  
  
  k_36 <- as.numeric(dat_i$pci_comp_12_prise_charge_suivi_personnel_expose_ebola_alerte_equipe_intervention[i])
  
  if(is.na(k_36)){
    gaps_vector <- c(gaps_vector, NA)
    action_vector <- c(action_vector, NA)
    commentaires_epi <- c(commentaires_epi, pci_criteres[36])
    criteres_pci_num <- c(criteres_pci_num, criteres_pci[12])
  }else{
    if(k_36==0){
      
      gaps_vector <- c(gaps_vector, gaps[36])
      action_vector <- c(action_vector, action_plan[36])
      commentaires_epi <- c(commentaires_epi, NA)
      criteres_pci_num <- c(criteres_pci_num, criteres_pci[12])
      
    }
  }
  
  
  
  dat_pao_i <- data.frame(gaps_vector, action_vector, criteres_pci_num, commentaires_epi)
  
  names(dat_pao_i) <- c("Gaps identifies", "Plan d'Action", "Critere PCI", "Critere qui etait non applicable")
  
  #
  if(nrow(dat_pao_i)>=1){
    
    names_dat_i_keep_i <- names(dat_i_keep_i)
    dat_i_keep_i_dim <- data.frame()
    for(l in 1:nrow(dat_pao_i)){
      
      dat_i_keep_i_dim <- rbind(dat_i_keep_i_dim, dat_i_keep_i) 
      
    }
    names(dat_i_keep_i_dim) <- names_dat_i_keep_i
    #
    
    together_i <- cbind(dat_i_keep_i_dim, dat_pao_i)
    
    dat_pao_hf_i <- together_i
    
    dat_pao_hf <- rbind(dat_pao_hf, dat_pao_hf_i)
    
  }
  
  
  
} # End for





# Save gaps identified

dat_pao_hf_all <- dat_pao_hf


dead_line_all <- dat_pao_hf_all$date_enquete + 21

dat_pao_hf_all <- cbind(dat_pao_hf_all, dead_line_all)

save_dat_pao_hf_all <- dplyr::select(dat_pao_hf_all, date_enquete, zone_de_sante, aire_de_sante, nom_fosa,
                                     categorie, secteur, nbre_partenaires, noms_partnaires, score_pci_100, `Gaps identifies`, 
                                     `Critere PCI`, `Plan d'Action`, `Critere qui etait non applicable`, dead_line_all)

save_dat_pao_hf_all_rm_crit_NA <- dplyr::filter(save_dat_pao_hf_all, is.na(save_dat_pao_hf_all$`Critere qui etait non applicable`))


save_dat_pao_hf_all_rm_crit_NA_final <- dplyr::select(save_dat_pao_hf_all_rm_crit_NA, date_enquete, zone_de_sante, aire_de_sante, nom_fosa,
                                                      categorie, secteur, nbre_partenaires, noms_partnaires, score_pci_100, `Gaps identifies`, 
                                                      `Plan d'Action`, `Critere PCI`, dead_line_all)


write.csv(save_dat_pao_hf_all_rm_crit_NA_final, file = paste0("results/PCI_PAO_2019_21_13_HZs.csv"))




# Frequencies of gaps by Health Zone level

gap_freq_data_frame <- data.frame(matrix(ncol = 3, nrow = 0)) # for all HF in one HZ


colnames(gap_freq_data_frame) <- c("zone_sante",    
                                   "gap_frequency", 
                                   "gap_name")


zones <- unique(dat_pao_hf_all$zone_de_sante)



for(i in 1:length(zones)){
  
  nom_zone <- zones[i]
  
  df_i <- dplyr::filter(dat_pao_hf_all, dat_pao_hf_all$zone_de_sante==paste0(nom_zone))
  
  
  gap_name <- names(table(df_i$`Gaps identifies`))
  gap_frequency <- as.numeric(table(df_i$`Gaps identifies`))
  zone_sante <- rep(nom_zone, length(gap_frequency))
  
  gap_freq_table <- data.frame(zone_sante, gap_frequency, gap_name)
  
  gap_freq_data_frame <- rbind(gap_freq_data_frame, gap_freq_table)
  
  
}



names(gap_freq_data_frame) <- c("zone_de_sante", "frequence_du_gap", "nom_du_gap")


gap_freq_data_frame_x <- dplyr::filter(gap_freq_data_frame, gap_freq_data_frame$frequence_du_gap!=0)


write.csv(gap_freq_data_frame_x, file = paste0("results/Frequencies_gaps_2019_21_13_PAO_HZs.csv"))



