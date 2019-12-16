
# Ebola Response Team in DRC
# WHO & MoH
# IPC Interventions

# Objective: Prioritization of IPC interventions for critical Healthcare Facilities

# Author: David Niyukuri (niyukuri@aims.ac.za & niyukurud@who.int)
# With support: Landry Kabego (megaglk3@gmail.com)

# Introduction -------

# To build and test the algorithm for Healthcare Facilities prioritization and the model
# we used data from Butembo Sub-Coordination

# Geographical location - 12 Health Zones: 

# "Alimbongo"   "Biena"       "Butembo"     
# "Kalunguta"   "Katwa"       "Kayna"       
# "Kyondo"      "Lubero" "Mangurejipa" 
# "Masereka"    "Musienene"   "Vuhovi"



# Databases: 


#   1.VHF for EVD cases in Health Areas
#   2. Scorecard for IPC score of Health Facilities
#   3. EDS for failure of safe burial


# Time frame


# Time interval: 11 June – 11 October 2019



# Note: After loading packages you may start with building and testing themodel- You may start here with dataset bencmarked ) ----

# Pricipal ID: FOSA in SCORECARD DATABASE

# Loading packages ----------

suppressMessages(library(readxl))
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(ggforce))
suppressMessages(library(kableExtra))
suppressMessages(library(lubridate))
suppressMessages(library(ggpubr))
suppressMessages(library(linelist))
suppressMessages(library(randomForest))
suppressMessages(library(ROCR))


# record current date (time at which document is compiled)
today <- as.Date(Sys.time())



# read scripts

path_to_scripts <- here::here("Utility_Functions")
scripts_files <- dir(path_to_scripts, pattern = ".R$", full.names=TRUE)
for (file in scripts_files) source(file)


# Note: no more first data loading and cleaning

# path to raw data

# 
# path_to_data_kobo <- here::here("data", "raw", "scorecard-2019-10-27-10-32-34.xlsx")
# 
# 
# path_to_data_vhf <- here::here("Evaluation_October_2019/data_algo/", "vhf-nshasha-sc-butembo-reduit.xlsx")
# 
# 
# path_to_data_eds <- here::here("Evaluation_October_2019/data_algo/", "eds-nshasha-sc-butembo.xlsx")
# 
# 
# path_to_data_alerts <- here::here("Evaluation_October_2019/data_algo/", "alerts-nshasha-sc-butembo-reduit.xlsx")
# 



# get data files

# data_vhf <- rio::import(path_to_data_vhf,
#                         guess_max = 1e4) %>%
#   clean_data(guess_dates = FALSE) %>%
#   as_tibble()
# 
# 
# data_kobo <- rio::import(path_to_data_kobo,
#                          guess_max = 1e4) %>%
#   clean_data(guess_dates = FALSE) %>%
#   as_tibble()
# 
# data_eds <- rio::import(path_to_data_eds,
#                          guess_max = 1e4) %>%
#   clean_data(guess_dates = FALSE) %>%
#   as_tibble()
# 
# 
# data_alerts <- rio::import(path_to_data_alerts,
#                         guess_max = 1e4) %>%
#   clean_data(guess_dates = FALSE) %>%
#   as_tibble()




# Save an object to a file

# saveRDS(data_vhf, file = "Evaluation_October_2019/data_algo/data_vhf_sc_butembo.rds")


data_vhf <- readRDS("/home/david/Dropbox/WHO_EBV_RESPONSE_2018/IPC_DRC_Paul_Allen/ipc/Evaluation_October_2019/data_algo/data_vhf_sc_butembo.rds") # selected some HZ


# saveRDS(data_kobo, file = "Evaluation_October_2019/data_algo/data_kobo_all_hz.rds")

# this is all Kobo data != select only BBO-SC
data_kobo <- readRDS("/home/david/Dropbox/WHO_EBV_RESPONSE_2018/IPC_DRC_Paul_Allen/ipc/Evaluation_October_2019/data_algo/data_kobo_all_hz.rds")



# saveRDS(data_eds, file = "Evaluation_October_2019/data_algo/data_eds_sc_butembo.rds")


data_eds <- readRDS("/home/david/Dropbox/WHO_EBV_RESPONSE_2018/IPC_DRC_Paul_Allen/ipc/Evaluation_October_2019/data_algo/data_eds_sc_butembo.rds")



# saveRDS(data_alerts, file = "Evaluation_October_2019/data_algo/data_alerts_sc_butembo.rds")


# data_alerts <- readRDS("/home/david/Dropbox/WHO_EBV_RESPONSE_2018/IPC_DRC_Paul_Allen/ipc/Evaluation_October_2019/data_algo/data_alerts_sc_butembo.rds")



data_score_ipc <- scorecard_evaluation_1st_cleaning(data_score = data_kobo)



# Data gathering and formatting: clean and select HZ names -----------

# VHF


data_vhf$zone_de_sante[data_vhf$zone_de_sante=="biene"] <- "biena"
data_vhf$zone_de_sante[data_vhf$zone_de_sante=="kalnguta"] <- "kalunguta"
data_vhf$zone_de_sante[data_vhf$zone_de_sante=="mangurudjipa"] <- "manguredjipa"
data_vhf$zone_de_sante[data_vhf$zone_de_sante=="musinene"] <- "musienene"

data_vhf_bbo_sc <- data_vhf

# > sort(unique(data_vhf$zone_de_sante))
# [1] "alimbongo"    "biena"        "butembo"      "kalunguta"    "katwa"        "kayna"        "kyondo"       "lubero"      
# [9] "manguredjipa" "masereka"     "musienene"    "vuhovi" 


# SCORECARD: all HZs are well written


# data_score_ipc - pull data from the 12 HZs in BBO SC

data_score_ipc_bbo_sc <- dplyr::filter(data_score_ipc, data_score_ipc$zone_de_sante =="alimbongo" |
                                         data_score_ipc$zone_de_sante == "biena" |
                                         data_score_ipc$zone_de_sante == "butembo" |
                                         data_score_ipc$zone_de_sante == "kalunguta" |
                                         data_score_ipc$zone_de_sante == "katwa" |
                                         data_score_ipc$zone_de_sante == "kayna" |
                                         data_score_ipc$zone_de_sante == "kyondo" |
                                         data_score_ipc$zone_de_sante == "lubero" |
                                         data_score_ipc$zone_de_sante == "manguredjipa" |
                                         data_score_ipc$zone_de_sante == "masereka" |
                                         data_score_ipc$zone_de_sante == "musienene" |
                                         data_score_ipc$zone_de_sante == "vuhovi")

# > sort(unique(data_score_ipc_bbo_sc$zone_de_sante))
# [1] "butembo"   "kalunguta" "katwa"     "kyondo"    "lubero"    "masereka"  "vuhovi" 


# EDS

# Note: we find HZ which are not in BBO SC: kibirizi, pinga, binza

# Pull data for BBO SC only

data_eds_bbo_sc <- dplyr::filter(data_eds, data_eds$zone_de_sante =="alimbongo" |
                                   data_eds$zone_de_sante == "biena" |
                                   data_eds$zone_de_sante == "butembo" |
                                   data_eds$zone_de_sante == "kalunguta" |
                                   data_eds$zone_de_sante == "katwa" |
                                   data_eds$zone_de_sante == "kayna" |
                                   data_eds$zone_de_sante == "kyondo" |
                                   data_eds$zone_de_sante == "lubero" |
                                   data_eds$zone_de_sante == "manguredjipa" |
                                   data_eds$zone_de_sante == "masereka" |
                                   data_eds$zone_de_sante == "musienene" |
                                   data_eds$zone_de_sante == "vuhovi")



# > sort(unique(data_eds_bbo_sc$zone_de_sante))
# [1] "alimbongo"    "biena"        "butembo"      "kalunguta"    "katwa"        "kayna"        "kyondo"       "lubero"      
# [9] "manguredjipa" "masereka"     "musienene"    "vuhovi"      



### NOTE: based on the fact we want prioritization of Healthcare Facilities

# and due to the fact we have only 7 HZs with IPC sore data ("butembo", "kalunguta", "katwa", kyondo", "lubero", "masereka", vuhovi")
# we may select same HZ for other databases (VHF and EDS).

data_eds_bbo_hzs <- dplyr::filter(data_eds_bbo_sc, 
                                  data_eds_bbo_sc$zone_de_sante == "butembo" |
                                    data_eds_bbo_sc$zone_de_sante == "kalunguta" |
                                    data_eds_bbo_sc$zone_de_sante == "katwa" |
                                    data_eds_bbo_sc$zone_de_sante == "kyondo" |
                                    data_eds_bbo_sc$zone_de_sante == "lubero" |
                                    data_eds_bbo_sc$zone_de_sante == "masereka" |
                                    data_eds_bbo_sc$zone_de_sante == "vuhovi")



data_vhf_bbo_hzs <- dplyr::filter(data_vhf_bbo_sc, 
                                  data_vhf_bbo_sc$zone_de_sante == "butembo" |
                                    data_vhf_bbo_sc$zone_de_sante == "kalunguta" |
                                    data_vhf_bbo_sc$zone_de_sante == "katwa" |
                                    data_vhf_bbo_sc$zone_de_sante == "kyondo" |
                                    data_vhf_bbo_sc$zone_de_sante == "lubero" |
                                    data_vhf_bbo_sc$zone_de_sante == "masereka" |
                                    data_vhf_bbo_sc$zone_de_sante == "vuhovi")


data_score_ipc_bbo_hzs <- data_score_ipc_bbo_sc

# 2. Minds dates

# data_vhf_bbo_sc, data_score_ipc_bbo_sc, data_eds_bbo_sc

# For Score: keep the dates

data_score_ipc_bbo_hzs_date <- dplyr::filter(data_score_ipc_bbo_hzs, data_score_ipc_bbo_hzs$date_enquete >= "2019-06-11" & data_score_ipc_bbo_hzs$date_enquete <= "2019-10-11")



x <- as.Date("2019-06-11")

# For VHF and EDS : go 21 days before


data_vhf_bbo_hzs_date <- dplyr::filter(data_vhf_bbo_hzs, data_vhf_bbo_hzs$date_onset >= (x-21) & data_vhf_bbo_hzs$date_onset <= "2019-10-11")


data_eds_bbo_hzs_date <- dplyr::filter(data_eds_bbo_hzs, data_eds_bbo_hzs$date >= (x-21) & data_eds_bbo_hzs$date <= "2019-10-11")

data_eds_bbo_hzs_date <- rename(data_eds_bbo_hzs_date, aire_de_sante = aires_de_sante)

data_eds_bbo_hzs_date <- dplyr::filter(data_eds_bbo_hzs_date, data_eds_bbo_hzs_date$situation_de_l_eds!="complet")


# 3. Cleaning aire de sante names

# Let cosnider in the following HZ only: 

#        "butembo"   "kalunguta" "katwa"     "kyondo"    "lubero"    "masereka"  "vuhovi" 


# Butembo


data_score_ipc_bbo_hzs_date_butembo <- dplyr::filter(data_score_ipc_bbo_hzs, data_score_ipc_bbo_hzs$zone_de_sante=="butembo")
data_vhf_bbo_hzs_date_butembo <- dplyr::filter(data_vhf_bbo_hzs_date, data_vhf_bbo_hzs_date$zone_de_sante=="butembo")
data_eds_bbo_hzs_date_butembo <- dplyr::filter(data_eds_bbo_hzs_date, data_eds_bbo_hzs_date$zone_de_sante=="butembo")

# data_score_ipc_bbo_hzs_date_butembo # Ok

data_vhf_bbo_hzs_date_butembo$aire_de_sante[data_vhf_bbo_hzs_date_butembo$aire_de_sante=="vuthetse"] <- "vutetse"
data_vhf_bbo_hzs_date_butembo$aire_de_sante[data_vhf_bbo_hzs_date_butembo$aire_de_sante=="vutatse"] <- "vutetse"
data_vhf_bbo_hzs_date_butembo$aire_de_sante[data_vhf_bbo_hzs_date_butembo$aire_de_sante=="vutstse"] <- "vutetse"
data_vhf_bbo_hzs_date_butembo$aire_de_sante[data_vhf_bbo_hzs_date_butembo$aire_de_sante=="vulndi"] <- "vulindi"
data_vhf_bbo_hzs_date_butembo$aire_de_sante[data_vhf_bbo_hzs_date_butembo$aire_de_sante=="valamba"] <- "vulamba"
data_vhf_bbo_hzs_date_butembo$aire_de_sante[data_vhf_bbo_hzs_date_butembo$aire_de_sante=="mamanmusayi"] <- "mama_musayi"
data_vhf_bbo_hzs_date_butembo$aire_de_sante[data_vhf_bbo_hzs_date_butembo$aire_de_sante=="mamn_musayi"] <- "mama_musayi"
data_vhf_bbo_hzs_date_butembo$aire_de_sante[data_vhf_bbo_hzs_date_butembo$aire_de_sante=="mamanmusayi"] <- "mama_musayi"
data_vhf_bbo_hzs_date_butembo$aire_de_sante[data_vhf_bbo_hzs_date_butembo$aire_de_sante=="maman_muyisa"] <- "mama_musayi"
data_vhf_bbo_hzs_date_butembo$aire_de_sante[data_vhf_bbo_hzs_date_butembo$aire_de_sante=="katsia"] <- "katsya"



data_eds_bbo_hzs_date_butembo$aire_de_sante[data_eds_bbo_hzs_date_butembo$aire_de_sante=="kyavngike"] <- "kyangike"
data_eds_bbo_hzs_date_butembo$aire_de_sante[data_eds_bbo_hzs_date_butembo$aire_de_sante=="makati"] <- "makasi"
data_eds_bbo_hzs_date_butembo$aire_de_sante[data_eds_bbo_hzs_date_butembo$aire_de_sante=="mama_misayi"] <- "mama_musayi"
data_eds_bbo_hzs_date_butembo$aire_de_sante[data_eds_bbo_hzs_date_butembo$aire_de_sante=="maman_musayi"] <- "mama_musayi"
data_eds_bbo_hzs_date_butembo$aire_de_sante[data_eds_bbo_hzs_date_butembo$aire_de_sante=="mando"] <- "mondo"
data_eds_bbo_hzs_date_butembo$aire_de_sante[data_eds_bbo_hzs_date_butembo$aire_de_sante=="munzambayi"] <- "munzambaye"
data_eds_bbo_hzs_date_butembo$aire_de_sante[data_eds_bbo_hzs_date_butembo$aire_de_sante=="vuliki"] <- "vulindi"


# Kalunguta


data_score_ipc_bbo_hzs_date_kalunguta <- dplyr::filter(data_score_ipc_bbo_hzs, data_score_ipc_bbo_hzs$zone_de_sante=="kalunguta")
data_vhf_bbo_hzs_date_kalunguta <- dplyr::filter(data_vhf_bbo_hzs_date, data_vhf_bbo_hzs_date$zone_de_sante=="kalunguta")
data_eds_bbo_hzs_date_kalunguta <- dplyr::filter(data_eds_bbo_hzs_date, data_eds_bbo_hzs_date$zone_de_sante=="kalunguta")


data_score_ipc_bbo_hzs_date_kalunguta$aire_de_sante[data_score_ipc_bbo_hzs_date_kalunguta$aire_de_sante=="amani_kisungu"] <- "kisungu"
data_score_ipc_bbo_hzs_date_kalunguta$aire_de_sante[data_score_ipc_bbo_hzs_date_kalunguta$aire_de_sante=="mabaku"] <- "mabuku"
data_score_ipc_bbo_hzs_date_kalunguta$aire_de_sante[data_score_ipc_bbo_hzs_date_kalunguta$aire_de_sante=="rwarwa"] <- "rwahwa"

# can't get mambingi and vurondo

data_vhf_bbo_hzs_date_kalunguta$aire_de_sante[data_vhf_bbo_hzs_date_kalunguta$aire_de_sante=="amani_kisungu"] <- "kisungu"
data_vhf_bbo_hzs_date_kalunguta$aire_de_sante[data_vhf_bbo_hzs_date_kalunguta$aire_de_sante=="rwarwa"] <- "rwahwa"
data_vhf_bbo_hzs_date_kalunguta$aire_de_sante[data_vhf_bbo_hzs_date_kalunguta$aire_de_sante=="mbilinga"] <- "mbiringa"

# can't get kalunguta, inconnuXXX, kasebere, kivetya, kyavisogho, lukanga, mahohe, makoko, nambale, mambingi and visiki

data_eds_bbo_hzs_date_kalunguta$aire_de_sante[data_eds_bbo_hzs_date_kalunguta$aire_de_sante=="kanihunga"] <- "kanyihunga"
data_eds_bbo_hzs_date_kalunguta$aire_de_sante[data_eds_bbo_hzs_date_kalunguta$aire_de_sante=="kisiungu"] <- "kisungu"
data_eds_bbo_hzs_date_kalunguta$aire_de_sante[data_eds_bbo_hzs_date_kalunguta$aire_de_sante=="kkabasha"] <- "kabasha"
data_eds_bbo_hzs_date_kalunguta$aire_de_sante[data_eds_bbo_hzs_date_kalunguta$aire_de_sante=="manbingi"] <- "mambingi"
data_eds_bbo_hzs_date_kalunguta$aire_de_sante[data_eds_bbo_hzs_date_kalunguta$aire_de_sante=="manbingi"] <- "mambingi"



# Katwa



data_score_ipc_bbo_hzs_date_katwa <- dplyr::filter(data_score_ipc_bbo_hzs, data_score_ipc_bbo_hzs$zone_de_sante=="katwa")
data_vhf_bbo_hzs_date_katwa <- dplyr::filter(data_vhf_bbo_hzs_date, data_vhf_bbo_hzs_date$zone_de_sante=="katwa")
data_eds_bbo_hzs_date_katwa <- dplyr::filter(data_eds_bbo_hzs_date, data_eds_bbo_hzs_date$zone_de_sante=="katwa")


# data_score_ipc_bbo_hzs_date_katwa # no pblm

data_vhf_bbo_hzs_date_katwa$aire_de_sante[data_vhf_bbo_hzs_date_katwa$aire_de_sante=="kavika"] <- "kivika"
data_vhf_bbo_hzs_date_katwa$aire_de_sante[data_vhf_bbo_hzs_date_katwa$aire_de_sante=="muchunga"] <- "muchanga"
data_vhf_bbo_hzs_date_katwa$aire_de_sante[data_vhf_bbo_hzs_date_katwa$aire_de_sante=="muhisa"] <- "muyisa"
data_vhf_bbo_hzs_date_katwa$aire_de_sante[data_vhf_bbo_hzs_date_katwa$aire_de_sante=="miotoya"] <- "mitoya"
data_vhf_bbo_hzs_date_katwa$aire_de_sante[data_vhf_bbo_hzs_date_katwa$aire_de_sante=="tilizeni"] <- "tulizeni"
data_vhf_bbo_hzs_date_katwa$aire_de_sante[data_vhf_bbo_hzs_date_katwa$aire_de_sante=="vughole"] <- "vighole"


data_eds_bbo_hzs_date_katwa$aire_de_sante[data_eds_bbo_hzs_date_katwa$aire_de_sante=="kambulu"] <- "kambuli"
data_eds_bbo_hzs_date_katwa$aire_de_sante[data_eds_bbo_hzs_date_katwa$aire_de_sante=="makasi"] <- "masiki"
data_eds_bbo_hzs_date_katwa$aire_de_sante[data_eds_bbo_hzs_date_katwa$aire_de_sante=="mitoyo"] <- "mitoya"
data_eds_bbo_hzs_date_katwa$aire_de_sante[data_eds_bbo_hzs_date_katwa$aire_de_sante=="moitoya"] <- "mitoya"
data_eds_bbo_hzs_date_katwa$aire_de_sante[data_eds_bbo_hzs_date_katwa$aire_de_sante=="muvisa"] <- "muyisa"
data_eds_bbo_hzs_date_katwa$aire_de_sante[data_eds_bbo_hzs_date_katwa$aire_de_sante=="vingi"] <- "vungi"
data_eds_bbo_hzs_date_katwa$aire_de_sante[data_eds_bbo_hzs_date_katwa$aire_de_sante=="weyene"] <- "wayene"




# Kyondo


data_score_ipc_bbo_hzs_date_kyondo <- dplyr::filter(data_score_ipc_bbo_hzs, data_score_ipc_bbo_hzs$zone_de_sante=="kyondo")
data_vhf_bbo_hzs_date_kyondo <- dplyr::filter(data_vhf_bbo_hzs_date, data_vhf_bbo_hzs_date$zone_de_sante=="kyondo")
data_eds_bbo_hzs_date_kyondo <- dplyr::filter(data_eds_bbo_hzs_date, data_eds_bbo_hzs_date$zone_de_sante=="kyondo")

# data_score_ipc_bbo_hzs_date_kyondo # Ok

data_eds_bbo_hzs_date_kyondo$aire_de_sante[data_eds_bbo_hzs_date_kyondo$aire_de_sante=="kasogwere"] <- "kasongwere"
data_eds_bbo_hzs_date_kyondo$aire_de_sante[data_eds_bbo_hzs_date_kyondo$aire_de_sante=="vayana_vuhalirwa"] <- "vayana"



# Lubero

data_score_ipc_bbo_hzs_date_lubero <- dplyr::filter(data_score_ipc_bbo_hzs, data_score_ipc_bbo_hzs$zone_de_sante=="lubero")
data_vhf_bbo_hzs_date_lubero <- dplyr::filter(data_vhf_bbo_hzs_date, data_vhf_bbo_hzs_date$zone_de_sante=="lubero")
data_eds_bbo_hzs_date_lubero <- dplyr::filter(data_eds_bbo_hzs_date, data_eds_bbo_hzs_date$zone_de_sante=="lubero")


# data_score_ipc_bbo_hzs_date_lubero # Oks

data_vhf_bbo_hzs_date_lubero$aire_de_sante[data_vhf_bbo_hzs_date_lubero$aire_de_sante=="lubero_cite"] <- "lubero"

data_eds_bbo_hzs_date_lubero$aire_de_sante[data_eds_bbo_hzs_date_lubero$aire_de_sante=="liubero"] <- "lubero"
data_eds_bbo_hzs_date_lubero$aire_de_sante[data_eds_bbo_hzs_date_lubero$aire_de_sante=="miulo"] <- "mulo"


# Masereka


data_score_ipc_bbo_hzs_date_masereka <- dplyr::filter(data_score_ipc_bbo_hzs, data_score_ipc_bbo_hzs$zone_de_sante=="masereka")
data_vhf_bbo_hzs_date_masereka <- dplyr::filter(data_vhf_bbo_hzs_date, data_vhf_bbo_hzs_date$zone_de_sante=="masereka")
data_eds_bbo_hzs_date_masereka <- dplyr::filter(data_eds_bbo_hzs_date, data_eds_bbo_hzs_date$zone_de_sante=="masereka")



# data_score_ipc_bbo_hzs_date_masereka # Ok

data_vhf_bbo_hzs_date_masereka$aire_de_sante[data_vhf_bbo_hzs_date_masereka$aire_de_sante=="ikukula"] <- "ikuvula"
data_vhf_bbo_hzs_date_masereka$aire_de_sante[data_vhf_bbo_hzs_date_masereka$aire_de_sante=="katembe"] <- "kitembe"
data_vhf_bbo_hzs_date_masereka$aire_de_sante[data_vhf_bbo_hzs_date_masereka$aire_de_sante=="kisturu"] <- "kitsuku"
data_vhf_bbo_hzs_date_masereka$aire_de_sante[data_vhf_bbo_hzs_date_masereka$aire_de_sante=="mageria"] <- "magherya"

data_eds_bbo_hzs_date_masereka$aire_de_sante[data_eds_bbo_hzs_date_masereka$aire_de_sante=="bubondi"] <- "bukondi"
data_eds_bbo_hzs_date_masereka$aire_de_sante[data_eds_bbo_hzs_date_masereka$aire_de_sante=="liotu"] <- "luotu"
data_eds_bbo_hzs_date_masereka$aire_de_sante[data_eds_bbo_hzs_date_masereka$aire_de_sante=="lukaanga"] <- "lukanga"
data_eds_bbo_hzs_date_masereka$aire_de_sante[data_eds_bbo_hzs_date_masereka$aire_de_sante=="lukana"] <- "lukanga"
data_eds_bbo_hzs_date_masereka$aire_de_sante[data_eds_bbo_hzs_date_masereka$aire_de_sante=="magheria"] <- "magherya"



# Vuhovi


data_score_ipc_bbo_hzs_date_vuhovi <- dplyr::filter(data_score_ipc_bbo_hzs, data_score_ipc_bbo_hzs$zone_de_sante=="vuhovi")
data_vhf_bbo_hzs_date_vuhovi <- dplyr::filter(data_vhf_bbo_hzs_date, data_vhf_bbo_hzs_date$zone_de_sante=="vuhovi")
data_eds_bbo_hzs_date_vuhovi <- dplyr::filter(data_eds_bbo_hzs_date, data_eds_bbo_hzs_date$zone_de_sante=="vuhovi")

# data_score_ipc_bbo_hzs_date_vuhovi # Ok
# data_vhf_bbo_hzs_date_vuhovi # Ok

data_eds_bbo_hzs_date_vuhovi$aire_de_sante[data_eds_bbo_hzs_date_vuhovi$aire_de_sante=="kaghali"] <- "kighali"
data_eds_bbo_hzs_date_vuhovi$aire_de_sante[data_eds_bbo_hzs_date_vuhovi$aire_de_sante=="vuhov"] <- "vuhovi"


# All together


IPC <- rbind(data_score_ipc_bbo_hzs_date_butembo,
             data_score_ipc_bbo_hzs_date_kalunguta,
             data_score_ipc_bbo_hzs_date_katwa,
             data_score_ipc_bbo_hzs_date_kyondo,
             data_score_ipc_bbo_hzs_date_lubero,
             data_score_ipc_bbo_hzs_date_masereka,
             data_score_ipc_bbo_hzs_date_vuhovi)


VHF <- rbind(data_vhf_bbo_hzs_date_butembo,
             data_vhf_bbo_hzs_date_kalunguta,
             data_vhf_bbo_hzs_date_katwa,
             data_vhf_bbo_hzs_date_kyondo,
             data_vhf_bbo_hzs_date_lubero,
             data_vhf_bbo_hzs_date_masereka,
             data_vhf_bbo_hzs_date_vuhovi)

EDS <- rbind(data_eds_bbo_hzs_date_butembo,
             data_eds_bbo_hzs_date_kalunguta,
             data_eds_bbo_hzs_date_katwa,
             data_eds_bbo_hzs_date_kyondo,
             data_eds_bbo_hzs_date_lubero,
             data_eds_bbo_hzs_date_masereka,
             data_eds_bbo_hzs_date_vuhovi)




# Data benchmarking -------

IPC_PRIORITY <- dplyr::select(IPC, date_enquete, zone_de_sante, aire_de_sante, nom_fosa, 
                              type_structure_de_sante, categorie, secteur, 
                              presence_partenaire, nbre_partenaires, noms_partnaires,
                              score_pci_100)




IPC_PRIORITY$evd_cases <- rep(NA, nrow(IPC_PRIORITY))
IPC_PRIORITY$eds_deaths <- rep(NA, nrow(IPC_PRIORITY))
# IPC_PRIORITY$security <- rep(NA, nrow(IPC_PRIORITY))
# IPC_PRIORITY$rcce <- rep(NA, nrow(IPC_PRIORITY))


IPC_PRIORITY$priority <- rep(NA, nrow(IPC_PRIORITY))

for(i in 1:nrow(IPC_PRIORITY)){
  
  nom_fosa_i <- IPC_PRIORITY$nom_fosa[i]
  nom_ZS_i <- IPC_PRIORITY$zone_de_sante[i]
  nom_AS_i <- IPC_PRIORITY$aire_de_sante[i]
  ipc_score_i <- IPC_PRIORITY$score_pci_100[i]
  category_i <- IPC_PRIORITY$categorie[i]
  secteur_i <- IPC_PRIORITY$secteur[i]
  
  date_eval_i <- IPC_PRIORITY$date_enquete[i]
  
  # VHF
  
  data_vfh_HZs_21 <- dplyr::filter(VHF, VHF$date_onset > (date_eval_i-21) & VHF$date_onset <= date_eval_i)
  
  data_vfh_HZs_21_SA <- dplyr::filter(data_vfh_HZs_21, data_vfh_HZs_21$aire_de_sante==nom_AS_i)
  
  
  # EDS
  
  data_eds_HZs_21 <- dplyr::filter(EDS, EDS$date > (date_eval_i-21) & EDS$date <= date_eval_i)
  
  data_eds_HZs_21_SA <- dplyr::filter(data_eds_HZs_21, data_eds_HZs_21$aire_de_sante==nom_AS_i)
  
  
  
  if(nrow(data_vfh_HZs_21_SA)>0){ # if there is an EVD case in this HA all HF are top priority regardless IPC score the FOSA becomes HIGH priority
    
    IPC_PRIORITY$priority[i] <- "HIGH"
    IPC_PRIORITY$evd_cases[i] <- nrow(data_vfh_HZs_21_SA)
    IPC_PRIORITY$eds_deaths[i] <- nrow(data_eds_HZs_21_SA)
    
  }else{ # if no EVD case
    
    
    if(nrow(data_eds_HZs_21_SA)>0){ # even if no EVD case, but if there is failure EDS in the HA, the FOSA is HIGH priority
      
      IPC_PRIORITY$priority[i] <- "HIGH"
      IPC_PRIORITY$eds_deaths[i] <- nrow(data_eds_HZs_21_SA)
      IPC_PRIORITY$evd_cases[i] <- nrow(data_vfh_HZs_21_SA)
      
    }else{ # if no EVD case and no failure EDS
      
      if(ipc_score_i < 50){ # but the IPC score is less than 50 the FOSA becomes HIGH priority
        
        
        IPC_PRIORITY$priority[i] <- "HIGH"
        IPC_PRIORITY$evd_cases[i] <- nrow(data_vfh_HZs_21_SA)
        IPC_PRIORITY$eds_deaths[i] <- nrow(data_eds_HZs_21_SA)
        
      }else if(ipc_score_i < 80 & ipc_score_i >= 50){ # if no EVD case and the score is between 
        
        IPC_PRIORITY$priority[i] <- "MODERATE"
        IPC_PRIORITY$evd_cases[i] <- nrow(data_vfh_HZs_21_SA)
        IPC_PRIORITY$eds_deaths[i] <- nrow(data_eds_HZs_21_SA)
        
      }else if(ipc_score_i >= 80){
        
        IPC_PRIORITY$priority[i] <- "LOW"
        IPC_PRIORITY$evd_cases[i] <- nrow(data_vfh_HZs_21_SA)
        IPC_PRIORITY$eds_deaths[i] <- nrow(data_eds_HZs_21_SA)
        
      }
      
      
    } 
    
  }
  
  
}

IPC_PRIORITY$deadline_for_action <- IPC_PRIORITY$date_enquete + 21


# saveRDS(IPC_PRIORITY, file = "/home/david/Dropbox/WHO_EBV_RESPONSE_2018/IPC_DRC_Paul_Allen/ipc/Algorithm_Model_IPC/11_11_2019_Priority_FOSA_OutPut.rds")

# write.csv(IPC_PRIORITY, file = "/home/david/Dropbox/WHO_EBV_RESPONSE_2018/IPC_DRC_Paul_Allen/ipc/Algorithm_Model_IPC/11_11_2019_Priority_FOSA_OutPut.csv")




# You may start here with dataset bencmarked ---------


IPC_PRIORITY <- readRDS("/home/david/Dropbox/WHO_EBV_RESPONSE_2018/IPC_DRC_Paul_Allen/ipc/Algorithm_Model_IPC/11_11_2019_Priority_FOSA_OutPut.rds")


for(i in 1:nrow(IPC_PRIORITY)){
  
  if(is.na(IPC_PRIORITY$presence_partenaire[i])){
    
    IPC_PRIORITY$presence_partenaire[i] <- 0
    
  }
  
}



data_ipc_priority <- dplyr::select(IPC_PRIORITY, date_enquete, zone_de_sante, aire_de_sante,
                                   nom_fosa, type_structure_de_sante, categorie, secteur, presence_partenaire,
                                   nbre_partenaires, score_pci_100, evd_cases, eds_deaths, priority)

mydata <- data_ipc_priority

mydata$priority <- factor(mydata$priority)


# Model ----------


# Split the dataset in training and testing sub-sets

# sampling startegy to be able to catch all priority levels in any sample

# > table(mydata$priority)
# HIGH      LOW MODERATE 
# 518       18       23 

train <- data.frame(matrix(ncol = 13))
test <- data.frame(matrix(ncol = 13))

names(train) <- names(mydata)
names(test) <- names(mydata)


for(i in 1:length(unique(mydata$priority))){
  
  level_i <- unique(mydata$priority)[i]
  
  level_data <- dplyr::filter(mydata, mydata$priority==level_i)
  
  index_i <- sample(1:nrow(level_data),round(0.75*nrow(level_data)))
  
  train_i <- level_data[index_i,]
  test_i <- level_data[-index_i,]
  
  train <- rbind(train, train_i)
  test <- rbind(test, test_i)
  
}


train <- train[-1,]
test <- test[-1,]

train$priority <- factor(train$priority)
test$priority <- factor(test$priority)

train_mod <- train
test_mod <- test

train_mod <- train_mod[,10:13]
test_mod <- test_mod[,10:13]


normalize <- function(x){
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}


train_mod$score_pci_100 <- normalize(x=train_mod$score_pci_100)
train_mod$evd_cases <- normalize(x=train_mod$evd_cases)
train_mod$eds_deaths <- normalize(x=train_mod$eds_deaths)


test_mod$score_pci_100 <- normalize(x=test_mod$score_pci_100)
test_mod$evd_cases <- normalize(x=test_mod$evd_cases)
test_mod$eds_deaths <- normalize(x=test_mod$eds_deaths)




# Machine learning 

# Option 1: using caret package


fitControl <- caret::trainControl(method = "repeatedcv",   
                                  number = 10,     # number of folds  -resampling
                                  repeats = 10, # repeated ten times 
                                  search = "grid")    

# Training

model_rf <- caret::train(train_mod[, 1:3], train_mod[, 4], 
                         method="rf", # knn and others
                         metric = "Accuracy",
                         trControl = fitControl)

# Variable importance

ggplot(caret::varImp(model_rf, scale = TRUE))


# Prediction

# Predict the labels of the test set

predictions <- predict(model_rf, test_mod[,1:3]) # , type="prob") # model_knn


# Evaluate the predictions
table(predictions) == table(test_mod[,4])

# Confusion matrix 

confusionMatrix(predictions, test_mod[,4])


# Computing manually the precision

if(length(table(test_mod$priority == predictions))==1){
  
  precision <- (table(test_mod$priority == predictions)[[1]])/sum(table(test_mod$priority == predictions)[[1]])
  
  precision <- round(precision, digits = 3)
  
}else{
  
  
  precision <- (table(test_mod$priority == predictions)[[2]])/sum(table(test_mod$priority == predictions)[[1]], table(test_mod$priority == predictions)[[2]])
  
  precision <- round(precision, digits = 3)
  
}


# Manual cross validation

cv.precision <- vector()

k <- 10

# Initialize progress bar

# pbar <- plyr::create_progress_bar('text')
# 
# pbar$init(k)

for(i in 1:k){
  
  train <- data.frame(matrix(ncol = 13))
  test <- data.frame(matrix(ncol = 13))
  
  names(train) <- names(mydata)
  names(test) <- names(mydata)
  
  
  for(i in 1:length(unique(mydata$priority))){
    
    level_i <- unique(mydata$priority)[i]
    
    level_data <- dplyr::filter(mydata, mydata$priority==level_i)
    
    index_i <- sample(1:nrow(level_data),round(0.75*nrow(level_data)))
    
    train_i <- level_data[index_i,]
    test_i <- level_data[-index_i,]
    
    train <- rbind(train, train_i)
    test <- rbind(test, test_i)
    
  }
  
  
  train <- train[-1,]
  test <- test[-1,]
  
  train$priority <- factor(train$priority)
  test$priority <- factor(test$priority)
  
  train_mod <- train
  test_mod <- test
  
  train_mod <- train_mod[,10:13]
  test_mod <- test_mod[,10:13]
  
  model_rf <- caret::train(train_mod[, 1:3], train_mod[, 4], method="rf")
  
  predictions <- predict(object=model_rf, test_mod[,1:3]) # model_knn
  
  if(length(table(test_mod$priority == predictions))==1){
    
    precision <- (table(test_mod$priority == predictions)[[1]])/sum(table(test_mod$priority == predictions)[[1]])
    
    precision <- round(precision, digits = 3)
    
  }else{
    
    
    precision <- (table(test_mod$priority == predictions)[[2]])/sum(table(test_mod$priority == predictions)[[1]], table(test_mod$priority == predictions)[[2]])
    
    precision <- round(precision, digits = 3)
    
  }
  
  cv.precision <- c(cv.precision, precision) 
  
}


# Option 2: using randomForest package

rf_classifier <- randomForest(priority ~ ., data=train_mod, ntree=100, mtry=3, importance=TRUE)


varImpPlot(rf_classifier)


prediction_for_table <- predict(rf_classifier,test_mod[,1:3])

table(observed=test_mod[,4],predicted=prediction_for_table)


# ROC
# Calculate the probability of new observations belonging to each class
# prediction_for_roc_curve will be a matrix with dimensions data_set_size x number_of_classes

prediction_for_roc_curve <- predict(rf_classifier, test_mod[,1:3], type="prob")

# Use pretty colours:
pretty_colours <- c("#F8766D","#00BA38","#619CFF")


# Specify the different classes 
classes <- levels(test_mod$priority)
# For each class

for (i in 1:3){
  # Define which observations belong to class[i]
  true_values <- ifelse(test_mod[,4]==classes[i],1,0)
  # Assess the performance of classifier for class[i]
  pred <- prediction(prediction_for_roc_curve[,i],true_values)
  perf <- performance(pred, "tpr", "fpr")
  if (i==1)
  {
    plot(perf,main="ROC Curve",col=pretty_colours[i]) 
  }
  else
  {
    plot(perf,main="ROC Curve",col=pretty_colours[i],add=TRUE) 
  }
  # Calculate the AUC and print it to screen
  auc.perf <- performance(pred, measure = "auc")
  print(auc.perf@y.values)
}





