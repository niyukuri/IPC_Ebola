
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

# Set SEED

set.seed(777)

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


# NA for partners -- > 0


for(i in 1:nrow(IPC_PRIORITY)){
  
  if(is.na(IPC_PRIORITY$nbre_partenaires[i])){
    
    IPC_PRIORITY$nbre_partenaires[i] <- 0
    
  }
  
}



data_ipc_priority <- dplyr::select(IPC_PRIORITY, date_enquete, zone_de_sante, aire_de_sante,
                                   nom_fosa, type_structure_de_sante, categorie, secteur, presence_partenaire,
                                   nbre_partenaires, score_pci_100, evd_cases, eds_deaths, priority)

mydata <- data_ipc_priority


# Categorize IPC score 

mydata$score_category <- rep(NA, length(mydata$score_pci_100))

for(i in 1:length(mydata$score_pci_100)){
  
  if(mydata$score_pci_100[i]<50){
    
    mydata$score_category[i] <- "g_50"
    
  }else if(mydata$score_pci_100[i]>=80){
    
    mydata$score_category[i] <- "g_80"
    
  }else{
    mydata$score_category[i] <- "g_50_80"
  }
  
}


# Categorize EVD: presence or not

mydata$evd_category <- rep(NA, length(mydata$evd_cases))

for(i in 1:length(mydata$evd_cases)){
  
  if(mydata$evd_cases[i]==0){
    
    mydata$evd_category[i] <- "NO"
    
  }else{
    
    mydata$evd_category[i] <- "OUI"
  }
  
}



# Categorize unsafe burial: presence or not

mydata$eds_category <- rep(NA, length(mydata$eds_deaths))

for(i in 1:length(mydata$eds_deaths)){
  
  if(mydata$eds_deaths[i]==0){
    
    mydata$eds_category[i] <- "NO"
    
  }else{
    
    mydata$eds_category[i] <- "OUI"
  }
  
}



# DATA for two options:


select_variables_ml_1 <- c("nbre_partenaires", "type_structure_de_sante", 
                           "categorie", "secteur",
                           "score_pci_100", 
                           "evd_cases", 
                           "eds_deaths", "priority")


select_variables_ml_2 <- c("presence_partenaire", "type_structure_de_sante", 
                           "categorie", "secteur",
                           "score_category", 
                           "evd_category", 
                           "eds_category", "priority")


mydata_1 <- dplyr::select(mydata, select_variables_ml_1)

names(mydata_1) <- c("Partners", "Type", "Category", "Sector", "Score", "EVD", "SDB", "Priority")

mydata_2 <- dplyr::select(mydata, select_variables_ml_2)

names(mydata_2) <- c("Partners", "Type", "Category", "Sector", "Score", "EVD", "SDB", "Priority")




# Model ----------


# Split the dataset in training and testing sub-sets

# sampling startegy to be able to catch all priority levels in any sample


# For option 1:

train_1 <- data.frame(matrix(ncol = 8))
test_1 <- data.frame(matrix(ncol = 8))

names(train_1) <- names(mydata_1)
names(test_1) <- names(mydata_1)


for(i in 1:length(unique(mydata_1$Priority))){
  
  level_i <- unique(mydata_1$Priority)[i]
  
  level_data <- dplyr::filter(mydata_1, mydata_1$Priority==level_i)
  
  index_i <- sample(1:nrow(level_data),round(0.75*nrow(level_data)))
  
  train_i <- level_data[index_i,]
  test_i <- level_data[-index_i,]
  
  train_1 <- rbind(train_1, train_i)
  test_1 <- rbind(test_1, test_i)
  
}

train_1 <- train_1[-1,]
test_1 <- test_1[-1,]



# For option 2: 

train_2 <- data.frame(matrix(ncol = 8))
test_2 <- data.frame(matrix(ncol = 8))

names(train_2) <- names(mydata_2)
names(test_2) <- names(mydata_2)


for(i in 1:length(unique(mydata_2$Priority))){
  
  level_i <- unique(mydata_2$Priority)[i]
  
  level_data <- dplyr::filter(mydata_2, mydata_2$Priority==level_i)
  
  index_i <- sample(1:nrow(level_data),round(0.75*nrow(level_data)))
  
  train_i <- level_data[index_i,]
  test_i <- level_data[-index_i,]
  
  train_2 <- rbind(train_2, train_i)
  test_2 <- rbind(test_2, test_i)
  
}


train_2 <- train_2[-1,]
test_2 <- test_2[-1,]


# Normalize for numeric values

normalize <- function(x){
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}


# Only option 1 needs normaliziation

train_1$Score <- normalize(x=train_1$Score)
train_1$EVD <- normalize(x=train_1$EVD)
train_1$SDB <- normalize(x=train_1$SDB)

test_1$Score <- normalize(x=test_1$Score)
test_1$EVD <- normalize(x=test_1$EVD)
test_1$SDB <- normalize(x=test_1$SDB)


# Factors

# Option 1:

# Train

train_1$Type <- factor(train_1$Type)
train_1$Category <- factor(train_1$Category)
train_1$Sector <- factor(train_1$Sector)
train_1$Priority <- factor(train_1$Priority)


# Test

test_1$Type <- factor(test_1$Type)
test_1$Category <- factor(test_1$Category)
test_1$Sector <- factor(test_1$Sector)
test_1$Priority <- factor(test_1$Priority)

#

# Option 2: 

# Train

train_2$Partners <- factor(train_2$Partners)
train_2$Score <- factor(train_2$Score)
train_2$EVD <- factor(train_2$EVD)
train_2$SDB <- factor(train_2$SDB)


train_2$Type <- factor(train_2$Type)
train_2$Category <- factor(train_2$Category)
train_2$Sector <- factor(train_2$Sector)
train_2$Priority <- factor(train_2$Priority)


# Test

test_2$Partners <- factor(test_2$Partners)
test_2$Score <- factor(test_2$Score)
test_2$EVD <- factor(test_2$EVD)
test_2$SDB <- factor(test_2$SDB)

test_2$Type <- factor(test_2$Type)
test_2$Category <- factor(test_2$Category)
test_2$Sector <- factor(test_2$Sector)
test_2$Priority <- factor(test_2$Priority)


# Modele 1: -----------


# Using caret -------------------

fitControl <- caret::trainControl(method = "repeatedcv",   
                                  number = 10,     # number of folds  -resampling
                                  repeats = 10, # repeated ten times 
                                  search = "grid")    

# Model training

model_rf_caret_1 <- caret::train(train_1[, 1:7], train_1[, 8], 
                                 method="rf", # knn and others
                                 metric = "Accuracy",
                                 trControl = fitControl)

# Variable importance

ggplot(caret::varImp(model_rf_caret_1, scale = TRUE))


# Prediction

# Predict the labels of the test set

predictions_1 <- predict(model_rf_caret_1, test_1[,1:7]) # , type="prob") # model_knn


# Evaluate the predictions
table(predictions_1) == table(test_1[,8])

# Confusion matrix 

confusionMatrix(predictions_1, test_1[,8])


# ROC
# Calculate the probability of new observations belonging to each class
# prediction_for_roc_curve will be a matrix with dimensions data_set_size x number_of_classes

prediction_for_roc_curve_caret_1 <- predict(model_rf_caret_1, test_1[,1:7], type="prob") # predict(rf_classifier, test_mod[,1:7], type="prob")

# Use pretty colours:
pretty_colours <- c("#F8766D","#00BA38","#619CFF")


# Specify the different classes 
classes <- levels(test_1$Priority)
# For each class

for (i in 1:3){
  # Define which observations belong to class[i]
  true_values <- ifelse(test_1[,8]==classes[i],1,0)
  # Assess the performance of classifier for class[i]
  pred <- prediction(prediction_for_roc_curve_caret_1[,i],true_values)
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






# Using randomForest package -----------


model_rf_randFor_1 <- randomForest(Priority ~ ., data=train_1, ntree=1000, mtry=7, importance=TRUE)


varImpPlot(model_rf_randFor_1)


prediction_for_table_1 <- predict(model_rf_randFor_1, test_1[,1:7])

table(observed=test_1[,8],predicted=prediction_for_table_1)

confusionMatrix(prediction_for_table_1, test_1[,8])

# ROC
# Calculate the probability of new observations belonging to each class
# prediction_for_roc_curve will be a matrix with dimensions data_set_size x number_of_classes

prediction_for_roc_curve_rf_1 <- predict(model_rf_randFor_1, test_1[,1:7], type="prob") # predict(rf_classifier, test_mod[,1:7], type="prob")

# Use pretty colours:
pretty_colours <- c("#F8766D","#00BA38","#619CFF")


# Specify the different classes 
classes <- levels(test_1$Priority)
# For each class

for (i in 1:3){
  # Define which observations belong to class[i]
  true_values <- ifelse(test_1[,8]==classes[i],1,0)
  # Assess the performance of classifier for class[i]
  pred <- prediction(prediction_for_roc_curve_rf_1[,i],true_values)
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






# Modele 2: -----------





# Using caret -------------------


# Model training

model_rf_caret_2 <- caret::train(train_2[, 1:7], train_2[, 8], 
                                 method="rf", # knn and others
                                 metric = "Accuracy",
                                 trControl = fitControl)

# Variable importance

ggplot(caret::varImp(model_rf_caret_2, scale = TRUE))


# Prediction

# Predict the labels of the test set

predictions_2 <- predict(model_rf_caret_2, test_2[,1:7]) # , type="prob") # model_knn


# Evaluate the predictions
table(predictions_2) == table(test_2[,8])

# Confusion matrix 

confusionMatrix(predictions_2, test_2[,8])


# ROC
# Calculate the probability of new observations belonging to each class
# prediction_for_roc_curve will be a matrix with dimensions data_set_size x number_of_classes

prediction_for_roc_curve_caret_2 <- predict(model_rf_caret_2, test_2[,1:7], type="prob") # predict(rf_classifier, test_mod[,1:7], type="prob")

# Use pretty colours:
pretty_colours <- c("#F8766D","#00BA38","#619CFF")


# Specify the different classes 
classes <- levels(test_2$Priority)
# For each class

for (i in 1:3){
  # Define which observations belong to class[i]
  true_values <- ifelse(test_2[,8]==classes[i],1,0)
  # Assess the performance of classifier for class[i]
  pred <- prediction(prediction_for_roc_curve_caret_2[,i],true_values)
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






# Using randomForest package -----------


model_rf_randFor_2 <- randomForest(Priority ~ ., data=train_2, ntree=1000, mtry=7, importance=TRUE)


varImpPlot(model_rf_randFor_2)


prediction_for_table_2 <- predict(model_rf_randFor_2, test_2[,1:7])

table(observed=test_2[,8],predicted=prediction_for_table_2)

confusionMatrix(prediction_for_table_2, test_2[,8])

# ROC
# Calculate the probability of new observations belonging to each class
# prediction_for_roc_curve will be a matrix with dimensions data_set_size x number_of_classes

prediction_for_roc_curve_rf_2 <- predict(model_rf_randFor_2, test_2[,1:7], type="prob") # predict(rf_classifier, test_mod[,1:7], type="prob")

# Use pretty colours:
pretty_colours <- c("#F8766D","#00BA38","#619CFF")


# Specify the different classes 
classes <- levels(test_2$Priority)
# For each class

for (i in 1:3){
  # Define which observations belong to class[i]
  true_values <- ifelse(test_2[,8]==classes[i],1,0)
  # Assess the performance of classifier for class[i]
  pred <- prediction(prediction_for_roc_curve_rf_2[,i],true_values)
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





