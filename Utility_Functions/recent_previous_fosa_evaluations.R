
recent_previous_fosa_evaluations <- function(data_hz = data_hz){
  
 
  index_fosa <- which(is.na(data_hz$nom_fosa_new))
  
  if(length(index_fosa) >= 1){
    
    data_hz_df <- data_hz[-c(index_fosa),]
    
  }else{
    
    data_hz_df <- data_hz
    
  }
  
  
  
  f_names <- names(table(data_hz_df$nom_fosa_new))
  
  scores_recent <- vector()
  scores_previous <- vector()
  fosa <- vector()
  partenaires_recent <- vector()
  partenaires_previous <- vector()
  categorie_fosa <- vector()
  nom_zone_sante_hz <- vector()
  
  for(i in 1:length(table(data_hz_df$nom_fosa_new))){
    
    j <- table(data_hz_df$nom_fosa_new)[[i]]
    
    if(j>1){
      
      dt_f <- dplyr::filter(data_hz_df, data_hz_df$nom_fosa_new==paste0(f_names[i]))
      
      scores_recent_i <- dt_f$score_pci_100[nrow(dt_f)]
      scores_previous_i <- dt_f$score_pci_100[nrow(dt_f)-1]
      fosa_i <- paste0(f_names[i])
      nom_zone_sante_hz_i <- dt_f$zone_de_sante[1]
      
      if(is.na(dt_f$nbre_partenaires[nrow(dt_f)])){
        partenaires_recent_i <- 0
      }else{
        partenaires_recent_i <- dt_f$nbre_partenaires[nrow(dt_f)]
      }
      
      
      if(is.na(dt_f$nbre_partenaires[nrow(dt_f)-1])){
        partenaires_previous_i <- 0
      }else{
        partenaires_previous_i <- dt_f$nbre_partenaires[nrow(dt_f)-1]
      }
      
      
      categorie_fosa_i <- unique(dt_f$categorie)[1]
      
      scores_recent <- c(scores_recent, scores_recent_i)
      scores_previous <- c(scores_previous, scores_previous_i)
      fosa <- c(fosa, fosa_i)
      nom_zone_sante_hz <- c(nom_zone_sante_hz, nom_zone_sante_hz_i)
      
      
      partenaires_recent <- c(partenaires_recent, partenaires_recent_i)
      partenaires_previous <- c(partenaires_previous, partenaires_previous_i)
      categorie_fosa <- c(categorie_fosa, categorie_fosa_i)
      
      
    }
    
  }
  

  hz_ev <- data.frame(nom_zone_sante_hz, fosa, categorie_fosa, scores_recent, scores_previous, partenaires_previous, partenaires_recent)
  return(hz_ev)
  
  
}

