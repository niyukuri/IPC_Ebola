
# Function to pull data of recent evaluation in the scorecard database

recent_evaluation_only_fosa <- function(data_hz = data_hz){ # ------------ function
  
  index_fosa <- which(is.na(data_hz$nom_fosa_new)) 
  
  if(length(index_fosa) >= 1){
    
    data_hz_df <- data_hz[-c(index_fosa),]
    
  }else{
    
    data_hz_df <- data_hz
    
  }
  
  
  
  f_names <- unique(data_hz_df$nom_fosa_new)
  
  
  new_data <- data.frame()
  
  for(i in 1:length(f_names)){
    
    
    nom_fosa_i <- f_names[i]
    
    
    dt_f <- dplyr::filter(data_hz_df, data_hz_df$nom_fosa_new==paste0(nom_fosa_i))
    
    dt_f <- arrange(dt_f,date_enquete)
    
    date_recent <- dt_f$date_enquete[nrow(dt_f)]
    
    dt_f_i <- dplyr::filter(dt_f, dt_f$date_enquete==paste0(date_recent)) # RMQ: more than one submission of the surveys
    
    dt_f_i <- dt_f_i[1,] # just take te first survey
    
    new_data <- rbind(new_data, dt_f_i)
    
    
  }
  
  return(new_data)
  
  
}

