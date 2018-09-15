get_countries <- function(df) {

  vlat <- c()
  vlong <- c()
  i <- 1
 
  for(i in 1:nrow(df_fire)) {
    vlat[i] <- as.numeric(df_fire[i, "lat"])
    vlong[i] <- as.numeric(df_fire[i, "long"])
    i <- i + 1
  }  

  vector_resultados <- find_pref2_mod(vlong, vlat, apiKey)  
  
  return(vector_resultados)
}


