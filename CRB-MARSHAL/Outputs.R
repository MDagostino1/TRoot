Outputs <- function(fileName){

  #fileName = "C:/Users/dagos/Desktop/Unif/2020-2021/Q2/LBRAI2219/Hydrus_1D_Couvreur/Hydrus_1D_Couvreur - Copie/Valence/T_level.out"
  
  outputs <- read_file(fileName)
  outputs <- strsplit(outputs, "\r\n")
  outputs <- outputs[[1]]
  #names <- c(Time,rTop,rRoot,vTop,vRoot,vBot,sum_rTop,sum_rRoot,sum_vTop,sum_vRoot,sum_vBot,Top,hRoot,hBot,RunOff,sum_RunOff,Volume,sum_Infil,sum_Evap,TLevel,Cum_WTrans,SnowLayer)
  
  data <- data.frame(Time = c(),
                     rTop = c(),
                     rRoot = c(),
                     vTop = c(),
                     vRoot = c(),
                     vBot = c(),
                     sum_rTop = c(),
                     sum_rRoot = c(),
                     sum_vTop = c(),
                     sum_vRoot = c(),
                     sum_vBot = c(),
                     hTop = c(),
                     hRoot = c(),
                     hBot = c(),
                     RunOff = c(),
                     sum_RunOff = c(),
                     Volume = c(),
                     sum_Infil = c(),
                     sum_Evap = c(),
                     TLevel = c(),
                     Cum_WTrans = c(),
                     SnowLayer = c())
  
  for(line in outputs[10:(length(outputs)-1)]){
    
    #line = outputs[11]
    line <- strsplit(line," ")
    line2 = c()
    for(j in line[[1]]){
      if(j == ""){next}
      else{line2 = c(line2,j)}
    }
    
    temp <- data.frame(Time = as.numeric(line2[1]),
                       rTop = as.numeric(line2[2]),
                       rRoot = as.numeric(line2[3]),
                       vTop = as.numeric(line2[4]),
                       vRoot = as.numeric(line2[5]),
                       vBot = as.numeric(line2[6]),
                       sum_rTop = as.numeric(line2[7]),
                       sum_rRoot = as.numeric(line2[8]),
                       sum_vTop = as.numeric(line2[9]),
                       sum_vRoot = as.numeric(line2[10]),
                       sum_vBot = as.numeric(line2[11]),
                       hTop = as.numeric(line2[12]),
                       hRoot = as.numeric(line2[13]),
                       hBot = as.numeric(line2[14]),
                       RunOff = as.numeric(line2[15]),
                       sum_RunOff = as.numeric(line2[16]),
                       Volume = as.numeric(line2[17]),
                       sum_Infil = as.numeric(line2[18]),
                       sum_Evap = as.numeric(line2[19]),
                       TLevel = as.numeric(line2[20]),
                       Cum_WTrans = as.numeric(line2[21]),
                       SnowLayer = as.numeric(line2[22]))
    data <- rbind(data,temp)
  }
  return(data)
}

#test <- data %>% select(Time,vRoot)
#ggplot(test, aes(Time,vRoot)) + geom_line() + theme_bw() + ggtitle("vRoot")
#test <- Outputs("T_level.out")
