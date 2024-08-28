Hydrus_valence <- function(SUF,Krs = 0.1636661, Kcomp = 0.1749219){

  #===============================================================================
  # INPUTS
  #===============================================================================
  # NB : modifier les parametres sol etc mannuellement dans hydrus
  
  setwd(Hydrus_Path)
  
  #___________
  # 1.PROFIL : 
  
  profile <- read_file(paste0("Valence/PROFILE - Copie.DAT"))
  profile <- strsplit(profile, "\r\n")
  
  indice = 1
  profile2 = profile[[1]][1:5]
  
  for(line in profile[[1]][6:55]){
    
    #TEST
    #line = profile[[1]][16]
    #print("line : ")
    #print(line)
    
    # extrait la ieme ligne et la decompose
    line <- strsplit(line," ")
    line2 = c()
    for(j in line[[1]]){
      if(j == ""){next}
      else{line2 = c(line2,j)}
    }
    
    line2[6] = SUF[indice]
    #print(line2)
    
    line_clean = NULL
    for(i in line2){line_clean = paste(line_clean," ",i)}
    
    profile2 <- c(profile2, line_clean)
    
    # augmente l'indice du SUF
    indice <- indice + 1
  }
  
  profile2 <- c(profile2, profile[[1]][56])
  
  final_profile <- file(paste0(Hydrus_Path,"/Valence/PROFILE.DAT"))
  writeLines(profile2,final_profile)
  close(final_profile)
  
  #___________
  # 2. Krs & Kcomp :
  
  Options <- read_file(paste0(Hydrus_Path,"Valence/Options - Copie.in"))
  Options <- strsplit(Options, "\r\n")
  Options <- Options[[1]]
  
  # Krs : 
  Krs_line <- Options[10]
  Krs_line <- strsplit(Krs_line, " ")
  Krs_line[[1]][1] <- Krs                  #Change Krs
  
  Krs_final <- NULL
  for(i in Krs_line[[1]]){Krs_final = paste(Krs_final," ",i)}
  
  # Kcomp :
  Kcomp_line <- Options[13]
  Kcomp_line <- strsplit(Kcomp_line, " ")
  Kcomp_line[[1]][1] <- Kcomp                       #Change Kcomp
  
  Kcomp_final <- NULL
  for(i in Kcomp_line[[1]]){Kcomp_final = paste(Kcomp_final," ",i)}
  
  # Export
  l <-c(Options[1:9],
        Krs_final,
        Options[11:12],
        Kcomp_final)
  
  final_options <- file(paste0(Hydrus_Path,"Valence/Options.in"))
  writeLines(l,final_options)
  close(final_options)
  
  #===============================================================================
  # RUN HYDRUS
  #===============================================================================
  
  system("H1D_calc.exe")
  
  #===============================================================================
  # OUTPUTS
  #===============================================================================
  
  setwd(Main_Path)
  source("Outputs.R")
  outputs <- Outputs(paste0(Hydrus_Path,"Valence/T_level.out"))
  return(outputs)
}

################################################################################
################################################################################

Hydrus_toulouse <- function(SUF,Krs = 0.1636661, Kcomp = 0.1749219){
  
  #===============================================================================
  # INPUTS
  #===============================================================================
  # NB : modifier les parametres sol etc mannuellement dans hydrus
  
  setwd(Hydrus_Path)
  
  #___________
  # 1.PROFIL : 
  
  profile <- read_file(paste0(Hydrus_Path,"Toulouse/PROFILE - Copie.DAT"))
  profile <- strsplit(profile, "\r\n")
  
  indice = 1
  profile2 = profile[[1]][1:5]
  
  for(line in profile[[1]][6:55]){
    
    # extrait la ieme ligne et la decompose
    line <- strsplit(line," ")
    line2 = c()
    for(j in line[[1]]){
      if(j == ""){next}
      else{line2 = c(line2,j)}
    }
    
    line2[6] = SUF[indice]
    
    line_clean = NULL
    for(i in line2){line_clean = paste(line_clean," ",i)}
    
    profile2 <- c(profile2, line_clean)
    
    # augmente l'indice du SUF
    indice <- indice + 1
  }
  
  profile2 <- c(profile2, profile[[1]][56])
  
  final_profile <- file(paste0(Hydrus_Path,"Toulouse/PROFILE.DAT"))
  writeLines(profile2,final_profile)
  close(final_profile)
  
  #___________
  # 2. Krs & Kcomp :
  
  Options <- read_file(paste0(Hydrus_Path,"Toulouse/Options - Copie.in"))
  Options <- strsplit(Options, "\r\n")
  Options <- Options[[1]]
  
  # Krs : 
  Krs_line <- Options[10]
  Krs_line <- strsplit(Krs_line, " ")
  Krs_line[[1]][1] <- Krs                  #Change Krs
  
  Krs_final <- NULL
  for(i in Krs_line[[1]]){Krs_final = paste(Krs_final," ",i)}
  
  # Kcomp :
  Kcomp_line <- Options[13]
  Kcomp_line <- strsplit(Kcomp_line, " ")
  Kcomp_line[[1]][1] <- Kcomp                       #Change Kcomp
  
  Kcomp_final <- NULL
  for(i in Kcomp_line[[1]]){Kcomp_final = paste(Kcomp_final," ",i)}
  
  # Export
  l <-c(Options[1:9],
        Krs_final,
        Options[11:12],
        Kcomp_final)
  
  final_options <- file(paste0(Hydrus_Path,"Toulouse/Options.in"))
  writeLines(l,final_options)
  close(final_options)
  
  #===============================================================================
  # RUN HYDRUS
  #===============================================================================
  
  system("H1D_calc.exe")
  
  #===============================================================================
  # OUTPUTS
  #===============================================================================
  
  setwd(Main_Path)
  source("Outputs.R")
  outputs <- Outputs(paste0(Hydrus_Path,"Toulouse/T_level.out"))
  return(outputs)
}

################################################################################
################################################################################

Hydrus_laspalmas <- function(SUF,Krs = 0.1636661, Kcomp = 0.1749219){
  
  #===============================================================================
  # INPUTS
  #===============================================================================
  # NB : modifier les parametres sol etc mannuellement dans hydrus
  
  setwd(Hydrus_Path)
  
  #___________
  # 1.PROFIL : 
  
  profile <- read_file(paste0(Hydrus_Path,"Las Palmas/PROFILE - Copie.DAT"))
  profile <- strsplit(profile, "\r\n")
  
  indice = 1
  profile2 = profile[[1]][1:5]
  
  for(line in profile[[1]][6:55]){
    
    #TEST
    #line = profile[[1]][16]
    #print("line : ")
    #print(line)
    
    # extrait la ieme ligne et la decompose
    line <- strsplit(line," ")
    line2 = c()
    for(j in line[[1]]){
      if(j == ""){next}
      else{line2 = c(line2,j)}
    }
    
    line2[6] = SUF[indice]
    #print(line2)
    
    line_clean = NULL
    for(i in line2){line_clean = paste(line_clean," ",i)}
    
    profile2 <- c(profile2, line_clean)
    
    # augmente l'indice du SUF
    indice <- indice + 1
  }
  
  profile2 <- c(profile2, profile[[1]][56])
  
  final_profile <- file(paste0(Hydrus_Path,"Las Palmas/PROFILE.DAT"))
  writeLines(profile2,final_profile)
  close(final_profile)
  
  #___________
  # 2. Krs & Kcomp :
  
  Options <- read_file(paste0(Hydrus_Path,"Las Palmas/Options - Copie.in"))
  Options <- strsplit(Options, "\r\n")
  Options <- Options[[1]]
  
  # Krs : 
  Krs_line <- Options[10]
  Krs_line <- strsplit(Krs_line, " ")
  Krs_line[[1]][1] <- Krs                  #Change Krs
  
  Krs_final <- NULL
  for(i in Krs_line[[1]]){Krs_final = paste(Krs_final," ",i)}
  
  # Kcomp :
  Kcomp_line <- Options[13]
  Kcomp_line <- strsplit(Kcomp_line, " ")
  Kcomp_line[[1]][1] <- Kcomp                       #Change Kcomp
  
  Kcomp_final <- NULL
  for(i in Kcomp_line[[1]]){Kcomp_final = paste(Kcomp_final," ",i)}
  
  # Export
  l <-c(Options[1:9],
        Krs_final,
        Options[11:12],
        Kcomp_final)
  
  final_options <- file(paste0(Hydrus_Path,"Las Palmas/Options.in"))
  writeLines(l,final_options)
  close(final_options)
  
  #===============================================================================
  # RUN HYDRUS
  #===============================================================================
  
  system("H1D_calc.exe")
  
  #===============================================================================
  # OUTPUTS
  #===============================================================================
  
  setwd(Main_Path)
  source("Outputs.R")
  #setwd("C:/Users/dagos/Desktop/Unif/2020-2021/Q2/LBRAI2219/Hydrus_1D_Couvreur/Hydrus_1D_Couvreur - Copie/Valence")
  outputs <- Outputs(paste0("Las Palmas/T_level.out"))
  return(outputs)
}

################################################################################
################################################################################

get_h <- function(path){
  
  profile_out <- read_file(path)
  profile_out <- strsplit(profile_out, "\r\n")
  profile_out = profile_out[[1]]

  H_list = list()  
  indice = 14
  for(i in c(1:8)){
    day = profile_out[indice:(indice+49)]
    day_vec = c()
    for(line in day){
      line <- strsplit(line," ")
      line2 = c()
      for(j in line[[1]]){
        if(j == ""){next}
        else{line2 = c(line2,j)}}
      day_vec = c(day_vec, as.numeric(line2[3]))
    }
    
    day_vec = list(day_vec)
    H_list = c(H_list, day_vec)
    
    
    indice = indice+49+10
  }
  return(H_list)  
}

