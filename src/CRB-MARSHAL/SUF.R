# Integration des SUF

Calcul_SUF = function(hydraulics){
  
  # Integre les SUF sur 50 noeuds

  SUF_init <- hydraulics$suf1@x                        # valeures de base                                     
  SUF <- c()                                           # initiation valeurs finales
  
  lmax <- abs(min(current_rootsystem$z1))              # longueur max de la plante
  nodes <- 50                                          # nombre de noeuds hydrus
  pmax <- 200                                          # profondeur du profil de sol (hydrus)
  
  max_nodes <- as.integer(lmax*nodes/pmax)   # nombre de noeuds a integrer = longueur max de la plante * 20/200
  step <- as.integer(length(hydraulics$suf1@x)/max_nodes)
  start <- 1
  stop <- step
  for (i in c(1:max_nodes)){
    #print(paste0("start : ",start," stop : ",stop))
    SUF <- c(SUF, sum(SUF_init[start:stop]))
    start <- start+step
    stop <- stop+step
  }
  
  # Complete SUF pour avoir un vecteur de longueur #nodes
  zeros = c(1:(50-length(SUF)))*0
  SUF = c(SUF,zeros)
  return(SUF)
}
