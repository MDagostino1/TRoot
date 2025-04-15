VR <- Virtual_Roots3   # generated in local

n_roots <- length(unique(VR$root))
# n_steps <- length(unique(VR$x))
VR_conductivities <- NULL

# Delete files in VR_loop and Jobs folders
unlink("Data/VR_loop/*")
unlink("Jobs/*")

#===============================================================================
# for n roots :
# each job will run 1 roots, with 30 nodes in each root -> n jobs
#===============================================================================
for (i in seq(1:length(unique(VR$root)))) {
  # print(i)
  root_temp <- unique(VR$root)[i]
  print(paste0("Exporting ", root_temp))
  VR_temp <- VR %>% filter(root == root_temp[1])
  write.csv(VR_temp, paste0("Data/VR_loop/VR_temp_", i, ".csv"))
}


#===============================================================================
# CREATE GRANAR JOB
#===============================================================================

file_list <- list.files("Data/VR_loop", pattern = ".csv")

for (j in seq(1,length(file_list))) {

  print(paste0("Creating job ", j))

  JOB <- c(paste0('#!/bin/sh \n#SBATCH --job-name=Job_',j,'      # Job name \n
#SBATCH --ntasks=1                    # Run on a single CPU \n
#SBATCH --mem=15gb                     # Memory limit \n
#SBATCH --output=my_job_',j,'.out    # Standard output and error log \n
#SBATCH --time=30:00:00               # Time limit hrs:min:sec \n
#SBATCH --output=results/my_job_',j,'.out    # Standard output and error logZ \n
#SBATCH --partition=Def \n
#
#
#
JOB_DIR=$HOME/GRANAR_MECHA_CECI/
SRC_DIR=$HOME/GRANAR_MECHA_CECI/
DST_DIR=$HOME/GRANAR_MECHA_CECI/results/
TMP_DIR=$GLOBALSCRATCH/users/temp_',j,'
#
#
# cd $JOB_DIR                             # Set directory to home
# if [ ! -d $TMP_DIR ]; then           # If TMP_DIR folder do not exist ;
echo $PWD
# echo Making TMP_DIR = $TMP_DIR
# mkdir -p $TMP_DIR                    # We create a folder called TMP_DIR
# fi
# cp -R $SRC_DIR/* $TMP_DIR            # We copy SRC_DIR into TMP_DIR
# cd $TMP_DIR/                         # We set the new TMP_DIR as directory
#
# echo $PWD
#
# Start simulations
module load releases/2022b
module load R
Rscript $SRC_DIR/Job_G_',j,'.R
#
#Done with simulations
touch $DST_DIR/Done_',j,'.txt
# Finish processing and copy results to user directory
#
# Copy all individual output files with the (numerical) order they belong
# cp -r $TMP_DIR/results/ $DST_DIR
# cd $DST_DIR   # go back to DST folder
# rm -rf $TMP_DIR  # delete TMP_DIR'
  ))

  write(JOB, paste0("Jobs/Job_G_",j,".job"))

  R_JOB <- c(paste0(
    'DIR_path <- getwd()
    setwd(dir = DIR_path)

    JOB_NUMBER <- ',j,'
    GRANAR2_path <- "GRANAR/R/"
    source("GRANAR/R/load_packages.R")
    library(rgeos)
    for (i in list.files(GRANAR2_path, pattern=".R")) {
      # print(i)
      source(paste0(GRANAR2_path, i))
    }

    file_list <- list.files("Data/VR_loop/", pattern = ".csv")
    VR_temp <- read.csv(paste0("Data/VR_loop/", file_list[',j,']))
    print("Loading data successfull.")

    print("Launching Make_Virtual_Anatomies...")

    Make_Virtual_Anatomies_G(VR_temp)
    '
  ))

  write(R_JOB, paste0("Jobs/Job_G_",j,".R"))

}

#===============================================================================
# CREATE MECHA JOB
#===============================================================================

file_list <- list.files("Data/VR_loop", pattern = ".csv")

for (j in seq(1,length(file_list))) {
  
  print(paste0("Creating job ", j))
  
  JOB <- c(paste0('#!/bin/sh \n#SBATCH --job-name=Job_',j,'      # Job name \n
#SBATCH --ntasks=1                    # Run on a single CPU \n
#SBATCH --mem=15gb                     # Memory limit \n
#SBATCH --output=my_job_',j,'.out    # Standard output and error log \n
#SBATCH --time=30:00:00               # Time limit hrs:min:sec \n
#SBATCH --output=results/my_job_',j,'.out    # Standard output and error logZ \n
#SBATCH --partition=Def \n
#
#
#
JOB_DIR=$HOME/GRANAR_MECHA_CECI/
SRC_DIR=$HOME/GRANAR_MECHA_CECI/
DST_DIR=$HOME/GRANAR_MECHA_CECI/results/
TMP_DIR=$GLOBALSCRATCH/users/temp_',j,'
#
#
cd $JOB_DIR                             # Set directory to home
if [ ! -d $TMP_DIR ]; then           # If TMP_DIR folder do not exist ;
echo $PWD
echo Making TMP_DIR = $TMP_DIR
mkdir -p $TMP_DIR                    # We create a folder called TMP_DIR
fi
cp -R $SRC_DIR/* $TMP_DIR            # We copy SRC_DIR into TMP_DIR
cd $TMP_DIR/                         # We set the new TMP_DIR as directory
#
echo $PWD
#
# Start simulations
module load R
Rscript $SRC_DIR/Job_M_',j,'.R
#
#Done with simulations
touch $DST_DIR/Done_',j,'.txt
# Finish processing and copy results to user directory
#
# Copy all individual output files with the (numerical) order they belong
cp -r $TMP_DIR/results/ $DST_DIR
cd $DST_DIR   # go back to DST folder
rm -rf $TMP_DIR  # delete TMP_DIR'
  ))
  
  write(JOB, paste0("Jobs/Job_M_",j,".job"))
  
  R_JOB <- c(paste0(
    'DIR_path <- getwd()
    setwd(dir = DIR_path)

    JOB_NUMBER <- ',j,'
    GRANAR2_path <- "GRANAR/R/"
    source("GRANAR/R/load_packages.R")
    for (i in list.files(GRANAR2_path, pattern=".R")) {
      # print(i)
      source(paste0(GRANAR2_path, i))
    }

    file_list <- list.files("Data/VR_loop/", pattern = ".csv")
    VR_temp <- read.csv(paste0("Data/VR_loop/", file_list[',j,']))
    print("Loading data successfull.")

    use_condaenv("MECHA2")
    print("Launching Make_Virtual_Anatomies...")

    mecha <- c("./MECHA/MECHAv4_septa_temp_V2.py")
    output <- c("./MECHA/Projects/granar/out/Tomato/Root/Project_Test/results/")
    
    AQP_list <- seq(from = 0.0001, to = 0.001, length.out = 10)
    
    Make_Virtual_Anatomies_M(Virtual_Roots = VR_temp,
                             MECHA_path = mecha,
                             output_path = output,
                             AQP_list = AQP_list)
    '
  ))
  
  write(R_JOB, paste0("Jobs/Job_M_",j,".R"))
  
}
