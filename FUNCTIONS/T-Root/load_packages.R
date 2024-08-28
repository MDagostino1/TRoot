load_packages <- function(pkg_list){
  for(i in pkg_list){
    if (!requireNamespace(i, quietly = TRUE)) {
      # If not installed, install the package
      install.packages(i, dependencies = TRUE)
    }
    # Load the package
    library(i, character.only = TRUE)  
  }
}

