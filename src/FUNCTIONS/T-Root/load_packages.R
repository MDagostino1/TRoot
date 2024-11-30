load_packages <- function(pkg_list){
  for(i in pkg_list){
    if (!requireNamespace(i, quietly = TRUE)) {
      try(install.packages(i, dependencies = TRUE))
      # If not installed, install the package
    }
    # Load the package
    library(i, character.only = TRUE)  
  }
}

