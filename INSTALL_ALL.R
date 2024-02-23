# if renv not installed, install it
if(!require("renv")) install.packages("renv")

# collect all dependencies in current directory
depend <- renv::dependencies(
  path = ".")$Package |> unique()

# check which dependencies are not installed
packages_not_installed <- depend[!depend %in% installed.packages()]

# install needed dependencies for the pkge
install.packages(packages_not_installed)
