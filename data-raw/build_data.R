# Code used for downloading and creating of the test data objects in the packages

# devtools::install_github("mpascariu/MortalityLaws")
library(MortalityLaws)

country = 'SWE'
usr = "..."
passw = "..."

HMD_Dx <- ReadHMD(what = 'Dx', 
                  countries = country, 
                  username = usr,
                  password = passw,
                  save = FALSE)$data

HMD_Ex <- ReadHMD(what = 'Ex', 
                  countries = country, 
                  username = usr,
                  password = passw,
                  save = FALSE)$data

yr <- 1980:2014
hmdDx <- as.data.frame(matrix(subset(HMD_Dx, Year %in% yr)$Total, nrow = 111))
hmdEx <- as.data.frame(matrix(subset(HMD_Ex, Year %in% yr)$Total, nrow = 111))
dimnames(hmdDx) = dimnames(hmdEx) <- list(0:110, yr)

out <- list(Dx = round(hmdDx, 0), Ex = round(hmdEx, 0))
ungroup.data <- structure(class = "ungroup.data", out)

devtools::use_data(ungroup.data, overwrite = TRUE)




