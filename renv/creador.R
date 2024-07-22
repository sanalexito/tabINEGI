library(devtools)
library(roxygen2)

load_all()

devtools::document()

roxygen2::roxygenise(clean = TRUE)


devtools::install()



#devtools::install_github("sanalexito/tabINEGI")

#install_github("Momocs",username="jfpalomeque")

# Datos que usaré de ejemplo.
load("D:/OneDrive - INEGI/Respaldo/ENCIG/2019/Bases/base_trabajo.rdata")
pregs <- c("N_TRA", "P7_3", "P7_1","FAC_TRA", "UPM_DIS", "EST_DIS","ENT")
#Creating total variables
tencig <- tablas[[4]][, pregs]


# Datos ejemplo ENVE
load("D:/OneDrive - INEGI/Respaldo/ENVE/Bases_ENVE/bases_2012-2022.Rdata")
modulo <- tmv_2022[,c("CONSEC", "ID_DELITO", "FAC_EXPA")]
prncpl <- tpv_2022[, c("CONSEC", "ENT")]
use_data(modulo, prncpl, tencig, overwrite = T)


#estados
estados <- c(
  "Estados Unidos Mexicanos",
  "Aguascalientes",
  "Baja California",
  "Baja California Sur",
  "Campeche",
  "Coahuila de Zaragoza",
  "Colima",
  "Chiapas",
  "Chihuahua",
  "Ciudad de México",
  "Durango",
  "Guanajuato",
  "Guerrero",
  "Hidalgo",
  "Jalisco",
  "México",
  "Michoacán de Ocampo",
  "Morelos",
  "Nayarit",
  "Nuevo León",
  "Oaxaca",
  "Puebla",
  "Querétaro",
  "Quintana Roo",
  "San Luis Potosí",
  "Sinaloa",
  "Sonora",
  "Tabasco",
  "Tamaulipas",
  "Tlaxcala",
  "Veracruz de Ignacio de la Llave",
  "Yucatán",
  "Zacatecas")
use_data(estados, overwrite = TRUE)
