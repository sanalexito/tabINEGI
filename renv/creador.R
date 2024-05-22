library(devtools)
library(roxygen2)

load_all()

devtools::document()

roxygen2::roxygenise(clean = TRUE)


devtools::install()



#devtools::install_github("sanalexito/tabINEGI")

# Datos que usaré de ejemplo.
load("D:/OneDrive - INEGI/Respaldo/ENCIG/2019/Bases/base_trabajo.rdata")
pregs <- c("N_TRA", "P7_3", "P7_1","FAC_TRA", "UPM_DIS", "EST_DIS","ENT")
#Creating total variables
tencig <- tablas[[4]][, pregs]
use_data(tencig, overwrite = T)


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
  "Estado de México",
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
use_data(estados)
