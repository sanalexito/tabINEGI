#' @title A Dog Function
#'
#' @description This function allows you to express your love for the superior furry animal.
#' @param agree Do you agree dogs are the best pet? Defaults to TRUE.
#' @details
#' Additional details...
#'
#' @keywords dogs, cats
#' @export
#' @examples
#' dogs_over_cats()


dogs_over_cats <- function(agree=TRUE){
  if(agree==TRUE){
    print("Woof woof!")
  }
  else {
    print("Try again.")
  }
}

# TAB 2 ------------------------------------------------------------------------
#' @title Tab_2
#'
#' @description This function allows you to obtain a list with tabulations corresponding to estimations, coefficients of variation, confidence intervals and standard errors. It requires a data set with the appropriate expansion factor and the sampling design of the "survey" library.
#' @param xx Vector containing the names of the variables that will be used in the calculation. Usually: c("TOT", paste0("TOT_", 1:n))
#' @param D Variable to make a disaggregation between quotation marks.
#' @param z  Labels for the data frame.
#' @keywords estimator, survey.
#' @return List of four entries.
#' @examples
#'
#'#Variable used in this case correspond to the ENCIG of the INEGI.
#'library(survey)
#'pregs <- c("N_TRA", "P7_3", "P7_1","FAC_TRA", "UPM_DIS", "EST_DIS","ENT")
#'#'#Creating total variables
#'t7 <- tencig[, pregs]
#'t7$TOT = ifelse(!t7[,pregs[1]]%in% '20', 1, 0)
#'t7$TOT_1 = ifelse(t7[,pregs[2]]%in%c('1','6'), 1, 0)
#'t7$TOT_2 = ifelse(t7[,pregs[2]]%in%'2', 1, 0)
#'t7$TOT_3 = ifelse(t7[,pregs[2]]%in%'3', 1, 0)
#'t7$TOT_4 = ifelse(t7[,pregs[2]]%in%'4', 1, 0)
#'t7$TOT_5 = ifelse(t7[,pregs[2]]%in%'5', 1, 0)
#'#Defining the sampling design using "survey".
#' asp <- svydesign(id = ~as.numeric(UPM_DIS), strata = ~as.numeric(EST_DIS), weight = ~FAC_TRA, data = t7)
#'#List with the calculations. In the first entry is the estimations resulting, CV, CI and SE, respectively.
#'tabulado <- Tab_2(c("TOT", paste0("TOT_",1:5)), "ENT", estados)
#'tabulado[[1]]
#'#Run this example on the console
#'
#'
Tab_2 <- function(xx, D, z) {

  pob0 <- svytotal(t7[,xx], asp)
  pob <- as.data.frame(pob0)
  cv_pob <- cv(pob0)*100
  int_pob <- confint(pob0, level = 0.95)
  int_pob <- as.data.frame(int_pob)
  se_pob <- SE(pob0)

  rel0 <- svyratio(t7[,xx], denominator=t7[,xx[1]], asp)
  rel_pob <- as.data.frame(rel0[[1]]*100)
  cv_rel_pob <- cv(rel0)*100
  int_rel_pob <- confint(rel0, level = 0.95)*100
  int_rel_pob <- as.data.frame(int_rel_pob)
  se_rel_pob <- SE(rel0)*100

  #desagregados de los nacionales por entidad
  ent0 <- svyby(t7[,xx], by = t7[,D], asp, svytotal)
  ent <- as.data.frame(ent0[, 2:(length(xx)+1)])
  cv_ent <- cv(ent0)*100
  int_ent <- confint(ent0, level = 0.95)
  int_ent <- as.data.frame(int_ent)
  se_ent <- SE(ent0)

  rel_ent0 <- svyby(t7[,xx], by = t7[,D], denominator= ~TOT, asp, svyratio)
  rel_ent <- as.data.frame(rel_ent0[,2:(length(xx)+1)])*100
  cv_rel_ent <- cv(rel_ent0)*100
  int_rel_ent <- confint(rel_ent0, level = 0.95)*100
  int_rel_ent <- as.data.frame(int_rel_ent)
  se_rel_ent <- SE(rel_ent0)*100

#---
  est_nal <- list()
  for(i in 1:length(xx)){
    est_nal[[i]] <- data.frame(pob[[1]][i], rel_pob[i, 1],NA)
  }
  est_nac <- do.call(cbind, est_nal)
  est_nac <- est_nac[-c(2, dim(est_nac)[2])]

  cv_nal <- list()
  for(i in 1:length(xx)){
    cv_nal[[i]] <- data.frame(cv_pob[[i]], cv_rel_pob[i], NA)
  }
  cv_nac <- do.call(cbind, cv_nal)
  cv_nac <- cv_nac[-c(2, dim(cv_nac)[2])]

  int_nal <- list()
  for(i in 1:length(xx)){
    int_nal[[i]] <- data.frame(int_pob[i, ], NA, int_rel_pob[i,], NA)
  }
  int_nac <- do.call(cbind, int_nal)
  int_nac <- int_nac[-c(3:5, dim(int_nac)[2])]

  se_nal <- list()
  for(i in 1:length(xx)){
    se_nal[[i]] <- data.frame(se_pob[[i]], se_rel_pob[i], NA)
  }
  se_nac <- do.call(cbind, se_nal)
  se_nac <- se_nac[-c(2, dim(se_nac)[2])]

#---
  est_des <- list()
  for(i in 1:length(xx)){
    est_des[[i]] <- data.frame(ent[i], rel_ent[i], NA)
  }
  est_ent <- do.call(cbind, est_des)
  est_ent <- est_ent[-c(2, dim(est_ent)[2])]

  cv_des <- list()
  for(i in 1:length(xx)){
    cv_des[[i]] <- data.frame(cv_ent[i], cv_rel_ent[i], NA)
  }
  cv_ent <- do.call(cbind, cv_des)
  cv_ent <- cv_ent[-c(2, dim(cv_ent)[2])]

  aa <- seq(1,dim(int_ent)[1]+length(xx), length(table(t7[,D])))

  for(i in 1:length(xx))eval(parse(text = paste0("
          int_a_",i," <- list()
          int_b_",i," <- list()
")))

  for(j in 1: length(xx))eval(parse(text = paste0("
for(i in aa[j]:(aa[j+1] - 1))
    int_a_",j,"[[i]] <- data.frame(int_ent[i,])
")))

  for(j in 1: length(xx))
    eval(parse(text = paste0("
    int_a_",j," <- do.call(rbind, int_a_",j,")
")))

  for(j in 1: length(xx))eval(parse(text = paste0("
for(i in aa[j]:(aa[j+1] - 1))
    int_b_",j,"[[i]] <- data.frame(int_rel_ent[i,])
")))

  for(j in 1: length(xx))
    eval(parse(text = paste0("
    int_b_",j," <- do.call(rbind, int_b_",j,")
")))

  int_ent <-data.frame(int_a_1, NA, int_b_1)

  for(i in 2:length(xx))eval(parse(text = paste0("
  int_ent <- data.frame(int_ent, NA, int_a_",i,",NA,int_b_",i,")
")))
  int_ent <- int_ent[-c(3:5)]

  se_des <- list()
  for(i in 1:length(xx)){
    se_des[[i]] <- data.frame(se_ent[i], se_rel_ent[i], NA)
  }
  se_ent <- do.call(cbind, se_des)
  se_ent <- se_ent[-c(2, dim(se_ent)[2])]
#---
  tab <- list()
  colnames(est_ent)<-colnames(est_nac)
  colnames(cv_ent)<-colnames(cv_nac)
  colnames(int_ent)<-colnames(int_nac)
  colnames(se_ent)<-colnames(se_nac)

  tab[[1]] <- rbind(est_nac, est_ent)
  tab[[2]] <- rbind(cv_nac, cv_ent)
  tab[[3]] <- rbind(int_nac, int_ent)
  tab[[4]] <- rbind(se_nac, se_ent)

  tab[[1]] <- cbind(z, tab[[1]])
  tab[[2]] <- cbind(z, tab[[2]])
  tab[[3]] <- cbind(z, tab[[3]])
  tab[[4]] <- cbind(z, tab[[4]])
  return(tab)
}

# TAB TASA ---------------------------------------------------------------------
#' @title Tab_tasa
#' @description This function allows you to obtain a list with rate for each 10 000 tabulations corresponding to estimations, coefficients of variation, confidence intervals and standard errors.
#' It requires a data set with the appropriate expansion factor and the sampling design of the "survey" library.
#' @param xx Vector containing the names of the variables that will be used in the calculation. Usually: c("TOT", paste0("TOT_", 1:n))
#' @param D Variable to make a disaggregation between quotation marks.
#' @param z  Labels for the data frame.
#' @keywords estimator, survey.
#' @return List of four entries.
#' @examples
Tab_tasa <- function(xx, D, z) {

  pob0 <- svytotal(t7[,xx],asp)
  pob <- as.data.frame(pob0)
  cv_pob <- cv(pob0)*100
  int_pob <- confint(pob0, level = 0.95)
  int_pob <- as.data.frame(int_pob)
  se_pob <- SE(pob0)

  rel0 <- svyratio(t7[,xx], denominator=t7[,xx[1]], asp)
  rel_pob <- as.data.frame(rel0[[1]]*10000)
  cv_rel_pob <- cv(rel0)*100
  int_rel_pob <- confint(rel0, level = 0.95)*10000
  int_rel_pob <- as.data.frame(int_rel_pob)
  se_rel_pob <- SE(rel0)*10000

  #desagregados de los nacionales por entidad
  ent0 <- svyby(t7[,xx], by = t7[,D], asp, svytotal)
  ent <- as.data.frame(ent0[, 2:(length(xx)+1)])
  cv_ent <- cv(ent0)*100
  int_ent <- confint(ent0, level = 0.95)
  int_ent <- as.data.frame(int_ent)
  se_ent <- SE(ent0)

  rel_ent0 <- svyby(t7[,xx], by = t7[,D], denominator= ~TOT, asp, svyratio)
  rel_ent <- as.data.frame(rel_ent0[,2:(length(xx)+1)])*10000
  cv_rel_ent <- cv(rel_ent0)*100
  int_rel_ent <- confint(rel_ent0, level = 0.95)*10000
  int_rel_ent <- as.data.frame(int_rel_ent)
  se_rel_ent <- SE(rel_ent0)*10000

  #---
  est_nal <- list()
  for(i in 1:length(xx)){
    est_nal[[i]] <- data.frame(pob[[1]][i], rel_pob[i, 1],NA)
  }
  est_nac <- do.call(cbind, est_nal)
  est_nac <- est_nac[-c(2, dim(est_nac)[2])]

  cv_nal <- list()
  for(i in 1:length(xx)){
    cv_nal[[i]] <- data.frame(cv_pob[[i]], cv_rel_pob[i], NA)
  }
  cv_nac <- do.call(cbind, cv_nal)
  cv_nac <- cv_nac[-c(2, dim(cv_nac)[2])]

  int_nal <- list()
  for(i in 1:length(xx)){
    int_nal[[i]] <- data.frame(int_pob[i, ], NA, int_rel_pob[i,], NA)
  }
  int_nac <- do.call(cbind, int_nal)
  int_nac <- int_nac[-c(3:5, dim(int_nac)[2])]

  se_nal <- list()
  for(i in 1:length(xx)){
    se_nal[[i]] <- data.frame(se_pob[[i]], se_rel_pob[i], NA)
  }
  se_nac <- do.call(cbind, se_nal)
  se_nac <- se_nac[-c(2, dim(se_nac)[2])]

  #---
  est_des <- list()
  for(i in 1:length(xx)){
    est_des[[i]] <- data.frame(ent[i], rel_ent[i], NA)
  }
  est_ent <- do.call(cbind, est_des)
  est_ent <- est_ent[-c(2, dim(est_ent)[2])]

  cv_des <- list()
  for(i in 1:length(xx)){
    cv_des[[i]] <- data.frame(cv_ent[i], cv_rel_ent[i], NA)
  }
  cv_ent <- do.call(cbind, cv_des)
  cv_ent <- cv_ent[-c(2, dim(cv_ent)[2])]

  aa <- seq(1,dim(int_ent)[1]+length(xx), length(table(t7[,D])))

  for(i in 1:length(xx))eval(parse(text = paste0("
          int_a_",i," <- list()
          int_b_",i," <- list()
")))

  for(j in 1: length(xx))eval(parse(text = paste0("
for(i in aa[j]:(aa[j+1] - 1))
    int_a_",j,"[[i]] <- data.frame(int_ent[i,])
")))

  for(j in 1: length(xx))
    eval(parse(text = paste0("
    int_a_",j," <- do.call(rbind, int_a_",j,")
")))

  for(j in 1: length(xx))eval(parse(text = paste0("
for(i in aa[j]:(aa[j+1] - 1))
    int_b_",j,"[[i]] <- data.frame(int_rel_ent[i,])
")))

  for(j in 1: length(xx))
    eval(parse(text = paste0("
    int_b_",j," <- do.call(rbind, int_b_",j,")
")))

  int_ent <-data.frame(int_a_1, NA, int_b_1)

  for(i in 2:length(xx))eval(parse(text = paste0("
  int_ent <- data.frame(int_ent, NA, int_a_",i,",NA,int_b_",i,")
")))
  int_ent <- int_ent[-c(3:5)]

  se_des <- list()
  for(i in 1:length(xx)){
    se_des[[i]] <- data.frame(se_ent[i], se_rel_ent[i], NA)
  }
  se_ent <- do.call(cbind, se_des)
  se_ent <- se_ent[-c(2, dim(se_ent)[2])]
  #-
  tab <- list()
  colnames(est_ent)<-colnames(est_nac)
  colnames(cv_ent)<-colnames(cv_nac)
  colnames(int_ent)<-colnames(int_nac)
  colnames(se_ent)<-colnames(se_nac)

  tab[[1]] <- rbind(est_nac, est_ent)
  tab[[2]] <- rbind(cv_nac, cv_ent)
  tab[[3]] <- rbind(int_nac, int_ent)
  tab[[4]] <- rbind(se_nac, se_ent)

  tab[[1]] <- cbind(z, tab[[1]])
  tab[[2]] <- cbind(z, tab[[2]])
  tab[[3]] <- cbind(z, tab[[3]])
  tab[[4]] <- cbind(z, tab[[4]])
  return(tab)
}

# TAB VERTICAL -----------------------------------------------------------------
#' @title Tab_vert_a
#'
#' @description This function allows you to obtain a list with tabulations corresponding to estimations, coefficients of variation, confidence intervals and standard errors. This data frame is developed dividing between the first entry. It requires a data set with the appropriate expansion factor and the sampling design of the "survey" library.
#' @param xx Vector containing the names of the variables that will be used in the calculation. Usually: xx  = c("TOT", paste0("TOT_", 1:n)).
#' @param etiquetas  Labels for the data frame.
#' @details
#' To use this function the number of labels have to be the same that the totals plus one.
#' @keywords estimator, survey, vertical
#' @examples
#'
#'Tab_vert_a(xx, c("Estados Unidos Mexicanos", etiquetas))
#'
#'
Tab_vert_a <- function(xx, etiquetas)
{
  # D <- 'EDAD_a'
  #etiquetas <- c("Estados Unidos Mexicanos",etiquetas0)
  pob0 <- svytotal(t7[, xx], asp)
  pob <- as.data.frame(pob0)
  cv_pob <- as.data.frame(cv(pob0)*100)
  int_pob <- confint(pob0, level = 0.90)
  int_pob <- as.data.frame(int_pob)
  se_pob <- pob[2]

  rel0 <- svyratio(t7[,xx], denominator=t7[,xx[1]], asp)
  rel_pob <- as.data.frame(rel0[[1]]*100)
  cv_rel_pob <- cv(rel0)*100
  int_rel_pob <- confint(rel0, level = 0.90)*100
  int_rel_pob <- as.data.frame(int_rel_pob)
  se_rel_pob <- as.data.frame(SE(rel0)*100)


  #--------------------------------------------------------
  xx <- list()
  xx[[1]] <- cbind(etiquetas, pob[1], rel_pob[1])
  xx[[2]] <- cbind(etiquetas, cv_pob, cv_rel_pob)
  xx[[3]] <- cbind(etiquetas, int_pob,NA,int_rel_pob)
  xx[[4]] <- cbind(etiquetas, se_pob, se_rel_pob)

  #--------------------------------------------------------
  for(j in c(2,4)){
    for(i in 1:dim(xx[[2]])[1]){
      if(xx[[j]][i,3]==0){
        xx[[j]][i,3] <- 0.0000001
      }
    }
  }

  for(i in 1:dim(xx[[3]])[1]){
    for(j in c(2,5)){
      if(xx[[3]][i,j]==xx[[3]][i,j+1]){
        xx[[3]][i,j]<- 999999999
        xx[[3]][i,j+1]<- 999999999
      }
    }
  }


  return(xx)

}


# ORDENADORA ------------------------------------------------------------------
#' @title ordena
#'
#' @description This function allows you to order into a decreasing way a data frame by a given column. Column defautl is 5.
#' @param lista Is a data frame. Commonly the first entry of output tabulations of this package.
#' @param columna Number of column to make the order.
#' @param otros  Set the "otros" option in the bottom of the ordered vector.
#' @details
#' Use this function to make ordered data frames multi columns.
#' @keywords order
#' @return Vector with the row names of the data frame ordered.

ordena <- function(lista, columna = 5, otros = NULL) {
  if(!is.null(otros)){
    indice <- lista[c( 1, order(lista[c(2:(dim(lista)[1]-1)),columna],decreasing = TRUE)+1,dim(lista)[1]),]
  }else{
    indice <- lista[c( 1, order(lista[c(2:dim(lista)[1]),columna],decreasing = TRUE)+1),]
  }
  orden <- rownames(indice)
  return(orden)
}

# PEGADORA --------------------------------------------------------------------
#' @title pegadora
#'
#' @description This function allows you to paste the first or second, etc, row of a data frames into an easy way
#' @param lista Is a data frame. Commonly the first entry of output tabulations of this package.
#' @param cuantos Number of data frames to be pasted.
#' @param prec  Indicates if the data frames are estimations, coefficients of variation, etc.
#' @param renglon Is the number of row in each data frame to be used.
#' @param nombres Vector with the labels for the resulting data frame
#' @param columna Makes numeric the indicated column. Set default is 5.
#' @details
#' For a suitable use you´ll should use lista = bla[[1]], and the other data frames must necessarily be called bla_1, bla_2, etc.
#'
#' @return Data frame combining the correspond rows of diferent data sets.
pegadora <- function(lista, cuantos, prec, renglon, nombres, columna = 5){
  x <- list()
  x[[1]] <- lista[renglon,]
  for(i in 1:cuantos)
    eval(parse(text = paste0("
       x[[1]] <- rbind(x[[1]], bla_",i,"[[prec]][renglon, ])
    ")))

  x[[1]][,1] <- nombres
  x[[1]][,columna]<-as.numeric(as.character(x[[1]][,columna]))
  rownames(x[[1]]) <- seq( 1:dim(x[[1]])[1] )
  return(x[[1]])
}

# MOCHA ------------------------------------------------------------------------
#' @title mocha
#' @description This function allows you to erase some elemnts in the first row and/or second column into a data frame.
#' @param tabla Is a data frame.
#' @param quita1 Deletes elements in the first row of a data frame.
#' @param quita2 Deletes elements in the second column of a data frame.
#' @details
#' Is possible combine the options and works to confident intervals.
#'
#' @return Data frame with some delete elements.

mocha <- function(tabla, quita1 = NULL, quita2 = NULL){

  lista <- rbind(tabla, NA)

  if(!is.null(quita1)){
    lista[2:dim(lista)[1], 2:3] <- NA
  }else{lista <- lista}

  if(!is.null(quita2)){
    lista[1, 4:dim(lista)[2]] <- NA
  }else{lista <- lista}

  return(lista)
}


# FUNCION "n" ------------------------------------------------------------------
#' @title N
#' @description This function makes a vector of the length indicated but placing 0 on the left side of the first 9 numbers.
#' @param x Integer.
#' @details
#' The function does not includes "00".
#'
#' @return vector of the form: c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", ...)

n <- function(x){
  y<-paste0("0",1:9)
  if( x<10 ) z <- y[1:x] else z <- c(y,10:x)
  return(z)
}

# ENEADORA CV y SE -------------------------------------------------------------
#' @title eneadora_cv_se
#' @description This function places a numeric indicator representing a NA cell.
#' @param A A data frame.
#' @details
#' The function uses the number 888888887 to represents a cell with NA instead the string "NA".
#' This is a useful feature in order to maintain the original format of a Excel workbook.

eneadora_cv_se <- function(A){
  #Para las NA usas 888888887

  pares <- seq(4, ncol(A), 3)
  relativos <- seq(5, ncol(A), 3)
  relativos_prom <- c(relativos, ncol(A))
  absolutos <- c(2, pares[-length(pares)])

  A[A[,2]%in%0,2] <- 888888887

  for(i in 2:dim(A)[2]){
    A[A[,i]%in%NaN,i] <- 888888887
  }

  for(i in pares){
    A[,i] <- ifelse(A[,i]%in%0, 888888887, A[,i])
  }

  for(i in relativos_prom){
    A[,i] <- ifelse(A[,i]%in%0.0, 888888887, A[,i])
  }

  return(A)
}

# ENEADORA INTERV --------------------------------------------------------------
#' @title eneadora_interv
#' @description This function places a numeric indicator representing a NA cell on the confidence intervals data frame.
#' @param A A data frame of confidence intervals.
#' @details
#' The function uses the number 888888887 to represents a cell with NA instead the string "NA".
#' This is a useful feature in order to maintain the original format of a Excel workbook.
eneadora_interv <- function(A){

  absolutos <- c(2,seq(5, ncol(A)-2, 6))
  absolutos1 <- absolutos+1

  relativos <- c(seq(8, ncol(A), 6), ncol(A)-1)
  relativos1 <- relativos+1

  for(i in absolutos){
    A[,i] <- ifelse(A[,i]%in%NaN | A[,i]%in%0 , 888888887, A[,i])
  }
  for(i in absolutos1){
    A[,i] <- ifelse(A[,i]%in%NaN | A[,i]%in%0, 888888887, A[,i])
  }
  for(i in relativos){
    A[,i] <- ifelse(A[,i]%in%NaN | A[,i]%in%0.0, 888888887, A[,i])
  }
  for(i in relativos1){
    A[,i] <- ifelse(A[,i]%in%NaN| A[,i]%in%0.0, 888888887, A[,i])
  }


  return(A)
}


# ENEADORA ---------------------------------------------------------------------
#' @title eneadora
#' @description This function places a numeric indicator representing a NA cell on a list of the typical tabulations.
#' @param lista A list coming from the function that makes tabulations.
#' @details
#' The function places the number 888888887 to represents a cell with NA instead the string "NA" on the data frames coming as result of tabulate functions.
#' This is a useful feature in order to maintain the original format of a Excel workbook.
eneadora <- function(lista){
  X <- list()
  X[[1]] <- lista[[1]]
  X[[2]] <- eneadora_cv_se(lista[[2]])
  X[[4]] <- eneadora_cv_se(lista[[4]])
  X[[3]] <- eneadora_interv(lista[[3]])
  return(X)
}

# ASTERISCOS ---------------------------------------------------------------------
#' @title asteriscos
#' @description This function places a numeric indicator representing the strings 0\* and 0.0\* on the estimations tabulation.
#' @param t The estimations data frame.
#' @details
#' The function places the number 888888888 to represents a cell with the string 0\* when the absolute value is 0.
#' Similarly places 0.0\* on the corresponding cell when appears 0.0 on the relatives columns.
#' This is a useful feature in order to maintain the original format of a Excel workbook.
#' The key is as follow:0\* <- 888888888; 0.0\* <- 888888889
asteriscos <- function(t){
  for(i in 0:((dim(t)[2]-5)/3))
  {
    filtro<-t[,(4+3*i)]%in%0 & !t[,(4+3*i)]%in%NA
    if(sum(filtro)>0){
      t[filtro,(4+3*i)]<- 888888888
      t[filtro,(5+3*i)]<- 888888889
    }
    filtro<-t[,(5+3*i)]%in%100 & !t[,(5+3*i)]%in%NA
    if(sum(filtro)>0){
      t[filtro,(5+3*i)]<- 999999999
    }
  }
  return(t)
}


# ASTERISCOS INT---------------------------------------------------------------------
#' @title asteriscos_int
#' @description This function places a numeric indicator representing the strings 0\*, 0.0\* and 100.0\* on the confidence intervals tabulation.
#' @param A The confidence intervals data frame.
#' @details
#' The function places the number 888888888 to represents a cell with the string 0\* when the absolute value is 0.
#' Similarly places 0.0\* on the corresponding cell when appears 0.0 on the relatives columns. The same in the 100.0 cases.
#' This is a useful feature in order to maintain the original format of a Excel workbook.
#' The key is as follow:0\* <- 888888888; 0.0\* <- 888888889; 100.0\* <- 999999999
asteriscos_int <- function(A){
  #"0*" <- 888888888; #"0.0*" <- 888888889
  absolutos <- c(2,seq(5, ncol(A)-2, 6))
  absolutos1 <- absolutos+1

  relativos <- c(seq(8, ncol(A), 6), ncol(A)-1)
  relativos1 <- relativos+1

  for(i in absolutos){
    A[,i] <- ifelse(as.numeric(A[,i])<0 & !is.na(A[,i]), 888888888, A[,i])
  }
  for(i in relativos){
    A[,i] <- ifelse(as.numeric(A[,i])<0 & !is.na(A[,i]), 888888889, A[,i])
  }
  for(i in relativos1){
    A[,i] <- ifelse(as.numeric(A[,i])>100 & !is.na(A[,i]), 999999999, A[,i])
  }

  return(A)
}


# DA ORDEN ---------------------------------------------------------------------
#' @title da_orden
#' @description This function gives the order for blocks in the case of large data frames.
#' @param estimaciones Data frame with the estimations.
#' @param z Length of the initial block.
#' @param desagregados Vector of labels corresponding to a elements in the first bolck.
#' @param abajo Correspond to the "Otros" option.
#' @details
#' If abajo is diferent of NULL, then the function fix the "Otros" in the bottom of the blocks.
#' Otherwise ordered into a common way.
#' @return Vector with the ordered row names to evaluate tabulations.
da_orden <- function(estimaciones, z, desagregados, abajo){
  inicia_e0 <- 1
  acaba_e0 <- inicia_e0 + length(z)

  for(i in 1:length(desagregados[-1]))eval(parse(text = paste0("
 inicia_e",i," <- acaba_e",i-1," + 2
 acaba_e",i," <- inicia_e",i," + length(z)
 ")))

  orden <- list()
  for(i in 0:length(desagregados[-1]))eval(parse(text = paste0("
  auxiliar <- estimaciones[inicia_e",i,":acaba_e",i,",]
  orden[[",i+1,"]]<- ordena(auxiliar, columna = 5, otros = abajo, creciente = T)
  ")))

  return(orden)
}


# PREC -------------------------------------------------------------------------
#' @title prec
#' @description This function gives the statisticas presitions of a total.
#' @param A The variable to weight.
#' @param B The sampling design.
#' @details
#' The result is an arrange containing estimation weighted, cv(%), ci and se.
Prec <- function(A,B){

  a <-data.frame(svytotal(~A, design = B))
  x <- svytotal(~A, design = B)
  prec_nac<-data.frame(x[[1]],cv(x)*100,confint(x,level=0.95),a[[2]])
  colnames(prec_nac)<-c("pob_tot","CVpob_tot ","intervalo de","confianza","SE")
  return(prec_nac)
}

# PROM -------------------------------------------------------------------------
#' @title Prom
#' @description This function gives the statistics precisions of an arithmetical mean for a weighted variable.
#' @param x The denominator.
#' @param y The denominator.
#' @param design Set as default asp.
#' @details
#' Given by the set default for the parameter named "design" is "asp", the sampling design must be assigned with
#' this name.
Prom <- function(x, y, design = asp){
  x<-svyratio(~x,denominator=~y, design = asp)
  Relativo<-data.frame(x[[1]],cv(x)*100,confint(x,level=0.95), sqrt(as.numeric(x[2])))
  colnames(Relativo)<-c("relativo","CVrelativo","intervalo de","confianza")

  return(Relativo)
}


# PINTA ------------------------------------------------------------------------
#' @title pinta
#' @description This function makes the color code for the quality measure of the estimations using the coefficients of variation.
#' @param est The estimations data frame.
#' @param cv The coefficients of variation data frame.
#' @param hoja Sheet number to print the data painted.
#' @param ruta Path to load the templates workbook.
#' @param salva Indicates if auto-saves the workbook.
#' @details
#' The function prints the painted estimations on the final templates. At the same time saves the workbook with
#' the new data. It's important to keep in mind that if the parameter salva=T the workbook is auto-saved but for many iterations
#' this condition could make slower the full process.
pinta <- function(est, cv, hoja, ruta, salva){

  A <- openxlsx::read.xlsx(ruta, sheet = hoja, colNames = F, skipEmptyRows = F)
  ren <- which(A$X1%in%'Estados Unidos Mexicanos')
  wb <- openxlsx::loadWorkbook(ruta)

  estilo_a0 <- openxlsx::createStyle(fontName = 'Arial', fontSize = 8, numFmt = "### ### ##0", textDecoration = "Bold",
                                     halign = 'right', valign = 'center', fgFill = '#FF5400')

  estilo_a1 <- openxlsx::createStyle(fontName = 'Arial', fontSize = 8, numFmt = "### ### ##0",
                                     halign = 'right', valign = 'center', fgFill = '#FFEA00')
  estilo_a2 <- openxlsx::createStyle(fontName = 'Arial', fontSize = 8,  numFmt = "### ### ##0",
                                     halign = 'right', valign = 'center', fgFill = '#FF5400')
  estilo_a3 <- openxlsx::createStyle(fontName = 'Arial', fontSize = 8,  numFmt = "### ### ##0",
                                     halign = 'right', valign = 'center')

  estilo_r1 <- openxlsx::createStyle(fontName = 'Arial', fontSize = 8, numFmt = "0.0",
                                     halign = 'right', valign = 'center', fgFill = '#FFEA00')
  estilo_r2 <- openxlsx::createStyle(fontName = 'Arial', fontSize = 8,  numFmt = "0.0",
                                     halign = 'right', valign = 'center', fgFill = '#FF5400')
  estilo_r3 <- openxlsx::createStyle(fontName = 'Arial', fontSize = 8,  numFmt = "0.0",
                                     halign = 'right', valign = 'center', fgFill = '#FFFFFF')


  for( i in 1:dim(cv)[1] ){
    if(cv[i,2]=="NA" | is.na(cv[i,2]) | cv[i,2]==0){
      openxlsx::addStyle(wb, sheet = hoja, estilo_a3, rows = (ren - 1 +i), cols = 2)
    }else{
      if(any( 15<=as.numeric(cv[i,2]) & as.numeric(cv[i,2])<30, na.rm = T )==T){
        openxlsx::addStyle(wb, sheet = hoja, estilo_a1, rows = (ren - 1 +i), cols = 2)
      }else if(any(30 <= as.numeric(cv[i,2]), na.rm = T)==T ){
        openxlsx::addStyle(wb, sheet = hoja, estilo_a2, rows = (ren - 1 +i), cols = 2)
      }else{

      }
    }
  }


  for(k in 2:dim(est)[2]){
    if(sum(is.na(est[,2])) < dim(est)[1])
      for(j in 1:dim(est)[1]){
        if(sum(!cv[j,k]%in%c("NA", NA, 0) )>0){
          if(any( 15 <= as.numeric(cv[j,k]) & as.numeric(cv[j,k]) < 30, na.rm = T)==T){
            openxlsx::addStyle(wb, sheet = hoja, estilo_a1, rows = ren-1 + j, cols = k)
          }else if(any( 30 <= as.numeric(cv[j,k]), na.rm = T)==T){
            openxlsx::addStyle(wb, sheet = hoja, estilo_a2, rows = ren-1+j, cols = k)
          }else{
          }
        }
      }
  }

  openxlsx::writeData(wb, sheet = hoja,est, startRow = ren,colNames = F, na.string = F )

  if(salva==T){
  openxlsx::saveWorkbook(wb, ruta, overwrite = T)
  }else{
   print("Ejecuta con el libro cerrado: openxlsx::saveWorkbook(wb, ruta, overwrite = T)")
 }

}


# TABULEDOR ------------------------------------------------------------------
#' @title tabuledor
#' @description This function makes the arrange for many data frames mixing in a suitable way the corresponding rows.
#' @param precision Set as default 1:4.
#' @param muchos Number of extra tabulations besides the national to be mixed.
#' @param noms Vector of labels.
#' @param tipo Type of dis-aggregations.
#' @param quita1 If distinct of NULL indicates NA in the first row.
#' @param quita2 If distinct of NULL indicates MA in the second column.
#' @param con_otros If distinct of NULL indicates the "Otros" options buttom in the data frames.
#' @details
#' You can use noms = purrr::map(.x = sector, .f = ~ c(.x, z) ), where z is a labels vector and
#' labels in sector will be the firs label in the resulting data frame.
#'
tabuledor <- function(precision = 1:4, muchos, noms, tipo,
                      quita1=NULL, quita2=NULL, con_otros= NULL){
  tabs <- list()

  x1<-list()
  x1[[1]] <- pegadora(lista = bla[[1]], cuantos = muchos, prec = precision[1], renglon = 1, nombres = noms[[1]] )
  orden <- ordena(lista = x1[[1]], otros = con_otros)
  x1[[1]] <- x1[[1]][orden,]
  x1[[1]] <- mocha(tabla = x1[[1]], quita1, quita2)

  for(i in 2:length(tipo)) eval(parse(text = paste0("
      x1[[i]] <- pegadora(lista = bla[[1]], cuantos = muchos, prec = precision[1], renglon = ",i,",  nombres = noms[[i]] )
      orden_",i," <- ordena(lista = x1[[i]], otros = con_otros)
      x1[[i]] <- x1[[i]][orden_",i,",]
      x1[[i]] <- mocha(tabla = x1[[i]], quita1, quita2)
      #x1[[i]] <- x1[[i]][c(1:6,15),]
  ")))
  tabs[[1]] <- do.call(rbind,x1)
  tabs[[1]] <- tabs[[1]] [-dim(tabs[[1]] )[1],]

  for(j in 2:length(precision)){
    x1 <- list()
    x1[[1]] <- pegadora(lista = bla[[j]], cuantos = muchos, prec = precision[j], renglon = 1, nombres = noms[[1]] )
    x1[[1]] <- x1[[1]][orden,]
    x1[[1]] <- mocha(tabla = x1[[1]], quita1, quita2)

    for(i in 2:length(tipo)) eval(parse(text = paste0("
         x1[[i]] <- pegadora(lista = bla[[j]], cuantos = muchos, prec = precision[j], renglon = ",i,",  nombres = noms[[i]] )
         x1[[i]] <- x1[[i]][orden_",i,",]
         x1[[i]] <- mocha(tabla = x1[[i]], quita1, quita2)
         #x1[[i]] <- x1[[i]][c(1:6,15),]
     ")))

    tabs[[j]] <- do.call(rbind,x1)
    tabs[[j]] <- tabs[[j]] [-dim(tabs[[j]] )[1],]
  }

  return(tabs)}

# Tabuleadora ------------------------------------------------------------------
#' @title tabuleadora
#' @description This function makes the arrange for many data frames mixing in a suitable way the corresponding rows.
#' @param precision Set as default 1:4.
#' @param muchos Number of extra tabulations besides the national to be mixed.
#' @param noms Vector of labels.
#' @param tipo Type of dis-aggregations.
#' @param quita1 If distinct of NULL indicates NA in the first row.
#' @param quita2 If distinct of NULL indicates MA in the second column.
#' @param con_otros If distinct of NULL indicates the "Otros" options buttom in the data frames.
#' @param recorta The number of rows in each resulting block.
#' @details
#' You can use noms = purrr::map(.x = sector, .f = ~ c(.x, z) ), where z is a labels vector and
#' labels in sector will be the firs label in the resulting data frame.
#'
#' When recorta!=NULL the final data frame will contain less rows in each block than the "muchos" parameter.
tabuleadora <- function(precision = 1:4,
                        muchos,
                        noms,
                        tipo,
                        quita1=NULL,
                        quita2=NULL,
                        con_otros = NULL,
                        recorta=NULL){
  if(is.null(recorta)) {
    tabs <- tabuledor(precision = 1:4, muchos=muchos, noms=noms, tipo = tipo,
                      quita1=quita1, quita2=quita2, con_otros=con_otros)
  }else{
    tabs <- list()

    x1<-list()
    x1[[1]] <- pegadora(lista = bla[[1]], cuantos = muchos, prec = precision[1], renglon = 1, nombres = noms[[1]] )
    orden <- ordena(lista = x1[[1]], otros = con_otros)
    x1[[1]] <- x1[[1]][orden,]
    x1[[1]] <- mocha(tabla = x1[[1]], quita1, quita2)

    for(i in 2:length(tipo)) eval(parse(text = paste0("
        x1[[i]] <- pegadora(lista = bla[[1]], cuantos = muchos, prec = precision[1], renglon = ",i,",  nombres = noms[[i]] )
        orden_",i," <- ordena(lista = x1[[i]], otros = con_otros)
        x1[[i]] <- x1[[i]][orden_",i,",]
        x1[[i]] <- mocha(tabla = x1[[i]], quita1, quita2)
        x1[[i]] <- x1[[i]][c(1:recorta, dim(x1[[i]])[1]),]
       ")))

    tabs[[1]] <- do.call(rbind,x1)
    tabs[[1]] <- tabs[[1]] [-dim(tabs[[1]] )[1],]

    for(j in 2:length(precision)){
      x1 <- list()
      x1[[1]] <- pegadora(lista = bla[[j]], cuantos = muchos, prec = precision[j], renglon = 1, nombres = noms[[1]] )
      x1[[1]] <- x1[[1]][orden,]
      x1[[1]] <- mocha(tabla = x1[[1]], quita1, quita2)

      for(i in 2:length(tipo)) eval(parse(text = paste0("
        x1[[i]] <- pegadora(lista = bla[[j]], cuantos = muchos, prec = precision[j], renglon = ",i,",  nombres = noms[[i]] )
        x1[[i]] <- x1[[i]][orden_",i,", ]
        x1[[i]] <- mocha(tabla = x1[[i]], quita1, quita2)
        x1[[i]] <- x1[[i]][c(1:recorta, dim(x1[[i]])[1]),]
       ")))

      tabs[[j]] <- do.call(rbind,x1)
      tabs[[j]] <- tabs[[j]] [-dim(tabs[[j]] )[1],]
    }
  }
  return(tabs)

}
