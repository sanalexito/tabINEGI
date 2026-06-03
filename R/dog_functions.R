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
#' @param nc  Confidence level for the intervals.
#' @keywords estimator, survey.
#' @return List of four entries.
#' @examples
#'
#'#Variable used in this case correspond to the ENCIG of the INEGI.
#'library(survey)
#'pregs <- c("N_TRA", "P7_3", "P7_1","FAC_TRA", "UPM_DIS", "EST_DIS","ENT")
#'#Creating total variables
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
#'tabulado <- Tab_2(c("TOT", paste0("TOT_",1:5)), "ENT", estados, nc = 0.95)
#'tabulado[[1]]
#'#Run this example on the console
#'
#'
Tab_2 <- function(xx, D, z, nc = 0.95) {

  pob0 <- svytotal(t7[,xx], asp)
  pob <- as.data.frame(pob0)
  cv_pob <- cv(pob0)*100
  int_pob <- confint(pob0, level = nc)
  int_pob <- as.data.frame(int_pob)
  se_pob <- SE(pob0)

  rel0 <- svyratio(t7[,xx], denominator=t7[,xx[1]], asp)
  rel_pob <- as.data.frame(rel0[[1]]*100)
  cv_rel_pob <- cv(rel0)*100
  int_rel_pob <- confint(rel0, level = nc)*100
  int_rel_pob <- as.data.frame(int_rel_pob)
  se_rel_pob <- SE(rel0)*100

  #desagregados de los nacionales por entidad
  ent0 <- svyby(t7[,xx], by = t7[,D], asp, svytotal)
  ent <- as.data.frame(ent0[, 2:(length(xx)+1)])
  cv_ent <- cv(ent0)*100
  int_ent <- confint(ent0, level = nc)
  int_ent <- as.data.frame(int_ent)
  se_ent <- SE(ent0)

  rel_ent0 <- svyby(t7[,xx], by = t7[,D], denominator= ~TOT, asp, svyratio)
  rel_ent <- as.data.frame(rel_ent0[,2:(length(xx)+1)])*100
  cv_rel_ent <- cv(rel_ent0)*100
  int_rel_ent <- confint(rel_ent0, level = nc)*100
  int_rel_ent <- as.data.frame(int_rel_ent)
  se_rel_ent <- SE(rel_ent0)*100

#---
  est_nal = lapply(1:length(xx),
                    function(k){
                     el_df = data.frame(pob[[1]][k], rel_pob[k, 1], NA)
                    return(el_df)
                    }
                   )

  est_nac <- do.call(cbind, est_nal)
  est_nac <- est_nac[-c(2, dim(est_nac)[2])]
#---
  cv_nal = lapply(1:length(xx),
                   function(k){
                     el_df = data.frame(cv_pob[[k]], cv_rel_pob[k], NA)
                   return(el_df)
                   }
                  )
  cv_nac <- do.call(cbind, cv_nal)
  cv_nac <- cv_nac[-c(2, dim(cv_nac)[2])]
#---
  int_nal = lapply(1:length(xx),
                    function(k){
                     el_df = data.frame(int_pob[k, ], NA, int_rel_pob[k, ], NA)
                    return(el_df)
                    }
                  )
  int_nac <- do.call(cbind, int_nal)
  int_nac <- int_nac[-c(3:5, dim(int_nac)[2])]
#---
  se_nal = lapply(1:length(xx),
                  function(k){
                    el_df = data.frame(se_pob[[k]], se_rel_pob[k], NA)
                    return(el_df)
                  }
  )
  se_nac <- do.call(cbind, se_nal)
  se_nac <- se_nac[-c(2, dim(se_nac)[2])]

#---
  est_des = lapply(1:length(xx),
                 function(k){
                   el_df = data.frame(ent[k], rel_ent[k], NA)
                   return(el_df)
                 }
  )
  est_ent <- do.call(cbind, est_des)
  est_ent <- est_ent[-c(2, dim(est_ent)[2])]

  cv_des = lapply(1:length(xx),
                  function(k){
                    el_df = data.frame(cv_ent[k], cv_rel_ent[k], NA)
                    return(el_df)
                  }
  )
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

  se_des = lapply(1:length(xx),
                  function(k){
                    el_df = data.frame(se_ent[k], se_rel_ent[k], NA)
                    return(el_df)
                  }
  )
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
#' @param nc  Confidence level for the intervals.
#' @keywords estimator, survey.
#' @return List of four entries.
#' @examples
Tab_tasa <- function(xx, D, z, nc = 0.95) {

  pob0 <- svytotal(t7[,xx],asp)
  pob <- as.data.frame(pob0)
  cv_pob <- cv(pob0)*100
  int_pob <- confint(pob0, level = nc)
  int_pob <- as.data.frame(int_pob)
  se_pob <- SE(pob0)

  rel0 <- svyratio(t7[,xx], denominator=t7[,xx[1]], asp)
  rel_pob <- as.data.frame(rel0[[1]]*10000)
  cv_rel_pob <- cv(rel0)*100
  int_rel_pob <- confint(rel0, level = nc)*10000
  int_rel_pob <- as.data.frame(int_rel_pob)
  se_rel_pob <- SE(rel0)*10000

  #desagregados de los nacionales por entidad
  ent0 <- svyby(t7[,xx], by = t7[,D], asp, svytotal)
  ent <- as.data.frame(ent0[, 2:(length(xx)+1)])
  cv_ent <- cv(ent0)*100
  int_ent <- confint(ent0, level = nc)
  int_ent <- as.data.frame(int_ent)
  se_ent <- SE(ent0)

  rel_ent0 <- svyby(t7[,xx], by = t7[,D], denominator= ~TOT, asp, svyratio)
  rel_ent <- as.data.frame(rel_ent0[,2:(length(xx)+1)])*10000
  cv_rel_ent <- cv(rel_ent0)*100
  int_rel_ent <- confint(rel_ent0, level = nc)*10000
  int_rel_ent <- as.data.frame(int_rel_ent)
  se_rel_ent <- SE(rel_ent0)*10000

#---
  est_nal = lapply(1:length(xx),
                     function(k){
                       el_df = data.frame(pob[[1]][k], rel_pob[k, 1],NA)
                       return(el_df)
                     }
                   )
  est_nac <- do.call(cbind, est_nal)
  est_nac <- est_nac[-c(2, dim(est_nac)[2])]
#---
  cv_nal = lapply(1:length(xx),
                function(k){
                  el_df = data.frame(cv_pob[[k]], cv_rel_pob[k], NA)
                  return(el_df)
                }
               )
  cv_nac <- do.call(cbind, cv_nal)
  cv_nac <- cv_nac[-c(2, dim(cv_nac)[2])]
#---
  int_nal = lapply(1:length(xx),
                    function(k){
                     el_df = data.frame(int_pob[k, ], NA, int_rel_pob[k,], NA)
                     return(el_df)
                    }
                   )
  int_nac <- do.call(cbind, int_nal)
  int_nac <- int_nac[-c(3:5, dim(int_nac)[2])]
#---
  se_nal = lapply(1:length(xx),
                  function(k){
                    el_df = data.frame(se_pob[[k]], se_rel_pob[k], NA)
                    return(el_df)
                  }
                 )
  se_nac <- do.call(cbind, se_nal)
  se_nac <- se_nac[-c(2, dim(se_nac)[2])]

#---
  est_des = lapply(1:length(xx),
                 function(k){
                   el_df = data.frame(ent[k], rel_ent[k], NA)
                   return(el_df)
                 }
                )
  est_ent <- do.call(cbind, est_des)
  est_ent <- est_ent[-c(2, dim(est_ent)[2])]
#---
  cv_des = lapply(1:length(xx),
                  function(k){
                    el_df = data.frame(cv_ent[k], cv_rel_ent[k], NA)
                    return(el_df)
                  }
                 )
  cv_ent <- do.call(cbind, cv_des)
  cv_ent <- cv_ent[-c(2, dim(cv_ent)[2])]
#---
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
#---
  se_des = lapply(1:length(xx),
                function(k){
                  el_df = data.frame(se_ent[k], se_rel_ent[k], NA)
                  return(el_df)
                }
  )
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
#' @param nc Confidence level for the intervals.
#' @details
#' To use this function the number of labels have to be the same that the totals plus one.
#' @keywords estimator, survey, vertical
#' @examples
#'
#'Tab_vert_a(xx, c("Estados Unidos Mexicanos", etiquetas))
#'
#'
Tab_vert_a <- function(xx, etiquetas, nc = 0.95) {
  pob0 <- svytotal(t7[, xx], asp)
  pob <- as.data.frame(pob0)
  cv_pob <- as.data.frame(cv(pob0) * 100)
  int_pob <- as.data.frame(confint(pob0, level = nc))
  se_pob <- pob[2]

  rel0 <- svyratio(t7[, xx], denominator = t7[, xx[1]], asp)
  rel_pob <- as.data.frame(rel0[[1]] * 100)
  cv_rel_pob <- cv(rel0) * 100
  int_rel_pob <- as.data.frame(confint(rel0, level = nc) * 100)
  se_rel_pob <- as.data.frame(SE(rel0) * 100)

  xx <- list(
    cbind(etiquetas, pob[1], rel_pob[1]),
    cbind(etiquetas, cv_pob, cv_rel_pob),
    cbind(etiquetas, int_pob, NA, int_rel_pob),
    cbind(etiquetas, se_pob, se_rel_pob)
  )
  xx[[1]][xx[[1]][, 3] == 0 , 2] <- 888888888
  xx[[1]][xx[[1]][, 3] == 0 , 3] <- 888888888

  for (j in c(2, 4)) {
    xx[[j]][xx[[j]][, 3] == 0 | xx[[j]][, 3]%in%NaN, 3] <- 888888887
    xx[[j]][xx[[j]][, 2] == 0 | xx[[j]][, 2]%in%NaN, 2] <- 888888887
  }

  for (i in 1:nrow(xx[[3]])) {
    if (xx[[3]][i, 2] == xx[[3]][i, 3]) {
      xx[[3]][i, 2:3] <- 999999999
    }
    if (xx[[3]][i, 5] == xx[[3]][i, 6]) {
      xx[[3]][i, 5:6] <- 999999999
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

eneadora_cv_se=function (A) {
  if (ncol(A) <= 3){
    pa_usar = columnas_no_vacias(A)
    pa_usar = which(!pa_usar%in%NA)[-1]

    A[, pa_usar[1]] <- ifelse(A[, pa_usar[1]] %in% NaN | A[, pa_usar[1]] %in% 0, 888888887, A[, pa_usar[1]])
  }else{
    pares <- seq(4, ncol(A), 3)
    relativos <- seq(5, ncol(A), 3)
    relativos_prom <- c(relativos, ncol(A))
    absolutos <- c(2, pares[-length(pares)])
    A[A[, 2] %in% 0, 2] <- 888888887
    for (i in 2:dim(A)[2]) {
      A[A[, i] %in% NaN, i] <- 888888887
    }
    for (i in pares) {
      A[, i] <- ifelse(A[, i] %in% 0, 888888887, A[, i])
    }
    for (i in relativos_prom) {
      A[, i] <- ifelse(A[, i] %in% 0, 888888887, A[, i])
    }

  }

  return(A)
}

# COLUMNAS NO VACIAS -----------------------------------------------------------
#' @title columnas_no_vacias
#' @description This function gives a vector with the number of column containing information.
#' @param A A data frame.
#' @details
#' The function uses the number 888888887 to represents a cell with NA instead the string "NA".
#' This is a useful feature in order to maintain the original format of a Excel workbook.
columnas_no_vacias = function(A){
  auxiliar = vector()
  for(i in 1:ncol(A)){
    if(any(!A[,i]%in%NA)){
      auxiliar[i] = i
    }

    if(any(A[,i]%in%"*")){
      auxiliar[i]=NA
    }

    para_usar = auxiliar
  }
  return(para_usar)
}


# ENEADORA INTERV --------------------------------------------------------------
#' @title eneadora_interv
#' @description This function places a numeric indicator representing a NA cell on the confidence intervals data frame.
#' @param A A data frame of confidence intervals.
#' @details
#' The function uses the number 888888887 to represents a cell with NA instead the string "NA".
#' This is a useful feature in order to maintain the original format of a Excel workbook.
eneadora_interv = function(A) {
  if (ncol(A) <= 4 ){
    pa_usar = columnas_no_vacias(A)
    pa_usar = which(!pa_usar%in%NA)[-1]

    A[, pa_usar[1]] <- ifelse(A[, pa_usar[1]] %in% NaN | A[, pa_usar[1]] %in% 0, 888888887, A[, pa_usar[1]])
    A[, pa_usar[1]+1] <- ifelse(A[, pa_usar[1]+1] %in% NaN | A[, pa_usar[1]+1] %in% 0, 888888887, A[, pa_usar[1]+1])

    for (j in 1:nrow(A)) {
      if (!is.na(A[j, pa_usar[1]]) & A[j, pa_usar[1]] %in% A[j, pa_usar[1] + 1]) {
        A[j, pa_usar[1]] = 888888887
        A[j, pa_usar[1] + 1] = 888888887
      }
    }
  }else{
    pa_usar = columnas_no_vacias(A)
    pa_usar = which(!pa_usar%in%NA)[-1]
    pares <- pa_usar[seq(1,length(pa_usar),2)]

    for (i in pa_usar) {
      A[, i] <- ifelse(A[, i] %in% NaN | A[, i] %in% 0, 888888887, A[, i])
    }

    for (i in pares) {
      for (j in 1:nrow(A)) {
        if (!is.na(A[j, i]) & A[j, i] %in% A[j, i + 1]) {
          A[j, i] = 888888887
          A[j, i + 1] = 888888887
        }
      }
    }
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
Prec <- function(A, B, nc = 0.95){

  a <-data.frame(svytotal(~A, design = B))
  x <- svytotal(~A, design = B)
  prec_nac<-data.frame(x[[1]],cv(x)*100,confint(x,level=nc),a[[2]])
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
Prom <- function(x, y, design = asp, nc = 0.95){
  x<-svyratio(~x,denominator=~y, design = asp)
  Relativo<-data.frame(x[[1]],cv(x)*100,confint(x,level=nc), sqrt(as.numeric(x[2])))
  colnames(Relativo)<-c("relativo","CVrelativo","intervalo de","confianza")

  return(Relativo)
}


# PINTA_TAB ------------------------------------------------------------------------
#' @title pinta_tab
#' @description This function makes the color code for the quality measure of the estimations using the coefficients of variation.
#' @param WB A workbook-type object.
#' @param est The estimations data frame.
#' @param cv The coefficients of variation data frame.
#' @param hoja Sheet number to print the data painted.
#' @param ruta Path to load the templates workbook.
#' @param salva Indicates if auto-saves the workbook. If you place F, then only at the last step you should save the workbook.
#' @param nivel Its the lower level of the CV. Typically 15 or 30.
#' @details
#' The function prints the painted estimations on the final templates. At the same time saves the workbook with
#' the new data. It's important to keep in mind that if the parameter salva=T the workbook is auto-saved but for many iterations
#' this condition could make slower the full process.
pinta_tab = function (WB,est, cv, hoja, ruta, salva, nivel) {
  A <- openxlsx::read.xlsx(WB, sheet = hoja, colNames = F, skipEmptyRows = F)
  ren <- which(A$X1 %in% "Estados Unidos Mexicanos")
  #_______________________________________________________________________________________________
  estiloA = list()
  estiloA[[1]] <- openxlsx::createStyle(fontName = "Arial", fontSize = 8,
                                        numFmt = "### ### ##0", halign = "right", valign = "center",
                                        fgFill = "#FFEA00")
  estiloA[[2]] <- openxlsx::createStyle(fontName = "Arial", fontSize = 8,
                                        numFmt = "### ### ##0", halign = "right", valign = "center",
                                        fgFill = "#FF5400")
  estiloA[[3]] <- openxlsx::createStyle(fontName = "Arial", fontSize = 8,
                                        numFmt = "### ### ##0", halign = "right", valign = "center")
  #_______________________________________________________________________________________________
  estiloR = list()
  estiloR[[1]] <- openxlsx::createStyle(fontName = "Arial", fontSize = 8,
                                        numFmt = "0.0", halign = "right", valign = "center",
                                        fgFill = "#FFEA00")
  estiloR[[2]] <- openxlsx::createStyle(fontName = "Arial", fontSize = 8,
                                        numFmt = "0.0", halign = "right", valign = "center",
                                        fgFill = "#FF5400")
  estiloR[[3]] <- openxlsx::createStyle(fontName = "Arial", fontSize = 8,
                                        numFmt = "0.0", halign = "right", valign = "center")


  # columna de totales____________________________________________________________
  for (i in 1:nrow(cv)) {
    if (cv[i, 2] == "NA" | is.na(cv[i, 2]) | cv[i, 2] ==  0) {
      openxlsx::addStyle(WB, sheet = hoja, estiloA[[3]], rows = (ren - 1 + i), cols = 2)
    }

    if (any(nivel <= as.numeric(cv[i, 2]) & as.numeric(cv[i,2]) < 30, na.rm = T) == T) {
      openxlsx::addStyle(WB, sheet = hoja, estiloA[[1]], rows = (ren - 1 + i), cols = 2)
    }

    if (any(30 <= as.numeric(cv[i, 2]), na.rm = T) == T) {
      openxlsx::addStyle(WB, sheet = hoja, estiloA[[2]], rows = (ren - 1 + i), cols = 2)
    }
  }

  #Columnas de absolutos _________________________________________________________
  casos = list(seq(4, ncol(est), 3), seq(5, ncol(est), 3))

  for(k in casos[[1]]) {
    for(i in 1:nrow(cv)){
      if (cv[i, k] == "NA" | is.na(cv[i, k]) | cv[i, k] ==  0) {
        openxlsx::addStyle(WB, sheet = hoja, estiloA[[3]], rows = (ren - 1 + i), cols = k)
      }

      if (any(nivel <= as.numeric(cv[i, k]) & as.numeric(cv[i,k]) < 30, na.rm = T) == T) {
        openxlsx::addStyle(WB, sheet = hoja, estiloA[[1]], rows = (ren - 1 + i), cols = k)
      }

      if (any(30 <= as.numeric(cv[i, k]), na.rm = T) == T) {
        openxlsx::addStyle(WB, sheet = hoja,  estiloA[[2]], rows = (ren - 1 + i), cols = k)
      }
    }
  }

  #Columnas de relativos _________________________________________________________
  for(k in casos[[2]]) {
    for(i in 1:nrow(cv)){
      if (cv[i, k] == "NA" | is.na(cv[i, k]) | cv[i, k] ==  0) {
        openxlsx::addStyle(WB, sheet = hoja,  estiloR[[3]], rows = (ren - 1 + i), cols = k)
      }

      if (any(nivel <= as.numeric(cv[i, k]) & as.numeric(cv[i,k]) < 30, na.rm = T) == T) {
        openxlsx::addStyle(WB, sheet = hoja, estiloR[[1]], rows = (ren - 1 + i), cols = k)
      }

      if (any(30 <= as.numeric(cv[i, k]), na.rm = T) == T) {
        openxlsx::addStyle(WB, sheet = hoja, estiloR[[2]], rows = (ren - 1 + i), cols = k)
      }
    }
  }


  #Pone datos ____________________________________________________________________
  openxlsx::writeData(WB, sheet = hoja, est, startRow = ren, colNames = F, na.string = F)

  #Opciones de salvado ___________________________________________________________
  if(salva%in%T) {
    openxlsx::saveWorkbook(WB, ruta, overwrite = T)
  }else {
    print("Ejecuta con el libro cerrado mi perro: openxlsx::saveWorkbook(WB, ruta, overwrite = T)")
    m_wb = WB
    return(m_wb)

  }

}


# PINTA_TASA -------------------------------------------------------------------
#' @title pinta_tasa
#' @description This function makes the color code for the quality measure of the estimations using the coefficients of variation for rate-type tabulations.
#' @param WB A workbook type object.
#' @param est The estimations data frame.
#' @param cv The coefficients of variation data frame.
#' @param hoja Sheet number to print the data painted.
#' @param ruta Path to load the templates workbook.
#' @param salva Indicates if auto-saves the workbook. If you place F, then only at the last step you should save the workbook.
#' @param nivel Its the lower level of the CV. Typically 15 or 30.
#' @param la_col Is the number of column where the data appears on the estimations tabulation.
#' @details
#' The function prints the painted estimations on the final templates. At the same time saves the workbook with
#' the new data. It's important to keep in mind that if the parameter salva=T the workbook is auto-saved but for many iterations
#' put as TRUE this condition could make slower the full process.
pinta_tasa = function (WB,est, cv, hoja, ruta, salva, nivel, la_col) {

  A <- openxlsx::read.xlsx(WB, sheet = hoja, colNames = F, skipEmptyRows = F)
  ren <- which(A$X1 %in% "Estados Unidos Mexicanos")
  # Todos los estilos se usan así que no hay que borrarlos________________________
  estilo_b1 <- openxlsx::createStyle(fontName = "Arial", fontSize = 8,
                                     numFmt = "### ### ##0", textDecoration = "Bold",
                                     halign = "right", valign = "center", fgFill = "#FFEA00")
  estilo_b2 <- openxlsx::createStyle(fontName = "Arial", fontSize = 8,
                                     numFmt = "### ### ##0", textDecoration = "Bold",
                                     halign = "right", valign = "center", fgFill = "#FF5400")
  estilo_b3 <- openxlsx::createStyle(fontName = "Arial", fontSize = 8,
                                     numFmt = "### ### ##0", textDecoration = "Bold",
                                     halign = "right", valign = "center")


  estilo_a1 <- openxlsx::createStyle(fontName = "Arial", fontSize = 8,
                                     numFmt = "### ### ##0", halign = "right", valign = "center",
                                     fgFill = "#FFEA00")
  estilo_a2 <- openxlsx::createStyle(fontName = "Arial", fontSize = 8,
                                     numFmt = "### ### ##0", halign = "right", valign = "center",
                                     fgFill = "#FF5400")
  estilo_a3 <- openxlsx::createStyle(fontName = "Arial", fontSize = 8,
                                     numFmt = "### ### ##0", halign = "right", valign = "center")

  #_______________________________________________________________________________

  for (i in 1:nrow(cv)) {
    if ((cv[i, la_col] == "NA" | is.na(cv[i, la_col]) | cv[i, la_col]==0) & cv[i, 1]%in%estados) {
      openxlsx::addStyle(WB, sheet = hoja, estilo_b3, rows = (ren - 1 + i), cols = la_col)
    }else if(cv[i, la_col] == "NA" | is.na(cv[i, la_col]) | cv[i, la_col]==0 ){
      openxlsx::addStyle(WB, sheet = hoja, estilo_a3, rows = (ren - 1 + i), cols = la_col)
    }

    if ((any(nivel <= as.numeric(cv[i, la_col]) & as.numeric(cv[i,la_col]) < 30, na.rm = T) == T) & cv[i, 1]%in%estados) {
      openxlsx::addStyle(WB, sheet = hoja, estilo_b1,  rows = (ren - 1 + i), cols = la_col)
    }else if(any(nivel <= as.numeric(cv[i, la_col]) & as.numeric(cv[i,la_col]) < 30, na.rm = T) == T){
      openxlsx::addStyle(WB, sheet = hoja, estilo_a1,  rows = (ren - 1 + i), cols = la_col)
    }

    if ((any(30 <= as.numeric(cv[i, la_col]), na.rm = T) ==T)&cv[i, 1]%in%estados) {
      openxlsx::addStyle(WB, sheet = hoja, estilo_b2, rows = (ren - 1 + i), cols = la_col)
    }else if(any(30 <= as.numeric(cv[i, la_col]), na.rm = T) ==T){
      openxlsx::addStyle(WB, sheet = hoja, estilo_a2, rows = (ren - 1 + i), cols = la_col)
    }

  }

  openxlsx::writeData(WB, sheet = hoja, est, startRow = ren,colNames = F, na.string = F)
  if (salva == T) {
    openxlsx::saveWorkbook(WB, ruta, overwrite = T)
  }
  else {
    print("Ejecuta con el libro cerrado el la instrucción: openxlsx::saveWorkbook(WB, ruta, overwrite = T)")
    m_wb = WB
    return(m_wb)
  }
}




# PINTA_TAB_COLS -------------------------------------------------------------------
#' @title pinta_tab_cols
#' @description This function makes the color code for the quality measure of the estimations using the coefficients of variation when specific columns are used.
#' @param WB A workbook type object.
#' @param est The estimations data frame.
#' @param cv The coefficients of variation data frame.
#' @param hoja Sheet number to print the data painted.
#' @param ruta Path to load the templates workbook.
#' @param salva Indicates if auto-saves the workbook. If you place F, then only at the last step you should save the workbook.
#' @param nivel Its the lower level of the CV. Typically 15 or 30.
#' @param c_abs A vector to place the columns belonging to absolutes.
#' @param c_abs A vector to place the columns belonging to relatives.
#' @details
#' The function prints the painted estimations on the final templates. At the same time saves the workbook with
#' the new data. It's important to keep in mind that if the parameter salva=T the workbook is auto-saved but for many iterations
#' put as TRUE this condition could make slower the full process. Its useful when you're working with special tabulations.
pinta_tab_cols = function (WB,est, cv, hoja, ruta, salva, nivel, c_abs, c_rel) {

  A <- openxlsx::read.xlsx(WB, sheet = hoja, colNames = F, skipEmptyRows = F)
  ren <- which(A$X1 %in% "Estados Unidos Mexicanos")
  #_______________________________________________________________________________________________
  estiloA = list()
  estiloA[[1]] <- openxlsx::createStyle(fontName = "Arial", fontSize = 8,
                                        numFmt = "### ### ##0", halign = "right", valign = "center",
                                        fgFill = "#FFEA00")
  estiloA[[2]] <- openxlsx::createStyle(fontName = "Arial", fontSize = 8,
                                        numFmt = "### ### ##0", halign = "right", valign = "center",
                                        fgFill = "#FF5400")
  estiloA[[3]] <- openxlsx::createStyle(fontName = "Arial", fontSize = 8,
                                        numFmt = "### ### ##0", halign = "right", valign = "center")
  #_______________________________________________________________________________________________
  estiloR = list()
  estiloR[[1]] <- openxlsx::createStyle(fontName = "Arial", fontSize = 8,
                                        numFmt = "0.0", halign = "right", valign = "center",
                                        fgFill = "#FFEA00")
  estiloR[[2]] <- openxlsx::createStyle(fontName = "Arial", fontSize = 8,
                                        numFmt = "0.0", halign = "right", valign = "center",
                                        fgFill = "#FF5400")
  estiloR[[3]] <- openxlsx::createStyle(fontName = "Arial", fontSize = 8,
                                        numFmt = "0.0", halign = "right", valign = "center")


  # columna de totales____________________________________________________________
  for (i in 1:nrow(cv)) {
    if (cv[i, 2] == "NA" | is.na(cv[i, 2]) | cv[i, 2] ==  0) {
      openxlsx::addStyle(WB, sheet = hoja, estiloA[[3]], rows = (ren - 1 + i), cols = 2)
    }

    if (any(nivel <= as.numeric(cv[i, 2]) & as.numeric(cv[i,2]) < 30, na.rm = T) == T) {
      openxlsx::addStyle(WB, sheet = hoja, estiloA[[1]], rows = (ren - 1 + i), cols = 2)
    }

    if (any(30 <= as.numeric(cv[i, 2]), na.rm = T) == T) {
      openxlsx::addStyle(WB, sheet = hoja, estiloA[[2]], rows = (ren - 1 + i), cols = 2)
    }
  }

  #Columnas de absolutos _________________________________________________________
  casos = list(c_abs, c_rel)

  for(k in casos[[1]]) {
    for(i in 1:nrow(cv)){
      if (cv[i, k] == "NA" | is.na(cv[i, k]) | cv[i, k] ==  0) {
        openxlsx::addStyle(WB, sheet = hoja, estiloA[[3]], rows = (ren - 1 + i), cols = k)
      }

      if (any(nivel <= as.numeric(cv[i, k]) & as.numeric(cv[i,k]) < 30, na.rm = T) == T) {
        openxlsx::addStyle(WB, sheet = hoja, estiloA[[1]], rows = (ren - 1 + i), cols = k)
      }

      if (any(30 <= as.numeric(cv[i, k]), na.rm = T) == T) {
        openxlsx::addStyle(WB, sheet = hoja,  estiloA[[2]], rows = (ren - 1 + i), cols = k)
      }
    }
  }

  #Columnas de relativos _________________________________________________________
  for(k in casos[[2]]) {
    for(i in 1:nrow(cv)){
      if (cv[i, k] == "NA" | is.na(cv[i, k]) | cv[i, k] ==  0) {
        openxlsx::addStyle(WB, sheet = hoja,  estiloR[[3]], rows = (ren - 1 + i), cols = k)
      }

      if (any(nivel <= as.numeric(cv[i, k]) & as.numeric(cv[i,k]) < 30, na.rm = T) == T) {
        openxlsx::addStyle(WB, sheet = hoja, estiloR[[1]], rows = (ren - 1 + i), cols = k)
      }

      if (any(30 <= as.numeric(cv[i, k]), na.rm = T) == T) {
        openxlsx::addStyle(WB, sheet = hoja, estiloR[[2]], rows = (ren - 1 + i), cols = k)
      }
    }
  }


  #Pone datos ____________________________________________________________________
  openxlsx::writeData(WB, sheet = hoja, est, startRow = ren, colNames = F, na.string = F)

  #Opciones de salvado ___________________________________________________________
  if(salva%in%T) {
    openxlsx::saveWorkbook(WB, ruta, overwrite = T)
  }else {
    print("Ejecuta con el libro cerrado pendejete: openxlsx::saveWorkbook(WB, ruta, overwrite = T)")
    m_wb = WB
    return(m_wb)

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

# MOD A PRINCIPAL ---------------------------------------------------------------
#' @title mod_a_principal
#' @description This function allows you to carry a variable from a data basis with more rows provided for the named variable to another with minor number by some unique identifier.
#' @param modulo The data basis with the original variable and more rows.
#' @param principal The data basis where the calculation will be realized.
#' @param variable Variable to be moved from one data basis to other.
#' @param identificador Unique identifier.
#' @details The function is used in the context of a crime modulus. Where to transfer some variable to the principal
#' table is needed.
#' @return Data frame with new variables coming from the modulus.
#'
#' @keywords modulus, principal
#' @export
#' @examples
#'#Variables used in this case correspond to the ENVE of the INEGI.
#' modulo$corrupcion <- ifelse(modulo$ID_DELITO%in%'15', modulo$FAC_EXPA, 0)
#'#Using the function you'll add the "corrupcion" variable at the principal table.
#'names(prncpl)
#'prncpl <- mod_a_principal(modulo = modulo, principal = prncpl, variable = "corrupcion", identificador="CONSEC")
#'names(prncpl)
#'#Run this example on the console

mod_a_principal <- function(modulo, principal, variable, identificador){
  A <- data.frame(tapply(modulo[, variable], modulo[, identificador],sum))
  A <- data.frame(rownames(A), A)
  names(A) <- c(identificador, variable)
  principal <- merge(principal, A, by=identificador, all.x = T)
  principal[, variable] <- ifelse(principal[,variable]%in%NA, 0, principal[,variable])

  return(principal)
}


# IMPRESORA --------------------------------------------------------------------
#' @title impresora
#' @description This function helps you to write your tabulations onto an Excel workbook.
#' @param wbs Workbook used to write the tables.
#' @param lista Data table to be printed.
#' @param num_hoja Sheet of the corresponding workbook.
#' @details The function is used to print the estimation or any other tables in the workbook.
#'
#' @keywords imprime
#' @export
impresora <- function(wbs, lista, num_hoja, ren){
  openxlsx::writeData(wbs[[1]], sheet = num_hoja, lista[[1]], startRow = ren[1], colNames = F)
  openxlsx::writeData(wbs[[2]], sheet = num_hoja, lista[[2]], startRow = ren[2], colNames = F)
  openxlsx::writeData(wbs[[3]], sheet = num_hoja, lista[[3]], startRow = ren[3], colNames = F)
  openxlsx::writeData(wbs[[4]], sheet = num_hoja, lista[[4]], startRow = ren[4], colNames = F)
}


# DESCARGA ZIPS ----------------------------------------------------------------
#' @title descarga_zip
#' @description This function helps you to download files from the web in .zip format.
#' @param url This parameter is  the url of the files.
#' @param descarga Path where the .zip downloaded files will be saved. A name for the .zip files is need.
#' @param dir_descomprimido This is the directory where the descompressed files will be saved.
#' @param borra_zip If FALSE the .zip downloades files are preserved. Otherwise the files will be deleted.
#' @details In order to make the downloaded right you need the name of the downloaded zip files.
#'
#' @export
descarga_zip <- function(url, descarga, dir_descomprimido, borra_zip){
  download.file(url, destfile = descarga, mode = "wb")
  unzip(zipfile = descarga, exdir = dir_descomprimido)

  if(borra_zip==FALSE){
    print("Se almacenan el file en formato .ZIP")
  }else{
    print("Se borraron los files zipeados")
    unlink(descarga)
  }

}

# TAB PROMEDIO -----------------------------------------------------------------
#' @title Tab_promedio
#' @description This function calculates tables of averages.
#' @param xx Vector of totals. c("TOT", "TOT_1")
#' @param D Variable to disaggregate.
#' @param z Vector of labels corresponding to the disaggregator variable.
#' @param nc Confidence level for the intervals.
#' @details The D parameter must be enclosed in quotes. Its important to place xx = c('TOT_1', 'TOT').
#'
#' @export

Tab_promedio = function (xx, D, z, nc = 0.95){
  pob0 <- svytotal(t7[, xx], asp)
  pob <- as.data.frame(pob0)
  cv_pob <- cv(pob0) * 100
  int_pob <- confint(pob0, level = nc)
  int_pob <- as.data.frame(int_pob)
  se_pob <- SE(pob0)
  rel0 <- svyratio(t7[, xx], denominator = t7[, xx[1]], asp)
  rel_pob <- as.data.frame(rel0[[1]] * 1)
  cv_rel_pob <- cv(rel0) * 100
  int_rel_pob <- confint(rel0, level = nc)
  int_rel_pob <- as.data.frame(int_rel_pob)
  se_rel_pob <- SE(rel0)

  ent0 <- svyby(t7[, xx], by = t7[, D], asp, svytotal)
  ent <- as.data.frame(ent0[, 2:(length(xx) + 1)])
  cv_ent <- cv(ent0) * 100
  int_ent <- confint(ent0, level = nc)
  int_ent <- as.data.frame(int_ent)
  se_ent <- SE(ent0)

  rel_ent0 <- svyby(t7[, xx], by = t7[, D], denominator = ~TOT_1, asp, svyratio)
  rel_ent <- as.data.frame(rel_ent0[, 2:(length(xx) + 1)])
  cv_rel_ent <- cv(rel_ent0) * 100
  int_rel_ent <- confint(rel_ent0, level = nc)
  int_rel_ent <- as.data.frame(int_rel_ent)
  se_rel_ent <- SE(rel_ent0)
  est_nal <- list()
  for (i in 1:length(xx)) {
    est_nal[[i]] <- data.frame(pob[[1]][i], rel_pob[i, 1], NA)
  }
  est_nac <- do.call(cbind, est_nal)
  est_nac <- est_nac[-c(2, dim(est_nac)[2])]
  cv_nal <- list()
  for (i in 1:length(xx)) {
    cv_nal[[i]] <- data.frame(cv_pob[[i]], cv_rel_pob[i], NA)
  }
  cv_nac <- do.call(cbind, cv_nal)
  cv_nac <- cv_nac[-c(2, dim(cv_nac)[2])]
  int_nal <- list()
  for (i in 1:length(xx)) {
    int_nal[[i]] <- data.frame(int_pob[i, ], NA, int_rel_pob[i, ], NA)
  }
  int_nac <- do.call(cbind, int_nal)
  int_nac <- int_nac[-c(3:5, dim(int_nac)[2])]
  se_nal <- list()
  for (i in 1:length(xx)) {
    se_nal[[i]] <- data.frame(se_pob[[i]], se_rel_pob[i], NA)
  }
  se_nac <- do.call(cbind, se_nal)
  se_nac <- se_nac[-c(2, dim(se_nac)[2])]
  est_des <- list()
  for (i in 1:length(xx)) {
    est_des[[i]] <- data.frame(ent[i], rel_ent[i], NA)
  }
  est_ent <- do.call(cbind, est_des)
  est_ent <- est_ent[-c(2, dim(est_ent)[2])]
  cv_des <- list()
  for (i in 1:length(xx)) {
    cv_des[[i]] <- data.frame(cv_ent[i], cv_rel_ent[i], NA)
  }
  cv_ent <- do.call(cbind, cv_des)
  cv_ent <- cv_ent[-c(2, dim(cv_ent)[2])]
  aa <- seq(1, dim(int_ent)[1] + length(xx), length(table(t7[,
                                                             D])))
  for (i in 1:length(xx)) eval(parse(text = paste0("\n          int_a_",
                                                   i, " <- list()\n          int_b_", i, " <- list()\n")))
  for (j in 1:length(xx)) eval(parse(text = paste0("\nfor(i in aa[j]:(aa[j+1] - 1))\n    int_a_",
                                                   j, "[[i]] <- data.frame(int_ent[i,])\n")))
  for (j in 1:length(xx)) eval(parse(text = paste0("\n    int_a_",
                                                   j, " <- do.call(rbind, int_a_", j, ")\n")))
  for (j in 1:length(xx)) eval(parse(text = paste0("\nfor(i in aa[j]:(aa[j+1] - 1))\n    int_b_",
                                                   j, "[[i]] <- data.frame(int_rel_ent[i,])\n")))
  for (j in 1:length(xx)) eval(parse(text = paste0("\n    int_b_",
                                                   j, " <- do.call(rbind, int_b_", j, ")\n")))
  int_ent <- data.frame(int_a_1, NA, int_b_1)
  for (i in 2:length(xx)) eval(parse(text = paste0("\n  int_ent <- data.frame(int_ent, NA, int_a_",
                                                   i, ",NA,int_b_", i, ")\n")))
  int_ent <- int_ent[-c(3:5)]
  se_des <- list()
  for (i in 1:length(xx)) {
    se_des[[i]] <- data.frame(se_ent[i], se_rel_ent[i], NA)
  }
  se_ent <- do.call(cbind, se_des)
  se_ent <- se_ent[-c(2, dim(se_ent)[2])]
  tab <- list()
  colnames(est_ent) <- colnames(est_nac)
  colnames(cv_ent) <- colnames(cv_nac)
  colnames(int_ent) <- colnames(int_nac)
  colnames(se_ent) <- colnames(se_nac)
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



# TABULEADOR ORDENADO ----------------------------------------------------------
#' @title tabuleador_ordenado
#' @description This function makes ordered tables using several tables.
#' @param lista List with the tabulations.
#' @param cuantos Number of tabulations to combine.
#' @param nombres Vector of labels corresponding to the disaggregations.
#' @param tipo Indicator of the kind of tabulations.
#' @param ordena_por Column to establish the order.
#' @param quita1 Delete part of the first row of the tabulations.
#' @param quita2 Delete part of the first column of the tabulations.
#' @param con_otros Option "Otros" in the bottom of the table.
#' @param recorta Trim the tabulation to the desired number.
#'
#' @export

tabuleador_ordenado <- function(lista, cuantos, nombres, tipo, ordena_por, quita1=NULL, quita2=NULL, con_otros= NULL, recorta = NULL){
  precision = 1:4
  entidades <- list(); indice <- list()
  for(i in 1:length(tipo)){
    entidades[[i]] <- purrr::map(.x = 1:4, .f =~ pegadora(lista = lista[[.x]], cuantos = cuantos, prec = precision[.x], renglon = i,  nombres = nombres[[i]], columna = ordena_por ))
  }

  for(i in 1:length(tipo)) for(j in 1:4){
    entidades[[i]][[j]] <- mocha(entidades[[i]][[j]], quita1, quita2)
    #orden <- rownames(indice)
  }
  #---
  if(!is.null(ordena_por)){
    orden <- purrr::map(.x = 1:length(tipo),
                        .f =~ ordena(entidades[[.x]][[1]], columna = ordena_por, otros = con_otros))
    for(i in 1:length(tipo)){
      entidades[[i]] <- purrr::map(.x =1:4, .f =~ rbind(entidades[[i]][[.x]][orden[[i]],]))
    }
  }else{

  }
  #---
  if(!is.null(recorta) & !is.null(con_otros)){
    for(i in 1:length(tipo)){
      entidades[[i]] <- purrr::map(.x =1:4,
                                   .f =~ rbind(entidades[[i]][[.x]][c(1:(recorta),
                                                                      (dim(entidades[[i]][[.x]])[1]) -1,
                                                                      dim(entidades[[i]][[.x]])[1] ),]))
    }
  }else if(!is.null(recorta) & is.null(con_otros)){
    for(i in 1:length(tipo)){
      entidades[[i]] <- purrr::map(.x =1:4, .f =~ rbind(entidades[[i]][[.x]][c(1:(recorta+1), dim(entidades[[i]][[.x]])[1] ), ]))
    }
  }else if(is.null(recorta) & !is.null(con_otros)){
    for(i in 1:length(tipo)){
      entidades[[i]] <- purrr::map(.x =1:4, .f =~ rbind(entidades[[i]][[.x]]))
    }
  }else{
    for(i in 1:length(tipo)){
      entidades[[i]] <- purrr::map(.x =1:4, .f =~ rbind(entidades[[i]][[.x]]))
    }
  }
  #---
  tabulado0 <- purrr::map(.x = 1:4, .f =~ entidades[[1]][[.x]])

  for(i in 2:length(tipo)){
    tabulado0 <- purrr::map(.x = 1:4, .f =~ rbind(tabulado0[[.x]], entidades[[i]][[.x]]))
  }

  tabulado0 <- purrr::map(.x = precision, .f =~ tabulado0[[.x]][-dim(tabulado0[[.x]])[1],])

  return(tabulado0)
}


# TAB_T ------------------------------------------------------------------------
#' @title Tab_T
#' @description This function calculates vertical tabulations. Is similar to Tab_vert_a.
#' @param TOT Vector of totals.
#' @param x The totals corresponding to other characteristics.
#' @param z Vector of labels corresponding to the disaggregations.
#' @param nc Confidence level for the intervals
#' @export

Tab_T  <- function(TOT, x, y, nc = 0.95){
  pob <- Prec(t7$TOT, asp)

  for(i in 1:length(x)) eval(parse(text = paste0("
  x",i,"<-svytotal(~TOT_",i,",asp)
  pob_",i,"<-data.frame(x",i,"[[1]],cv(x",i,")*100,confint(x",i,",level=nc),SE(x",i,"))
  colnames(pob_",i,")<-c(\"pob_tot\",\"CVpob_tot \",\"intervalo de\",\"confianza\",\"SE\")

  y",i,"<-svyratio(~TOT_",i,",denominator=~TOT,asp)
  rel_",i,"<-data.frame(y",i,"[[1]]*100,cv(y",i,")*100,confint(y",i,",level=nc)*100,SE(y",i,")*100)
  colnames(rel_",i,")<-c(\"relativo\",\"CVrelativo\",\"intervalo de\",\"confianza\") ")))

  ###
  robo <- rbind(NA, pob_1)
  rel_robo<-rbind(NA, rel_1)

  for(i in 2:length(x))
    eval(parse(text = paste0("
  robo <- rbind(robo,pob_",i,")
  rel_robo <- rbind(rel_robo,rel_",i,") ")))

  ###
  xx<-list()
  xx[[1]] <- cbind(NA, robo, rel_robo)
  xx[[1]][,1]<-c("", y[1:length(x)])
  xx[[1]]<-xx[[1]][c( 1, order(xx[[1]][c(2:( dim(xx[[1]])[1]-1) ),length(x)],decreasing = TRUE)+1,((length(x)+1) ) ),]
  #xx[[1]]<-xx[[1]][c( 1, order(xx[[1]][c(2:( dim(xx[[1]])[1]-1) ),length(x)],decreasing = TRUE)+1)]

  est_robo<-cbind(xx[[1]][1],NA,NA,xx[[1]][2],xx[[1]][7])
  est_robo[1,1]<-estados[1];est_robo[1,2]<-pob[1]
  cv_robo<-cbind(xx[[1]][1],NA,NA,xx[[1]][3],xx[[1]][8])
  cv_robo[1,1]<-estados[1];cv_robo[1,2]<-pob[2]
  int_robo<-cbind(xx[[1]][1],NA,NA,NA,xx[[1]][4:5],NA,xx[[1]][9:10])
  int_robo[1,1]<-estados[1];int_robo[1,c(2:3)]<-c(pob[3],pob[4])
  SE_robo<-cbind(xx[[1]][1],NA,NA,xx[[1]][6],xx[[1]][11])
  SE_robo[1,1]<-estados[1];SE_robo[1,2]<-pob[5]
  ###
  z<-list(); z[[1]] <- est_robo; z[[2]] <- cv_robo; z[[3]] <- int_robo; z[[4]]<-SE_robo

  return(z)
}


# PONER SUPERSCRIPT ---------------------------------------------------------
#' @title poner_supscript
#' @description This function places the corresponding number of some call on the labels
#' in a data table.
#' @param etiquetas Vector of labels with the end mark _x1, _x2, etc.
#' @param numeros Vector of numbers corresponding to the superscript.
#'
#' @retun The vector of labels with the number instead _x1, _x2, etc.
#' @examples
#'
#' mis_etiqts <- c("Robo total de vehículo",
#'                 "Robo de accesorios de vehículo",
#'                 "Robo de mercancía en tránsito",
#'                 "Robo hormiga",
#'                 "Robo/asalto de bienes o dinero_x1",
#'                 "Otros_x3",
#'                 "Fraude_x2",
#'                 "Extorsión",
#'                 "Daños a las instalaciones, maquinaria o equipo")
#'
#' mis_numeros = c("3", "4", "5")
#'
#' mis_sufijos = c("_x1", "_x2", "_x3")
#'
#' poner_supscript(etiquetas = mis_etiqts, numeros = mis_numeros, sufijos = mis_sufijos)

poner_supscript = function (etiquetas, numeros, sufijos) {
  posiciones = grep("_", etiquetas)
  candidatos = etiquetas[posiciones]
  for (i in 1:length(posiciones)) {
    indica = substr(candidatos[i], nchar(candidatos[i]) - 2, nchar(candidatos[i]))
    lugar = grep(indica, sufijos)
    zz = stringr::str_replace(candidatos[i], indica, numeros[lugar])
    etiquetas[posiciones[i]] = zz
  }
  return(etiquetas)
}

# REDUCE TOT MODULOS------------------------------------------------------------
#' @title reduce_tot_mod
#' @description This function makes two processes. The first one is the reduction of the
#' total of modulus of crimes keeping only those that are unique.The second part is the sum of this total.
#' @param df Data frame with the ID, TOT_MODULO and ID_DELITO.
#'
#' @retun A list with the reduced data frame and the sum of crime modulus.
#' @examples
#'

reduce_tot_mod = function(df){
  df1 = df[!duplicated(df$ID_DELITO),]
  df1$ID_DELITO =  as.numeric(df1$ID_DELITO)
  df1 = df1[c( order( df1[,"ID_DELITO"], decreasing = FALSE) ), ]

  suma_modulos = sum(df1$TOT_MODULO)

  x = list()
  x[[1]] = df1
  x[[2]] = suma_modulos

  return(x)

}


# TRANSPONE BLOQUE DE MODULOS --------------------------------------------------
#' @title transpone_mod_del
#' @description This function makes two processes. The first one is the reduction of the
#' total of modulus of crimes keeping only those that are unique. The second part is the transpose and
#' presentation of the modulus as a table of crimes in the main table.
#' @param consec Economic unit identifier.
#'
#' @retun A list with the reduced data frame and the transpose.
#' @examples

transpone_mod_del = function(CONSEC){
  xx = as.data.frame(rep(CONSEC, 15))
  xx$ID_DELITO = c(paste0('0', 1:9), 10:15)
  xx$ID_DELITO = as.numeric(xx$ID_DELITO)
  names(xx)[1] = 'CONSEC'
  xx$TOT_MODULO = NA
  xx$ID_DEL_A = paste0(xx$CONSEC, xx$ID_DELITO)
  # En este caso df es el data frame de referencia, O sea el que se pone como
  # parámetro inicial
  df = tmv[tmv$CONSEC%in%CONSEC, c('CONSEC', 'TOT_MODULO', 'ID_DELITO')]
  df = df[!duplicated(df$ID_DELITO),]
  df$ID_DELITO = as.numeric(df$ID_DELITO)

  df <- df[c(order(df[c(1:nrow(df)), 3], decreasing = FALSE) ), ]
  # En el df de entrada hago el ID_DELITO_A que es un auxiliar solo para
  # asegurar que lo hago correctamente
  df$ID_DEL_A = paste0(df$CONSEC, df$ID_DELITO)

  # En mi dataframe artificial pego el numero de delitos y lepongo las
  # variables que le pudieran faltar usando el parámetro
  xx[xx$ID_DEL_A%in%df$ID_DEL_A,'TOT_MODULO'] = df[,'TOT_MODULO']


  xxx = tidyr::pivot_wider(xx, id_cols = 'CONSEC', names_from = 'ID_DELITO', values_from = 'TOT_MODULO')
  xxx = as.data.frame(xxx)
  nams <- c("P4_6", "P4_5_2", "P4_5_3", "P4_4_4", "P4_5_5", "P4_5_6", "P4_5_7", "P4_5_8",
            "P4_5_9", "P4_5_10",	"P4_5_11",	"P4_5_12",	"P4_5_13",	"P4_5_14",	"P6_5")
  colnames(xxx) = c("CONSEC", nams)

  regresa = list()
  regresa[[1]] = xxx
  regresa[[2]] = df[,-4]
  return(regresa)
}

# PONE NOTAS -------------------------------------------------------------------
#' @title pone_notas
#' @description This function makes the processes of replace some key word in the
#' template for a given number. To save the workbook is needed the corresponding path.
#' @param mi_wb A workbook class object.
#' @param num_hoja Number of the sheet to modify the key word.
#' @param clave Key word.
#' @param numero Number used to replace the keyword.
#' @retun A Excel workboob to be saved.
#' @examples

pone_notas = function(mi_wb, num_hoja, clave, numero) {
  hoja = openxlsx::read.xlsx(mi_wb, sheet=num_hoja, skipEmptyRows = FALSE, colNames = TRUE)

  pos1 = which(stringr::str_detect(hoja[,1], clave))
  hoja[pos1, 1] = stringr::str_replace(hoja[pos1, 1], clave, format(numero, big.mark=" "))

  openxlsx::writeData(mi_wb, sheet = num_hoja, x = hoja[pos1 , 1], startRow = (pos1 +1), startCol = 1 )

}


# HACE PH ----------------------------------------------------------------------
#' @title ph_a
#' @description This function performs the hypothesis testing for the INEGI survey estimates.
#' @param df A data frame with both estimations and the standard errors.
#' @param nacional If TRUE, the result of the testing includes the national figure.
#' @retun A data frame with the hypothesis testing.
#' @details The df data frame should includes the labels, old estimating, new estimating and the corresponding standard errors.
#' @export

ph_a <- function(df, nacional = TRUE){ #df lleva etiquetas, est1, est2, err1, err2
  aux1 <- df[, c(1:3)]
  aux1$Cambio_Porcent <- (as.numeric(aux1[,3])-as.numeric(aux1[,2]))/as.numeric(aux1[,2])*100

  aux2<- df[, c(5:6)]

  df_aux <- cbind(aux1, NA, aux2)
  df_aux[,2] <- as.numeric(as.character(df_aux[,2]))
  df_aux[,3] <- as.numeric(as.character(df_aux[,3]))
  df_aux[,4] <- as.numeric(as.character(df_aux[,4]))
  df_aux[,6] <- as.numeric(as.character(df_aux[,6]))
  df_aux[,7] <- as.numeric(as.character(df_aux[,7]))


  # aux1 debe ir en la forma est_vieja, est_nueva, cambio_porcen, se_vieja, se_nueva
  df_aux$Estadistico <- ifelse((df_aux[,2]-df_aux[,3])/sqrt(df_aux[,6]^2 + df_aux[,7]^2)<0,
                               (df_aux[,2]-df_aux[,3])/sqrt(df_aux[,6]^2 + df_aux[,7]^2),
                               -(df_aux[,2]-df_aux[,3])/sqrt(df_aux[,6]^2 + df_aux[,7]^2))
  df_aux$Distribucion <- abs(pnorm(df_aux$Estadistico)) #### iba con un *2 pero dijeron que no les gusta

  df_aux$Cambio_Sig<-ifelse(df_aux$Distribucion>=0.05,"No","Sí")
  df_aux$Dif_abs <- df_aux[,3]- df_aux[,2]
  df_aux$Tendencia<-ifelse(df_aux$Cambio_Sig%in%"No" ,"Igual",
                           ifelse(df_aux$Cambio_Porcent>0,"Subió","Bajó"))

  df_nuevo <- df_aux[ ,c(1:7, dim(df_aux)[2])]

  if(!nacional%in%TRUE){
    for(i in 2:dim(df_nuevo)[2]){
      df_nuevo[1,i] <- NA
    }
  }
  return(df_nuevo)

}


# ESCRIBE PH -------------------------------------------------------------------
#' @title escribe_ph
#' @description This function write the result of the hypothesis testing using
#' the function hace_ph. Having prior the templates to place the information is mandatory.
#' @param WB A data frame with both estimations and the standard errors.
#' @param nom_hoja If TRUE, the result of the testing includes the national figure.
#' @param df A data frame resulting of using the hace_ph function.
#' @param cv The variations coefficients of the corresponding estimates in the df data frame.
#' @param guarda NULL by default. Accept values of TRUE or FALSE.
#' @param format_num description
#' @retun A data frame with the hypothesis testing.
#' @details If the parameter "guarda" is FALSE, then the result must be saved later.
#' @export

escribe_ph <- function(WB, nom_hoja, df, cv, guarda = NULL, format_num){ #esta funcion pinta cvs y phs

  A <- openxlsx::read.xlsx(WB, sheet = nom_hoja, colNames = F, skipEmptyRows = F)
  ren <- which(A$X1%in%'Estados Unidos Mexicanos')
  #_______________________________________________________________________________
  e1 <- openxlsx::createStyle(fontName = 'Arial',fontSize = 8, numFmt = "0.0",
                              halign = 'right', valign = 'center', fgFill = '#6BC7DC')
  e2 <- openxlsx::createStyle(fontName = 'Arial',  fontSize = 8, numFmt = "0.0",
                              halign = 'right', valign = 'center', fgFill = '#6AB76B')

  e3 <- openxlsx::createStyle(fontName = 'Arial', fontSize = 8, numFmt = format_num,
                              halign = 'right', valign = 'center', fgFill = '#FF0000')
  e4 <- openxlsx::createStyle(fontName = 'Arial', fontSize = 8, numFmt = format_num,
                              halign = 'right', valign = 'center', fgFill = '#92D050')

  #_______________________________________________________________________________
  estilo_a1 <- openxlsx::createStyle(fontName = 'Arial', fontSize = 8, numFmt = format_num,
                                     halign = 'right', valign = 'center', fgFill = '#FFEA00')
  estilo_a2 <- openxlsx::createStyle(fontName = 'Arial', fontSize = 8,  numFmt = format_num,
                                     halign = 'right', valign = 'center', fgFill = '#FF5400')
  estilo_a3 <- openxlsx::createStyle(fontName = 'Arial', fontSize = 8,  numFmt = format_num,
                                     halign = 'right', valign = 'center', fgFill = '#FFFFFF')


  #_______________________________________________________________________________
  for(i in c(2,3,4,6,7)){
    df[,i] <- as.numeric(as.character(df[,i]))
  }
  #addWorksheet(wb = WB, sheetName = nom_hoja)
  openxlsx::writeData(wb = WB, sheet = nom_hoja, x = df, startRow = ren, colNames = FALSE)

  if(sum(is.na(df[,2])) < dim(df)[1])
    for(j in 1:dim(df)[1]){
      if(sum(!df[j,4]%in%c("NA", NA,0) )>0){
        if(any(df[j,4] < -25, na.rm = T)==T){
          openxlsx::addStyle(wb = WB, sheet = nom_hoja, e1, rows = ren-1 + j, cols = 4, stack = T)
        }else if(any( 25 < df[j,4], na.rm = T)==T){
          openxlsx::addStyle(wb = WB, sheet = nom_hoja, e2, rows = ren-1 + j, cols = 4, stack = T)
        }else{
        }
      }
    }

  if(sum(is.na(df[,2])) < dim(df)[1])
    for(j in 1:dim(df)[1]){
      if(sum(!df[j,8]%in%c("NA", NA,0) )>0){
        if(any(df[j,8]=='Subió', na.rm = T)==T){
          openxlsx::addStyle(wb = WB, sheet = nom_hoja, e3, rows = ren-1 + j, cols = 8, stack = T)
        }else if(any(df[j,8]=='Bajó', na.rm = T)==T){
          openxlsx::addStyle(wb = WB, sheet = nom_hoja, e4, rows = ren-1 + j, cols = 8, stack = T)
        }else{
        }
      }
    }

  for(k in 2:3){
    if(sum(is.na(df[,2])) < dim(df)[1])
      for(j in 1:dim(df)[1]){
        if(sum(!cv[j,k]%in%c("NA", NA, 0) )>0){
          if(any( 20 <= as.numeric(cv[j,k]) & as.numeric(cv[j,k]) < 30, na.rm = T)==T){
            openxlsx::addStyle(wb = WB, sheet = nom_hoja, estilo_a1, rows = ren-1 + j, cols = k, stack = T)
          }else if(any( 30 <= as.numeric(cv[j,k]), na.rm = T)==T){
            openxlsx::addStyle(wb = WB, sheet = nom_hoja, estilo_a2, rows = ren-1 + j, cols = k, stack = T)
          }else{
          }
        }
      }
  }


  if(guarda == TRUE){
    openxlsx::writeData(WB,sheet = nom_hoja, df, startRow = ren, colNames = F, na.string = F )
    openxlsx::saveWorkbook(WB,ruta,overwrite = T)
  }else if(guarda == FALSE){
    openxlsx::writeData(WB,sheet = nom_hoja, df, startRow = ren, colNames = F, na.string = F )
    #return(WB)
  }else{
    warning("Se debe declarar un valor de T ó F para el parametro de guardar")
  }

}



# TRUNCAR ----------------------------------------------------------------------
#' @title truncar
#' @description This function truncates to a given length.
#' @param x The number of decimal places required.
#' @param digits Number of decimals desired.
#' @param chars Format of the output.
#' @retun A truncate to the decimal places number given by the user.
#' @details This function is not the same that rounding function.
#' @export
#'
truncar <- function(x, digits, chars = TRUE) {
  if(grepl(x = x, pattern = "\\.")) {
    y=as.character(x)
    pos=grep(unlist(strsplit(x = y, split = "")), pattern = "\\.", value = FALSE)
    if(chars) {
      return(substr(x = x, start = 1, stop = pos + digits))
    }
    return(
      as.numeric(substr(x = x, start = 1, stop = pos + digits))
    )
  } else {
    return(
      #format(round(x, 2), nsmall = 2)
      x
    )
  }
}



# COMPARA UN LIBRO ----------------------------------------------------
compara_un_libro <- function(df1, df2, hoja_reporte, wb2){
  #I define a style to inlclude in the reports
  estilo <- openxlsx::createStyle(fontName = "Arial", fontSize = 10,  fontColour = NULL,
                                  numFmt = "0.0",
                                  border = NULL,
                                  borderColour = getOption("openxlsx.borderColour", "black"),
                                  borderStyle = getOption("openxlsx.borderStyle", "dotted"),
                                  bgFill = "#14C3E6",
                                  fgFill = "#14C3E6",
                                  halign = "right",
                                  valign = "center",
                                  textDecoration = "bold",
                                  wrapText = FALSE,
                                  textRotation = NULL,
                                  indent = NULL,
                                  locked = NULL,
                                  hidden = NULL)
  if(any(dim(df1)!=dim(df2))== TRUE){
    cat("No son iguales las dimensiones del tabulado", hoja_reporte ,". Ábrete ALV a revisar!!!! \n")
  }else{

    # I'll separate the columns of each tabulation
    aa1 <- lapply(1:ncol(df1), function(i){return(df1[,i])})
    aa2 <- lapply(1:ncol(df2), function(i){return(df2[,i])})
    # In this step make truncation of the items
    for(j in 1:length(aa1)){
      aa1[[j]] <- lapply(1:length(aa1[[j]]), function(i){rec_dig_M( aa1[[j]][i], 4) })
      aa2[[j]] <- lapply(1:length(aa2[[j]]), function(i){rec_dig_M( aa2[[j]][i], 4) })
    }
    # In this point I make the comparison
    malos <- list()
    columna_mala <- list()
    for(j in 1:length(aa1)){
      if(any(all.equal(aa1[[j]],aa2[[j]]) != T)){

        malos[[j]] <- all.equal(aa1[[j]],aa2[[j]])
        columna_mala[[j]] <- j
      }
    }
    columna_mala <- unlist(columna_mala) # I use this data to evaluate te entries of the malos list

    if(length(columna_mala)>0){
      cat("Hay diferencias en el tabulado ", hoja_reporte , " mi perro!!! \n")
      bb <- purrr::map(.x = columna_mala, .f =~ malos[.x])

      a <- list()
      b <- list()
      cc <- list()

      for(k in 1:length(bb)){
        cc[[k]] <- stringr::str_split(bb[[k]][[1]], ":")
      }

      purrr::map(.x= 1:length(cc[[1]]), .f=~ substr(cc[[1]][[.x]][1], 11, nchar(cc[[1]][[.x]][1])))

      for(i in 1:length(cc)){
        b[[i]] <- purrr::map(.x= 1:length(cc[[i]]), .f=~ substr(cc[[i]][[.x]][1], 11, nchar(cc[[i]][[.x]][1])))
      }

      # Since there's a bad cases I create the report and add the style to each bad column

      #openxlsx::addWorksheet(wb_reporte, sheetName = hoja_reporte)
      #openxlsx::writeData(wb_reporte , sheet = hoja_reporte, df1 , startCol = 1, colNames = F)
      for(k in 1:length(b)){
        for(i in 1:length(b[[k]])){
          openxlsx::addStyle(wb2 , sheet = hoja_reporte , style = estilo, rows = b[[k]][[i]], cols = columna_mala[k])
        }
      }

    }else{
      openxlsx::removeWorksheet(wb2, sheet = hoja_reporte)
    }
  }
  #return(wb2)

}


# ESCRIBE_MACRO ----------------------------------------------------------------------
#' @title escribe_macro
#' @description This function makes a routine in VBS and places it in the specified directory.
#' @param v_texto Chunk that in a standard way makes the code that will be written in VBS.
#' @param direccion_vbs Location where the VBS will be placed.
#' @export
#'
escribe_macro = function(v_texto, direccion_vbs,...) {
  vbs = file(direccion.vbs,...)
  writeLines(v_texto,vbs)
  close(vbs)
  }


# CORRE_MACRO ------------------------------------------------------------------
#' @title corre_macro
#' @description This function runs an external macros once the routine to make the
#' corresponding execution in R has been written using escribe_macro.
#' @param direcccion_vbs Location where the VBS will be placed.
#' @details This function runs a VBS that has been written and placed suitable using escribe_macro function.
#' @export
#'
corre_macro = function(direccion_vbs){
  shell(shQuote(normalizePath(direccion.vbs)), "cscript", flag = "//nologo")
}

# PINTAR_EXCEL -----------------------------------------------------------------
#' @title pintar_EXCEL
#' @description This function runs an external macros making the painted tabulations
#' process using an external macro developed in VBA.
#' @param libro1 Location where the workbook with the estimation tabulations is placed.
#' @param libro2 Location where the workbook with the variation coefficients tabulations is placed.
#' @param dir_local_macro Route in local where the macros is located.
#' @details This function makes the process of painting the estimations tabulations
#' using an external macros. That macros needs be written in a suitable way in order
#' to guarantee the appropriate execution.
#' @export
#'
pintar_EXCEL = function(libro1,libro2, dir_local_macro) {
a0=c("Set objExcel = CreateObject(\"Excel.Application\")",
     "objExcel.Visible = TRUE",
     "objExcel.DisplayAlerts=FALSE",
     "Public f1",
     paste0("f1 = CStr(\"",libro1 ,"\")"),
     "Public f2",
     paste0("f2 = CStr(\"",libro2 ,"\")"),

     paste0("objExcel.Application.Run ",paste0("\"'",dir_local_macro,"macro_pinta_enve_asp1.xlsm'!pinta\" "),", f1,f2"),
     "objExcel.Application.Quit")
d0=paste0('D:/OneDrive - INEGI/Documents/R_VBS/',"pintarHTSP1.vbs")
escribe_macro(a0,d0)
corre_macro(d0)
}
