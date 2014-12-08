# Functions for DataManagementSurvey.R
#     recode_NA()
#     calculate_age()
#     drink_group()
#     eat_group()
#     sleep_group()
#     behaviour_group()

recode_NA <- function (varnaam) {
    varnaam[varnaam %in% c("888","999",NA)] <- 0
    varnaam
}

calculate_age <- function(date_of_birth, date_of_age){
   dob <- as.POSIXlt(date_of_birth)
   doa <- as.POSIXlt(date_of_age)
   
   if (!is.na(dob) & !is.na(doa)) {
        age <- doa$year - dob$year
        if (dob$mon > doa$mon 
            | (  dob$mon == doa$mon 
               & dob$mday > doa$mday)) {
        age <- age - 1 
        }
    }
   else {
       age <- NA
   }
   age
}

drink_group <- function (nod){
    if (is.na(nod)){
        group <- 0
    }
    else if (nod == 0){
        group <- 0
        }
    else if (nod %in% c(1:6)) {
        group <- 1
    }
    else group <- 2
    group
}

eat_group <- function (nom) {
    print(nom)
    if (nom == "NA/unknown/decline"){
        group <- 0
        }
    else if (nom == "Absolutely not"){
        group <- 0
    }
    else if (nom == "Only once") {
        group <- 1
    }
    else group <- 2
    group
}

sleep_group <- function (nos) {
    if (nos == "NA/unknown/decline"){
        group <- 0
        }
    else if (nos == "Every day"){
        group <- 0
    }
    else if (nos == "Not every day") {
        group <- 1
    }
    else group <- 2
    group
}

behaviour_group <- function(drink, eat, sleep){
    if (drink == 2 | eat  == 2 | sleep == 2){
        behaviour  <- 2
    }
    else if (drink == 1 | eat== 1 | sleep== 1) {
        behaviour  <- 1
    }
    else behaviour <- 0
    behaviour
}