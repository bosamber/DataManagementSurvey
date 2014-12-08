# Clear existing data and graphics
rm(list=ls())

## Set working directory
setwd("~/Documents/Coursera/ 6. data management for clinicial research/8 Assignment 3/DataManagementSurvey")
source("Functions_ModifyData.R")
## Read raw data
data          <- read.csv('SurveyKingsDayToYour_DATA_2014-12-06_0940.csv')

## Count number of surveys in raw dataset
raw_records   <- nrow(data)
raw_records

# Keep only completed surveys
survey_data              <- data[data$health_survey_kings_day_complete == 2, ]

# Convert character survey-timestamp to Date-class
survey_data$survey_date  <- as.Date(survey_data$health_survey_kings_day_timestamp
                                   ,"%Y-%m-%d %H:%M:%S")
survey_begin             <- as.Date("2014-11-30", "%Y-%m-%d")
survey_end               <- as.Date("2014-12-05", "%Y-%m-%d")

# Select records between survey_begin and survey_end
survey_data  <- survey_data[survey_data$survey_date  >= survey_begin
                            & survey_data$survey_date <= survey_end
                            & !is.na(survey_data$survey_date), ]

# Remove REDCap columns not needed for research) names(survey_data[29])
# 1 "record_id"
# 2 "redcap_survey_identifier"
# 3 "health_survey_kings_day_timestamp"  
# 18 "health_survey_kings_day_complete"
# 29 "health_survey_kings_day_complete.factor"
survey_data  <- subset(survey_data, ,-c(1,2,3,18,29))

# Remove duplicate records
survey_data  <- survey_data[!duplicated(survey_data), ]

# For categorical variables recode NA's, Do not know and Decline to answer to 0
survey_data$health_headache            <- recode_NA(survey_data$health_headache)
survey_data$gender                     <- recode_NA(survey_data$gender)
survey_data$present_yn                 <- recode_NA(survey_data$present_yn)
survey_data$participation_orange_craze <- recode_NA(survey_data$participation_orange_craze) 
survey_data$participation_kings_night  <- recode_NA(survey_data$participation_kings_night)  
survey_data$participation_free_market  <- recode_NA(survey_data$participation_free_market)  
survey_data$behaviour_skip_meals       <- recode_NA(survey_data$behaviour_skip_meals)       
survey_data$behaviour_bed_time         <- recode_NA(survey_data$behaviour_bed_time)         
survey_data$health_fatigue             <- recode_NA(survey_data$health_fatigue)            
survey_data$health_headache            <- recode_NA(survey_data$health_headache)           
survey_data$health_nausea              <- recode_NA(survey_data$health_nausea)             

# For reporting: create factor variables for categorical variables
survey_data$gender.factor            <- factor(survey_data$gender,levels=c("0", "1","2","3"))
survey_data$present_yn.factor        <- factor(survey_data$present_yn,levels=c("0","1","2"))
survey_data$part_orange_craze.factor <- factor(survey_data$participation_orange_craze,levels=c("0","1","2","3"))
survey_data$part_kings_night.factor  <- factor(survey_data$participation_kings_night,levels=c("0","1","2","3"))
survey_data$part_free_market.factor  <- factor(survey_data$participation_free_market,levels=c("0","1","2","3"))
survey_data$behav_skip_meals.factor  <- factor(survey_data$behaviour_skip_meals,levels=c("0","1","2","3"))
survey_data$behav_bed_time.factor    <- factor(survey_data$behaviour_bed_time,levels=c("0","1","2","3"))
survey_data$health_fatigue.factor    <- factor(survey_data$health_fatigue,levels=c("0","1","2","3","4"))
survey_data$health_headache.factor   <- factor(survey_data$health_headache,levels=c("0","1","2","3","4"))
survey_data$health_nausea.factor     <- factor(survey_data$health_nausea,levels=c("0","1","2","3","4"))

# Set reporting labels for the values
NA_answer                                     <- "NA/unknown/decline"
participation_levels                          <- c(NA_answer
                                                   ,"Yes, I participated"
                                                   ,"I was only watching"
                                                   ,"No, I did not join at all"
                                                   )
health_levels                                 <- c(NA_answer
                                                   , "Not at all"
                                                   ,"A little"
                                                   ,"Moderately"
                                                   ,"Very much so"
                                                   )
levels(survey_data$gender.factor)             <- c(NA_answer
                                                   ,"Male","Female","Other")
levels(survey_data$present_yn.factor)         <- c(NA_answer
                                                   ,"No","Yes")
levels(survey_data$part_orange_craze.factor)  <- participation_levels
levels(survey_data$part_kings_night.factor)   <- participation_levels
levels(survey_data$part_free_market.factor)   <- participation_levels
levels(survey_data$behav_skip_meals.factor)   <- c(NA_answer
                                                   ,"Absolutely not"
                                                   ,"Only once"
                                                   ,"More than once"
                                                   )
levels(survey_data$behav_bed_time.factor)     <- c(NA_answer
                                                   ,"Every day"
                                                   ,"Not every day"
                                                   ,"I did not go to bed at all"
                                                   )
levels(survey_data$health_fatigue.factor)     <- health_levels
levels(survey_data$health_headache.factor)    <- health_levels
levels(survey_data$health_nausea.factor)      <- health_levels

# Set reporting labels for the variables
label(survey_data$date_of_birth)              <- "What is your date of birth? "
label(survey_data$gender.factor)              <- "What is your gender? "
label(survey_data$weight_kg)                  <- "What is your weight (in kg)?"
label(survey_data$height_cm)                  <- "What is your height (in cm)?"
label(survey_data$present_yn.factor)          <- "Were you present during King's Day?"
label(survey_data$part_orange_craze.factor)   <- "Did you participate in the Orange Craze? "
label(survey_data$part_kings_night.factor)    <- "Did you participate in King's Night?"
label(survey_data$part_free_market.factor)    <- "Did you participate in the Free Market?"
label(survey_data$behav_skip_meals.factor)    <- "Did you skip any of your regular meals? "
label(survey_data$behaviour_number_of_drinks) <- "# of drinks containing alcohol "
label(survey_data$behav_bed_time.factor)      <- "Did you go to bed at the same time as usual?"
label(survey_data$health_fatigue.factor)      <- "Did you suffer from fatigue?"
label(survey_data$health_headache.factor)     <- "Did you suffer from headache?"
label(survey_data$health_nausea.factor)       <- "Did you suffer from nausea?"

# Convert character date_of_birth to Date-class
survey_data$date_of_birth <- as.Date(survey_data$date_of_birth, "%Y-%m-%d")
# Calculate age at date of survey
age                            <- mapply(calculate_age
                                         , survey_data$date_of_birth
                                         , survey_data$survey_date)
age_group                      <- ceil(age/5)
age_group[is.na(age_group)]    <- 0
survey_data$age.factor         <- as.factor(age_group)
levels(survey_data$age.factor) <- c(NA_answer, '16-20','26-30','31-35'
                                    , '36-40','41-45','46-50', '51-55')
label(survey_data$age.factor) <- "Age category of participant"
# drop date of birth and tmp-variables
survey_data                    <- survey_data[ , -c(1)]
rm(age, age_group)

# calculate BMI
bmi                            <- round(survey_data$weight_kg**2 / survey_data$height_cm
                                      , digits=2)
bmi_group                      <- floor(bmi/10)
bmi_group[is.na(bmi_group)]    <- 0
survey_data$bmi.factor         <- as.factor(bmi_group)
levels(survey_data$bmi.factor) <- c(NA_answer
                                    , "normal weight"
                                    , "slight overweight"
                                    , "overweight"
                                    , "extra overweight"
                                    )
label(survey_data$bmi.factor) <- "BMI category of participant"
# Remove temporary variables
rm(bmi, bmi_group)

# participation scale
survey_data$participation_scale <- (as.numeric(survey_data$participation_orange_craze  %in% c(1,2))
                      + as.numeric(survey_data$participation_kings_night %in% c(1,2))
                      + as.numeric(survey_data$participation_free_market %in% c(1,2)))

# health scale with clumsy recoding
fatigue_count  <- survey_data$health_fatigue
fatigue_count[fatigue_count !=0 ]   <- fatigue_count[fatigue_count !=0 ]-1
headache_count <- survey_data$health_headache
headache_count[headache_count !=0 ] <- headache_count[headache_count !=0 ]-1
nausea_count   <- survey_data$health_nausea
nausea_count[nausea_count !=0 ]     <- nausea_count[nausea_count !=0 ]-1
survey_data$health_scale            <- fatigue_count + headache_count + nausea_count

# remove temporary variables
rm(fatigue_count, headache_count, nausea_count)

# behaviour scales
behaviour_drink <- sapply(survey_data$behaviour_number_of_drinks, FUN=drink_group)
behaviour_eat   <- sapply(survey_data$behav_skip_meals.factor, FUN=eat_group)
behaviour_sleep <- sapply(survey_data$behav_bed_time.factor, FUN=sleep_group)
behaviour_group <- mapply(FUN=behaviour_group, behaviour_drink,behaviour_eat, behaviour_sleep)
survey_data$behaviour.factor         <- as.factor(behaviour_group)
levels(survey_data$behaviour.factor) <- c("abstination"
                                          ,"moderate"
                                          ,"excessive")

##================ Start analysis ================
# report number of records used for further analysis
report_records <- nrow(survey_data)
report_records

# report on categorical variables
frequency_variables <- c("gender.factor"
                         , "age.factor"
                         , "bmi.factor"
                         , "present_yn.factor"
                         , "part_orange_craze.factor" 
                         , "part_kings_night.factor"
                         , "part_free_market.factor"
                         , "behav_skip_meals.factor"
                         , "behav_bed_time.factor"
                         , "health_fatigue.factor"
                         , "health_headache.factor"
                         , "health_nausea.factor")
par(cex=1,las=1, mar=c(5,10,4,1))
for (plotvar in frequency_variables){
#print(label(survey_data[plotvar]))
    print(table(survey_data[plotvar]))
    plot(survey_data[, plotvar]
         , main=label(survey_data[plotvar])
         , horiz=TRUE, xlim=c(0,20))
}


plot(survey_data$height_cm, survey_data$weight_kg
     , main = "Body weight en height of partcipants"
     , xlab = label(survey_data$height_cm)
     , ylab = label(survey_data$weight_kg)
     , cex=0.5, pch=16 )

table(survey_data$participation_scale, survey_data$present_yn.factor)
table(survey_data$participation_scale[survey_data$present_yn==2]
      , survey_data$health_scale[survey_data$present_yn==2])
table(survey_data$health_scale[survey_data$present_yn==2]
      ,survey_data$behaviour.factor[survey_data$present_yn==2])

