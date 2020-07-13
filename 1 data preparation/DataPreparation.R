# DATA PREPARATION FILE
######  --------
######  Set up --------
# packages ------
library(dplyr)
library(ggplot2)
library(RPostgreSQL)
library(lubridate)
library(mstate)

# database connection -----

# Make connection to the database using dbConnect
# This includes password etc, so do it outside of this file 

# db <- dbConnect(.....)
# dbListTables(db) 


# dates ####
# 1) the date for which we'll identify variables of interest
# only up to the date, not after
healthy.start.date<-as.Date("29/02/2020", 
                            "%d/%m/%y")
# 2) the date at which time at risk stats
tar.start.date<-as.Date("01/03/2020", 
                    "%d/%m/%y")
#3) the date at which time at risk ends for administrative censoring
tar.end.date<-as.Date("06/05/2020", 
                       "%d/%m/%y")
###### #######
######  Identify study population -------
# Collect from person table ------
person<-tbl(db, sql("SELECT * FROM omop20v2.person")) %>% 
  select("person_id" , "gender_concept_id", "year_of_birth", "month_of_birth", "day_of_birth") %>% 
  collect()
observation_period<-tbl(db, sql("SELECT * FROM omop20v2.observation_period")) %>% 
  select("person_id", "observation_period_start_date", "observation_period_end_date") %>% 
  collect()
person<-person %>% 
  left_join(observation_period, 
            by="person_id")
rm(observation_period)

# only those with observation start date pior to or on healthy.start.date -----
person<-person %>% 
  filter(observation_period_start_date<=healthy.start.date)

# only those with observation end date on or after 1st March -----
person<-person %>% 
  filter(observation_period_end_date>=tar.start.date)
 
## get age and gender -----
#names(person)
person$dob<- paste(person$year_of_birth, 
                   person$month_of_birth, 
                   person$day_of_birth, sep="-") %>% ymd() %>% as.Date()
# age as of 1st March
person<-person %>% 
  mutate(age=floor(as.numeric(difftime(healthy.start.date,
                                        dob,
                                        units="days"))/365.25))
# gender
#8507 male
#8532 female
person$gender<-ifelse(person$gender_concept_id==8507, "Male",
                      ifelse(person$gender_concept_id==8532, "Female", NA ))
#table(person$gender, useNA = "always")

## get charlson comorbidity index------
# based on OHDSI charlson implementation
# https://github.com/OHDSI/FeatureExtraction

condition_occurrence<-tbl(db, sql("SELECT * FROM omop20v2.condition_occurrence")) %>% 
  filter(condition_start_date<={{healthy.start.date}})

Myocardial_infarction.codes<-tbl(db, sql("SELECT * FROM omop20v2.concept_ancestor")) %>% 
  filter(ancestor_concept_id ==4329847) %>% 
  collect()

Congestive_heart_failure.codes<-tbl(db, sql("SELECT * FROM omop20v2.concept_ancestor")) %>% 
  filter(ancestor_concept_id ==316139) %>% 
  collect()

Peripheral_vascular_disease.codes<-tbl(db, sql("SELECT * FROM omop20v2.concept_ancestor")) %>% 
  filter(ancestor_concept_id ==321052) %>% 
  collect()

Cerebrovascular_disease.codes<-tbl(db, sql("SELECT * FROM omop20v2.concept_ancestor")) %>%
  filter(ancestor_concept_id  %in% c(381591,434056) ) %>% 
  collect()

Dementia.codes<-tbl(db, sql("SELECT * FROM omop20v2.concept_ancestor")) %>% 
  filter(ancestor_concept_id ==4182210) %>% 
  collect()

Chronic_pulmonary_disease.codes<-tbl(db, sql("SELECT * FROM omop20v2.concept_ancestor")) %>% 
  filter(ancestor_concept_id ==4063381) %>% 
  collect()

Rheumatologic_disease.codes<-tbl(db, sql("SELECT * FROM omop20v2.concept_ancestor")) %>% 
  filter(ancestor_concept_id   %in% c(257628, 134442, 80800, 80809, 256197, 255348))  %>%
  collect()

Peptic_ulcer_disease.codes<-tbl(db, sql("SELECT * FROM omop20v2.concept_ancestor")) %>% 
  filter(ancestor_concept_id ==4247120) %>% 
  collect()

Mild_liver_disease.codes<-tbl(db, sql("SELECT * FROM omop20v2.concept_ancestor")) %>% 
  filter(ancestor_concept_id   %in% c(4064161, 4212540)) %>% 
  collect()

Diabetes_with_chronic_complications.codes<- tbl(db, sql("SELECT * FROM omop20v2.concept_ancestor")) %>% 
  filter(ancestor_concept_id   %in% c(4192279, 443767, 442793))  %>% 
  collect()

Hemoplegia_or_paralegia.codes<-tbl(db, sql("SELECT * FROM omop20v2.concept_ancestor")) %>% 
  filter(ancestor_concept_id   %in% c(192606, 374022)) %>% 
  collect()

Renal_disease.codes<- tbl(db, sql("SELECT * FROM omop20v2.concept_ancestor")) %>% 
  filter(ancestor_concept_id==4030518) %>% 
  collect()

Any_malignancy.codes<- tbl(db, sql("SELECT * FROM omop20v2.concept_ancestor")) %>% 
  filter(ancestor_concept_id == 443392) %>% 
  collect()


Moderate_to_severe_liver_disease.codes<-tbl(db, sql("SELECT * FROM omop20v2.concept_ancestor")) %>% 
  filter(ancestor_concept_id   %in% 
           c(4245975, 4029488, 192680, 24966)) %>% 
  collect()


Metastatic_solid_tumor.codes<-tbl(db, sql("SELECT * FROM omop20v2.concept_ancestor")) %>% 
  filter(ancestor_concept_id == 432851) %>% 
  collect()

AIDS.codes<- tbl(db, sql("SELECT * FROM omop20v2.concept_ancestor")) %>% 
  filter(ancestor_concept_id   %in% 439727) %>% 
  collect()



# get from condition table-----
# looking at only records on or prior to specified date
condition_occurrence<-tbl(db, sql("SELECT * FROM omop20v2.condition_occurrence")) %>% 
  filter(condition_start_date<={{healthy.start.date}})

Myocardial_infarction<-condition_occurrence %>% 
  filter(condition_concept_id %in% !!Myocardial_infarction.codes$descendant_concept_id) %>% 
  select(person_id) %>% 
  distinct() %>% 
  collect() %>% 
  mutate(weight=1)

Congestive_heart_failure<-condition_occurrence %>% 
  filter(condition_concept_id %in% !!Congestive_heart_failure.codes$descendant_concept_id) %>% 
  select(person_id) %>% 
  distinct() %>% 
  collect() %>% 
  mutate(weight=1)

Peripheral_vascular_disease<-condition_occurrence %>% 
  filter(condition_concept_id %in% !!Peripheral_vascular_disease.codes$descendant_concept_id) %>% 
  select(person_id) %>% 
  distinct() %>% 
  collect() %>% 
  mutate(weight=1)

Cerebrovascular_disease<-condition_occurrence %>% 
  filter(condition_concept_id %in% !!Cerebrovascular_disease.codes$descendant_concept_id) %>% 
  select(person_id) %>% 
  distinct() %>% 
  collect() %>% 
  mutate(weight=1)

Dementia<-condition_occurrence %>% 
  filter(condition_concept_id %in% !!Dementia.codes$descendant_concept_id) %>% 
  select(person_id) %>% 
  distinct() %>% 
  collect() %>% 
  mutate(weight=1)

# memory problems with connection, so get this in stages
Chronic_pulmonary_disease1<-condition_occurrence %>% 
  filter(condition_concept_id %in% !!Chronic_pulmonary_disease.codes$descendant_concept_id[1:50]) %>% 
  select(person_id) %>% 
  distinct() %>% 
  collect() %>% 
  mutate(weight=1)
Chronic_pulmonary_disease2<-condition_occurrence %>% 
  filter(condition_concept_id %in% !!Chronic_pulmonary_disease.codes$descendant_concept_id[51:100]) %>% 
  select(person_id) %>% 
  distinct() %>% 
  collect() %>% 
  mutate(weight=1)
Chronic_pulmonary_disease3<-condition_occurrence %>% 
  filter(condition_concept_id %in% !!Chronic_pulmonary_disease.codes$descendant_concept_id[101:150]) %>% 
  select(person_id) %>% 
  distinct() %>% 
  collect() %>% 
  mutate(weight=1)
Chronic_pulmonary_disease4<-condition_occurrence %>% 
  filter(condition_concept_id %in% !!Chronic_pulmonary_disease.codes$descendant_concept_id[151:203]) %>% 
  select(person_id) %>% 
  distinct() %>% 
  collect() %>% 
  mutate(weight=1)

Chronic_pulmonary_disease<-rbind(Chronic_pulmonary_disease1, Chronic_pulmonary_disease2,
                                 Chronic_pulmonary_disease3, Chronic_pulmonary_disease4) %>% 
  distinct()
rm(Chronic_pulmonary_disease1, Chronic_pulmonary_disease2,
   Chronic_pulmonary_disease3, Chronic_pulmonary_disease4)

Rheumatologic_disease<-condition_occurrence %>% 
  filter(condition_concept_id %in% !!Rheumatologic_disease.codes$descendant_concept_id) %>% 
  select(person_id) %>% 
  distinct() %>% 
  collect() %>% 
  mutate(weight=1)

Peptic_ulcer_disease<-condition_occurrence %>% 
  filter(condition_concept_id %in% !!Peptic_ulcer_disease.codes$descendant_concept_id) %>% 
  select(person_id) %>% 
  distinct() %>% 
  collect() %>% 
  mutate(weight=1)

Mild_liver_disease<-condition_occurrence %>% 
  filter(condition_concept_id %in% !!Mild_liver_disease.codes$descendant_concept_id) %>% 
  select(person_id) %>% 
  distinct() %>% 
  collect() %>% 
  mutate(weight=1)

Diabetes_with_chronic_complications<-condition_occurrence %>% 
  filter(condition_concept_id %in% !!Diabetes_with_chronic_complications.codes$descendant_concept_id) %>% 
  select(person_id) %>% 
  distinct() %>% 
  collect() %>% 
  mutate(weight=2)

Hemoplegia_or_paralegia<-condition_occurrence %>% 
  filter(condition_concept_id %in% !!Hemoplegia_or_paralegia.codes$descendant_concept_id) %>% 
  select(person_id) %>% 
  distinct() %>% 
  collect() %>% 
  mutate(weight=2)

# there were memory problems with connection when trying to get renal disease out in one go
# due to number of codes
# so get this one out in stages and then combine
Renal_disease1<-condition_occurrence %>% 
  filter(condition_concept_id %in% !!Renal_disease.codes$descendant_concept_id[1:50]) %>% 
  select(person_id) %>% 
  distinct() %>% 
  collect() %>% 
  mutate(weight=2)
Renal_disease2<-condition_occurrence %>% 
  filter(condition_concept_id %in% !!Renal_disease.codes$descendant_concept_id[51:100]) %>% 
  select(person_id) %>% 
  distinct() %>% 
  collect() %>% 
  mutate(weight=2)
Renal_disease3<-condition_occurrence %>% 
  filter(condition_concept_id %in% !!Renal_disease.codes$descendant_concept_id[101:50]) %>% 
  select(person_id) %>% 
  distinct() %>% 
  collect() %>% 
  mutate(weight=2)
Renal_disease4<-condition_occurrence %>% 
  filter(condition_concept_id %in% !!Renal_disease.codes$descendant_concept_id[151:233]) %>% 
  select(person_id) %>% 
  distinct() %>% 
  collect() %>% 
  mutate(weight=2)

Renal_disease<-rbind(Renal_disease1, Renal_disease2,
                     Renal_disease3, Renal_disease4) %>% 
  distinct()
rm(Renal_disease1, Renal_disease2,
   Renal_disease3, Renal_disease4)


Any_malignancy<-condition_occurrence %>% 
  filter(condition_concept_id %in% !!Any_malignancy.codes$descendant_concept_id) %>% 
  select(person_id) %>% 
  distinct() %>% 
  collect() %>% 
  mutate(weight=2)

Moderate_to_severe_liver_disease<-condition_occurrence %>% 
  filter(condition_concept_id %in% !!Moderate_to_severe_liver_disease.codes$descendant_concept_id) %>% 
  select(person_id) %>% 
  distinct() %>% 
  collect() %>% 
  mutate(weight=3)

Metastatic_solid_tumor<-condition_occurrence %>% 
  filter(condition_concept_id %in% !!Metastatic_solid_tumor.codes$descendant_concept_id) %>% 
  select(person_id) %>% 
  distinct() %>% 
  collect() %>% 
  mutate(weight=6)

AIDS<-condition_occurrence %>% 
  filter(condition_concept_id %in% !!AIDS.codes$descendant_concept_id) %>% 
  select(person_id) %>% 
  distinct() %>% 
  collect() %>% 
  mutate(weight=6)

rm(
  Myocardial_infarction.codes,
  Congestive_heart_failure.codes,
  Peripheral_vascular_disease.codes,
  Cerebrovascular_disease.codes,
  Dementia.codes,
  Chronic_pulmonary_disease.codes,
  Rheumatologic_disease.codes,
  Peptic_ulcer_disease.codes,
  Mild_liver_disease.codes,
  Diabetes_with_chronic_complications.codes,
  Hemoplegia_or_paralegia.codes,
  Renal_disease.codes,
  Any_malignancy.codes,
  Moderate_to_severe_liver_disease.codes,
  Metastatic_solid_tumor.codes,
  AIDS.codes)

# add components of charlson ------
# nrow(Myocardial_infarction)/length(unique(Myocardial_infarction$person_id))
# nrow(Congestive_heart_failure)/length(unique(Congestive_heart_failure$person_id))
# nrow(Peripheral_vascular_disease)/length(unique(Peripheral_vascular_disease$person_id))
# nrow(Cerebrovascular_disease)/length(unique(Cerebrovascular_disease$person_id))
# nrow(Dementia)/length(unique(Dementia$person_id))
# nrow(Chronic_pulmonary_disease)/length(unique(Chronic_pulmonary_disease$person_id))
# nrow(Rheumatologic_disease)/length(unique(Rheumatologic_disease$person_id))
# nrow(Peptic_ulcer_disease)/length(unique(Peptic_ulcer_disease$person_id))
# nrow(Mild_liver_disease)/length(unique(Mild_liver_disease$person_id))
# nrow(Diabetes_with_chronic_complications)/length(unique(Diabetes_with_chronic_complications$person_id))
# nrow(Hemoplegia_or_paralegia)/length(unique(Hemoplegia_or_paralegia$person_id))
# nrow(Renal_disease)/length(unique(Renal_disease$person_id))
# nrow(Any_malignancy)/length(unique(Any_malignancy$person_id))
# nrow(Moderate_to_severe_liver_disease)/length(unique(Moderate_to_severe_liver_disease$person_id))
# nrow(Metastatic_solid_tumor)/length(unique(Metastatic_solid_tumor$person_id))
# nrow(AIDS)/length(unique(AIDS$person_id))



person <- person %>% 
  left_join(Myocardial_infarction %>% 
            select(person_id) %>% 
            mutate(Myocardial_infarction=1),
          by="person_id")  %>% 
  mutate(Myocardial_infarction=ifelse(is.na(Myocardial_infarction), 
                                      0, Myocardial_infarction))

person <- person %>% 
  left_join(Congestive_heart_failure %>% 
              select(person_id) %>% 
              mutate(Congestive_heart_failure=1),
            by="person_id")  %>% 
  mutate(Congestive_heart_failure=ifelse(is.na(Congestive_heart_failure), 
                                      0, Congestive_heart_failure))
person <- person %>% 
  left_join(Peripheral_vascular_disease %>% 
              select(person_id) %>% 
              mutate(Peripheral_vascular_disease=1),
            by="person_id")  %>% 
  mutate(Peripheral_vascular_disease=ifelse(is.na(Peripheral_vascular_disease), 
                                      0, Peripheral_vascular_disease))
person <- person %>% 
  left_join(Cerebrovascular_disease %>% 
              select(person_id) %>% 
              mutate(Cerebrovascular_disease=1),
            by="person_id")  %>% 
  mutate(Cerebrovascular_disease=ifelse(is.na(Cerebrovascular_disease), 
                                      0, Cerebrovascular_disease))
person <- person %>% 
  left_join(Dementia %>% 
              select(person_id) %>% 
              mutate(Dementia=1),
            by="person_id")  %>% 
  mutate(Dementia=ifelse(is.na(Dementia), 
                                      0, Dementia))
person <- person %>% 
  left_join(Chronic_pulmonary_disease %>% 
              select(person_id) %>% 
              mutate(Chronic_pulmonary_disease=1),
            by="person_id")  %>% 
  mutate(Chronic_pulmonary_disease=ifelse(is.na(Chronic_pulmonary_disease), 
                                      0, Chronic_pulmonary_disease))
person <- person %>% 
  left_join(Rheumatologic_disease %>% 
              select(person_id) %>% 
              mutate(Rheumatologic_disease=1),
            by="person_id")  %>% 
  mutate(Rheumatologic_disease=ifelse(is.na(Rheumatologic_disease), 
                                      0, Rheumatologic_disease))
person <- person %>% 
  left_join(Peptic_ulcer_disease %>% 
              select(person_id) %>% 
              mutate(Peptic_ulcer_disease=1),
            by="person_id")  %>% 
  mutate(Peptic_ulcer_disease=ifelse(is.na(Peptic_ulcer_disease), 
                                      0, Peptic_ulcer_disease))


person <- person %>% 
  left_join(Mild_liver_disease %>% 
              select(person_id) %>% 
              mutate(Mild_liver_disease=1),
            by="person_id")  %>% 
  mutate(Mild_liver_disease=ifelse(is.na(Mild_liver_disease), 
                                      0, Mild_liver_disease))
person <- person %>% 
  left_join(Diabetes_with_chronic_complications %>% 
              select(person_id) %>% 
              mutate(Diabetes_with_chronic_complications=1),
            by="person_id")  %>% 
  mutate(Diabetes_with_chronic_complications=ifelse(is.na(Diabetes_with_chronic_complications), 
                                      0, Diabetes_with_chronic_complications))
person <- person %>% 
  left_join(Hemoplegia_or_paralegia %>% 
              select(person_id) %>% 
              mutate(Hemoplegia_or_paralegia=1),
            by="person_id")  %>% 
  mutate(Hemoplegia_or_paralegia=ifelse(is.na(Hemoplegia_or_paralegia), 
                                      0, Hemoplegia_or_paralegia))
person <- person %>% 
  left_join(Renal_disease %>% 
              select(person_id) %>% 
              mutate(Renal_disease=1),
            by="person_id")  %>% 
  mutate(Renal_disease=ifelse(is.na(Renal_disease), 
                                      0, Renal_disease))
person <- person %>% 
  left_join(Any_malignancy %>% 
              select(person_id) %>% 
              mutate(Any_malignancy=1),
            by="person_id")  %>% 
  mutate(Any_malignancy=ifelse(is.na(Any_malignancy), 
                                      0, Any_malignancy))

person <- person %>% 
  left_join(Moderate_to_severe_liver_disease %>% 
              select(person_id) %>% 
              mutate(Moderate_to_severe_liver_disease=1),
            by="person_id")  %>% 
  mutate(Moderate_to_severe_liver_disease=ifelse(is.na(Moderate_to_severe_liver_disease), 
                                      0, Moderate_to_severe_liver_disease ))

person <- person %>% 
  left_join(Metastatic_solid_tumor %>% 
              select(person_id) %>% 
              mutate(Metastatic_solid_tumor=1),
            by="person_id")  %>% 
  mutate(Metastatic_solid_tumor=ifelse(is.na(Metastatic_solid_tumor), 
                                      0, Metastatic_solid_tumor))

person <- person %>% 
  left_join(AIDS %>% 
              select(person_id) %>% 
              mutate(AIDS=1),
            by="person_id")  %>% 
  mutate(AIDS=ifelse(is.na(AIDS), 
                                      0, AIDS))




# calculate  charlson -----
charlson<-rbind(
  Myocardial_infarction,
  Congestive_heart_failure,
  Peripheral_vascular_disease,
  Cerebrovascular_disease,
  Dementia,
  Chronic_pulmonary_disease,
  Rheumatologic_disease,
  Peptic_ulcer_disease,
  Mild_liver_disease,
  Diabetes_with_chronic_complications,
  Hemoplegia_or_paralegia,
  Renal_disease,
  Any_malignancy,
  Moderate_to_severe_liver_disease,
  Metastatic_solid_tumor,
  AIDS) %>% 
  group_by(person_id) %>% 
  summarise(charlson=sum(weight))

# head(charlson$charlson)
# table(charlson$charlson)

rm(Myocardial_infarction,
  Congestive_heart_failure,
  Peripheral_vascular_disease,
  Cerebrovascular_disease,
  Dementia,
  Chronic_pulmonary_disease,
  Rheumatologic_disease,
  Peptic_ulcer_disease,
  Mild_liver_disease,
  Diabetes_with_chronic_complications,
  Hemoplegia_or_paralegia,
  Renal_disease,
  Any_malignancy,
  Moderate_to_severe_liver_disease,
  Metastatic_solid_tumor,
  AIDS)

# add to person table
nrow(person)/length(unique(person$person_id))
person <-  person %>% 
  left_join(charlson,
            by="person_id")
rm(charlson)

# if missing, zero
person<-person %>% 
  mutate(charlson=ifelse(is.na(charlson),0,charlson))

# categorise charlson ----
table(person$charlson, useNA = "always")
person<-person %>% 
  mutate(charlson=
    ifelse(charlson==0, "0",
    ifelse(charlson==1, "1",
    ifelse(charlson==2, "2",
    ifelse(charlson>=3, "3+", NA )))))
table(person$charlson, useNA = "always")


###### #######
## ATLAS cohorts ------
#339	[MultiStateCovid] COVID-19 diagnosis 2020
#340	[MultiStateCovid] COVID-19 hospitalisation 2020
#303	[MultiStateCovid] Mortality 2020

#344	[MultiStateCovid] COVID-19 positive test 2020

# 331	[MultiStateCovid] Autoimmune condition
# 337	[MultiStateCovid] Chronic kidney disease - single diagnosis
# 333	[MultiStateCovid] COPD
# 334	[MultiStateCovid] Dementia
# 315	[MultiStateCovid] Heart disease
# 316	[MultiStateCovid] Hyperlipidemia
# 312	[MultiStateCovid] Hypertension
# 319	[MultiStateCovid] Malignant neoplasm excluding non-melanoma skin cancer
# 336	[MultiStateCovid] Type 2 Diabetes Mellitus
# 345 [MultiStateCovid] Obesity, latest event before 1st March 2020
# nb start date this obesity cohort is based on the last available date (as we want to limit to last 5 years)

covidmultistatecohorts_db<-tbl(db, sql("SELECT * FROM results20v2.covidmultistatecohorts")) %>% 
  collect()

#table(covidmultistatecohorts_db$cohort_definition_id)
covid.diagnosis<-covidmultistatecohorts_db %>% 
                 filter(cohort_definition_id==339)
covid_hospitalised<-covidmultistatecohorts_db %>% 
  filter(cohort_definition_id==340)
death<-covidmultistatecohorts_db %>% 
  filter(cohort_definition_id==303)

covid.positive<-covidmultistatecohorts_db %>% 
  filter(cohort_definition_id==344)


# comorbidity cohorts- add indicator to person table 
# start with A (from Atlas) to keep these distinct from charlson conditions
person<-person %>% 
  left_join(covidmultistatecohorts_db%>% 
              filter(cohort_definition_id==331) %>% 
              select(subject_id) %>% 
              mutate(a_autoimmune_condition=1) %>% 
              distinct() %>% 
              collect(),
            by=c("person_id"="subject_id"))

person<-person %>% 
  left_join(covidmultistatecohorts_db%>% 
              filter(cohort_definition_id==337) %>% 
              select(subject_id) %>% 
              mutate(a_chronic_kidney_disease=1) %>% 
              distinct() %>% 
              collect(),
            by=c("person_id"="subject_id"))

person<-person %>% 
  left_join(covidmultistatecohorts_db%>% 
              filter(cohort_definition_id==333) %>% 
              select(subject_id) %>% 
              mutate(a_copd=1) %>% 
              distinct() %>% 
              collect(),
            by=c("person_id"="subject_id"))

person<-person %>% 
  left_join(covidmultistatecohorts_db%>% 
              filter(cohort_definition_id==334) %>% 
              select(subject_id) %>% 
              mutate(a_dementia=1) %>% 
              distinct() %>% 
              collect(),
            by=c("person_id"="subject_id"))

person<-person %>% 
  left_join(covidmultistatecohorts_db%>% 
              filter(cohort_definition_id==315) %>% 
              select(subject_id) %>% 
              mutate(a_heart_disease=1) %>% 
              distinct() %>% 
              collect(),
            by=c("person_id"="subject_id"))


person<-person %>% 
  left_join(covidmultistatecohorts_db%>% 
              filter(cohort_definition_id==316) %>% 
              select(subject_id) %>% 
              mutate(a_hyperlipidemia=1) %>% 
              distinct() %>% 
              collect(),
            by=c("person_id"="subject_id"))


person<-person %>% 
  left_join(covidmultistatecohorts_db%>% 
              filter(cohort_definition_id==312) %>% 
              select(subject_id) %>% 
              mutate(a_hypertension=1) %>% 
              distinct() %>% 
              collect(),
            by=c("person_id"="subject_id"))

person<-person %>% 
  left_join(covidmultistatecohorts_db%>% 
              filter(cohort_definition_id==319) %>% 
              select(subject_id) %>% 
              mutate(a_malignant_neoplasm=1) %>% 
              distinct() %>% 
              collect(),
            by=c("person_id"="subject_id"))

person<-person %>% 
  left_join(covidmultistatecohorts_db%>% 
              filter(cohort_definition_id==336) %>% 
              select(subject_id) %>% 
              mutate(a_t2_diabetes=1) %>% 
              distinct() %>% 
              collect(),
            by=c("person_id"="subject_id"))



# for obesity, only for last 5 years
obesity<-covidmultistatecohorts_db%>% 
  filter(cohort_definition_id==345)

obesity$days<- as.numeric(difftime(obesity$cohort_start_date,
                               healthy.start.date,
                               units="days") )
#hist(obesity$days/365.25)
obesity_5y<- obesity %>% 
  filter(days>-(365.25*5))

person<-person %>% 
  left_join(obesity_5y %>% 
              select(subject_id) %>%
              mutate(obesity.5y=1),
            by=c("person_id"="subject_id"))

rm(obesity_5y)
rm(covidmultistatecohorts_db)



# if missing a value (ie not included in cohort), zero
person<-person %>% 
  mutate(a_autoimmune_condition=ifelse(is.na(a_autoimmune_condition),0,a_autoimmune_condition)) %>% 
  mutate(a_chronic_kidney_disease=ifelse(is.na(a_chronic_kidney_disease),0,a_chronic_kidney_disease)) %>% 
  mutate(a_copd=ifelse(is.na(a_copd),0,a_copd)) %>% 
  mutate(a_dementia=ifelse(is.na(a_dementia),0,a_dementia)) %>% 
  mutate(a_heart_disease=ifelse(is.na(a_heart_disease),0,a_heart_disease)) %>% 
  mutate(a_hyperlipidemia=ifelse(is.na(a_hyperlipidemia),0,a_hyperlipidemia))%>% 
  mutate(a_hypertension=ifelse(is.na(a_hypertension),0,a_hypertension))%>% 
  mutate(a_malignant_neoplasm=ifelse(is.na(a_malignant_neoplasm),0,a_malignant_neoplasm))%>% 
  mutate(a_t2_diabetes=ifelse(is.na(a_t2_diabetes),0,a_t2_diabetes))%>% 
  mutate(a_obesity.5y=ifelse(is.na(obesity.5y),0,obesity.5y))

person<-person %>%
  select(-c(obesity.5y)) 

# each cohort individuals should be unique ----
nrow(person)/length(person$person_id)
nrow(covid.diagnosis)/length(covid.diagnosis$subject_id)
nrow(covid_hospitalised)/length(covid_hospitalised$subject_id)

# for healthy pop, exclude if less than a year of prior history -----
sum(person$observation_period_start_date>tar.start.date-years(1)) #169577
person<-person %>% 
  filter(observation_period_start_date<=tar.start.date-years(1))

# for healthy pop, exclude if prior postitive test for covid ----
# head(covid.positive %>% 
#   arrange(cohort_start_date)) # <5 with a prior positive test

covid.positive.before_start<-covid.positive %>% 
  filter(cohort_start_date<tar.start.date)

person<-person %>% 
 anti_join(covid.positive.before_start %>% select(subject_id),
           by=c("person_id"="subject_id"))


# for healthy pop, exclude if prior diagnosis of covid -----
diag.before.march<-covid.diagnosis %>% 
  filter(cohort_start_date<tar.start.date) 
nrow(diag.before.march)#312

person<-person %>% 
  anti_join(diag.before.march,
            by=c("person_id"="subject_id"))

# for healthy pop, exclude if prior hospitalisation with covid -----
hosp.before.march<-covid_hospitalised %>% 
  filter(cohort_start_date<tar.start.date) 
nrow(hosp.before.march)#19

person<-person %>% 
  anti_join(hosp.before.march,
            by=c("person_id"="subject_id"))


# for healthy pop, exclude if in hospital on 1st March -----

any.hospital<-tbl(db, sql("SELECT * FROM omop20v2.visit_occurrence")) %>% 
  filter(visit_concept_id==262) %>% 
  collect()

any.hospital<-any.hospital %>% 
  filter(visit_end_date>= tar.start.date) %>% # end date from march 1st
  filter(visit_start_date<= tar.start.date)%>%  # and start  date from march 1st
  select(person_id)%>% 
  distinct() 

nrow(any.hospital)#1306

person<-person %>% 
  anti_join(any.hospital %>% select("person_id"),
            by="person_id")
rm(any.hospital)
# for covid diagnosis, remove diagnosis if it came after hospitalisation  -----
# given the multistate model structure, we're looking for diagnoses before hospitalisation
diag.after.hosp<-covid.diagnosis %>% 
     inner_join(covid_hospitalised,
                by="subject_id",
                suffix = c(".diag", ".hosp")) # those in hospitalised

diag.after.hosp<-diag.after.hosp %>% 
  mutate(days=as.numeric(difftime(cohort_start_date.diag,
                                     cohort_start_date.hosp, units="days")))

sum(diag.after.hosp$days>=0) #7600

diag.after.hosp<-diag.after.hosp %>% 
   filter(days>=0) %>%  # diag after hospitalisation
   select(subject_id)

covid.diagnosis<-covid.diagnosis %>% 
  anti_join(diag.after.hosp,
            by="subject_id")
rm(diag.after.hosp)


##### 
# covid.data -----
# our row per person dataframe 
covid.diagnosis<-covid.diagnosis %>% 
  select("subject_id",  "cohort_start_date")
names(covid.diagnosis)<-c("person_id", 
                          "covid_diagnosis_date")

# combine person and diagnosis
covid.data<-person %>% 
  left_join(covid.diagnosis,
            by="person_id")
rm(person)

#combine with hospitalised
covid_hospitalised<-covid_hospitalised %>% 
  select("subject_id",  "cohort_start_date")
names(covid_hospitalised)<-c("person_id", 
                             "covid_hospitalised_date")
covid.data<- covid.data %>% 
  left_join(covid_hospitalised,
            by="person_id")
rm(covid_hospitalised)


# add deaths -----
# only those in covid.data

# before start date
death.before.start<-death %>% 
  filter(cohort_start_date<={{healthy.start.date}}) %>% 
  inner_join(covid.data ,
  by=c("subject_id"="person_id"))
nrow(death.before.start) #15 

covid.data<-covid.data %>% 
  anti_join(death.before.start %>% select(subject_id),
            by=c("person_id"="subject_id"))

# any deaths after tar end?
sum(death$cohort_start_date>{{tar.end.date}}) #0


covid.data<-covid.data %>% 
  left_join(death %>% 
              select(subject_id,cohort_start_date) %>% 
              rename(death_date=cohort_start_date),
            by=c("person_id"="subject_id"))
  

# check that there were no events after 5th May -----
sum(covid.data$covid_diagnosis_date>tar.end.date, na.rm = T) #0
sum(covid.data$covid_hospitalised_date>tar.end.date, na.rm = T) #0
sum(covid.data$death_date>tar.end.date, na.rm = T) #0
# all looks ok

# add indicator variable  #####
covid.data$covid_diagnosis_status<-ifelse(!is.na(covid.data$covid_diagnosis_date),
                                          1,0)
covid.data$covid_hospitalised_status<-ifelse(!is.na(covid.data$covid_hospitalised_date),
                                             1,0)

covid.data$death_status<-ifelse(is.na(covid.data$death_date),
                                0,1)


# censoring -----

# if event, date of event
# if no event,  censor at tar.end.date or end of observation period, whichever comes first
covid.data<-covid.data %>%
  mutate(covid_diagnosis_date=if_else(covid_diagnosis_status==1,
                                      covid_diagnosis_date, 
                               if_else(observation_period_end_date < {{tar.end.date}},
                                       observation_period_end_date, {{tar.end.date}})))

covid.data<-covid.data %>%
  mutate(covid_hospitalised_date=if_else(covid_hospitalised_status==1,
                                      covid_hospitalised_date, 
                                      if_else(observation_period_end_date < {{tar.end.date}},
                                              observation_period_end_date, {{tar.end.date}})))
covid.data<-covid.data %>%
  mutate(death_date=if_else(death_status==1,
                            death_date, 
                                         if_else(observation_period_end_date < {{tar.end.date}},
                                                 observation_period_end_date, {{tar.end.date}})))


# time to event -----

# time from 
covid.data<-covid.data %>% 
  mutate(covid_diagnosis_time= as.numeric(difftime(covid_diagnosis_date,
                                                   {{healthy.start.date}}, 
                                                   units="days")))
covid.data<-covid.data %>% 
  mutate(covid_hospitalised_time= as.numeric(difftime(covid_hospitalised_date,
                                                      {{healthy.start.date}}, 
                                                   units="days")))
covid.data<-covid.data %>% 
  mutate(death_time= as.numeric(difftime(death_date,
                                         {{healthy.start.date}}, 
                                                      units="days")))
# quantile(covid.data$covid_diagnosis_time)
# quantile(covid.data$covid_hospitalised_time)
# quantile(covid.data$death_time)

# do we have transitions on the same day  -------
# eg someone getting diagnosed and dying on the same day

nrow(covid.data %>% 
  filter(covid_diagnosis_status==1) %>% 
  filter(covid_hospitalised_status==1) %>% 
  filter(covid_diagnosis_time==covid_hospitalised_time))

nrow(covid.data %>% 
       filter(covid_diagnosis_status==1) %>% 
       filter(death_status==1) %>% 
       filter(covid_diagnosis_time==death_time))  # 44 deaths on same day as diagnosis

nrow(covid.data %>% 
       filter(covid_hospitalised_status==1) %>% 
       filter(death_status==1) %>% 
       filter(covid_hospitalised_time==death_time)) # 1 death on same date as hospitalised

# deaths on the same day as diagnosis: drop diagnosis  -------
# where diagnoses are seen on the same day as a diagnosis, we can not be sure about the meaning of this diagnosis
# (it does not necessarily mean someone was diagnosed in the morning and died in the afternoon, but could also be 
# directly tied to the reporting of death)
# so we will drop these diagnoses

diag.death<-covid.data %>% 
  filter(covid_diagnosis_status==1) %>% 
  filter(death_status==1) %>% 
  filter(covid_diagnosis_time==death_time)

diag.death$covid_diagnosis_status<-0

covid.data<-covid.data %>% 
  anti_join(diag.death %>% 
              select(person_id),
              by="person_id")
covid.data<-rbind(covid.data, diag.death)

nrow(covid.data %>% 
       filter(covid_diagnosis_status==1) %>% 
       filter(death_status==1) %>% 
       filter(covid_diagnosis_time==death_time))  



# death on  same day as hospitalisation: add 0.5 days to death -------
# unlike for diagnosees above, we can be more sure about the timeline where we
# see a hospitalisation and a death on the same 
# i.e. we can be more sure they went to hospital first, and then died
# so, for these cases add 0.5 days to mortality (so that mstate can deal with both in order)

hosp.death<-covid.data %>% 
  filter(covid_hospitalised_status==1) %>% 
  filter(death_status==1) %>% 
  filter(covid_hospitalised_time==death_time)

hosp.death$death_time<-hosp.death$death_time+0.5

covid.data<-covid.data %>% 
  anti_join(hosp.death %>% 
              select(person_id),
            by="person_id")
covid.data<-rbind(covid.data, hosp.death)

nrow(covid.data %>% 
       filter(covid_hospitalised_status==1) %>% 
       filter(death_status==1) %>% 
       filter(covid_hospitalised_time==death_time)) 




# tidy up workspace ----- 

rm(list= ls()[!(ls() %in% c('covid.data'))])

## set up for cause specific survival models------ 

tmat <- matrix(NA, 4, 4)
dimnames(tmat) <- list(from = c("healthy","diagnosed","hospitalised","death"), 
                       to = c("healthy","diagnosed","hospitalised","death"))
tmat[1, 2:4]<- 1:3
tmat[2, 3:4]<- 4:5
tmat[3, 4]<- 6

r<-msprep(time = c(NA, 
                   "covid_diagnosis_time",
                   "covid_hospitalised_time", 
                   "death_time"), 
          status = c(NA, 
                     "covid_diagnosis_status",
                     "covid_hospitalised_status",
                     "death_status"), 
          id="person_id",
          data = as.data.frame(covid.data),  
          trans = tmat)
events(r)

# add age, age group, gender, charlson, etc ------
covid.data<-covid.data %>% 
  mutate(age_gr=ifelse(age<18, 
                       "Under 18",
                        ifelse(age>=18 &
                         age<=39, 
                       "18 to 39",
                ifelse(age>=40 &
                       age<=59, 
                       "40 to 59",
               ifelse(age>=60 &
                      age<=69, 
                      "60 to 69",
              ifelse(age>=70 &
                     age<=79, 
                     "70 to 79",      
              ifelse(age>=80, 
                     "80 or older",  
                      NA)))))))
table(covid.data$age_gr, useNA = "always")

r<-as.data.frame(r) %>% 
  left_join(covid.data,
            by="person_id")





# as factors----
r$age_gr <- factor(r$age_gr, 
                   levels = c("Under 18","18 to 39", "40 to 59", "60 to 69",
                                        "70 to 79", "80 or older"))
r$gender <- factor(r$gender, 
                   levels = c("Male", "Female"))

r$charlson <- factor(r$charlson, 
                   levels = c("0", "1", "2", "3+"))

r$medea <- factor(r$medea, 
                     levels = c("R", "U", "U1", "U2", "U3", "U4", "U5"))


# split r by transition ------
r.healthy.diagnosis<-r %>%
  filter(trans==1) %>% # transition from healthy to diagnosis
  select(-from, -to, -trans, -Tstart, -Tstop)
r.healthy.hospitalised<-r %>%
  filter(trans==2) %>% # transition from healthy to hospitalised
  select(-from, -to, -trans, -Tstart, -Tstop)
r.healthy.death<-r %>%
  filter(trans==3) %>% # transition from healthy to death
  select(-from, -to, -trans, -Tstart, -Tstop)

r.diagnosis.hospitalised<-r %>%
  filter(trans==4) %>% # traonsition from diagnosis to hospitalised
  select(-from, -to, -trans, -Tstart, -Tstop)
r.diagnosis.death<-r %>%
  filter(trans==5) %>% # transition from diagnosis to death
  select(-from, -to, -trans, -Tstart, -Tstop)

r.hospitalised.death<-r %>%
  filter(trans==6) %>% # transition from hospitalised to death
  select(-from, -to, -trans, -Tstart, -Tstop)


# add variables for estimating cumulative incidence-----

# from healthy
healthy_c.event<-rbind(
  r.healthy.diagnosis %>% 
    filter(status==1) %>% 
    select(person_id, time) %>% 
    mutate(healthy_c.event=1),
  r.healthy.hospitalised %>% 
    filter(status==1) %>% 
    select(person_id,time) %>% 
    mutate(healthy_c.event=2),
  r.healthy.death %>% 
    filter(status==1) %>% 
    select(person_id,time) %>% 
    mutate(healthy_c.event=3)) %>% 
  rename(healthy_c.time=time)
#nrow(healthy_c.event)/length(unique(healthy_c.event$person_id))
# add those without event 
healthy_c.event<-rbind(healthy_c.event,
                       r.healthy.diagnosis %>% 
                         anti_join(healthy_c.event, by="person_id") %>% 
                         select(person_id, time) %>% 
                         mutate(healthy_c.event=0) %>% 
                         rename(healthy_c.time=time))
#nrow(healthy_c.event)/length(unique(healthy_c.event$person_id))

covid.data <-
  covid.data %>% 
  left_join(healthy_c.event, by="person_id")


# from diagnosis
diagnosis_c.event<-rbind(
  r.diagnosis.hospitalised %>% 
    filter(status==1) %>% 
    select(person_id, time) %>% 
    mutate(diagnosis_c.event=1),
  r.diagnosis.death %>% 
    filter(status==1) %>% 
    select(person_id,time) %>% 
    mutate(diagnosis_c.event=2)) %>% 
  rename(diagnosis_c.time=time)
#nrow(diagnosis_c.event)/length(unique(diagnosis_c.event$person_id))
# add those without event 
diagnosis_c.event<-rbind(diagnosis_c.event,
                         r.diagnosis.hospitalised %>% 
                         anti_join(diagnosis_c.event, by="person_id") %>% 
                         select(person_id, time) %>% 
                         mutate(diagnosis_c.event=0) %>% 
                         rename(diagnosis_c.time=time))
#nrow(diagnosis_c.event)/length(unique(diagnosis_c.event$person_id))

covid.data <-
  covid.data %>% 
  left_join(diagnosis_c.event, by="person_id")






# exclude those with zero days time at risk from specific transitions-----

# we don't have anyone with zero days time at risk for the first set
# of transitions by design 
# everyone has tar from starting state
sum(r.healthy.diagnosis$time==0)
sum(r.healthy.hospitalised$time==0)
sum(r.healthy.death$time==0)

# but we can have people with no time at risk for the later transitions
# i.e. someone who got a diagnosis on the last day of follow up
# so, for our cause-specific survival data we use for subsequent modelling
# we'll drop that individual from that specific dataframe
# (but, note, we're still capturing and reporting their diagnosis, as per the example above)
sum(r.diagnosis.hospitalised$time==0)
r.diagnosis.hospitalised<-r.diagnosis.hospitalised %>% 
  filter(time>0)

sum(r.diagnosis.death$time==0)
r.diagnosis.death<-r.diagnosis.death %>% 
  filter(time>0)

sum(r.hospitalised.death$time==0)
r.hospitalised.death<-r.hospitalised.death %>% 
  filter(time>0)

# save -----
save(list=c("covid.data",
            "r",
            "r.diagnosis.death",
            "r.diagnosis.hospitalised",
            "r.healthy.death"  ,
            "r.healthy.diagnosis",
            "r.healthy.hospitalised",
            "r.hospitalised.death" ),
     file = "#########")
