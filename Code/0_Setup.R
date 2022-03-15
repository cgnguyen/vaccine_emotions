#Setup---------------------------------
library(tidyverse)
library(haven)
library(labelled)

#Data Cleaning -----------------------
D <- read_sav("./data/data_raw.sav")

##Recode Conditions -------------------
D <-
  D %>%
  dplyr::mutate(
    condition = as.factor(c_0102),
    anger = as.factor(case_when(
      condition == 1 ~ "Yes",
      condition == 4 ~ "Yes",
      TRUE ~ "No"
    )),
    anxiety = as.factor(case_when(
      condition == 2 ~ "Yes",
      condition == 5 ~ "Yes",
      TRUE ~ "No"
    )),
    corona = as.factor(
      case_when(
        condition == 4 ~ "Yes",
        condition == 5 ~ "Yes",
        condition == 6 ~ "Yes",
        TRUE ~ "No"
      )
    ),
    emotion = as.factor(
      case_when(
        anger == "Yes" ~ "Anger",
        anxiety == "Yes" ~ "Anxiety",
        TRUE ~ "Control"
      )
    )
  )

D$condition <- relevel(D$condition, ref = "3")
D$emotion <- relevel(D$emotion, ref = "Control")



## Vaccine Preferences ------------------------
#Rescale to -2 to +2
D <-
  D %>%
  mutate(
    vac_pre = (car::recode(v_42, "98:99=NA")) * -1 + 3,
    vac_post = car::recode(v_59, "98:99=NA") * -1 + 3,
    vac_absolute = abs(car::recode(v_59, "98:99=NA") - 3)
  ) %>%
  mutate(vac_factor = as_factor(v_59)) %>%
  mutate(vac_factor = car::recode(vac_factor, "
                                    'Weiß nicht'= NA;
                                    'Keine Angabe'= NA"))
D$vac_factor <- factor(
  D$vac_factor,
  levels = c(
    "Stimme überhaupt nicht zu",
    "Stimme eher nicht zu",
    "Teils/teils",
    "Stimme eher zu",
    "Stimme voll und ganz zu"
  ),
  ordered = T
)

## Affective Corona ----------------

D <-
  D %>%
  dplyr::mutate(across(c("v_49", "v_52", "v_51", "v_54"),
                       ~ as.numeric(car::recode(., "98:99=NA")))) %>%
  unite(ap_corona_1, c("v_49", "v_52"), na.rm = T) %>%
  unite(ap_corona_2, c("v_51", "v_54"), na.rm = T) %>%
  dplyr::mutate(across(starts_with("ap_corona"),
                       ~ as.numeric(.))) %>%
  mutate(ap_corona = rowMeans(dplyr::select(., dplyr::starts_with("ap_corona")), na.rm = TRUE))

##Immunization Status---------------
D <-
  D %>%
  mutate(
    vac_status = car::recode(v_69, "98:99=NA"),
    vac_reason = car::recode(v_70, "98:99=0")
  ) %>%
  mutate(vac_binary = case_when(vac_status == 1 ~ 1,
                                vac_reason == 4 ~ 0))

##Socdem variables -----------------
D <-
  D %>%
  mutate(
    gender = as_factor(D$v_28),
    age = as_factor(D$age),
    edu = as_factor(v_30),
    activity = as_factor(v_90)
  )


#Recode /translation
D$age <- D$age %>%
  fct_recode(
    "18-29" = "18-29 Jahre",
    "30-39" = "30-39 Jahre",
    "40-49" = "40-49 Jahre",
    "50-59" = "50-59 Jahre",
    "60-69" = "60-69 Jahre"
  )

D$edu <- D$edu %>%
  fct_recode("Low" = "niedrig",
             "Middle" = "mittel",
             "High" = "hoch")

## Recoding D$activity into D$activity_rec
D$activity <- D$activity %>%
  fct_recode(
    "Full-Time" = "Vollzeiterwerbstätig",
    "Part-Time" = "Teilzeiterwerbstätig",
    "Part-Time" = "Altersteilzeit, unabhängig davon, ob in der Arbeits- oder Freistellungsphase befindlich",
    "Part-Time" = "Geringfügig erwerbstätig, 450-Euro-Job, Minijob",
    "Part-Time" = "^Ein-Euro-Job^, bei Bezug von Arbeitslosengeld II",
    "Part-Time" = "Gelegentlich oder unregelmäßig erwerbstätig",
    "Education" = "In einer beruflichen Ausbildung/Lehre oder in Umschulung",
    "Other" = "Freiwilliger Wehrdienst",
    "Other" = "Bundesfreiwilligendienst oder Freiwilliges Soziales Jahr  oder  Freiwilliges Ökologisches Jahr",
    "Other" = "Mutterschafts-, Erziehungsurlaub, Elternzeit oder sonstige Beurlaubung",
    "Not Working" = "Nicht erwerbstätig, (einschließlich: Schüler/-innen oder Studierende, die nicht gegen Geld arbeiten, Arbeitslose, Vorruheständler/-innen, Rentner/-innen ohne Nebenverdienst)"
  )


## Recoding D$gender
D$gender <- D$gender %>%
  fct_recode("Male" = "männlich",
             "Female" = "weiblich")



##Left-Right Self Placement -------------
D <-
  D %>%
  mutate(
    lr = car::recode(v_43 , "98:99=NA"))



##Data Exclusion and Noncompliance ----------------
#Anyone who did not write anything + interrupted the analysis
D_full<-D

D <-
  D %>%
  mutate(wordcount = nchar(v_44)) %>%
  filter(wordcount > 1) %>%
  filter(duration > 0)

D_full <-
  D_full %>%
  mutate(wordcount = nchar(v_44)) %>%
  mutate(exclude = as.factor(
    case_when(wordcount >1 & duration > 0 ~ "Included",
              TRUE  ~ "Excluded")))
D_full$exclude<-relevel(D_full$exclude,ref="Included")
                           

#Write Data for analysis / shared data 
D_simple<- 
  D%>%
  dplyr::select(condition,vac_pre,vac_post,ap_corona,age,gender,lr,activity,edu,vac_binary,wordcount,ap_corona_1,ap_corona_2,v_44)

write_rds(D_simple, file="./data/data_shared.rds")
