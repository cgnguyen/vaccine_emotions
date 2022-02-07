#Notes--------------------------------------------------------
#Requires main analysis script first 
  library(gtsummary)
  library(flextable)
  library(stargazer)
  library(modelsummary)
  
  


# Summary Statistics of Sample  --------------------------------------
## Socdem -----------------------------------------------------------

  D %>%
    select(age,gender,activity,edu,lr)%>%
    tbl_summary()%>%
    as_flex_table()%>%
    width(width=2)%>%
    save_as_docx(path="./Tables/socdem.docx")

##Political / Attitude Variables 
  D%>%
    mutate(vac_binary=as.factor(vac_binary))

  
  
#Randomization Check -------------------------
  library(nnet)
  
  mod_randomization<-multinom(condition~  age+gender+activity+edu+as.factor(vac_binary)+lr, data=D)
  


  modelsummary(mod_randomization, group = term ~ model + y.level , 
                estimate  = c("{estimate}{stars}"),
               output="flextable" ) %>%
  width(width=1)%>%
  save_as_docx(path="./Tables/randomization.docx")
  
  
  
#Exclusion Pattern Check --------------------
  
  mod_exclusion<-glm(exclude ~ age+gender+activity+as.factor(vac_binary)+lr+vac_post+condition, data=D_full,
                     family = binomial(link = "logit"))
                     
  summary(mod_exclusion)
  
  
  modelsummary(mod_exclusion,
                estimate  = c("{estimate} ({std.error}){stars}"),
               statistic=  NULL, 
               output="flextable" ) %>%
  width(width=2)%>%
  save_as_docx(path="./Tables/exclusion.docx")


#Check Pre-Test emotion manipulation  --------------------------------------------------
 D_pre<-read_sav("./../Data/wave_3.sav")
 
 
 
 ##Data Exclusion and Noncompliance ----------------
 select_vec <-
   look_for(D_pre, c("W1", "W2", "W3", "W4", "W5", "W6", "W7", "W8"))$variable
 
 D_pre <-
   D_pre %>%
   dplyr::mutate(across(select_vec,
                        ~ car::recode(., "-66=NA"))) %>%
   unite(text, select_vec, na.rm = T)
 
 #Anyone who did not write enough + interrupted the analysis
 
 
 D_pre <- 
    D_pre %>%
    mutate(wordcount = nchar(text)) %>%
    filter(wordcount > 25) %>%
    filter(duration > 0)
 

##Conditions --------------------------------------------------------------
 D_pre <-
   D_pre %>%
   dplyr::mutate(
     condition = as.factor(c_0066),
     anger = as.factor(case_when(
       condition == 1 ~ "Yes",
       condition == 5 ~ "Yes",
       TRUE ~ "No"
     )),
     anxiety = as.factor(case_when(
       condition == 2 ~ "Yes",
       condition == 6 ~ "Yes",
       TRUE ~ "No"
     )),
     disgust = as.factor(case_when(
       condition == 3 ~ "Yes",
       condition == 7 ~ "Yes",
       TRUE ~ "No"
     )),
     corona = as.factor(
       case_when(
         condition == 5 ~ "Yes",
         condition == 6 ~ "Yes",
         condition == 7 ~ "Yes",
         condition == 8 ~ "Yes",
         TRUE ~ "No"
       )
     ),
     emotion = as.factor(
       case_when(
         anger == "Yes" ~ "Anger",
         anxiety == "Yes" ~ "Anxiety",
         disgust == "Yes" ~ "Disgust",
         TRUE ~ "Control"
       )
     )
   )
 
 ## Recoding D_pre$condition
 D_pre$condition <- fct_recode(
   D_pre$condition,
   "Anger-General" = "1",
   "Anxiety-General" = "2",
   "Disgust-General" = "3",
   "Control" = "4",
   "Anger-Corona" = "5",
   "Anxiety-Corona" = "6",
   "Disgust-Corona" = "7",
   "Corona" = "8"
 )
 
 D_pre$condition <- relevel(D_pre$condition, ref = "Control")
  
 
 
 ## Manipulation Checks -------------------------------------------------------
 D_pre <-
   D_pre %>%
   dplyr::mutate(
     emo_anger_1 = car::recode(v_581, "98=NA;99=NA"),
     emo_anger_2 = car::recode(v_601, "98=NA;99=NA"),
     emo_anger = case_when(is.na(emo_anger_1) ~ (emo_anger_2),
                           TRUE ~ emo_anger_1)
   )
 
 D_pre <-
   D_pre %>%
   dplyr::mutate(
     emo_anxiety_1 = car::recode(v_582, "98=NA;99=NA"),
     emo_anxiety_2 = car::recode(v_602, "98=NA;99=NA"),
     emo_anxiety = case_when(is.na(emo_anxiety_1) ~ (emo_anxiety_2),
                             TRUE ~ emo_anxiety_1)
   )
 
 D_pre <-
   D_pre %>%
   dplyr::mutate(
     emo_happy_1 = car::recode(v_583, "98=NA;99=NA"),
     emo_happy_2 = car::recode(v_603, "98=NA;99=NA"),
     emo_happy = case_when(is.na(emo_happy_1) ~ (emo_happy_2),
                           TRUE ~ emo_happy_1)
   )
 
 D_pre <-
   D_pre %>%
   dplyr::mutate(
     emo_sad_1 = car::recode(v_584, "98=NA;99=NA"),
     emo_sad_2 = car::recode(v_604, "98=NA;99=NA"),
     emo_sad = case_when(is.na(emo_sad_1) ~ (emo_sad_2),
                         TRUE ~ emo_sad_1)
   )
 
 D_pre <-
   D_pre %>%
   dplyr::mutate(
     emo_disgust_1 = car::recode(v_585, "98=NA;99=NA"),
     emo_disgust_2 = car::recode(v_605, "98=NA;99=NA"),
     emo_disgust = case_when(is.na(emo_disgust_1) ~ (emo_disgust_2),
                             TRUE ~ emo_disgust_1)
   )
 
 D_pre <-
   D_pre %>%
   dplyr::mutate(
     emo_guilty_1 = car::recode(v_586, "98=NA;99=NA"),
     emo_guilty_2 = car::recode(v_606, "98=NA;99=NA"),
     emo_guilty = case_when(is.na(emo_guilty_1) ~ (emo_guilty_2),
                            TRUE ~ emo_guilty_1)
   )
 
 D_pre <-
   D_pre %>%
   dplyr::mutate(
     emo_powerless_1 = car::recode(v_587, "98=NA;99=NA"),
     emo_powerless_2 = car::recode(v_607, "98=NA;99=NA"),
     emo_powerless = case_when(
       is.na(emo_powerless_1) ~ (emo_powerless_2),
       TRUE ~ emo_powerless_1
     )
   )  

 D_pre
 
##Model -------------------------------------
 mod_anger<-
   D_pre%>%
   filter(emotion!="Disgust")%>% 
   lm(emo_anger ~ anger*corona+anxiety*corona, data = .) 
 
  summary(mod_anger)
  
  htmlreg(mod_anger,
          file="./Tables/a_manipulation.html",
          custom.coef.names = c("Intercept","Anger","Corona","Anxiety",
                                "Anger * Corona","Anxiety * Corona"),
          single.row = T)
  
 

 

  D_pre %>%
   filter(emotion != "Disgust") %>%
   lm(emo_powerless ~ anger*corona+anxiety*corona, data = .) %>%
   summary(.)
  
  
  
  

  
 