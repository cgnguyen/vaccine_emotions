#Notes--------------------------------------------------------
#Requires main analysis script first 
  




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
  
  
  
  
#Analysis of words in answer scheme -------------------------------------------
  D_corpus<-
    D%>%
      dplyr::select(v_44)%>%
      as.data.frame(with.meta = TRUE) %>%
      corpus(text_field="v_44", unique_docnames = F) %>%
      tokens(remove_punct = TRUE) %>%
      tokens_tolower() %>%
      tokens_remove(pattern = quanteda::stopwords("de", source = "marimo")) 
  
  
  
  D$condition_fac <- D$condition %>%
  fct_recode(
    "Control" = "3",
    "Anger- General" = "1",
    "Anxiety - General" = "2",
    "Anger - Corona" = "4",
    "Anxiety - Corona" = "5",
    "Corona" = "6"
  )
  
  D_corpus$condition<-D$condition_fac
      
  #Turn to document feature matrix
  
  D_dfm<-  
    D_corpus%>%
    dfm()%>%
    dfm_group(condition)
  
  
  #Most frequent words
  table_freq<-
    D_dfm %>% 
    textstat_frequency(n = 15, group = condition)
  

 fig_frequency<-
   table_freq%>%
    ggplot()+
    aes(x=reorder(feature,frequency),y=frequency)+
      geom_col()+
      facet_wrap(.~group,ncol = 3, scales = "free")+
      coord_flip()+
      theme_minimal()+
      xlab("Word")+ylab("Frequency")
 
 ggsave(plot = fig_frequency, filename = "./Figures/fig_a1.svg")
  ggsave(
  plot = fig_frequency,
  filename = "./Figures/fig_a1.png",
  width = 8,
  height = 4
)

  
  
  #Check frequency listing for key words in each condition: "Corona" , "Pandemie", "Mask"
  table_freq_full<-
    D_dfm %>% 
    textstat_frequency(group = condition)
  
  table_freq_full%>%
    filter(feature %in% c("corona","maske","pandemie"))%>%
    arrange(group,feature)
 
  
##Sentiment analysis of Texts for anger/wut. Based on Tobias Widemann's work. 
  #Unpublished, so dictionary cannot be made available but should be freely made available by contacting him
  
  
#### ed8 application #################################################################

### This script applies the ed8 dictionary to a data frame with a text column
### The code in this script is based on Christian Rauh's "augmented dictionary"
### Please always cite Rauh's workbench paper when using the ed8 dictionary
### Rauh, C. (2018). Validating a sentiment dictionary for German political language. A workbench note. Journal of Information Technology & Politics, 0(0), 1–25. https://doi.org/10.1080/19331681. 2018.1485608

# Load strinR package
library(stringr)

# Set the working directory to the folder where the ed8_files folder is located

  data<-
    D%>%
      dplyr::select(v_44)
 

 data$text<-as.character(data$v_44)
  

# Preprocessing
data$sent.text <- tolower(data$text) # Everything to lower case
data$sent.text <- str_trim(data$sent.text, side = "both") #get rid of white space on both ends
data$sent.text <- gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", data$sent.text, fixed = FALSE) # Remove URL Links
data$sent.text <- paste(" ", data$sent.text, " ", sep="") # Add white space in the beginning and end
data$sent.text <- gsub("[[:punct:]]", "", data$sent.text, fixed = FALSE) # Remove punctuation

data$sent.text <- gsub("  ", " ", data$sent.text, fixed = TRUE) # Remove doubled whitespaces

### Count length of text ###

data$terms <- str_count(data$sent.text, " ") - 1  # Number of terms based on number of spaces (preceding whitespace subtracted)

# Removing stopwords from length count
# Using the German StopWOrd list supplied with the Snowball Stemmer
stopwords <- as.data.frame(readLines("./DATA/SnowballStopwordsGerman.txt"))
names(stopwords) <- "term"
stopwords$term <- sub("\\|.*", "", stopwords$term, fixed = FALSE) # Remove comments from file
stopwords <- stopwords[grepl("[a-z]", stopwords$term), ] # keep only lines with terms
#stopwords <- str_trim(stopwords) # remove superfluous whitespaces
stopwords <- paste(stopwords, collapse = " | ") # Regular 'or' expression
stopwords <- paste(" ", stopwords, " ", sep = "") # Add whitespace left and right
stopwords <- paste(stopwords, "| dass ", sep = "") # neue Rechtschreibung ;)
stopwords

data$stopwords <- str_count(data$sent.text, stopwords) # Count stopwords in sentence
data$terms.raw <- data$terms # Copy raw term count
data$terms <- data$terms.raw - data$stopwords # Correct term count by subtracting stopwords
data$terms[data$terms == 0] <- 1 # To avoid dividing by zero, doesn't change sentiment because no weighted terms can be in there

### EMODIC ###

load("./data/negative_ed8.RData") 
load("./data/ed8.RData") 

data$sent.text2 <- data$sent.text # Copy of the sampled sent.texts

# Loop over negation dictionary, and replace instances in text
for (i in 1:nrow(negative_ed8)){
  data$sent.text2 <- gsub(negative_ed8$pattern[i], negative_ed8$replacement[i], data$sent.text2, fixed = FALSE)
}

# How many observations were affected?
nrow(data[grepl("NOT_", data$sent.text2), ]) # Abs
nrow(data[grepl("NOT_", data$sent.text2), ])/nrow(data) # Share


# Scoring

angerterms <- ed8[ed8$anger == 1, c(1,2)]
fearterms <- ed8[ed8$fear == 1, c(1,3)]
disgustterms <- ed8[ed8$disgust == 1, c(1,4)]
sadnessterms <- ed8[ed8$sadness == 1, c(1,5)]
joyterms <- ed8[ed8$joy == 1, c(1,6)]
enthusiasmterms <- ed8[ed8$enthusiasm == 1, c(1,7)]
prideterms <- ed8[ed8$pride == 1, c(1,8)]
hopeterms <- ed8[ed8$hope == 1, c(1,9)]


data$anger <- 0

for (i in 1:nrow(angerterms)) {
  occur <- str_count(data$sent.text2, angerterms$feature[i])
  data$anger <- data$anger + occur                      
}

data$fear <- 0

for (i in 1:nrow(fearterms)) {
  occur <- str_count(data$sent.text2, fearterms$feature[i])
  data$fear <- data$fear + occur                      
}

data$disgust <- 0

for (i in 1:nrow(disgustterms)) {
  occur <- str_count(data$sent.text2, disgustterms$feature[i])
  data$disgust <- data$disgust + occur                      
}

data$sadness <- 0

for (i in 1:nrow(sadnessterms)) {
  occur <- str_count(data$sent.text2, sadnessterms$feature[i])
  data$sadness <- data$sadness + occur                      
}

data$joy <- 0

for (i in 1:nrow(joyterms)) {
  occur <- str_count(data$sent.text2, joyterms$feature[i])
  data$joy <- data$joy + occur                      
}

data$enthusiasm <- 0

for (i in 1:nrow(enthusiasmterms)) {
  occur <- str_count(data$sent.text2, enthusiasmterms$feature[i])
  data$enthusiasm <- data$enthusiasm + occur                      
}

data$pride <- 0

for (i in 1:nrow(prideterms)) {
  occur <- str_count(data$sent.text2, prideterms$feature[i])
  data$pride <- data$pride + occur                      
}

data$hope <- 0

for (i in 1:nrow(hopeterms)) {
  occur <- str_count(data$sent.text2, hopeterms$feature[i])
  data$hope <- data$hope + occur                      
}


# Normalized emotional score
data$anger.norm <- data$anger / data$terms
data$fear.norm <- data$fear / data$terms
data$disgust.norm <- data$disgust / data$terms
data$sadness.norm <- data$sadness / data$terms
data$joy.norm <- data$joy / data$terms
data$enthusiasm.norm <- data$enthusiasm / data$terms
data$pride.norm <- data$pride / data$terms
data$hope.norm <- data$hope / data$terms

# Remove temporary files
rm(angerterms, fearterms, disgustterms, sadnessterms, joyterms,
   enthusiasmterms, prideterms, hopeterms)


#Add grouping variable back in 

data$condition<-D$condition_fac

table_emotion<-
  data %>%
  group_by(condition)%>%
  summarize(mean_anger=mean(anger.norm),
            se=std.error(anger.norm))

fig_emotion<-
  table_emotion %>%
    ggplot(aes(x=condition, y=mean_anger, ymin=mean_anger-1.96*se, ymax=mean_anger+1.96*se))+
    geom_col()+
    geom_errorbar(width=0.2)+
    theme_minimal()+
    ylab("Mean Anger Sentiment")+xlab("Experimental Condition")
 

fig_emotion

 ggsave(plot = fig_emotion, filename = "./Figures/fig_A2.svg")
  ggsave(
  plot = fig_emotion,
  filename = "./Figures/fig_A2.png",
  width = 8,
  height = 4
)
  