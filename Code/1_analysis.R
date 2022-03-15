#Setup --------------------------------
  #Define needed packages
  packages <- c("tidyverse",
                "texreg",
                "jtools",
                "ltm",
                "scales",
                "ggpubr",
                "ggnewscale",
                "patchwork",
                "broom.mixed",
                "gtsummary",
                "flextable",
                "stargazer",
                "modelsummary",
                "quanteda",
                "quanteda.textmodels",
                "quanteda.textplots",
                "quanteda.textstats")
  
  
  
  
  # Install packages not yet installed
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }
  
  # Packages loading
  invisible(lapply(packages, library, character.only = TRUE))



  D<-read_rds("./data/data_shared.rds")


#Checks---------------------

##Cronbach Alpha of index-------


D%>%
  dplyr::select(ap_corona_1,ap_corona_2)%>%
  drop_na()%>%
  cronbach.alpha(., CI=TRUE)




#Figure 1 - Approval of Vaccine Mandates by vaccine status. 

#Figure 1: Relative distribution of Vaccine Mandate approval in general / by vaccine status in relative terms ----
   #Full Data
   data_temp_1<-
     D%>%
        dplyr::filter(!is.na(vac_post))%>%
        dplyr::filter(!is.na(vac_binary))%>%
        # dplyr::filter(corona=="No")%>%
        group_by(vac_post)%>%
        summarise(n=n())%>%
        mutate(n = prop.table(n))%>%
        mutate(vac_binary="Full Sample")
       
    #Data by vaccine status
    data_temp_2<-
     D%>%
        dplyr::filter(!is.na(vac_post))%>%
        dplyr::filter(!is.na(vac_binary))%>%
        group_by(vac_binary,vac_post)%>%
        summarise(n=n())%>%
        mutate(n = prop.table(n))%>%
        mutate(vac_binary=factor(vac_binary, levels=c("1","0"),
                                 labels=c("Vaccinated","Unvacinated")))%>%
        rbind(data_temp_1)%>%
        ungroup()
    
    
      #Get mid category_values
      temp_mid<-
          data_temp_2%>%
          group_by(vac_binary)%>%
          summarize(mid_n=n[vac_post==0])
    
    
      #Add new rows and add split mid values, then change data to be in two directions
      data_fig<-
        data_temp_2%>%
          group_by(vac_binary)%>%
          group_modify(~ add_row(.x,  vac_post=-0.5))%>%
          group_modify(~ add_row(.x,  vac_post=0.5))%>%
          right_join(temp_mid)%>%
          mutate(n=case_when(vac_post==0.5|vac_post==-0.5 ~mid_n/2, 
                             TRUE ~n))%>%
          #shift categories around so negative values are actually negative
          mutate(n=case_when(vac_post<=0 ~ -1*n,
                           vac_post >0 ~1*n))%>%
       
        #Rearrange factors, note order for above 0 is funny
        mutate(vac_post_factor=factor(vac_post,levels= c(-2,-1,-0.5,0,2,1,0.5)))%>%
        #Add  fake factor levels for proper legend
        mutate(vac_post_fake = fct_recode(vac_post_factor,
                                          "0" = "-0.5",
                                          "0" = "0.5"))%>%
        filter(vac_post!=0)


    fig_1<-
      data_fig%>%         
        ggplot()+
          aes(x=as.factor(vac_binary),y=n,fill=vac_post_factor, label=n)+
          geom_col(position="stack", width  = 0.5, show.legend = F)+
          geom_hline(aes(yintercept=0))+
          scale_fill_manual(values= c("#d7191c", "#fdae61","light gray","#2c7bb6","#abd9e9","light gray"),
                            labels = c("Strongly Disagree","Disagree","Neither Disagree/Agree",
                                       "Strongly Agree","Agree", "Neither Disagree/Agree"))+ 
          theme_minimal()+
          theme(legend.position = "bottom")+ 
          guides(fill = guide_legend(title = "In times of pandemics like corona, there should be a general vaccination mandate", title.position = "top"))+
          
          xlab("")+
          ylab("Percentage of Respondents in Support/Opposition to Vaccine Mandates")+
           scale_y_continuous(breaks = scales::pretty_breaks(n = 10),
                             labels = function(x) percent(abs(x)), limits=c(-1,1))+
          scale_x_discrete(name ="Vaccination Status", 
                    labels=c("0"="Unvaccinated","1"="Vaccinated"))
      
      fig_1
      
      #Make a second figure just for the legend
      fig_aux<-   
        data_fig%>%    
        ggplot()+
     
          aes(x=as.factor(vac_binary),y=n,fill=vac_post_fake, label=n)+
          geom_col(position="stack", width  = 0.5, show.legend = T)+
          scale_fill_manual(values= c("#d7191c", "#fdae61","light gray","#abd9e9","#2c7bb6","light gray"),
                            labels = c("Strongly Disagree","Disagree","Neither Disagree/Agree",
                                       "Agree","Strongly Agree", "Neither Disagree/Agree"))+ 
          theme_minimal()+
          theme(legend.position = "bottom")+ 
          guides(fill = guide_legend(title = "In times of pandemics like corona, there should be a general vaccination mandate", title.position = "top"))
      
      
      #Get legend
      fig_1_legend <- get_legend(fig_aux)
      
      #Combine with original graph
      fig_1_full<-
        fig_1/fig_1_legend+
        plot_layout(heights=c(10,1))
      
      fig_1_full

      ggsave(plot=fig_1_full,filename = "./Figures/fig_1.svg")
      ggsave(plot=fig_1_full,filename = "./Figures/fig_1.png", width= 6, height =6)    



      
#Issue Polarization: Vaccine Mandate Approval for Vaccinated and Vaccinated------
##Model -------------------
mod_1_vac <-
  D %>%
    filter(vac_binary == 1) %>%
    lm(vac_post ~ condition+vac_pre, data = .)

summary(mod_1_vac)

mod_1_unvac <-
  D %>%
  filter(vac_binary == 0) %>%
   lm(vac_post ~condition+vac_pre, data = .)
summary(mod_1_unvac)

##Figure 2-----
fig_2 <-
  plot_coefs(
    mod_1_vac,
    mod_1_unvac,
    coefs = c(
      "Anger - Generalized" = "condition1",
      "Anxiety - Generalized" = "condition2",
      "Anger - Corona" = "condition4",
      "Anxiety - Corona" = "condition5",
      "Neutral - Corona" = "condition6"
    ),
    groups = list(
      Anger = c("Anger - Generalized", "Anger - Corona"),
      Anxiety = c("Anxiety - Generalized", "Anxiety - Corona"),
      Corona = c("Neutral - Corona")
    ),
    facet.label.pos = "left",
    model.names = c("Vaccinated", "Unvaccinated"),
    ci_level = 0.95
  ) +
  
  theme(legend.position = "top",) +
  xlim(-0.5, 0.5)

fig_2

#Export Plot
ggsave(plot = fig_2, filename = "./Figures/fig_2.svg")
ggsave(
  plot = fig_2,
  filename = "./Figures/fig_2.png",
  width = 8,
  height = 6
)

##Table for Appendix -----------------
htmlreg(
  list(mod_1_vac, mod_1_unvac),
  custom.model.names = c("Vaccinated", "Unvaccinated"),
  custom.coef.names = c(
    "Intercept",
    "Anger - Generalized",
    "Anxiety - Generalized",
    "Anger - Corona",
    "Anxiety - Corona",
    "Corona",
    "Pre-Treatment Approval"
  ),
  reorder.coef = c(2, 4, 3, 5, 6, 1, 7),
  file = "./Tables/table_1.html",
  single.row = T
)


#Affective Polarization -------------------------------------
##Models ----------------------------
mod_2 <-
  D %>%
  lm(ap_corona ~ condition, data = .)
  summary(mod_2)

  mod_2_unvac <-
  D %>%
  filter(vac_binary == 0) %>%
  
  lm(ap_corona ~ condition, data = .)

  summary(mod_2_unvac)

mod_2_vac <-
  D %>%
  filter(vac_binary == 1) %>%
  lm(ap_corona ~ condition, data = .)

summary(mod_2_vac)


##Figure 3-----------------------------

fig_3 <-
  plot_coefs(
    mod_2,
    mod_2_vac,
    mod_2_unvac,
    coefs = c(
      "Anger - Generalized" = "condition1",
      "Anxiety - Generalized" = "condition2",
      "Anger - Corona" = "condition4",
      "Anxiety - Corona" = "condition5",
      "Neutral - Corona" = "condition6"
    ),
    groups = list(
      Anger = c("Anger - Generalized", "Anger - Corona"),
      Anxiety = c("Anxiety - Generalized", "Anxiety - Corona"),
      Corona = c("Neutral - Corona")
    ),
    model.names = c("Full Sample", "Vaccinated", "Unvaccinated"),
    facet.label.pos = "left",
    ci_level = 0.95
  ) +
  theme(legend.position = "top")

fig_3

ggsave(plot = fig_2, filename = "./Figures/fig_3.svg")
ggsave(
  plot = fig_3,
  filename = "./Figures/fig_3.png",
  width = 8,
  height = 6
)

##Table for Appendix ----------------

htmlreg(
  list(mod_2, mod_2_vac, mod_2_unvac),
  custom.model.names = c("Full Sample", "Vaccinated", "Unvaccinated"),
  custom.coef.names = c(
    "Intercept",
    "Anger - Generalized",
    "Anxiety - Generalized",
    "Anger - Corona",
    "Anxiety - Corona",
    "Corona"
  ),
  reorder.coef = c(2, 4, 3, 5, 6, 1),
  file = "./Tables/table_2.html",
  single.row = T
)

