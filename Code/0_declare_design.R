# Setup -------------------------------------------------------------------
  library(tidyverse)
  library(haven)
  library(naniar)
  library(car)
  library(DeclareDesign)
  library(truncnorm)

# Build anger vs. no anger model from scratch glm----------------------
## Define Parameters ---------------------------------------------
    #Define Number of trials
    N=1000
    p=0.15 # Probability of hesitancy
    #Treatment effect of anger. Assume it changes probability by 0.05 to 0.15 percentage points
    treat_anger = 0.02
  
  ##Declare Design - first define model, then outcomes, then quantity of interest--------------
   design_simple<-
     declare_model(
      N = N, 
      potential_outcomes(Y ~ rbinom(N, size = 1, prob = p + treat_anger * Z))
    ) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))+
    declare_assignment(Z = complete_ra(N, m = 50)) +
    declare_measurement(Y = reveal_outcomes(Y ~ Z)) + 
    declare_estimator(Y ~ Z,  
                      model = glm,
                      family = binomial("logit"), 
                      inquiry = "ATE")
  
  ## Diagnose Design ---------------------------
  diagnosands <-
    declare_diagnosands(bias = mean(estimate - estimand),
                        power = mean(p.value <= 0.05))
  
  diagnosis <- diagnose_design(design_simple, diagnosands = diagnosands)
  
  
  ##Check Multiple Effect Sizes / Sample Sizes -------------------------
  

  check_multiple<-function(sample_size,effect_size_anger){
    N=sample_size
    treat_anger=treat_anger
    
    declare_model(
      N = N, 
      potential_outcomes(Y ~ rbinom(N, size = 1, prob = p + treat_anger * Z))
    ) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))+
    declare_assignment(Z = complete_ra(N, m = 50)) +
    declare_measurement(Y = reveal_outcomes(Y ~ Z)) + 
    declare_estimator(Y ~ Z, model = lm_robust, inquiry = "ATE")
    
    }
  
  #Create several designs based on assumed sample size
    design_multiple <- expand_design(
      check_multiple, 
      sample_size = seq(100,2000,by=100), 
      effect_size_anger = c(0.01,0.05,0.1,0.2))
    
    
    diagnosis <- diagnose_design(design_multiple, sims=100)
    
  #Diagnose designs
  diagnose_design(designs)
  

# Build two treamtment arm model ----------------------
## Define Parameters ---------------------------------------------
    #Define Number of trials
    N=2000
    p=0.1 # Probability of hesitancy
    #Treatment effect of anger. Assume it changes probability by 0.05 to 0.15 percentage points
    treat_anger = 0.05
    treat_anxiety = -0.01
  ##Declare Design - first define model, then outcomes, then quantitity of interest--------------
    
    dec_pop<- declare_population(N=N)
    #Declare outcome for multiarm treatment 
    dec_out<- declare_potential_outcomes(Y ~ rbinom(N, size = 1, prob = p + treat_anger * (Z=="2")+ treat_anxiety*(Z=="3")),
                         conditions = c("1","2","3"))
    #Declare quantity of interests - difference between anger/ anxiety and control 
    dec_inq<- declare_inquiry(
      ATE_anger = mean(Y_Z_2 - Y_Z_1),
      ATE_anxiety = mean(Y_Z_3 - Y_Z_1))

    #Assign people to treatment conditions
    dec_ass <- declare_assignment(Z = complete_ra(N,  conditions = c("1", "2", "3")),
                                    anger = as.numeric(Z %in% 2), 
                                    anxiety = as.numeric(Z %in% 3))
    
    #Declare measurement 
    dec_measure<-declare_measurement(Y = reveal_outcomes(Y ~ Z))
    
    #Declare Measures - based on multiarm design code
    # dec_est<-declare_estimator(handler = function(data){
    #   estimates <- rbind.data.frame(
    #     ate_Y_2_1 = difference_in_means(formula = Y ~ anger, data = data, 
    #                                     condition1 = "1", condition2 = "2"), 
    #     ate_Y_3_1 = difference_in_means(formula = Y ~ anxiety, data = data, 
    #                                     condition1 = "1", condition2 = "3"))
    #     names(estimates)[names(estimates) == "N"] <- "N_DIM"
    #     estimates$estimator_label <- c("DIM (Z_2 - Z_1)", "DIM (Z_3 - Z_1)")
    #     estimates$inquiry_label <- rownames(estimates)
    #     estimates$estimate <- estimates$coefficients
    #     estimates$term <- NULL
    # return(estimates)})
    
    dec_est<-declare_estimator(Y ~ anger+anxiety, 
                               model=lm_robust,
                               term=c("anger","anxiety"),
                               estimand=c("ATE_anger","ATE_anxiety"))
    
    
    
    
    #Combine it all - actually check step by step- can be ignored later
    temp_pop<-dec_pop()

    temp_pop_1<-dec_out(temp_pop)
    temp_pop_2<-dec_ass(temp_pop_1)
    temp_pop_3<- dec_measure(temp_pop_2)

    design<-dec_pop+dec_out+dec_inq+dec_ass+dec_measure+dec_est
    
    ## Diagnose Design ---------------------------
    diagnosands <-
      declare_diagnosands(bias = mean(estimate - estimand),
                        power = mean(p.value <= 0.05))

    diagnosis <- diagnose_design(design, sims=500)
   
    
## Check Various effect sizes/population sizes -------------------------------------------


  check_multiple <- function(sample_size,effect_size_anger,effect_size_anxiety){
    N=sample_size
    p=0.1 # Probability of hesitancy
    treat_anger = effect_size_anger
    treat_anxiety = effect_size_anxiety
    dec_pop<- declare_population(N=N)
    #Declare outcome for multiarm treatment 
    dec_out<- declare_potential_outcomes(Y ~ rbinom(N, size = 1, prob = p + treat_anger * (Z=="2")+ treat_anxiety*(Z=="3")),
                         conditions = c("1","2","3"))
    #Declare quantity of interests - difference between anger/ anxiety and control 
    dec_inq<- declare_inquiry(
      ATE_anger = mean(Y_Z_2 - Y_Z_1),
      ATE_anxiety = mean(Y_Z_3 - Y_Z_1))

    #Assign people to treatment conditions
    dec_ass <- declare_assignment(Z = complete_ra(N,  conditions = c("1", "2", "3")),
                                    anger = as.numeric(Z %in% 2), 
                                    anxiety = as.numeric(Z %in% 3))
    
    #Declare measurement 
    dec_measure<-declare_measurement(Y = reveal_outcomes(Y ~ Z))
    
    #Declare Measures - based on multiarm design code
    dec_est<-declare_estimator(Y ~ anger+anxiety, 
                               model=lm_robust,
                               term=c("anger","anxiety"),
                               estimand=c("ATE_anger","ATE_anxiety"))
    
    #Combine it all - actually check step by step- can be ignored later
    # temp_pop<-dec_pop()
    # 
    # temp_pop_1<-dec_out(temp_pop)
    # temp_pop_2<-dec_ass(temp_pop_1)
    # temp_pop_3<- dec_measure(temp_pop_2)

    design<-dec_pop+dec_out+dec_inq+dec_ass+dec_measure+dec_est}
    
  
    design_multiple <- expand_design(
      check_multiple, 
      sample_size = seq(100,1000,by=100), 
      effect_size_anger = c(0.01,0.05,0.1),
      effect_size_anxiety =  c(-0.01,-0.05,-0.1))
    
    diagnosands <-
      declare_diagnosands(bias = mean(estimate - estimand),
                        power = mean(p.value <= 0.05))
  
    diagnosis <- diagnose_design(design_multiple, sims=500)
    
    diagnosis[2]
    temp<-diagnosis[[1]]
 
    diagnosis$diagnosands_df%>%
        filter(term=="anger")%>%
        mutate(effect_size_anger=as.factor(effect_size_anger))%>%
        ggplot()+
          aes(x=sample_size, color=effect_size_anger,y=power,)+
          geom_line()
    
    
    diagnosis$diagnosands_df%>%
        filter(term=="anxiety")%>%
        mutate(effect_size=as.factor(effect_size_anxiety))%>%
        ggplot()+
          aes(x=sample_size, color=effect_size,y=power,)+
          geom_line()
    
    
# Build 3x2 Treatment Model - Actually Planned ------------------------------
    ## Define Parameters ---------------------------------------------
    #Define Number of trials
    N=2000
    p=0.1 # Probability of hesitancy
    #Treatment effect of anger. Assume it changes probability by set percentage points. 
    #Assume only additive effect between corona/ emotion treatments, no amplification / interaction effects
    treat_anger = 0.05
    treat_anxiety = -0.01
    treat_corona =0.01
  ##Declare Design - first define model, then outcomes, then quantitity of interest--------------
    
    dec_pop<- declare_population(N=N)
    #Declare outcome for multiarm (control,anger,anxiety ) x factorial (corona,no corona)
    dec_out<- declare_potential_outcomes(
      Y ~ rbinom(N, 
                 size = 1, 
                 prob = p + 
                   treat_anger * (Z %in% c("2","5"))+ 
                   treat_anxiety*(Z %in%c("3","6"))+
                   treat_corona*(Z %in% c("4","5","6"))),
                 conditions = c("1","2","3","4","5","6"))
    #Declare quantity of interests - difference between anger/ anxiety and control / corona baseline 
    dec_inq<- declare_inquiry(
      ATE_anger_general = mean(Y_Z_2 - Y_Z_1),
      ATE_anger_corona = mean(Y_Z_5 - Y_Z_4),
      ATE_anger_combined = mean(c(mean(Y_Z_2 - Y_Z_1),mean(Y_Z_5 - Y_Z_4))))
      
      
      
      

    #Assign people to treatment conditions
    dec_ass <- declare_assignment(Z = complete_ra(N,  conditions = c("1", "2", "3","4","5","6")),
                                    anger = as.numeric(Z %in% c(2,5)), 
                                    anxiety = as.numeric(Z %in% c(3,6)),
                                    corona = as.numeric(Z %in% c(4,5,6)))
    
    #Declare measurement 
    dec_measure<-declare_measurement(Y = reveal_outcomes(Y ~ Z))
    

    
    dec_est<-declare_estimator(Y ~ anger*corona+anxiety*corona, 
                               model=lm_robust,
                               term=c("anger"),
                               estimand=c("ATE_anger_combined"))
    
    
    
    
    #Combine it all - actually check step by step- can be ignored later
    temp_pop<-dec_pop()

    temp_pop_1<-dec_out(temp_pop)
    temp_pop_2<-dec_ass(temp_pop_1)
    temp_pop_3<- dec_measure(temp_pop_2)

    design<-dec_pop+dec_out+dec_inq+dec_ass+dec_measure+dec_est
    
    ## Diagnose Design ---------------------------
    # diagnosands <-
    #   declare_diagnosands(bias = mean(estimate - estimand),
    #                     power = mean(p.value <= 0.05))

    diagnosis <- diagnose_design(design,  sims=500)
    
    
  ## Check multiple effect sizes -------------------------------------------------------
     
  ###Define Model Function ------------------------
  check_multiple<-function(sample_size,effect_size_anger,effect_size_anxiety,effect_size_corona){
    N=sample_size
    p=0.1 # Probability of hesitancy
    treat_anger = effect_size_anger
    treat_anxiety = effect_size_anxiety
    treat_corona = effect_size_corona

    dec_pop<- declare_population(N=N)
    #Declare outcome for multiarm (control,anger,anxiety ) x factorial (corona,no corona)
    dec_out<- declare_potential_outcomes(
      Y ~ rbinom(N, 
                 size = 1, 
                 prob = p + 
                   treat_anger * (Z %in% c("2","5"))+ 
                   treat_anxiety*(Z %in%c("3","6"))+
                   treat_corona*(Z %in% c("4","5","6"))),
                 conditions = c("1","2","3","4","5","6"))
    #Declare quantity of interests - difference between anger/ anxiety and control / corona baseline 
    dec_inq<- declare_inquiry(
      ATE_anger_general = mean(Y_Z_2 - Y_Z_1),
      ATE_anger_corona = mean(Y_Z_5 - Y_Z_4),
      ATE_anger_combined = mean(c(mean(Y_Z_2 - Y_Z_1),mean(Y_Z_5 - Y_Z_4))))

    #Assign people to treatment conditions
    dec_ass <- declare_assignment(Z = complete_ra(N,  conditions = c("1", "2", "3","4","5","6")),
                                    anger = as.numeric(Z %in% c(2,5)), 
                                    anxiety = as.numeric(Z %in% c(3,6)),
                                    corona = as.numeric(Z %in% c(4,5,6)))
    #Declare measurement 
    dec_measure<-declare_measurement(Y = reveal_outcomes(Y ~ Z))

    dec_est<-declare_estimator(Y ~ anger*corona+anxiety*corona, 
                               model=lm_robust,
                               term=c("anger"),
                               inquiry=c("ATE_anger_combined"))
    
    #Combine it all - actually check step by step- can be ignored later


    return(design<-dec_pop+dec_out+dec_inq+dec_ass+dec_measure+dec_est)
}
    
  ###Create several designs based on assumed sample size-----
    design_multiple <- expand_design(
      check_multiple, 
      sample_size = seq(from=2000, to=5000,by=100),
      effect_size_anger = seq(from=0.02, to=0.1,by=0.02),
      effect_size_corona =c(-0.025,0,0.025),
      effect_size_anxiety = c(-0.02))
    

    
    
    diagnosis <- diagnose_design(design_multiple, sims=500)
    
    diagnosis[[2]]%>%
      filter(term=="anger")%>%
      ggplot()+
        aes(x=sample_size, y=power, color=as.factor(effect_size_anger))+
        geom_point(size=0.1)+
        theme_bw()+
        geom_smooth()+
        geom_hline(yintercept = 0.8)
    
    diagnosis_multiple<-diagnosis

    write_rds(diagnosis_multiple,file="simulation.rds")    
    
    
# Build anger vs. no anger model from scratch - outcome is normally distributed 0-1 lm for difference in means-----
    ## Define Parameters ---------------------------------------------
    #Define Number of trials
    N=2500
    p=2.5 # Mean willingness to vaccinate
    #Treatment effect of anger. Assume it makes you 0.5 less likely to be vaccinated
    treat_anger = 0.5
  
  ##Declare Design - first define model, then outcomes, then quantity of interest--------------
   design_simple<-
     declare_model(
      N = N,
      U = rnorm(N),
      potential_outcomes(Y ~ p+treat_anger*Z+U)) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))+
    declare_assignment(Z = complete_ra(N, m = 50)) +
    declare_measurement(Y = reveal_outcomes(Y ~ Z)) + 
   declare_estimator(Y ~ Z, model = lm_robust, inquiry = "ATE")
  
  ## Diagnose Design ---------------------------
  diagnosands <-
    declare_diagnosands(bias = mean(estimate - estimand),
                        power = mean(p.value <= 0.05))
  
  diagnosis <- diagnose_design(design_simple)
  
  
  
  
  
#Analysis for Sabrina/ Susanne with standard normal variables-------------------
  
  # Build 3x2 Treatment Model - Actually Planned ------------------------------
    ## Define Parameters ---------------------------------------------
    #Define Number of trials
    N=2500
    p=2.5 # Probability of hesitancy
    #Treatment effect of anger. Assume it changes probability by set percentage points. 
    #Assume only additive effect between corona/ emotion treatments, no amplification / interaction effects
    treat_anger = 0.25
    treat_anxiety = -0.25
    treat_corona = 0.25
  ##Declare Design - first define model, then outcomes, then quantitity of interest--------------
    
    
    
     temp_pop <- fabricate(
      N = N,
      Y = rtruncnorm(n=N, a=1, b=5, mean = p, sd = 1))
    
    dec_pop<- declare_population(temp_pop)
    
    dec_out<-declare_potential_outcomes(
      Y ~ Y +
        treat_anger * (Z %in% c("2","5"))+ 
        treat_anxiety*(Z %in%c("3","6"))+
        treat_corona* (Z %in% c("4","5","6")),
      conditions = c("1","2","3","4","5","6"))
   

    
    
    #Declare quantity of interests - difference between anger/ anxiety and control / corona baseline 
    dec_inq<- declare_inquiry(
      ATE_anger_general = mean(Y_Z_2 - Y_Z_1),
      ATE_anger_corona = mean(Y_Z_5 - Y_Z_4),
      ATE_anger_combined = mean(c(mean(Y_Z_2 - Y_Z_1),mean(Y_Z_5 - Y_Z_4))),
      ATE_anxiety_combined = mean(c(mean(Y_Z_3 - Y_Z_1),mean(Y_Z_5 - Y_Z_4)))
      
      
      
      
      
      
      

    #Assign people to treatment conditions
    dec_ass <- declare_assignment(Z = complete_ra(N,  conditions = c("1", "2", "3","4","5","6")),
                                    anger = as.numeric(Z %in% c(2,5)), 
                                    anxiety = as.numeric(Z %in% c(3,6)),
                                    corona = as.numeric(Z %in% c(4,5,6)))
    
    #Declare measurement 
    dec_measure<-declare_measurement(Y = reveal_outcomes(Y ~ Z))
    

    
    dec_est<-declare_estimator(Y ~ anger*corona+anxiety*corona, 
                               model=lm_robust,
                               term=c("anger"),
                               inquiry=c("ATE_anger_combined"))
    
    design<-dec_pop+dec_out+dec_inq+dec_ass+dec_measure+dec_est
    
    ## Diagnose Design ---------------------------
    # diagnosands <-
    #   declare_diagnosands(bias = mean(estimate - estimand),
    #                     power = mean(p.value <= 0.05))

    diagnosis <- diagnose_design(design,  sims=500)
    
    
    
    
  
  
  


  

    
    
    
    
    
  
  randomization_assignment <- 
    declare_assignment(Z = complete_ra(N,  conditions = c("1", "2", "3","4","5","6")),
                                    anger = as.numeric(Z %in% c(2,5)), 
                                    anxiety = as.numeric(Z %in% c(3,6)),
                                    corona = as.numeric(Z %in% c(4,5,6)))
    

  
   temp_pop <- fabricate(
      N = 2500,
      Y = rnorm(N, mean=p),
      polarization_1 = draw_likert(x = Y, type = 5))
  
    
    dec_pop<- declare_population(temp_pop)
                                 
    
   temp<-dec_pop()
    
    
    dec_out<-declare_potential_outcomes(
       polarization_2 ~draw_likert(x = Y +
                                     treat_anger * (Z %in% c("2","5"))+ 
                                     treat_anxiety*(Z %in%c("3","6"))+
                                     treat_corona*(Z %in% c("4","5","6")),
                                   type=5),
                                   conditions = c("1","2","3","4","5","6")))
    

  
  
  
  
  #We have enough money for 2500 people
    N=2500
    p=2.5 # Baseline polarization 
    #Treatment effect of anger. Assume it changes probability by set percentage points. 
    #Assume only additive effect between corona/ emotion treatments, no amplification / interaction effects
    treat_anger = 0.25
    treat_anxiety = -0.25
    treat_corona =0.25

    
    dec_pop<- declare_population(N=N, U = rnorm(N))
    #Declare outcome for multiarm (control,anger,anxiety ) x factorial (corona,no corona)
    dec_out<- declare_potential_outcomes(Y ~ p+treat_anger*Z+U)


+

                   treat_anger * (Z %in% c("2","5"))+ 
                   treat_anxiety*(Z %in%c("3","6"))+
                   treat_corona*(Z %in% c("4","5","6"))),
                 conditions = c("1","2","3","4","5","6"))
    #Declare quantity of interests - difference between anger/ anxiety and control / corona baseline 
    dec_inq<- declare_inquiry(
      ATE_anger_general = mean(Y_Z_2 - Y_Z_1),
      ATE_anger_corona = mean(Y_Z_5 - Y_Z_4),
      ATE_anger_combined = mean(c(mean(Y_Z_2 - Y_Z_1),mean(Y_Z_5 - Y_Z_4))))
      
      
      
      

    #Assign people to treatment conditions
    dec_ass <- declare_assignment(Z = complete_ra(N,  conditions = c("1", "2", "3","4","5","6")),
                                    anger = as.numeric(Z %in% c(2,5)), 
                                    anxiety = as.numeric(Z %in% c(3,6)),
                                    corona = as.numeric(Z %in% c(4,5,6)))
    
    #Declare measurement 
    dec_measure<-declare_measurement(Y = reveal_outcomes(Y ~ Z))
    

    
    dec_est<-declare_estimator(Y ~ anger*corona+anxiety*corona, 
                               model=lm_robust,
                               term=c("anger"),
                               estimand=c("ATE_anger_combined"))
    
    
    
    
  pop_pol <- fabricate(
    N = 2500,
    U = rnorm(N),
    latent_polarization = rnorm(N, mean=1),
    polarization_label = draw_ordered(
      x = latent_polarization+U,
      break_labels = c(
        "Trifft überhaupt nicht zu", " Trifft eher nicht zu",
        "Weder noch",
        "Trifft eher zu","Trifft voll und ganz zu",
        "Liberal", "Very Liberal"),
      breaks= c(-0.5, 0,0.5,1)))

    
      draw_ordered(
          x = link(latent),
          
          break_labels = NULL,
          N = length(x),
          latent = NULL,
          strict = FALSE,
          link = "identity"
        )
    
    
    polarization = draw_normal(mean = 0, N = N),

    ),
    income = exp(rlnorm(n = N, meanlog = 2.4 - (ideology * 0.1), sdlog = 0.12)),
    Q1_immigration = draw_likert(x = ideology, type = 7),
    Q2_defence = draw_likert(x = ideology + 0.5, type = 7),
    treatment = draw_binary(0.5, N = N),
    proposition_vote = draw_binary(latent = ideology + 1.2 * treatment, link = "probit")
  )
    
    


#

