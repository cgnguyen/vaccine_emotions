#Setup --------------------------------
library(tidyverse)
library(texreg)
library(jtools)

D<-read_rds("./data/data_shared.rds")

#Issue Polarization: Vaccine Mandate Approval for Vaccinated and Vaccinated------
##Model -------------------
mod_1_vac <-
  D %>%
  filter(vac_binary == 1) %>%
  lm(vac_post ~ condition + vac_pre, data = .)

summary(mod_1_vac)

mod_1_unvac <-
  D %>%
  filter(vac_binary == 0) %>%
  lm(vac_post ~ condition + vac_pre, data = .)
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

