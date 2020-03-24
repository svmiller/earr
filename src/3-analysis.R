if (exists("AD")) {
  pi
} else {
  AD <- readRDS("data/ad.rds")
}

if (exists("VD")) {
  pi
} else {
  VD <- readRDS("data/vd.rds")
}



library(blme)

# ANES models -----
# Do the county models manually.
C1 <- bglmer(immigrld ~ z_age + female + collegeed + unemployed +
               lowincome + z_lcindex + z_pid + wny + wpy + z_ec_therm2a + z_cunempr +
                (1 | county) + (1 | county:survyr) + (1 | survyr), data=AD,
             family=binomial(link = "logit"),
             control=glmerControl(optimizer="Nelder_Mead",
                                  optCtrl=list(maxfun=2e5)))

C1wo <- bglmer(immigrld ~ z_age + female + collegeed + unemployed +
               lowincome + z_lcindex + z_pid + wny + wpy + z_cunempr +
               (1 | county) + (1 | county:survyr) + (1 | survyr), data=AD,
             family=binomial(link = "logit"),
             control=glmerControl(optimizer="Nelder_Mead",
                                  optCtrl=list(maxfun=2e5)))

ad_dvs = c("immigrld")
ad_ivs = c("z_sunempr", "z_nunempr", "z_rjp")


ADM = list()
for (i in ad_dvs){
  for (j in ad_ivs){
    ADM[[paste0(i, ": ", j)]] <- bglmer(as.formula(paste(i, "~", "z_age + female + collegeed + unemployed +
                                                            lowincome + z_lcindex + z_pid + wny + wpy + 
                                                            z_ec_therm2a +", j,
                                                            "  + (1 | survyr) + (1 | state) + (1 | state:survyr)")), 
                                           data=subset(AD), family = binomial(link="logit"),
                                           control=glmerControl(optimizer="Nelder_Mead",
                                                                optCtrl=list(maxfun=2e5)))
  }
}

c("immigrld: z_cunempr" = C1, ADM) -> ADM


saveRDS(ADM, "data/adm.rds")

# * ANES models without the ethnocentrism measure. -----

ADMwo = list()
for (i in ad_dvs){
  for (j in ad_ivs){
    ADMwo[[paste0(i, ": ", j)]] <- bglmer(as.formula(paste(i, "~", "z_age + female + collegeed + unemployed +
                                                            lowincome + z_lcindex + z_pid + wny + wpy +", j,
                                                         "  + (1 | survyr) + (1 | state) + (1 | state:survyr)")), 
                                        data=subset(AD), family = binomial(link="logit"),
                                        control=glmerControl(optimizer="Nelder_Mead",
                                                             optCtrl=list(maxfun=2e5)))
  }
}

c("immigrld: z_cunempr" = C1wo, ADMwo) -> ADMwo

saveRDS(ADMwo, "data/admwo.rds")


# VSG models -----

vd_dvs = c("immig_draind", "immig_harderd")
vd_ivs = c("z_sunempr", "z_percunempben", "z_avgunempcompen", "z_diffpunempben", "z_diffunempcompen",
           "z_rjpcbsa", "z_rjps")

VDM = list()
for (i in vd_dvs){
  for (j in vd_ivs){
    VDM[[paste0(i, ": ", j)]] <- bglmer(as.formula(paste(i, "~", "z_age + female + collegeed + unemployed +
                                                         lowincome + z_ideo + z_pid +
                                                         wpy + econgetw + z_ec_therm2b +", j,
                                                         "  +  (1 | state)")), 
                                        data=subset(VD), family = binomial(link="logit"),
                                        control=glmerControl(optimizer="Nelder_Mead",
                                                             optCtrl=list(maxfun=2e5)))
  }
}

saveRDS(VDM, "data/vdm.rds")

# * VSG models without the ethnocentrism measure. -----

VDMwo = list()
for (i in vd_dvs){
  for (j in vd_ivs){
    VDMwo[[paste0(i, ": ", j)]] <- bglmer(as.formula(paste(i, "~", "z_age + female + collegeed + unemployed +
                                                         lowincome + z_ideo + z_pid +
                                                         wpy + econgetw  +", j,
                                                         "  +  (1 | state)")), 
                                        data=subset(VD), family = binomial(link="logit"),
                                        control=glmerControl(optimizer="Nelder_Mead",
                                                             optCtrl=list(maxfun=2e5)))
  }
}

saveRDS(VDMwo, "data/vdmwo.rds")


# ANES models, looping in/out various ethnocentrism measures. -----

ad_dvs = c("immigrld")
ecivs = c("z_ec_therm1", "z_ec_therm2a", "z_ec_therm2b","z_ec_therm3")


ADMec = list()
for (i in ad_dvs){
  for (j in ecivs){
    ADMec[[paste0(i, ": ", j)]] <- bglmer(as.formula(paste(i, "~", "z_age + female + collegeed + unemployed +
                                                            lowincome + z_lcindex + z_pid + wny + wpy + 
                                                            z_rjp +", j,
                                                         "  + (1 | survyr) + (1 | state) + (1 | state:survyr)")), 
                                        data=subset(AD), family = binomial(link="logit"),
                                        control=glmerControl(optimizer="Nelder_Mead",
                                                             optCtrl=list(maxfun=2e5)))
  }
}

saveRDS(ADMec, "data/admec.rds")

# VSG models, looping in/out various ethnocentrism measures. -----

VDMec = list()
for (i in vd_dvs){
  for (j in ecivs){
    VDMec[[paste0(i, ": ", j)]] <- bglmer(as.formula(paste(i, "~", "z_age + female + collegeed + unemployed +
                                                             lowincome + z_ideo + z_pid +
                                                             wpy + econgetw  + z_rjps +", j,
                                                           "  + (1 | state)")), 
                                          data=subset(VD), family = binomial(link="logit"),
                                          control=glmerControl(optimizer="Nelder_Mead",
                                                               optCtrl=list(maxfun=2e5)))
  }
}

saveRDS(VDMec, "data/vdmec.rds")


# ANES models, interacting wpy with econ measure ----

ad_dvs = c("immigrld")
ad_ivs = c("z_sunempr", "z_nunempr", "z_rjp")


C1int <- bglmer(immigrld ~ z_age + female + collegeed + unemployed +
               lowincome + z_lcindex + z_pid + wny + wpy + z_ec_therm2a + z_cunempr*wpy +
               (1 | county) + (1 | county:survyr) + (1 | survyr), data=AD,
             family=binomial(link = "logit"),
             control=glmerControl(optimizer="Nelder_Mead",
                                  optCtrl=list(maxfun=2e5)))


ADMint = list()
for (i in ad_dvs){
  for (j in ad_ivs){
    ADMint[[paste0(i, ": ", j)]] <- bglmer(as.formula(paste(i, "~", "z_age + female + collegeed + unemployed +
                                                            lowincome + z_lcindex + z_pid + wny + wpy + 
                                                            z_ec_therm2a + wpy*", j,
                                                         "  + (1 | survyr) + (1 | state) + (1 | state:survyr)")), 
                                        data=subset(AD), family = binomial(link="logit"),
                                        control=glmerControl(optimizer="Nelder_Mead",
                                                             optCtrl=list(maxfun=2e5)))
  }
}

c("immigrld: z_cunempr" = C1int, ADMint) -> ADMint


saveRDS(ADMint, "data/admint.rds")


# VSG models, interacting wpy with econ measure

vd_dvs = c("immig_draind", "immig_harderd")
vd_ivs = c("z_sunempr", "z_percunempben", "z_avgunempcompen", "z_diffpunempben", "z_diffunempcompen",
           "z_rjpcbsa", "z_rjps")

VDMint = list()
for (i in vd_dvs){
  for (j in vd_ivs){
    VDMint[[paste0(i, ": ", j)]] <- bglmer(as.formula(paste(i, "~", "z_age + female + collegeed + unemployed +
                                                         lowincome + z_ideo + z_pid +
                                                         wpy + econgetw + z_ec_therm2b + wpy*", j,
                                                         "  +  (1 | state)")), 
                                        data=subset(VD), family = binomial(link="logit"),
                                        control=glmerControl(optimizer="Nelder_Mead",
                                                             optCtrl=list(maxfun=2e5)))
  }
}

saveRDS(VDMint, "data/vdmint.rds")


# secret weapon -----

SWM = list()
years = c(2000, 2004, 2008, 2012, 2016)

for (i in ad_dvs){
  for (j in years){
    SWM[[paste0(i, ": ", j)]] <- bglmer(as.formula(paste(i, "~", "z_age + female + collegeed + unemployed +
                                                            lowincome + z_lcindex + z_pid + wny + wpy + 
                                                            z_ec_therm2a + z_rjp + (1 | state)")), 
                                        data=subset(AD, survyr == j), family = binomial(link="logit"),
                                        control=glmerControl(optimizer="Nelder_Mead",
                                                             optCtrl=list(maxfun=2e5)))
  }
}

# Some month differences in the ANES models... -----

ad_ivs_diffsc <- c("z_cunempr", "z_cunempr3md", "z_cunempr6md","z_cunempr12md")
ad_ivs_diffssn <- c("z_sunempr", "z_sunempr3md", "z_sunempr6md","z_sunempr12md",
                    "z_nunempr", "z_nunempr3md", "z_nunempr6md","z_nunempr12md")




ADMdiffsc = list()
for (i in ad_dvs){
  for (j in ad_ivs_diffsc){
    ADMdiffsc[[paste0(i, ": ", j)]] <- bglmer(as.formula(paste(i, "~", "z_age + female + collegeed + unemployed +
                                                            lowincome + z_lcindex + z_pid + wny + wpy + 
                                                            z_ec_therm2a +", j,
                                                         "  + (1 | survyr) + (1 | county) + (1 | county:survyr)")), 
                                        data=subset(AD), family = binomial(link="logit"),
                                        control=glmerControl(optimizer="Nelder_Mead",
                                                             optCtrl=list(maxfun=2e5)))
  }
}

ADMdiffssn = list()
for (i in ad_dvs){
  for (j in ad_ivs_diffssn){
    ADMdiffssn[[paste0(i, ": ", j)]] <- bglmer(as.formula(paste(i, "~", "z_age + female + collegeed + unemployed +
                                                            lowincome + z_lcindex + z_pid + wny + wpy + 
                                                            z_ec_therm2a +", j,
                                                               "  + (1 | survyr) + (1 | county) + (1 | county:survyr)")), 
                                              data=subset(AD), family = binomial(link="logit"),
                                              control=glmerControl(optimizer="Nelder_Mead",
                                                                   optCtrl=list(maxfun=2e5)))
  }
}

ADMdiffs <- append(ADMdiffsc, ADMdiffssn)

saveRDS(ADMdiffs, "data/admdiffs.rds")

# Some month differences for VSG models.... -----

vd_ivss <- c("z_sunempr", "z_sunempr3md", "z_sunempr6md","z_sunempr12md", "z_diffpunempben", "z_diffunempcompen")

VDMdiffs = list()
for (i in vd_dvs){
  for (j in vd_ivss){
    VDMdiffs[[paste0(i, ": ", j)]] <- bglmer(as.formula(paste(i, "~", "z_age + female + collegeed + unemployed +
                                                         lowincome + z_ideo + z_pid +
                                                         wpy + econgetw + z_ec_therm2b + wpy*", j,
                                                            "  +  (1 | state)")), 
                                           data=subset(VD), family = binomial(link="logit"),
                                           control=glmerControl(optimizer="Nelder_Mead",
                                                                optCtrl=list(maxfun=2e5)))
  }
}

saveRDS(VDMdiffs, "data/vdmdiffs.rds")




SWM %>%
  map(broom::tidy ) %>%
  map(~filter(., group == "fixed" & term != "(Intercept)")) %>%
  map2_df(names(.), ~mutate(.x,name=.y)) %>%
  separate(name, c("dv","year"), ": ") %>%
  mutate(term = forcats::fct_relevel(term,
                                     "z_age", "female", "collegeed", "unemployed",
                                     "z_incomeperc", "z_lcindex", "z_pid", "wny",
                                     "wpy", "z_ec", "z_rjp")) %>%
  mutate(term = forcats::fct_recode(term,
                                    "Age" = "z_age",
                                    "Female" = "female",
                                    "College Educated" = "collegeed",
                                    "Unemployed" = "unemployed",
                                    "Income Percentile" = "z_incomeperc",
                                    "Liberalism-Conservatism Index" = "z_lcindex",
                                    "Party ID (D to R)" = "z_pid",
                                    "Will Economy Get Worse?" = "wny",
                                    "Did Economy Get Worse?" = "wpy",
                                    "Ethnocentrism" = "z_ec",
                                    "State-level Exposure to Automation/Outsourcing" = "z_rjp")) %>%
  ggplot(.,aes(x=as.factor(year), y=estimate,
               ymin=estimate-(1.96*std.error),
               ymax=estimate+(1.96*std.error))) + 
  geom_hline(yintercept = 0, linetype="dashed") +
  theme_steve() + 
  geom_pointrange() +
  xlab("") + ylab("Coefficient Estimate") +
  facet_wrap(~term) +
  labs(
    caption = "Data: White Americans, ANES. Models subsetted to survey year include state random effects.") #, scales="free"

