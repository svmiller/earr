ad_dvs = c("immigrld")
ad_ivs = c("z_sunempr", "z_nunempr", "z_rjp")
library(huxtable)

ADM %>%
  map(broom::tidy ) %>%
  map(~mutate(.,
              term = ifelse(term %in% c(ad_ivs, "z_cunempr"), "econanxiety", term),
              term = ifelse(grepl("sd_", term), group, term))) %>%
  #              term = ifelse(term == "sd__(Intercept)", group, term))) 
  unname(.) %>%
  huxreg(.,
         coefs = c("Age" = "z_age",
                   "Female" = "female",
                   "College Educated" = "collegeed",
                   "Liberalism-Conservatism Index" = "z_lcindex",
                   "Party ID (D to R)" = "z_pid",
                   "Ethnocentrism" = "z_ec_therm2a",
                   "Low Income" = "lowincome",
                   "Unemployed" = "unemployed",
                   "Will Economy Get Worse?" = "wny",
                   "Did Economy Get Worse?" = "wpy",
                   "Objective Indicator" = "econanxiety",
                   "sd(County)" = "county",
                   "sd(County-Year)" = "county:survyr",
                   "sd(Year)" = "survyr",
                   "sd(State)" = "state",
                   "sd(State-Year)" = "state:survyr"
         )) %>%
  filter_all(all_vars(!grepl('(NA)',.))) %>%
  set_caption('\\label{tab:ADM}The Covariates of White American Attitudes Toward Decreasing Immigration (ANES)') %>%
  add_rows(., matrix(c("","","","","",
                       "", "County","Unemployment","Rate","(1992-1996)",
                       "", "State", "Unemployment", "Rate","(1992-2016)",
                       "",  "National","Unemployment","Rate","(1992-2016)",
                       "State-Level","Exposure to","Automation and","Outsourcing","(2000-2016)",
                       "","","","",""), 5, 5), after=1) %>%
  set_latex_float(., "!htbp") %>%
  set_all_borders(., 2:6, 1:5, 0, TRUE) %>%
  set_top_border(.,7, 1:5, .4, TRUE)  %>%
  insert_row(., "Num. Obs.",
             as.character(prettyNum(nobs(ADM[[1]]))),
             as.character(prettyNum(nobs(ADM[[2]]))),
             as.character(prettyNum(nobs(ADM[[3]]))),
             as.character(prettyNum(nobs(ADM[[4]]))), after=34) %>%
  filter(names != "N") %>%
  set_top_border(., 34, 1:5, .4, TRUE) %>%
  insert_row(., "", "", "", "", "", after = 28) %>%
  insert_row(., "Random Effect", "", "", "", "", after = 29) %>%
  set_italic(., 30, 1, TRUE) %>%
  set_position(.,"center") %>%
  set_width(., .9) %>%
  set_align(1:nrow(.), 2:ncol(.), 'center') %>%
  set_wrap(., 1:nrow(.), 1, FALSE)  %>%
  insert_row(., "", "", "", "", "", after = 18) %>%
  insert_row(., "Economic Anxiety Variables", "", "", "", "", after = 19) %>%
  set_italic(., 20, 1, TRUE) %>%
  set_italic(2:6, 1:ncol(.), TRUE) %>%
  set_col_width(., 1, .9) -> tab_adm


# VSG models -----

vd_dvs = c("immig_draind", "immig_harderd")
vd_ivs = c("z_sunempr", "z_percunempben", "z_avgunempcompen", "z_diffpunempben", "z_diffunempcompen",
           "z_rjpcbsa", "z_rjps")


VDM[c(1:3,6:7)] %>%
  map(broom::tidy) %>%
  map(~mutate(.,
              term = ifelse(term %in% vd_ivs, "econanxiety", term),
              term = ifelse(grepl("sd_", term), group, term))) %>%
  unname(.)  %>%
  huxreg(.,
         coefs = c("Age" = "z_age",
                   "Female" = "female",
                   "College Educated" = "collegeed",
                   "Ideology (L to C)" = "z_ideo",
                   "Party ID (D to R)" = "z_pid",
                   "Ethnocentrism" = "z_ec_therm2b",
                   "Low Income" = "lowincome",
                   "Unemployed" = "unemployed",
                   "Economy is Getting Worse" = "econgetw",
                   "Personal Finances Got Worse" = "wpy",
                   "Objective Indicator" = "econanxiety",
                   "sd(State)" = "state"
         ))  %>%
  filter_all(all_vars(!grepl('(NA)',.))) %>%
  set_caption('\\label{tab:VDMdrain}The Covariates of White American Attitudes Toward Thinking of Immigrants as a Drain
on American Society (VSG, July 2017)') %>%
  add_rows(., matrix(c("","","","","",
                       "","","State","Unemployment","Rate",
                       "","ZIP-level","% of Tax Returns","w/ Unemployment","Compensation",
                       "", "ZIP-level", "Average","Unemployment","Compensation",
#                       "ZIP-level","12-mo. Change","in % of Tax Returns","w/ Unemployment","Compensation",
#                       "ZIP-level","12-mo. Change","in Average","Unemployment","Compensation",
                       "","CBSA-level","Exposure to","Automation &","Outsourcing",
                       "","State-level","Exposure to","Automation &","Outsourcing"), 5, 6), after=1) %>%
  set_latex_float(., "!htbp") %>%
  set_all_borders(., 2:6, 1:ncol(.), 0, TRUE) %>%
  set_top_border(.,7, 1:ncol(.), .4, TRUE) %>%
  insert_row(., "Num. Obs.",
             as.character(prettyNum(nobs(VDM[[1]]))),
             as.character(prettyNum(nobs(VDM[[2]]))),
             as.character(prettyNum(nobs(VDM[[3]]))),
 #            as.character(prettyNum(nobs(VDM[[4]]))),
  #           as.character(prettyNum(nobs(VDM[[5]]))), 
             as.character(prettyNum(nobs(VDM[[6]]))),
             as.character(prettyNum(nobs(VDM[[7]]))), after=30) %>%
filter(names != "N") %>%
  set_top_border(., 30, 1:ncol(.), .4, TRUE) %>%
  add_rows(., matrix(c("","Random Effect",
                       "","",
                       "","",
                       # "","",
                       # "","",
                       "","",
                       "","",
                       "",""), 2, 6), after=28) %>%
  set_italic(., 30, 1, TRUE) %>%
  add_rows(., matrix(c("","Economic Anxiety Variables",
                       "","",
                       "","",
                     #  "","",
                   #    "","",
                       "","",
                       "","",
                       "",""), 2, 6), after=18)  %>%
  set_italic(., 20, 1, TRUE) %>%
  set_italic(2:6, 1:ncol(.), TRUE) %>%
  set_position(.,"center") %>%
  set_width(., .9) %>%
  set_align(1:nrow(.), 2:ncol(.), 'center') %>%
  set_wrap(., 1:nrow(.), 1, FALSE) -> tab_vdm_drain



VDM[c(8:10, 13, 14)] %>%
  map(broom::tidy) %>%
  map(~mutate(.,
              term = ifelse(term %in% vd_ivs, "econanxiety", term),
              term = ifelse(grepl("sd_", term), group, term))) %>%
  unname(.)  %>%
  huxreg(.,
         coefs = c("Age" = "z_age",
                   "Female" = "female",
                   "College Educated" = "collegeed",
                   "Ideology (L to C)" = "z_ideo",
                   "Party ID (D to R)" = "z_pid",
                   "Ethnocentrism" = "z_ec_therm2b",
                   "Low Income" = "lowincome",
                   "Unemployed" = "unemployed",
                   "Economy is Getting Worse" = "econgetw",
                   "Personal Finances Got Worse" = "wpy",
                   "Objective Indicator" = "econanxiety",
                   "sd(State)" = "state"
         ))  %>%
  filter_all(all_vars(!grepl('(NA)',.))) %>%
  set_caption('\\label{tab:VDMharder}The Covariates of White American Attitudes Toward Making it Harder to Immigrate to
the U.S. (VSG, July 2017)') %>%
  add_rows(., matrix(c("","","","","",
                       "","","State","Unemployment","Rate",
                       "","ZIP-level","% of Tax Returns","w/ Unemployment","Compensation",
                       "", "ZIP-level", "Average","Unemployment","Compensation",
                       #"ZIP-level","12-mo. Change","in % of Tax Returns","w/ Unemployment","Compensation",
                       #"ZIP-level","12-mo. Change","in Average","Unemployment","Compensation",
                       "","CBSA-level","Exposure to","Automation &","Outsourcing",
                       "","State-level","Exposure to","Automation &","Outsourcing"), 5, 6), after=1) %>%
  set_latex_float(., "!htbp") %>%
  set_all_borders(., 2:6, 1:ncol(.), 0, TRUE) %>%
  set_top_border(.,7, 1:ncol(.), .4, TRUE) %>%
  insert_row(., "Num. Obs.",
             as.character(prettyNum(nobs(VDM[[8]]))),
             as.character(prettyNum(nobs(VDM[[9]]))),
             as.character(prettyNum(nobs(VDM[[10]]))),
         #    as.character(prettyNum(nobs(VDM[[11]]))),
        #     as.character(prettyNum(nobs(VDM[[12]]))), 
             as.character(prettyNum(nobs(VDM[[13]]))),
             as.character(prettyNum(nobs(VDM[[14]]))), after=30) %>%
  filter(names != "N") %>%
  set_top_border(., 30, 1:ncol(.), .4, TRUE) %>%
  add_rows(., matrix(c("","Random Effect",
                       "","",
                       "","",
                     #  "","",
                     #  "","",
                       "","",
                       "","",
                       "",""), 2, 6), after=28) %>%
  set_italic(., 30, 1, TRUE) %>%
  add_rows(., matrix(c("","Economic Anxiety Variables",
                       "","",
                       "","",
                     #  "","",
                    #   "","",
                       "","",
                       "","",
                       "",""), 2, 6), after=18) %>%
  set_italic(., 20, 1, TRUE) %>%
  set_italic(2:6, 1:ncol(.), TRUE) %>%
  set_position(.,"center") %>%
  set_width(., .9) %>%
  set_align(1:nrow(.), 2:ncol(.), 'center') %>%
  set_wrap(., 1:nrow(.), 1, FALSE) -> tab_vdm_harder


list("tab_adm" = tab_adm,
     "tab_vdm_drain" = tab_vdm_drain,
     "tab_vdm_harder" = tab_vdm_harder) -> tabs

saveRDS(tabs,"data/tabs.rds")
