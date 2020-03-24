ad_dvs = c("immigrld")
ad_ivs = c("z_sunempr", "z_nunempr", "z_rjp")

# Make ordinal tables...

ADM <- readRDS("~/Dropbox/projects/earr/data/adm.rds")
VDM <- readRDS("~/Dropbox/projects/earr/data/vdm.rds")
ADMB <- readRDS("~/Dropbox/projects/earr/data/admb.rds")
VDMB <- readRDS("~/Dropbox/projects/earr/data/vdmb.rds")
ADMdiffs <- readRDS("~/Dropbox/projects/earr/data/admdiffs.rds")
VDMdiffs <- readRDS("~/Dropbox/projects/earr/data/vdmdiffs.rds")
ADMec <- readRDS("~/Dropbox/projects/earr/data/admec.rds")
VDMec <- readRDS("~/Dropbox/projects/earr/data/vdmec.rds")
ADMwo <- readRDS("~/Dropbox/projects/earr/data/admwo.rds")
VDMwo <- readRDS("~/Dropbox/projects/earr/data/vdmwo.rds")
# library(broom.mixed)

fct_reorg <- function(fac, ...) {
  forcats::fct_recode(forcats::fct_relevel(fac, ...), ...)
}

ADMB %>%
  map(broom::tidy ) %>%
  map(~filter(., grepl("b_", term))) %>%
  map(~filter(., !grepl("Intercept", term))) %>%
  map(~mutate(.,
              term = str_remove(term, "b_|sd_"),
              term = ifelse(term %in% c(ad_ivs, "z_cunempr"), "econanxiety", term))) %>%
  map2_df(names(.), ~mutate(.x,name=.y)) %>%
  separate(name, c("dv","iv"), ": ") -> ADMBsums

ADM %>%
  map(broom::tidy ) %>%
  map(~filter(., group == "fixed" & term != "(Intercept)")) %>%
  map(~mutate(.,
              term = ifelse(term %in% c(ad_ivs, "z_cunempr"), "econanxiety", term),
              term = ifelse(grepl("sd_", term), group, term))) %>%
  map2_df(names(.), ~mutate(.x,name=.y)) %>%
  separate(name, c("dv","iv"), ": ") %>%
  bind_rows(ADMBsums, .) %>% tbl_df() %>%
  mutate(dv = ifelse(dv == "immigrld", "Manuscript Results", "Hierarchical Ordinal Logistic Models"),
         lwr = estimate - abs(qnorm(.025))*std.error,
         upr = estimate + abs(qnorm(.025))*std.error,
         iv = fct_reorg(iv,
                                  "County\nUnemployment Rate\n(1992-1996)" = "z_cunempr",
                                  "State\nUnemployment Rate\n(1992-2016)" = "z_sunempr",
                                  "National\nUnemployment Rate\n(1992-2016)" = "z_nunempr",
                                  "State-Level\nExposure to\nAutomation and Outsourcing\n(2000-2016)" = "z_rjp"),
         term = fct_reorg(term,
                          "Objective Economic Anxiety Indicator" = "econanxiety",
                          "Did Economy Get Worse?" = "wpy",
                          "Will Economy Get Worse?" = "wny",
                          "Unemployed" = "unemployed",
                          "Low Income" = "lowincome",
                          "Ethnocentrism" = "z_ec_therm2a",
                          "Party ID (D to R)" = "z_pid",
                          "Liberalism-Conservatism Index" = "z_lcindex",
                          "College Educated" = "collegeed",
                          "Female" = "female",
                          "Age" = "z_age")) %>%
    ggplot(.,aes(term, estimate, ymin=lwr, ymax=upr, color=dv, shape=dv)) +
  theme_steve_web() +
  facet_wrap(~iv,ncol=4) +
  geom_pointrange(position=position_dodge(width=.7), size=.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  labs(color = "",
       shape = "",
       x = "",
       y = "Coefficient (with 95% Intervals)") -> fig_ordinal

saveRDS(fig_ordinal, "appendix/data/fig_ordinal.rds")



ADMdiffs %>%
  map(broom::tidy ) %>%
  # map(~filter(., group == "fixed" & term != "(Intercept)")) %>%
  map(~mutate(.,
              term = ifelse(grepl("unempr", term), "econanxiety", term))) %>%
  map(~filter(., term %in% c("unemployed", "lowincome", "wny", "wpy", "econanxiety", "z_ec_therm2a"))) %>%
  map2_df(names(.), ~mutate(.x,name=.y)) %>%
  separate(name, c("dv","iv"), ": ") %>%
  mutate(lwr = estimate - abs(qnorm(.025))*std.error,
         upr = estimate + abs(qnorm(.025))*std.error) %>%
  mutate(term = fct_reorg(term,
                          "Objective Economic Anxiety Indicator" = "econanxiety",
                          "Did Economy Get Worse?" = "wpy",
                          "Will Economy Get Worse?" = "wny",
                          "Unemployed" = "unemployed",
                          "Low Income" = "lowincome",
                          "Ethnocentrism" = "z_ec_therm2a",
                          )) %>%
  mutate(iv = fct_reorg(iv,
                        "County Unemployment Rate\n(1992-1996)" = "z_cunempr",
                        "County Unemployment Rate\n(1992-1996)\n[3-Month Difference]" = "z_cunempr3md",
                        "County Unemployment Rate\n(1992-1996)\n[6-Month Difference]" = "z_cunempr6md",
                        "County Unemployment Rate\n(1992-1996)\n[12-Month Difference]" = "z_cunempr12md",
                        "State Unemployment Rate\n(1992-2016)" = "z_sunempr",
                        "State Unemployment Rate\n(1992-2016)\n[3-Month Difference]" = "z_sunempr3md",
                        "State Unemployment Rate\n(1992-2016)\n[6-Month Difference]" = "z_sunempr6md",
                        "State Unemployment Rate\n(1992-2016)\n[12-Month Difference]" = "z_sunempr12md",
                        "National Unemployment Rate\n(1992-2016)" = "z_nunempr",
                        "National Unemployment Rate\n(1992-2016)\n[3-Month Difference]" = "z_nunempr3md",
                        "National Unemployment Rate\n(1992-2016)\n[6-Month Difference]" = "z_nunempr6md",
                        "National Unemployment Rate\n(1992-2016)\n[12-Month Difference]" = "z_nunempr12md")) %>%
  ggplot(.,aes(term, estimate, ymin=lwr, ymax=upr)) +
  theme_steve_web() +
  facet_wrap(~iv) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype="dashed") +
  coord_flip() +
  labs(x = "",
       y = "Coefficient (with 95% Intervals)") -> fig_anes_diffs

saveRDS(fig_anes_diffs, "appendix/data/fig_anes_diffs.rds")

vd_ivss <- c("z_sunempr", "z_sunempr3md", "z_sunempr6md","z_sunempr12md", "z_diffpunempben", "z_diffunempcompen")


VDMdiffs %>%
  map(broom::tidy ) %>%
  map(~filter(., group == "fixed")) %>%
  map(~mutate(.,
              term = ifelse(term %in% vd_ivss, "econanxiety", term))) %>%
  map(~filter(., term %in% c("unemployed", "lowincome", "econgetw", "wpy", "econanxiety", "z_ec_therm2b"))) %>%
  map2_df(names(.), ~mutate(.x,name=.y)) %>%
  separate(name, c("dv","iv"), ": ") %>%
  mutate(lwr = estimate - abs(qnorm(.025))*std.error,
         upr = estimate + abs(qnorm(.025))*std.error) %>%
  mutate(term = fct_reorg(term,
                          "Objective Economic\nAnxiety Indicator" = "econanxiety",
                          "Did Economy Get Worse?" = "wpy",
                          "Will Economy Get Worse?" = "econgetw",
                          "Unemployed" = "unemployed",
                          "Low Income" = "lowincome",
                          "Ethnocentrism" = "z_ec_therm2b",
  )) %>% 
  mutate(dv = fct_reorg(dv,
                        "Immigrants are a Drain" = "immig_draind",
                        "Make Immigration Harder" = "immig_harderd")) %>%
  filter(iv != "z_sunempr") %>%
  mutate(iv = fct_reorg(iv,
                          "State\nUnemployment Rate\n(1992-2016)" = "z_sunempr",
                          "State\nUnemployment Rate\n(1992-2016)\n[3-Month Difference]" = "z_sunempr3md",
                          "State\nUnemployment Rate\n(1992-2016)\n[6-Month Difference]" = "z_sunempr6md",
                          "State\nUnemployment Rate\n(1992-2016)\n[12-Month Difference]" = "z_sunempr12md",
                        "Zip-Level Percentage\nof Tax Forms With\nUnemployment\nCompensation\n[12-Month Difference]" = "z_diffpunempben",
                        "Zip-Level Value\nof Unemployment\nCompensation\n[12-Month Difference]" = "z_diffunempcompen")) %>%
  ggplot(.,aes(term, estimate, ymin=lwr, ymax=upr)) +
  theme_steve_web() +
  facet_grid(dv ~ iv) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype="dashed") +
  coord_flip() +
  labs(x = "",
       y = "Coefficient (with 95% Intervals)") -> fig_vsg_diffs

saveRDS(fig_vsg_diffs, "appendix/data/fig_vsg_diffs.rds")



ADMec %>%
  map(broom::tidy ) %>%
  map(~mutate(., term = ifelse( grepl("ec_", term), "Ethnocentrism", term))) %>%
  map(~filter(., term %in% c("Ethnocentrism", "wny", "wpy", "z_rjp", "lowincome", "unemployed"))) %>%
  map2_df(names(.), ~mutate(.x,name=.y)) %>%
  separate(name, c("dv","iv"), ": ") %>%
  #filter(term == "Ethnocentrism") %>%
  mutate(iv = fct_reorg(iv,
                          "Ethnocentrism\n(Outgroup: Black People)" = "z_ec_therm1",
                        "Ethnocentrism\n(Outgroups: Black People, Hispanics)" = "z_ec_therm2a",
                        "Ethnocentrism\n(Outgroups: Black People, Asians)" = "z_ec_therm2b",
                        "Ethnocentrism\n(Outgroups: Black People, Hispanics, Asians)" = "z_ec_therm3")) %>%
  mutate(term = fct_reorg(term,
                          "State-Level Exposure\nto Automation and Outsourcing" = "z_rjp",
                          "Did Economy Get Worse?" = "wpy",
                          "Will Economy Get Worse?" = "wny",
                          "Unemployed" = "unemployed",
                          "Low Income" = "lowincome",
                          "Ethnocentrism" = "Ethnocentrism",
  )) %>%
  mutate(lwr = estimate - abs(qnorm(.025))*std.error,
         upr = estimate + abs(qnorm(.025))*std.error) %>%
  ggplot(.,aes(term, estimate, ymin=lwr, ymax=upr, color=iv, shape=iv)) +
  geom_hline(yintercept = 0, linetype="dashed") +
  theme_steve_web() +
  #facet_wrap(~iv) + 
  geom_pointrange(position=position_dodge(width=.7), size=.8, fill="white") +
  coord_flip() +
  theme(legend.text.align = 0.5) +
  scale_colour_brewer(palette = "Set1") +
  scale_shape_manual(values=c(21, 22, 23, 24)) +
  labs(y = "Coefficient (with 95% Intervals)",
       shape = "",
       color = "",
       x = "")  -> fig_anes_ec

saveRDS(fig_anes_ec, "appendix/data/fig_anes_ec.rds")

VDMec %>%
  map(broom::tidy ) %>%
  map(~mutate(., term = ifelse( grepl("ec_", term), "Ethnocentrism", term))) %>%
  map(~filter(., term %in% c("Ethnocentrism", "econgetw", "wpy", "z_rjps", "lowincome", "unemployed"))) %>%
  map2_df(names(.), ~mutate(.x,name=.y)) %>%
  separate(name, c("dv","iv"), ": ")  %>%
  #filter(term == "Ethnocentrism") %>%
  mutate(iv = fct_reorg(iv,
                        "Ethnocentrism\n(Outgroup: Black People)" = "z_ec_therm1",
                        "Ethnocentrism\n(Outgroups: Black People, Hispanics)" = "z_ec_therm2a",
                        "Ethnocentrism\n(Outgroups: Black People, Asians)" = "z_ec_therm2b",
                        "Ethnocentrism\n(Outgroups: Black People, Hispanics, Asians)" = "z_ec_therm3")) %>%
  mutate(term = fct_reorg(term,
                          "State-Level Exposure\nto Automation and Outsourcing" = "z_rjps",
                          "Personal Finances Got Worse" = "wpy",
                          "Economy is Getting Worse" = "econgetw",
                          "Unemployed" = "unemployed",
                          "Low Income" = "lowincome",
                          "Ethnocentrism" = "Ethnocentrism",
  ))  %>%
  mutate(dv = ifelse(dv == "immig_draind", "Immigrants are a Drain", "Make Immigration Harder")) %>%
  mutate(lwr = estimate - abs(qnorm(.025))*std.error,
         upr = estimate + abs(qnorm(.025))*std.error) %>%
  ggplot(.,aes(term, estimate, ymin=lwr, ymax=upr, color=iv, shape=iv)) +
  geom_hline(yintercept = 0, linetype="dashed") +
  theme_steve_web() +
  facet_wrap(~dv) + 
  geom_pointrange(position=position_dodge(width=.7), size=.8, fill="white") +
  coord_flip() +
  theme(legend.text.align = 0.5) +
  scale_colour_brewer(palette = "Set1") +
  scale_shape_manual(values=c(21, 22, 23, 24)) +
  labs(y = "Coefficient (with 95% Intervals)",
       shape = "",
       color = "",
       x = "") -> fig_vsg_ec

saveRDS(fig_vsg_ec, "appendix/data/fig_vsg_ec.rds")
  


ADMwo %>%
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
  set_caption('\\label{tab:ADMwo}The Covariates of White American Attitudes Toward Decreasing Immigration (ANES)') %>%
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
             as.character(prettyNum(nobs(ADMwo[[1]]))),
             as.character(prettyNum(nobs(ADMwo[[2]]))),
             as.character(prettyNum(nobs(ADMwo[[3]]))),
             as.character(prettyNum(nobs(ADMwo[[4]]))), after=32) %>%
  filter(names != "N") %>%
  set_top_border(., 32, 1:5, .4, TRUE) %>%
  insert_row(., "", "", "", "", "", after = 26) %>%
  insert_row(., "Random Effect", "", "", "", "", after = 27) %>%
  set_italic(., 28, 1, TRUE) %>%
  set_position(.,"center") %>%
  set_width(., .9) %>%
  set_align(1:nrow(.), 2:ncol(.), 'center') %>%
  set_wrap(., 1:nrow(.), 1, FALSE)  %>%
  insert_row(., "", "", "", "", "", after = 18) %>%
  insert_row(., "Economic Anxiety Variables", "", "", "", "", after = 19) %>%
  set_italic(., 20, 1, TRUE) %>%
  set_italic(2:6, 1:ncol(.), TRUE) %>%
  set_col_width(., 1, .9) -> tab_admwo


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
                   "Low Income" = "lowincome",
                   "Unemployed" = "unemployed",
                   "Economy is Getting Worse" = "econgetw",
                   "Personal Finances Got Worse" = "wpy",
                   "Objective Indicator" = "econanxiety",
                   "sd(State)" = "state"
         ))  %>%
  filter_all(all_vars(!grepl('(NA)',.))) %>%
  set_caption('\\label{tab:VDMdrainwo}The Covariates of White American Attitudes Toward Thinking of Immigrants as a Drain
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
             as.character(prettyNum(nobs(VDM[[7]]))), after=28) %>%
  filter(names != "N") %>%
  set_top_border(., 28, 1:ncol(.), .4, TRUE) %>%
  add_rows(., matrix(c("","Random Effect",
                       "","",
                       "","",
                       # "","",
                       # "","",
                       "","",
                       "","",
                       "",""), 2, 6), after=26) %>%
  set_italic(., 28, 1, TRUE) %>%
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
  set_wrap(., 1:nrow(.), 1, FALSE) -> tab_vdm_drainwo


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
                   "Low Income" = "lowincome",
                   "Unemployed" = "unemployed",
                   "Economy is Getting Worse" = "econgetw",
                   "Personal Finances Got Worse" = "wpy",
                   "Objective Indicator" = "econanxiety",
                   "sd(State)" = "state"
         ))  %>%
  filter_all(all_vars(!grepl('(NA)',.))) %>%
  set_caption('\\label{tab:VDMharderwo}The Covariates of White American Attitudes Toward Making it Harder to Immigrate to
the U.S. (VSG, July 2017)') %>%
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
             as.character(prettyNum(nobs(VDM[[8]]))),
             as.character(prettyNum(nobs(VDM[[9]]))),
             as.character(prettyNum(nobs(VDM[[10]]))),
             #            as.character(prettyNum(nobs(VDM[[4]]))),
             #           as.character(prettyNum(nobs(VDM[[5]]))), 
             as.character(prettyNum(nobs(VDM[[13]]))),
             as.character(prettyNum(nobs(VDM[[14]]))), after=28) %>%
  filter(names != "N") %>%
  set_top_border(., 28, 1:ncol(.), .4, TRUE) %>%
  add_rows(., matrix(c("","Random Effect",
                       "","",
                       "","",
                       # "","",
                       # "","",
                       "","",
                       "","",
                       "",""), 2, 6), after=26) %>%
  set_italic(., 28, 1, TRUE) %>%
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
  set_wrap(., 1:nrow(.), 1, FALSE) -> tab_vdm_harderwo

saveRDS( tab_admwo, "appendix/data/tab_admwo.rds")
saveRDS( tab_vdm_harderwo, "appendix/data/tab_vdm_harderwo.rds")
saveRDS( tab_vdm_drainwo, "appendix/data/tab_vdm_drainwo.rds")







