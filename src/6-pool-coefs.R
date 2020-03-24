if (exists("ADM") &
    exists("VDM")) {
  pi
} else {
  ADM <- readRDS("data/adm.rds")
  VDM <- readRDS("data/vdm.rds")
}


extract_pooledvars_anes <-  function (data) {
  data %>%
    map(broom::tidy ) %>%
    map(~mutate(.,
                term = ifelse(term %in% c("z_cunempr", "z_sunempr", "z_nunempr", "z_rjp"), "econanxiety", term),
                term = ifelse(grepl("sd_", term), group, term))) %>%
    map(~filter(., term %in% c("unemployed", "lowincome", "wny", "wpy", "econanxiety",  "z_ec_therm2a"))) %>%
    map2_df(names(.), ~mutate(.x,name=.y))
}

extract_pooledvars_vsg <-  function (data) {
  data %>%
    map(broom::tidy) %>%
    map(~mutate(.,
                term = ifelse(term %in% c("z_sunempr", "z_percunempben", "z_avgunempcompen", "z_rjpcbsa","z_rjps"),
                              "econanxiety", term),
                term = ifelse(grepl("sd_",term), group, term))) %>%
    map(~filter(.,term %in% c("unemployed", "lowincome", "econgetw","wpy","econanxiety", "z_ec_therm2b"))) %>%
    map2_df(names(.), ~mutate(.x,name=.y))
}





ADM %>%
  extract_pooledvars_anes -> ANESp

# Drain...
VDM %>%
  .[c(1:3, 6, 7)] %>%
  extract_pooledvars_vsg -> VSGpd

# Harder...
VDM %>%
  .[c(8:10, 13, 14)] %>%
  extract_pooledvars_vsg -> VSGph


bind_rows(ANESp, VSGpd, VSGph) %>%
  mutate(term = ifelse(grepl("therm", term), "Ethnocentrism", term),
         term = ifelse(term %in% c("wpy"), "Negative Retrospective Evaluation", term),
         term = ifelse(term %in% c("econgetw","wny"),"Negative Prospective Evaluation", term),
         term = ifelse(term == "econanxiety", "Objective Economic Anxiety Indicator", term),
         term = ifelse(term == "lowincome", "Low Income", term),
         term = stringr::str_to_title(term)) %>%
  separate(name, c("dv","iv"), ": ") %>%
  group_by(dv, term) %>%
  summarize(avgest = mean(estimate),
            varw = sum(std.error^2)/n_distinct(iv),
            varb = sum((estimate - avgest)^2)/(n_distinct(iv)-1),
            poolse = sqrt(varw + varb + (varb/n_distinct(iv))),
            meansd = mean(std.error),
            zstat = avgest/poolse,
            pval = abs(qnorm(.025))*pnorm(-abs(zstat))) %>%
  mutate(Analysis = forcats::fct_recode(dv,
                                        "VSG\n(Immigrants Are A Drain)" = "immig_draind",
                                        "VSG\n(Make Immigration Harder)" = "immig_harderd",
                                        "ANES\n(Decrease Immigration Levels)" = "immigrld")) %>%
  mutate(term = forcats::fct_relevel(term,
                                     "Unemployed",
                                     "Objective Economic Anxiety Indicator",
                                     "Negative Retrospective Evaluation",
                                     "Negative Prospective Evaluation",
                                     "Low Income",
                                     "Ethnocentrism")) %>%
  ggplot(.,aes(x=term, y=avgest, 
               ymin=(avgest - 1.96*poolse),
               ymax=(avgest + 1.96*poolse))) +
  facet_wrap(~Analysis) +
  geom_pointrange(fill = "WHITE",
                  shape = 21,
                  size=.7) + 
  theme_steve_web() +
  coord_flip() +
  geom_hline(yintercept=0, linetype="dashed") +
  xlab("") + 
  ylab("Pooled Coefficients (and 95% Confidence Intervals)") +
  labs(caption = "Coefficient and standard error pooling done via Rubin's (1987) rules for combining results from multiple data sets.") -> fig_pooled

saveRDS(fig_pooled, "data/fig_pooled.rds")
