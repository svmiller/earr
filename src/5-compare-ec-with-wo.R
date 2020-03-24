if (exists("ADM") &
    exists("ADMwo") &
    exists("VDM") &
    exists("VDMwo")) {
  pi
} else {
  ADM <- readRDS("data/adm.rds")
  ADMwo <- readRDS("data/admwo.rds")
  VDM <- readRDS("data/vdm.rds")
  VDMwo <- readRDS("data/vdmwo.rds")
}

# Get ANES graph...

# ad_ivs = c("z_cunempr", "z_sunempr", "z_nunempr", "z_rjp")

extract_econcovars_anes <-  function (data) {
 data %>%
    map(broom::tidy ) %>%
    map(~mutate(.,
                term = ifelse(term %in% c("z_cunempr", "z_sunempr", "z_nunempr", "z_rjp"), "econanxiety", term),
                term = ifelse(grepl("sd_", term), group, term))) %>%
    map(~filter(., term %in% c("unemployed", "lowincome", "wny", "wpy", "econanxiety"))) %>%
    map2_df(names(.), ~mutate(.x,name=.y))
}


ADM %>%
  extract_econcovars_anes %>%
  mutate(group = "With Ethnocentrism") -> ADMWEC


ADMwo %>%
  extract_econcovars_anes %>%
  mutate(group = "Without Ethnocentrism") -> ADMWOEC


bind_rows(ADMWEC, ADMWOEC) %>%
  separate(name, c("dv","iv"), ": ") %>%
  mutate(term = forcats::fct_recode(term,
                                    "Objective Economic\nAnxiety Indicator" = "econanxiety",
                                    "Low Income" = "lowincome",
                                    "Unemployed" = "unemployed",
                                    "Will Economy Get Worse?" = "wny",
                                    "Did Economy Get Worse?" = "wpy")) %>%
  mutate(lwr = estimate - abs(qnorm(.025))*std.error,
         upr = estimate + abs(qnorm(.025))*std.error) %>%
  ggplot(.,aes(factor(iv, levels = c("z_cunempr", "z_sunempr",
                                     "z_nunempr", "z_rjp")), 
               estimate, ymin=lwr, ymax=upr, group=group, color=group, shape=group)) + 
  theme_steve_web() +
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_pointrange(position=position_dodge(width=.7), size=.8) +
  facet_wrap(~term) + coord_flip() +
  scale_x_discrete(labels = c("County Unemployment Rate\n(1992-1996)",
                              "State Unemployment Rate\n(1992-2016)",
                              "National Unemployment Rate\n(1992-2016)",
                              "State-level Exposure to\nAutomation and Outsourcing\n(2000-2016)")) +
  theme(axis.text.y=element_text(hjust=0.5)) +
  labs(y = "Coefficient (with 95% Intervals)",
       x = "",
       shape = "",
       color = "") -> fig_anes_wwo
  
# Get VSG group...
# we're going to start with the immig_draind ones
# We're wanting just state unemp, % perc, z_rjpcbsa and z_rjps
# These are elements 1, 2, 3, 6, 7

# Let's create another function, but for VSG stuff
  
extract_econcovars_vsg <-  function (data) {
  data %>%
  map(broom::tidy) %>%
    map(~mutate(.,
                term = ifelse(term %in% c("z_sunempr", "z_percunempben", "z_avgunempcompen", "z_rjpcbsa","z_rjps"),
                              "econanxiety", term),
                term = ifelse(grepl("sd_",term), group, term))) %>%
    map(~filter(.,term %in% c("unemployed", "lowincome", "econgetw","wpy","econanxiety"))) %>%
    map2_df(names(.), ~mutate(.x,name=.y))

    }

# Drain...
VDM %>%
  .[c(1:3, 6, 7)] %>%
  extract_econcovars_vsg %>%
  mutate(group = "With Ethnocentrism") -> VDMWECd

VDMwo %>%
  .[c(1:3, 6, 7)] %>%
  extract_econcovars_vsg %>%
  mutate(group = "Without Ethnocentrism") -> VDMWOECd

# Harder...
VDM %>%
  .[c(8:10, 13, 14)] %>%
  extract_econcovars_vsg %>%
  mutate(group = "With Ethnocentrism") -> VDMWECh

VDMwo %>%
  .[c(8:10, 13, 14)] %>%
  extract_econcovars_vsg %>%
  mutate(group = "Without Ethnocentrism") -> VDMWOECh

# Another function for cleaining this shit


clean_binded_vsg <-  function (data) {
  data %>%
    separate(name, c("dv","iv"), ": ")  %>%
    mutate(term = forcats::fct_recode(term,
                                      "Objective Economic\nAnxiety Indicator" = "econanxiety",
                                      "Low Income" = "lowincome",
                                      "Unemployed" = "unemployed",
                                      "Economy is Getting Worse" = "econgetw",
                                      "Personal Finances Got Worse" = "wpy")) %>%
    mutate(lwr = estimate - abs(qnorm(.025))*std.error,
           upr = estimate + abs(qnorm(.025))*std.error) %>%
    ggplot(.,aes(factor(iv, levels = c("z_sunempr",  "z_percunempben",
                                       "z_avgunempcompen", "z_rjpcbsa",
                                       "z_rjps")), 
                 estimate, ymin=lwr, ymax=upr, group=group, color=group, shape=group)) +
    theme_steve_web() +
    geom_hline(yintercept = 0, linetype="dashed") +
    geom_pointrange(position=position_dodge(width=.7), size=.8) +
    facet_wrap(~term) + coord_flip() +
    scale_x_discrete(labels = c("State Unemployment Rate",
                                "ZIP-level % of Tax Returns\nw/ Unemployment Compensation",
                                "ZIP-level Average\nUnemployment Compensation",
                                "CBSA-level Exposure to\nAutomation & Outsourcing",
                                "State-level Exposure to\nAutomation & Outsourcing")) +
    theme(axis.text.y=element_text(hjust=0.5)) +
    labs(y = "Coefficient (with 95% Intervals)",
         x = "",
         color = "",
         shape = "")
}



bind_rows(VDMWECd, VDMWOECd) %>%
  clean_binded_vsg  -> fig_vsg_wwo_drain

bind_rows(VDMWECh, VDMWOECh) %>%
  clean_binded_vsg -> fig_vsg_wwo_harder

list("fig_anes_wwo" = fig_anes_wwo,
     "fig_vsg_wwo_drain" = fig_vsg_wwo_drain,
     "fig_vsg_wwo_harder" = fig_vsg_wwo_harder) -> figs_wwo

saveRDS(figs_wwo, "data/figs_wwo.rds")
