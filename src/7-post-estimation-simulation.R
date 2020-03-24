AD <- readRDS("data/ad.rds")
VD <- readRDS("data/vd.rds")

library(modelr)

AD %>%
  sjlabelled::remove_all_labels(.) %>%
  data_grid(.model=ADM[[1]], female = 0,
            immigrld = 0,
            survyr = 0, county = 0,
            z_rjp = 0,
            z_ec_therm2a = c(0, .5, 1)) %>%
  mutate_at(vars(contains("z_"), -z_ec_therm2a), list(~replace(., . != 0, 0))) %>%
  bind_rows(., .[1, ] %>% mutate_at(c("unemployed","lowincome","wny","wpy", "z_cunempr","z_rjp"), ~ifelse(. == 0, 1, .))) -> newdat_anes

catlabels <- c("Typical White Male and...",
               "...One S.D. Increase in Ethnocentrism",
               "...Two S.D. Increase in Ethnocentrism",
               "...Unemployed AND\nLow Income AND\nNegative Prospective Evaluation AND\nNegative Retrospective Evaluation AND\nTwo S.D. Increase in Objective Economic Anxiety Indicator")

get_sims(ADM[[1]], newdat_anes, 1000, 8675309) %>%
  mutate(fit = boot::inv.logit(y)) %>%
  mutate(Category = as_factor(rep(catlabels, 1000)),
         Model = "ANES\n(County Unemployment Rate)\n[Decrease Immigration Levels]") -> ADM1sims

get_sims(ADM[[4]], newdat_anes, 1000, 8675309) %>%
  mutate(fit = boot::inv.logit(y)) %>%
  mutate(Category = as_factor(rep(catlabels, 1000)),
         Model = "ANES\n(State-Level Exposure to\nAutomation/Outsourcing)\n[Decrease Immigration Levels]") -> ADM4sims

VD %>%
  sjlabelled::remove_all_labels() %>%
  data_grid(.model=VDM[[1]], immig_draind = 0, immig_harderd = 0, state = 0,
            z_rjpcbsa = 0,
            z_ec_therm2b = c(0, .5, 1)) %>%
  mutate_at(vars(contains("z_"), -z_ec_therm2b), list(~replace(., . != 0, 0)))   %>%
  bind_rows(., .[1, ] %>% mutate_at(c("unemployed","lowincome","econgetw","wpy", "z_sunempr","z_rjpcbsa"), ~ifelse(. == 0, 1, .))) -> newdat_vsg

get_sims(VDM[[6]], newdat_vsg, 1000, 8675309) %>%
  mutate(fit = boot::inv.logit(y)) %>%
  mutate(Category = as_factor(rep(catlabels, 1000)),
         Model = "VSG\n(CBSA-level Exposure to\nAutomation/Outourcing)\n[Immigrants are a Drain]") -> VDM6sims

get_sims(VDM[[8]], newdat_vsg, 1000, 8675309) %>%
  mutate(fit = boot::inv.logit(y)) %>%
  mutate(Category = as_factor(rep(catlabels, 1000)),
         Model = "VSG\n(State Unemployment Rate)\n[Make Immigration Harder]") -> VDM8sims

bind_rows(ADM1sims, ADM4sims, VDM6sims, VDM8sims) %>%
  group_by(Model, Category) %>%
  summarize(mean = mean(fit),
            lwr = quantile(fit, .025),
            upr = quantile(fit, .975)) %>%
  mutate(Category = forcats::fct_rev(Category)) %>%
  ggplot(.,aes(x=Category, y=mean, ymin=lwr, 
               ymax=upr)) +
  facet_wrap(~Model) +
  geom_pointrange(shape = 21, 
                  fill = "WHITE",
                  position = position_dodge(.4),
                  size=.7) + 
  theme_steve_web() +
  coord_flip() + 
  geom_hline(yintercept=.5, linetype="dashed") +
  xlab("") + ylab("Simulated Probability (and 95% Confidence Intervals)") -> fig_pes

saveRDS(fig_pes,"data/fig_pes.rds")
