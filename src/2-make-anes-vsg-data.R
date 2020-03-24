library(tidyverse)
library(stevemisc)

# Load ANES data -----

a92 <- haven::read_dta("~/Dropbox/data/anes/1992/NES1992.dta")
a93 <- haven::read_dta("~/Dropbox/data/anes/1993/NESPIL93.dta")
a94 <- haven::read_dta("~/Dropbox/data/anes/1994/NES1994.dta")
a96 <- haven::read_dta("~/Dropbox/data/anes/1996/nes96.dta")
a97 <- haven::read_dta("~/Dropbox/data/anes/1997/NESPIL97.dta")
a98 <- haven::read_dta("~/Dropbox/data/anes/1998/nes1998.dta")
# asked pre in 2000
a00 <- haven::read_dta("~/Dropbox/data/anes/2000/anes2000TS.dta")
# a02 <- haven::read_dta("~/Dropbox/data/anes/2002/anes2002TS.dta")
# back to post in 2004, 2008, 2012, 2016
a04 <- haven::read_dta("~/Dropbox/data/anes/2004/anes2004TS.dta")
a08 <- haven::read_dta("~/Dropbox/data/anes/2008/anes_timeseries_2008.dta")
a12 <- haven::read_dta("~/Dropbox/data/anes/2012/anes_timeseries_2012.dta")
a16 <- haven::read_dta("~/Dropbox/data/anes/2016-ts/anes_timeseries_2016.dta")

ANES <- haven::read_dta("~/Dropbox/data/anes/anes_timeseries_cdf_dta-2016/anes_timeseries_cdf.dta")
# ANESP0809 <- haven::read_dta("~/Dropbox/data/anes/2008-09-panel/anes2008_2009panel_dataset.dta")
#
# ANESP0809 %>% select_at(vars(contains()))

# Load VSG data -----

#VSG <- haven::read_dta("~/Dropbox/data/voter-study-group/VOTER_Survey_July17_Release1-dta.dta")
VSG <- haven::read_dta("~/Dropbox/data/voter-study-group/VOTER_Survey_April18_Release1.dta")

# Let's clean VSG first because there are more moving pieces here. -----
# Let's note here that I goofed on the first set of things I did.
# Namely, the "legally" qualifier for the "drain" question is introduced immi_contribution_b in 2017.
# Everything else around that is "illegally." Thus, I really can't do any panel stuff here.
# Further, I'm going to have to re-do some of the economic stuff, but that shouldn't be too difficult.
# Tedious, maybe, but not too difficult.

# Clean Voter Study Group (VSG) -----
# sjmisc::find_var(a93, "month", out = "table")
# 
# VSG %>%
#   mutate(survyr = 2016,
#          case = case_identifier,
#          idate = lubridate::ymd("2016-12-01"),
#          statefips = inputstate_2016,
#          zip = izip_2016,
#          # Recode these later...
#          immig_drain = immi_contribution_2016, # car::recode(immi_contribution_2016, "1:2=0;3=1;8=NA"),
#          immig_harder = immi_makedifficult_2016, # car::recode(immi_makedifficult_2016, "1:3=0; 4:5=1; 8=NA"),
#          # immig_mharder = car::recode(immi_makedifficult_2016, "1:4=0; 5=1; 8=NA"),
#          age = 2016 - birthyr_baseline,
#          female = car::recode(gender_baseline, "1=0;2=1"),
#          collegeed = car::recode(educ_2016, "1:4=0; 5:6=1; 8:9=NA"),
#          unemployed = car::recode(employ_2016, "1:2=0; 3:4=1; 5:9=0"),
#          faminc = car::recode(faminc_2016, "97=NA; 12:31=12"),
#          pid = car::recode(pid7_2016, "8=NA"),
#          ideo = car::recode(ideo5_2016, "6:9=NA"),
#          bwpy = car::recode(persfinretro_2016, "1=-1; 2=0; 3=1; 4:9=NA"),
#          bpy = car::recode(persfinretro_2016, "1=1; 2=0; 3=0; 4:9=NA"),
#          wpy = car::recode(persfinretro_2016, "1:2=0; 3=1; 4:9=NA"),
#          econgetw = car::recode(econtrend_2016, "1:2=0; 3=1; 4:9=NA"),
#          rr_lessdeserve = car::recode(race_deservemore_2016, "8:99=NA"),
#          rr_conditions = car::recode(race_slave_2016, "8:99=NA"),
#          rr_tryharder = car::recode(race_tryharder_2016, "1=5; 2=4; 3=3; 4=2; 5=1; 8:99=NA"),
#          rr_favors = car::recode(race_overcome_2016, "1=5; 2=4; 3=3; 4=2; 5=1; 8:99=NA"),
#          white = car::recode(race_2016, "1=1; 2:8=0; 98:99=NA"),
#          black = car::recode(race_2016, "1=0; 2=1; 3:8=0; 98:99=NA"),
#          hispanic = car::recode(race_2016, "1:2=0; 3=1; 4:8=0; 98:99=NA"),
#          asian = car::recode(race_2016,"1:3=0; 4=1; 5:8=0; 98:99=NA"),
#          bornagain = car::recode(pew_bornagain_2016, "1=1; 2=0; 8:9=NA"),
#          white_therm = car::recode(ft_white_2016, "997:999=NA"),
#          black_therm = car::recode(ft_black_2016, "997:999=NA"),
#          hisp_therm = car::recode(ft_hisp_2016, "997:999=NA"),
#          asian_therm = car::recode(ft_asian_2016, "997:999=NA")) %>%
#   select(survyr:ncol(.)) -> VD16

VSG %>%
  mutate(survyr = 2017,
         case = case_identifier,
         idate = lubridate::ymd("2017-07-01"),
         # There are no 2017 versions of this variable.
         statefips = inputstate_2016,
         zip = izip_2016,
         # immig_drain has an A/B component that are effectively identical, but for "legal" and "illegal".
         # I want "legal."
         # Recode these later...
         immig_drain = carr(immi_contribution_b_2017, "8=NA"), # car::recode(immi_contribution_b_2017, "1:2=0;3=1;8=NA"),
         immig_harder = carr(immi_makedifficult_2017, "8=NA"), # car::recode(immi_makedifficult_2017, "1:3=0; 4:5=1; 8=NA"),
         # Meh, recode them now...
         immig_draind = carr(immig_drain, "1:2=0;3=1;8=NA"), 
         immig_harderd = carr(immig_harder, "1:3=0;4:5=1;8=NA"),
         # Then make some ordered factors
         #immig_drain = as_factor(immig_drain),
         #immig_harder = as_factor(immig_harder),
         # immig_mharder = car::recode(immi_makedifficult_2017, "1:4=0; 5=1; 8=NA"),
         age = 2017 - birthyr_baseline,
         female = car::recode(gender_baseline, "1=0;2=1"),
         collegeed = car::recode(educ_2017, "1:4=0; 5:6=1; 8:9=NA"),
         unemployed = car::recode(employment_status_2017, "1:2=0; 3:4=1; 5:9=0"),
         # Let's create a new maximum of $250k and above.
         faminc = car::recode(faminc_new_2017, "97=NA; 14:31=14"), 
         # Let's create a low income variable
         lowincome =  ifelse(17 > ntile(faminc, 100), 1, 0),
         pid = car::recode(pid7_2017, "8=NA"),
         ideo = car::recode(ideo5_2017, "6:9=NA"),
         bwpy = car::recode(persfinretro_2017, "1=-1; 2=0; 3=1; 4:9=NA"),
         bpy = car::recode(persfinretro_2017, "1=1; 2=0; 3=0; 4:9=NA"),
         wpy = car::recode(persfinretro_2017, "1:2=0; 3=1; 4:9=NA"),
         econgetw = car::recode(econtrend_2017, "1:2=0; 3=1; 4:9=NA"),
         # Not asked in 2017, but not used in what I'm doing here. Meh...
         # They're still asked in 2016, and the data are kind of panel-ish.
         rr_lessdeserve = car::recode(race_deservemore_2016, "8:99=NA"),
         rr_conditions = car::recode(race_slave_2016, "8:99=NA"),
         rr_tryharder = car::recode(race_tryharder_2016, "1=5; 2=4; 3=3; 4=2; 5=1; 8:99=NA"),
         rr_favors = car::recode(race_overcome_2016, "1=5; 2=4; 3=3; 4=2; 5=1; 8:99=NA"),
         white = car::recode(race_2017, "1=1; 2:8=0; 98:99=NA"),
         black = car::recode(race_2017, "1=0; 2=1; 3:8=0; 98:99=NA"),
         hispanic = car::recode(race_2017, "1:2=0; 3=1; 4:8=0; 98:99=NA"),
         asian = car::recode(race_2017,"1:3=0; 4=1; 5:8=0; 98:99=NA"),
         bornagain = car::recode(pew_bornagain_2017, "1=1; 2=0; 8:9=NA"),
         white_therm = car::recode(ft_white_2017, "997:999=NA"),
         black_therm = car::recode(ft_black_2017, "997:999=NA"),
         hisp_therm = car::recode(ft_hisp_2017, "997:999=NA"),
         asian_therm = car::recode(ft_asian_2017, "997:999=NA")) %>%
  # Let's do it here: filter(white == 1)
  filter(white == 1) %>%
  select(survyr:ncol(.)) -> VD

# * Create ethnocentrism measure (VSG) ----
# There's just the thermometer ratings here, so there isn't a whole lot of work to do.
VD %>%
  mutate(
    # Here's thermometer with all three.
    og_therm3 = (hisp_therm + black_therm + asian_therm)/3,
    # Here's thermometer with no Asians
    og_therm2a = (hisp_therm + black_therm)/2,
    # Here's thermometer with no Hispanics
    og_therm2b = (black_therm + asian_therm)/2,
    # Thermometer...
    ec_therm3 = white_therm - og_therm3,
    ec_therm2a = white_therm - og_therm2a,
    ec_therm2b = white_therm - og_therm2b,
    ec_therm1 = white_therm - black_therm) -> VD

# * Create racial resentment (VSG) -----

VD %>%
  select(case, rr_lessdeserve:rr_favors) %>% data.frame -> RR

RR$removeme <- with(RR, ifelse(is.na(rr_conditions) &
                                 is.na(rr_favors) & 
                                 is.na(rr_tryharder) &
                                 is.na(rr_lessdeserve), 1, 0))
RR <- subset(RR, removeme == 0)
RR$removeme <- NULL

library(mirt)
RRM <- mirt(RR[ ,  2:ncol(RR)], model = 1,
            itemtype = "graded", SE = TRUE, verbose = FALSE)

rrscores <- fscores(RRM, full.scores = TRUE, full.scores.SE = TRUE)

cbind(RR, rrscores) %>%
  rename(lrr = F1,
         lrr_se = SE_F1) %>% tbl_df() %>%
  select(case, lrr, lrr_se) %>%
  left_join(VD, .) -> VD

# Clean ANES master data -----


ANES %>%
  mutate(survyr = VCF0004,
         ycid = VCF0006,
         state = VCF0901b,
         county = stringr::str_sub(VCF0170d, -3, -1),
         immigrl = car::recode(VCF0879, "8:9=NA"),
         immigrl = as.factor(immigrl),
         immigrld = car::recode(VCF0879, "8:9=NA; 1:3=0; 4:5=1"),
         immigrldal = car::recode(VCF0879, "8:9=NA; 1:4=0; 5=1"),
         age = car::recode(VCF0101, "0=NA; 97:99=NA"),
         female = car::recode(VCF0104, "0=NA; 1=0; 2=1"),
         educat = car::recode(VCF0140a, "8:9=NA"),
         collegeed = car::recode(VCF0140a, "8:9=NA; 1:5=0; 6:7=1"),
         urbanism = car::recode(VCF0111, "0=NA"),
         urban = car::recode(VCF0111, "0=NA; 1=1; 2:3=0"),
         suburban = car::recode(VCF0111, "0=NA; 1=0; 2=1; 3=0"),
         protestant = car::recode(VCF0128, "0=NA; 1=1; 2:4=0"),
         rural = car::recode(VCF0111, "0=NA; 1=0; 2=0; 3=1"),
         incomeperc = car::recode(VCF0114, "0=NA"),
         lowincome = ifelse(incomeperc == 1, 1, 0),
         lcindex = car::recode(VCF0801, "98:99=NA"),
         lcscale = car::recode(VCF0803, "0=NA; 9=NA"),
         polint = car::recode(VCF0313, "0=NA; 1:2=0; 3:4=1; 9=NA"),
         pid = car::recode(VCF0301, "0=NA"),
         retired = car::recode(VCF0116, "1:4=0; 5=1; 6:8=NA; 9=NA"),
         unemployed = car::recode(VCF0116, "1=0; 2:4=1; 5:8=0; 9=NA"),
         race7 = car::recode(VCF0105a, "9=NA"),
         race4 = car::recode(VCF0105b, "9=NA; 0=NA"),
         white = car::recode(VCF0105b, "9=NA; 0=NA; 1=1; 2:4=0"),
         black = car::recode(VCF0105b, "9=NA; 0=NA; 1=0; 2=1; 3:4=0"),
         hispanic = car::recode(VCF0105b, "9=NA; 0=NA; 1=0; 2=0; 3=1; 4=0"),
         # Little on the fence about separating the measure here, but meh...
         asian = car::recode(VCF0105a, "1:2=0; 3=1; 4:7=0; 9=NA"),
         rr_conditions = car::recode(VCF9039, "8:9=NA"),
         rr_favors  = car::recode(VCF9040, "1=5; 2=4; 3=3; 4=2; 5=1; 8:9=NA"),
         rr_tryharder = car::recode(VCF9041, "1=5; 2=4; 3=3; 4=2; 5=1; 8:9=NA"),
         rr_lessdeserve = car::recode(VCF9042, "8:9=NA"),
         white_therm = car::recode(VCF0207, "98:99=NA"),
         black_therm = car::recode(VCF0206, "98:99=NA"),
         hisp_therm = car::recode(VCF0217, "98:99=NA"),
         asian_therm = car::recode(VCF0227, "98:99=NA"),
         bwpy = car::recode(VCF0880, "0=NA; 9=NA; 1=3; 2=2; 3=1"),
         bwny = car::recode(VCF0881, "0=NA; 9=NA; 1=3; 2=2; 3=1"),
         bpy = car::recode(VCF0880, "0=NA; 9=NA; 1=1; 2=0; 3=0"),
         bny = car::recode(VCF0881, "0=NA; 9=NA; 1=1; 2=0; 3=0"),
         wpy = car::recode(VCF0880, "0=NA; 9=NA; 1=0; 2=0; 3=1"),
         wny = car::recode(VCF0881, "0=NA; 9=NA; 1=0; 2=0; 3=1")) %>%
  filter(survyr >= 1992) %>%
  select(survyr:ncol(.)) -> AD

# * Clean a92 -----

a92 %>%
  mutate(survyr = 1992,
         ycid = V923004,
         imonth = ifelse(V925005 == 0, NA, V925005),
         iyear = ifelse(imonth >= 11, 1992, 1993),
         bornagain = car::recode(V900545, "0=0; 1=1; 5=0; 8=NA; 9=0"),
         immigrld2 = NA,
         immigrldal2 = NA,
         white_hw = car::recode(V926221, "0=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 8:9=NA"),
         black_hw = car::recode(V926222, "0=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 8:9=NA"),
         asian_hw = car::recode(V926223, "0=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 8:9=NA"),
         hisp_hw = car::recode(V926224, "0=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 8:9=NA"),
         white_intel = car::recode(V926225, "0=NA; 8:9=NA"),
         black_intel = car::recode(V926226, "0=NA; 8:9=NA"),
         asian_intel = car::recode(V926227, "0=NA; 8:9=NA"),
         hisp_intel = car::recode(V926228, "0=NA; 8:9=NA"),
         white_peace = car::recode(V926229, "0=NA; 8:9=NA"),
         black_peace = car::recode(V926230, "0=NA; 8:9=NA"),
         asian_peace = car::recode(V926231, "0=NA; 8:9=NA"),
         hisp_peace = car::recode(V926232, "0=NA; 8:9=NA")) %>%
  select(survyr:ncol(.)) -> a92s

# left_join(AD, ., by = c("survyr", "ycid")) -> AD

# * Clean a93 -----
# sjmisc::find_var(a93, "month", out = "table")

# a93 %>%
#   mutate(survyr = 1993,
#          ycid = V923004,
#          imonth = ifelse(V937014 == 0, NA, V937014),
#          iyear = ifelse(V937013 == 0, NA, V937013),
#          bornagain = car::recode(V923847, "0=0; 1=1; 5=0; 8=NA; 9=0"),
#          immigrld2 = NA,
#          immigrldal2 = NA,)

# * Clean a94 -----

a94 %>%
  mutate(survyr = 1994,
         ycid = V940001,
         imonth = ifelse(V925005 == 0, NA, V925005),
         iyear = ifelse(imonth >= 11, 1994, 1995),
         bornagain = car::recode(V923847, "0=0; 1=1; 5=0; 8=NA; 9=0"),
         immigrld2 = NA,
         immigrldal2 = NA,
         white_hw = car::recode(V926221, "0=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 8:9=NA"),
         black_hw = car::recode(V926222, "0=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 8:9=NA"),
         asian_hw = car::recode(V926223, "0=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 8:9=NA"),
         hisp_hw = car::recode(V926224, "0=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 8:9=NA"),
         white_intel = car::recode(V926225, "0=NA; 8:9=NA"),
         black_intel = car::recode(V926226, "0=NA; 8:9=NA"),
         asian_intel = car::recode(V926227, "0=NA; 8:9=NA"),
         hisp_intel = car::recode(V926228, "0=NA; 8:9=NA"),
         white_peace = car::recode(V926229, "0=NA; 8:9=NA"),
         black_peace = car::recode(V926230, "0=NA; 8:9=NA"),
         asian_peace = car::recode(V926231, "0=NA; 8:9=NA"),
         hisp_peace = car::recode(V926232, "0=NA; 8:9=NA")) %>%
  select(survyr:ncol(.)) -> a94s

# * Clean a96 -----

a96 %>%
  mutate(survyr = 1996,
         ycid = V960001,
         imonth = ifelse(V960903 == 0, NA, V960903),
         iyear = ifelse(imonth >= 11, 1996, 1997),
         bornagain = car::recode(V960601, "0=0; 1=1; 5=0; 8=NA; 9=0"),
         immigrld2 = NA,
         immigrldal2 = NA,
         white_hw = car::recode(V961311, "0=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 8:9=NA"),
         black_hw = car::recode(V961312, "0=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 8:9=NA"),
         hisp_hw = car::recode(V961313, "0=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 8:9=NA"),
         white_intel = car::recode(V961314, "0=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 8:9=NA"),
         black_intel = car::recode(V961315, "0=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 8:9=NA"),
         hisp_intel = car::recode(V961316, "0=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 8:9=NA"),
         white_trust = car::recode(V961317,  "0=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 8:9=NA"),
         black_trust = car::recode(V961318,  "0=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 8:9=NA"),
         hisp_trust = car::recode(V961319,  "0=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 8:9=NA")) %>%
  select(survyr:ncol(.)) -> a96s

# * Clean a98 -----

a98 %>%
  mutate(survyr = 1998,
         ycid = V980001,
         imonth = V980003,
         iyear = ifelse(imonth >= 11, 1998, 1999),
         bornagain = car::recode(V980568, "0=0; 1=1; 5=0; 8=NA; 9=0"),
         immigrld2 = NA,
         immigrldal2 = NA,
         white_hw = NA,
         black_hw = NA,
         hisp_hw = NA,
         white_intel = NA,
         black_intel = NA,
         hisp_intel = NA,
         white_trust = NA,
         black_trust = NA,
         hisp_trust = NA) %>%
  select(survyr:ncol(.)) -> a98s

# * Clean a00 -----
# Remember: this question was asked *pre* in 2000.

a00 %>%
  mutate(survyr = 2000,
         ycid = V000001,
         imonth = ifelse(V000006 == 0, NA, V000006),
         iyear = 2000,
         bornagain = car::recode(V000903, "0=0; 1=1; 5=0; 8=NA; 9=0"),
         immigrld2 = car::recode(V000510, "8:9=NA; 1:3=0; 4:5=1"),
         immigrldal2 = car::recode(V000510, "8:9=NA; 1:4=0; 5=1"),
         white_hw = car::recode(V001574, "0=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 8:9=NA"),
         black_hw = car::recode(V001575, "0=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 8:9=NA"),
         hisp_hw = car::recode(V001576, "0=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 8:9=NA"),
         asian_hw = car::recode(V001577, "0=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 8:9=NA"),
         white_intel = car::recode(V001578, "0=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 8:9=NA"),
         black_intel = car::recode(V001579, "0=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 8:9=NA"),
         hisp_intel = car::recode(V001580, "0=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 8:9=NA"),
         asian_intel = car::recode(V001581, "0=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 8:9=NA"),
         white_trust = car::recode(V001582,  "0=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 8:9=NA"),
         black_trust = car::recode(V001583,  "0=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 8:9=NA"),
         hisp_trust = car::recode(V001584,  "0=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 8:9=NA"),
         asian_trust = car::recode(V001584,  "0=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 8:9=NA")) %>%
  select(survyr:ncol(.)) -> a00s

# * Nothing to show for a02, sadly... -----

# * Clean a04 -----

a04 %>%
  mutate(survyr = 2004,
         ycid = V040001,
         imonth = ifelse(V044002 <= 0, NA, V044002),
         iyear = ifelse(imonth >= 11, 2004, 2005),
         bornagain = NA,
         immigrld2 = NA,
         immigrldal2 = NA,
         white_hw = car::recode(V045222, "0=0; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 8:9=NA"),
         black_hw = car::recode(V045223, "0=0; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 8:9=NA"),
         hisp_hw = car::recode(V045224, "0=0; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 8:9=NA"),
         asian_hw = car::recode(V045225, "0=0; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 8:9=NA"),
         white_intel = car::recode(V045226, "0=0; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 8:9=NA"),
         black_intel = car::recode(V045227, "0=0; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 8:9=NA"),
         hisp_intel = car::recode(V045228, "0=0; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 8:9=NA"),
         asian_intel = car::recode(V045229, "0=0; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 8:9=NA"),
         white_trust = car::recode(V045230,  "0=0; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 8:9=NA"),
         black_trust = car::recode(V045231,  "0=0; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 8:9=NA"),
         hisp_trust = car::recode(V045232,  "0=0; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 8:9=NA"),
         asian_trust = car::recode(V045233,  "0=0; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 8:9=NA"))  %>%
  select(survyr:ncol(.)) -> a04s

# * Clean a08 -----

a08 %>%
  mutate(survyr = 2008,
         ycid = V080001,
         imonth = ifelse(V084001a <= 0, NA, V084001a),
         iyear = ifelse(imonth >= 11, 2008, 2009),
         bornagain = car::recode(V083203, "-9=NA; -8=NA; -1=0; 1=1; 5=0"),
         immigrld2 = NA,
         immigrldal2 = NA,
         white_hw = car::recode(V085174a, "0=0; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; -9:-2=NA"),
         black_hw = car::recode(V085174b, "0=0; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; -9:-2=NA"),
         hisp_hw = car::recode(V085174c, "0=0; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; -9:-2=NA"),
         asian_hw = car::recode(V085174d, "0=0; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1;-9:-2=NA"),
         white_intel = car::recode(V085175a, "0=0; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; -9:-2=NA"),
         black_intel = car::recode(V085175b, "0=0; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; -9:-2=NA"),
         hisp_intel = car::recode(V085175c, "0=0; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; -9:-2=NA"),
         asian_intel = car::recode(V085175d, "0=0; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; -9:-2=NA"),
         white_trust = NA,
         black_trust = NA,
         hisp_trust = NA,
         asian_trust = NA) %>%
  select(survyr:ncol(.)) -> a08s

# * Clean a12 -----

a12 %>%
  mutate(survyr = 2012,
         ycid = caseid,
         imonth = NA,
         iyear = NA,
#         voteromney = carr(prevote_presvtwho, "-9:-1=NA; 1=0; 2=1; 5=NA"),
         bornagain = car::recode(relig_bornagn, "-9=NA; -8=NA; -1=0; 1=1; 2:5=0"),
         immigrld2 = NA,
         immigrldal2 = NA,
         white_hw = car::recode(stype_hwkwhite, "0=0; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; -9:-2=NA"),
         black_hw = car::recode(stype_hwkblack, "0=0; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; -9:-2=NA"),
         hisp_hw = car::recode(stype_hwkhisp, "0=0; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; -9:-2=NA"),
         asian_hw = car::recode(stype_hwkasian, "0=0; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; -9:-2=NA"),
         white_intel = car::recode(stype_intwhite, "0=0; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; -9:-2=NA"),
         black_intel = car::recode(stype_intblack, "0=0; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; -9:-2=NA"),
         hisp_intel = car::recode(stype_inthisp, "0=0; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; -9:-2=NA"),
         asian_intel = car::recode(stype_intasian, "0=0; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; -9:-2=NA"),
         white_trust = NA,
         black_trust = NA,
         hisp_trust = NA,
         asian_trust = NA) %>%
  select(survyr:ncol(.)) -> a12s

# * Clean a16 -----

a16 %>%
  mutate(survyr = 2016,
         ycid = as.numeric(str_sub(V160001, -2, -1)),
         imonth = NA,
         iyear = NA,
         bornagain = car::recode(V161263, "-9:-4=NA; -1=0; 1=1; 2=0"),
         immigrld2 = NA,
         immigrldal2 = NA,
         white_hw = car::recode(V162345, "0=0; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; -9:-2=NA"),
         black_hw = car::recode(V162346, "0=0; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; -9:-2=NA"),
         hisp_hw = car::recode(V162347, "0=0; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; -9:-2=NA"),
         asian_hw = car::recode(V162348, "0=0; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; -9:-2=NA"),
         white_peace = car::recode(V162349, "0=0; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; -9:-2=NA"),
         black_peace = car::recode(V162350, "0=0; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; -9:-2=NA"),
         hisp_peace = car::recode(V162351, "0=0; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; -9:-2=NA"),
         asian_peace = car::recode(V162352, "0=0; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; -9:-2=NA"),
         white_trust = NA,
         black_trust = NA,
         hisp_trust = NA,
         asian_trust = NA) %>%
  select(survyr:ncol(.)) -> a16s

# * Row bind and merge -----

bind_rows(a92s, a94s, a96s, a98s, a00s, a04s, a08s, a12s, a16s) %>%
  left_join(AD, ., by = c("survyr", "ycid")) %>%
  distinct(survyr, ycid, .keep_all = T) %>%
  mutate(immigrld = ifelse(is.na(immigrld) & survyr == 2000,
                           immigrld2,
                           immigrld),
         immigrldal = ifelse(is.na(immigrldal) & survyr == 2000,
                             immigrldal2,
                             immigrldal))  -> AD

AD %>%
  mutate(imonth2 = 11,
         iyear2 = survyr,
         idate = lubridate::ymd(paste(iyear, imonth, "01")),
         idate2 = lubridate::ymd(paste(iyear2, imonth2, "01")),
         idate2 = if_else(survyr == 2000, idate, idate2)) %>%
  select(survyr,idate, iyear, imonth, idate2, iyear2, imonth2, everything())  -> AD


# * Create ethnocentrism measure (ANES) -----
# Kinder and Kam (2010) have two EC measures.
# EC1 = in-group hard-working, intelligent, trustworty - mean outgroups
# EC2 = same, but group therms
# What I need to do: sample out Asians, which are not reliably asked in earlier waves.
# That leaves whites, blacks, hispanics. Asians are small subset of national electorate.
# Also: focus on just the hard-working and intelligent
# -- Rationale: it used to be "peacefulness", then trustworthiness. Gotta maximize waves.
# R1 said ethnocentrism in part includes anti-Hispanic sentiment.
# Let's create a measure of ethnocentrism that doesn't have that. So, it's just the blacks and Asians.
# Given the data limitations, I think this is going to drop a wave or two. 
# Basically, there are no group thermometer/stereotype questions in 1998 and ANES didn't ask about Asians in 1996. *Grumble*

AD %>%
  # Let's do it here: filter(white == 1)
  filter(white == 1) %>%
  mutate(# Here's hw with all three.
         og_hw3 = (hisp_hw + black_hw + asian_hw)/3,
         # Here's hw with no Asians
         og_hw2a = (hisp_hw + black_hw)/2,
         # Here's hw with no Hispanics
         og_hw2b = (black_hw + asian_hw)/2,
         # Here's intelligence with all three.
         og_intel3 = (hisp_intel + black_intel + asian_intel)/3,
         # Here's intelligence with no Asians
         og_intel2a = (hisp_intel + black_intel)/2,
         # Here's intelligence with no Hispanics
         og_intel2b = (black_intel + asian_intel)/2,
         # Here's thermometer with all three.
         og_therm3 = (hisp_therm + black_therm + asian_therm)/3,
         # Here's thermometer with no Asians
         og_therm2a = (hisp_therm + black_therm)/2,
         # Here's thermometer with no Hispanics
         og_therm2b = (black_therm + asian_therm)/2,
         # Now, let's think of some ethnocentrism measures...
         # Hard-working...
         ec_hw3 = white_hw - og_hw3,
         ec_hw2a = white_hw - og_hw2a,
         ec_hw2b = white_hw - og_hw2b,
         ec_hw1 = white_hw - black_hw, # black only
         # Intelligent...
         ec_intel3 = white_intel - og_intel3,
         ec_intel2a = white_intel - og_intel2a,
         ec_intel2b = white_intel - og_intel2b,
         ec_intel1 = white_intel - black_intel, # black only
         # Thermometer...
         ec_therm3 = white_therm - og_therm3,
         ec_therm2a = white_therm - og_therm2a,
         ec_therm2b = white_therm - og_therm2b,
         # black only
         ec_therm1 = white_therm - black_therm) -> AD

# * Create racial resentment (ANES) -----

AD %>%
  select(survyr, ycid, rr_conditions:rr_lessdeserve) %>% data.frame -> RR

RR$removeme <- with(RR, ifelse(is.na(rr_conditions) &
                                 is.na(rr_favors) & 
                                 is.na(rr_tryharder) &
                                 is.na(rr_lessdeserve), 1, 0))
RR <- subset(RR, removeme == 0)
RR$removeme <- NULL


RRM <- mirt(RR[ ,  3:ncol(RR)], model = 1,
            itemtype = "graded", SE = TRUE, verbose = FALSE)

rrscores <- fscores(RRM, full.scores = TRUE, full.scores.SE = TRUE)

cbind(RR, rrscores) %>%
  rename(lrr = F1,
         lrr_se = SE_F1) %>%
  select(survyr, ycid, lrr, lrr_se) %>%
  left_join(AD, .) -> AD

# Let's merge ANES data with various economic data and so on. -----
# County unemployment rate

Cunemp %>%
  select(date, state, fips_county, county, value, cunempr3md:cunempr12md) %>%
  rename(cunempr = value,
         county_name = county,
         county = fips_county,
         idate = date) %>% 
  left_join(AD, .) %>%
  distinct(survyr, ycid, .keep_all = T) -> AD

# State unemployment rate 

Sunemp %>%
  rename(idate2 = date,
         state = stateabb) %>%
  select(-year, -month)  %>%
  na.omit %>%
  left_join(AD, .) -> AD

# National unemployment rate

Nunemp %>%
  rename(idate2 = date) %>%
  left_join(AD, .)  -> AD

# State-level exposure to automation/outsourcing

RJP_states %>%
  rename(survyr = year) %>%
  left_join(AD, .) -> AD

# Rescale...

AD %>%
  mutate(z_age = arm::rescale(age),
         z_educat = arm::rescale(educat),
         z_incomeperc = arm::rescale(incomeperc),
         z_lcindex = arm::rescale(lcindex),
         z_lcscale = arm::rescale(lcscale),
         z_pid = arm::rescale(pid),
         z_bwny = arm::rescale(bwny),
         z_bwpy = arm::rescale(bwpy),
         z_rrc = arm::rescale(rr_conditions),
         z_rrf = arm::rescale(rr_favors),
         z_rrth = arm::rescale(rr_tryharder),
         z_rrld = arm::rescale(rr_lessdeserve),
         z_lrr = arm::rescale(lrr),
#         z_ec = arm::rescale(ec),
#         z_ec2 = arm::rescale(ec2),
         z_nunempr = arm::rescale(nunempr),
         z_nunempr3md = arm::rescale(nunempr3md),
         z_nunempr6md = arm::rescale(nunempr6md),
         z_nunempr12md = arm::rescale(nunempr12md)) %>%
  # Do the new ec variables in one fell swoop.
  mutate_at(vars(contains("ec")), 
            list(z = ~r2sd(., 2))) %>%
  rename_at( vars( contains( "_z") ),
             list(~paste("z", gsub("_z", "", .), sep = "_") ) ) %>%
  group_by(survyr) %>%
  mutate(z_cunempr = arm::rescale(cunempr),
         z_cunempr3md = arm::rescale(cunempr3md),
         z_cunempr6md = arm::rescale(cunempr6md),
         z_cunempr12md = arm::rescale(cunempr12md),
         z_sunempr = arm::rescale(sunempr),
         z_sunempr3md = arm::rescale(sunempr3md),
         z_sunempr6md = arm::rescale(sunempr6md),
         z_sunempr12md = arm::rescale(sunempr12md),
         z_rjp = arm::rescale(rjp)) %>%
  ungroup() %>%
  mutate(county = paste0(county_name," (", state,")")) -> AD

# Now, let's do the same with the VSG data. -----
# * Merge in ZIP-level IRS data to VSG -----
# Recall: I did year diffs 

# Before I do that, I really need to create a state (stateabb) variable for VD

county_fips %>%
  distinct(state, fips_state) %>%
  rename(statefips = fips_state) %>%
  mutate(statefips = as.numeric(statefips)) %>%
  left_join(VD, .) %>%
  select(survyr:statefips, state, everything()) -> VD


IRS %>%
  arrange(state, zipcode, year) %>%
  filter(zipcode != "00000" & zipcode != "0") %>%
  group_by(state, zipcode) %>%
  mutate(diffpunempben = percunempben - lag(percunempben, 1),
         diffunempcompen = avgunempcompen - lag(avgunempcompen, 1)) %>%
  rename(zip = zipcode, survyr = year) %>%
  left_join(VD, .) %>%
  # practice safe group_by
  ungroup() -> VD


# * State unemployment data for VSG ----

Sunemp %>%
  rename(state = stateabb,
         idate = date) %>%
  select(idate, state, sunempr:sunempr12md) %>%
  left_join(VD, .) -> VD

# * Merge in automation/outsourcing measures to VSG -----

RJP_CBSA17 %>%
  rename(rjpcbsa = rjp) %>%
  left_join(VD, .) -> VD

RJP_states %>%
  filter(year == 2017) %>%
  rename(survyr = year,
         rjps = rjp) %>%
  left_join(VD, .) -> VD


# * Rescale ----

VD %>%
  mutate(z_age = arm::rescale(age),
         z_faminc = arm::rescale(faminc),
         z_pid = arm::rescale(pid),
         z_ideo = arm::rescale(ideo),
         z_bwpy = arm::rescale(bwpy),
         z_rrc = arm::rescale(rr_conditions),
         z_rrf = arm::rescale(rr_favors),
         z_rrth = arm::rescale(rr_tryharder),
         z_rrld = arm::rescale(rr_lessdeserve),
         z_lrr = arm::rescale(lrr),
         # z_ec = arm::rescale(ec),
         z_percunempben = arm::rescale(percunempben),
         z_avgunempcompen = arm::rescale(avgunempcompen),
         z_diffpunempben = arm::rescale(diffpunempben),
         z_diffunempcompen = arm::rescale(diffunempcompen),
         z_sunempr = arm::rescale(sunempr),
         z_sunempr3md = arm::rescale(sunempr3md),
         z_sunempr6md = arm::rescale(sunempr6md),
         z_sunempr12md = arm::rescale(sunempr12md),
         z_rjpcbsa = arm::rescale(rjpcbsa),
         z_rjps = arm::rescale(rjps)) %>%
  # Do the new ec variables in one fell swoop.
  mutate_at(vars(contains("ec")), 
            list(z = ~r2sd(.))) %>%
  #mutate_at(vars(contains("unemp")), 
  #          list(z = ~r2sd(., 2))) %>%
  #mutate_at(vars(contains("diff")), 
  #          list(z = ~r2sd(., 2))) %>%
  rename_at( vars( contains( "_z") ),
             list(~paste("z", gsub("_z", "", .), sep = "_") ) ) -> VD
  #select(-z_unemployed) #  -> VD


# Save... -----

saveRDS(AD,"data/ad.rds")
saveRDS(VD,"data/vd.rds")

# V2hc <- bglmer(immig_harderd ~ z_age + female + collegeed + unemployed +
#                 z_faminc  + z_ideo + z_pid + wpy + econgetw + z_rjpcbsa + z_ec_therm3 +
#                 (1 | state),data=subset(VD, white == 1), family=binomial(link = "logit"),
#               control=glmerControl(optimizer="bobyqa",
#                                    optCtrl=list(maxfun=2e5)))
# 
# 
# A1 <- glmer(immigrld ~ z_age + female + collegeed + unemployed +
#               z_incomeperc + z_lcindex + z_pid + wny + wpy + z_ec_therm3 + z_rjp + wpy +
#               (1 | survyr) + (1 | state) + (1 | state:survyr),
#             data=subset(AD, white == 1), family=binomial(link = "logit"),
#             control=glmerControl(optimizer="bobyqa",
#                                  optCtrl=list(maxfun=2e5)))
