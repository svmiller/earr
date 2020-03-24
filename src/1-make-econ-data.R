# Let's think of a plan of attack here. -----
# 1. Load/process county/state/national unemployment data from BLS.
# 2. Then: let's process the IRS data.
# 3. Next: Routine job percentage/exposure.

library(tidyverse)
library(blscrapeR)
library(fredr)

# 1. Load/process county/state/national unemployment data from BLS. -----
# * Get some FIPS codes from blscrapeR ----

county_fips %>% tbl_df() -> county_fips

#df <- bls_api("LAUCN010010000000003",
#              startyear = 2016, endyear = 2016)

# blsunemp <- read_tsv("~/Dropbox/data/bls/la.data.64.County-06172018.txt")

# * County unemployment -----
Cunemp <- read_tsv("https://download.bls.gov/pub/time.series/la/la.data.64.County") 

Cunemp %>%
  mutate(month = as.numeric(str_sub(period, 2,3)),
         srdc = as.numeric(str_sub(series_id, -1,-1)),
         fips_state = str_sub(series_id, 6,7),
         fips_county = str_sub(series_id, 8, 10)) %>%
  filter(srdc == 3) %>%
  select(fips_state, fips_county, srdc, year, month, value) %>%
  mutate(date = lubridate::ymd(paste(year,month,"01",sep="-"))) %>%
  left_join(.,county_fips)  -> Cunemp

Cunemp %>%
  select(-type) %>%
  group_by(state, county) %>%
  mutate(#cunempr3ma = lag(value, 3),
         #cunempr6ma = lag(value, 6),
         #cunempr12ma = lag(value, 12),
         cunempr3md = value - lag(value, 3),
         cunempr6md = value - lag(value, 6),
         cunempr12md = value - lag(value, 12)) -> Cunemp


# * State unemployment -----

stateunemprabbs <- paste0(c(state.abb, "DC"),"UR")

Sunemp <- fredr(series_id = "ALUR",
               observation_start = as.Date("1990-01-01")) 
# rename(sunempr = value,
#        stateabb = series_id) 

for (i in 2:length(stateunemprabbs)) {
  fredr(series_id = stateunemprabbs[i],
        observation_start = as.Date("1990-01-01")) %>%
    #    rename(sunempr = stateunemprabbs[i],
    #           state_abb = series_id) %>% tbl_df() %>%
    bind_rows(Sunemp, .) -> Sunemp
}

Sunemp %>%
  rename(sunempr = value,
         stateabb = series_id) -> Sunemp

states_and_abbs <- tibble(state = state.name) %>%
  bind_cols(tibble(stateabb = state.abb)) %>% 
  bind_rows(tibble(state = c("District of Columbia", "Puerto Rico"), stateabb = c("DC", "PR")))

read_tsv("https://download.bls.gov/pub/time.series/la/la.data.46.PuertoRico") %>%
  filter(series_id == "LASST720000000000003" & year >= 1990) %>%
  mutate(month = str_sub(period, 2,3),
         date = lubridate::ymd(paste0(year,month, "01")),
         sunempr = value,
         stateabb = "PRUR") %>%
  select(date, sunempr, stateabb) %>%
  bind_rows(Unemp, .) -> Sunemp



Sunemp %>%
  mutate(stateabb = stringr::str_sub(stateabb, 1,2)) %>%
  group_by(stateabb) %>%
  mutate(#sunempr3ma = lag(sunempr, 3),
         # sunempr6ma = lag(sunempr, 6),
         # sunempr12ma = lag(sunempr, 12),
         sunempr3md = sunempr - lag(sunempr, 3),
         sunempr6md = sunempr - lag(sunempr, 6),
         sunempr12md = sunempr - lag(sunempr, 12),
         year = lubridate::year(date),
         month = lubridate::month(date),
         date = lubridate::ymd(paste(year, month, "01"))) %>%
  left_join(.,states_and_abbs) %>%
  ungroup() %>%
  select(date, year, month, stateabb, sunempr, sunempr3md:sunempr12md) -> Sunemp

# * National unemployment -----

Nunemp <- fredr(series_id = "UNRATE",
                observation_start = as.Date("1990-01-01")) %>% 
  rename(nunempr = value) %>%
  mutate(nunempr3ma = lag(nunempr, 3),
         nunempr6ma = lag(nunempr, 6),
         nunempr12ma = lag(nunempr, 12),
         nunempr3md = nunempr - nunempr3ma,
         nunempr6md = nunempr - nunempr6ma,
         nunempr12md = nunempr - nunempr12ma) %>%
  select(date, nunempr, nunempr3md:nunempr12md)

# * save some RDS files
saveRDS(Cunemp, "data/cunemp.rds")
saveRDS(Sunemp, "data/sunemp.rds")
saveRDS(Nunemp, "data/nunemp.rds")

# 2. IRS data -----

IRS09 <- read_csv("~/Dropbox/data/irs/zipcode2009/09zpallnoagi.csv") %>%
  mutate(year = 2009) %>%
  select(year, STATE, ZIPCODE, N1, N02300, A02300)
IRS10 <- read_csv("~/Dropbox/data/irs/zipcode2010/10zpallnoagi.csv") %>%
  mutate(year = 2010) %>%
  select(year, STATE, ZIPCODE, N1, N02300, A02300)
IRS11 <- read_csv("~/Dropbox/data/irs/zipcode2011/11zpallnoagi.csv") %>%
  mutate(year = 2011) %>%
  select(year, STATE, ZIPCODE, N1, N02300, A02300)
IRS12 <- read_csv("~/Dropbox/data/irs/zipcode2012/12zpallnoagi.csv") %>%
  mutate(year = 2012) %>%
  select(year, STATE, ZIPCODE, N1, N02300, A02300)
IRS13 <- read_csv("~/Dropbox/data/irs/zipcode2013/13zpallnoagi.csv") %>%
  mutate(year = 2013) %>%
  select(year, STATE, ZIPCODE, N1, N02300, A02300)
IRS14 <- read_csv("~/Dropbox/data/irs/zipcode2014/14zpallnoagi.csv") %>%
  mutate(year = 2014) %>%
  select(year, STATE, ZIPCODE, N1, N02300, A02300)
IRS15 <- read_csv("~/Dropbox/data/irs/zipcode2015/15zpallnoagi.csv")  %>%
  mutate(year = 2015) %>%
  select(year, STATE, ZIPCODE, N1, N02300, A02300)
IRS16 <- read_csv("~/Dropbox/data/irs/zipcode2016/16zpallnoagi.csv")  %>%
  mutate(year = 2016) %>%
  select(year, STATE, ZIPCODE, N1, N02300, A02300) %>%
  mutate(ZIPCODE = as.character(ZIPCODE))
IRS17 <- read_csv("~/Dropbox/data/irs/zipcode2017/17zpallnoagi.csv")  %>%
  mutate(year = 2017) %>%
  select(year, STATE, ZIPCODE, N1, N02300, A02300)

mget(ls(pattern="IRS"))  %>%
  bind_rows() %>%
  rename_all(tolower) %>%
  rename(numreturns = n1,
         numunempben = n02300,
         totuncompen = a02300) %>%
  # group_by(year, state, zipcode) %>%
  mutate(percunempben = numunempben/numreturns,
         avgunempcompen = totuncompen/numunempben) -> IRS

saveRDS(IRS,"data/irs.rds")

# IRS15 %>%
#   group_by(STATE, ZIPCODE) %>%
#   mutate(numreturns15 = N1,
#          numunempben15 = N02300,
#          totalunempcompen15 = A02300,
#          percunempben15 = numunempben15/numreturns15,
#          avgunempcompen15 = totalunempcompen15/numunempben15) %>%
#   select(STATE, ZIPCODE, numreturns15:ncol(.)) %>%
#   rename(state = STATE, zip = ZIPCODE) -> IRS15s
# 
# IRS14 <- read_csv("~/Dropbox/data/irs/zipcode2014/14zpallnoagi.csv")
# 
# IRS14 %>%
#   group_by(STATE, ZIPCODE) %>%
#   mutate(numreturns14 = N1,
#          numunempben14 = N02300,
#          totalunempcompen14 = A02300,
#          percunempben14 = numunempben14/numreturns14,
#          avgunempcompen14 = totalunempcompen14/numunempben14) %>%
#   select(STATE, ZIPCODE, numreturns14:ncol(.)) %>%
#   rename(state = STATE, zip = ZIPCODE) -> IRS14s

# 3. Routine job percentage/exposure. -----
# Routine = manufacturing, goods-production (51-0000), 
# administrative, clerical, and sales (41-0000)
# 
# Routine-cognitive = sales and related (41-000), office and admin (43-000)
# Routine-manual = production occupations (51-000), transport and material (53-000),
#    natural resources, construction and extraction (47-000), 
#    installation and maintenance (49-000)

# * Let's first get the CBSA level for 2017 -----
# We're going to use this for the VSG analyses, which have ZIP data.
OESMSA17 <- readxl::read_excel("~/Dropbox/data/bls/oes/oesm17ma/MSA_M2017_dl.xlsx") %>%
  mutate(routine = ifelse(OCC_CODE %in% c("00-0000", "41-0000", "43-0000",
                                          "45-0000", 
                                          # ^ There's some misgivings about farm belonging here.
                                          "47-0000", "49-0000",
                                          "51-0000", "53-0000"), 1, 0))

OESMSA17 %>%
  rename_all(tolower) %>%
  rename(cbsa = area) %>%
  mutate(cbsa = as.numeric(cbsa)) %>%
  group_by(cbsa) %>%
  mutate(tot_emp = as.numeric(tot_emp)) %>%
  filter(routine == 1) %>%
  select(1:7) %>%
  group_by(cbsa) %>%
  # Rather than sum, we'll use the highest tot_emp variable.
  # Basically: the highest tot_emp in a given CBSA is the total of the occ_group.
  # That, btw, is why we selected on occ_code also %in% "00-0000" in the earlier command.
  mutate(totemp = max(tot_emp, na.rm=T))  %>%
  # Now that we have a column of the max, we don't want the occ_code == 00-0000 anymore.
  filter(occ_code != "00-0000") %>%
  # Calculate a routine job percentage variable.
  # If NA, pretty sure that's the OES way of saying zero employment. So, an na.rm=T does no harm.
  summarize(rjp = sum(tot_emp, na.rm=T)/max(totemp)) -> OESMSA17_cbsa


# This will help us square ZIP codes (in the VSG data) with CBSAs (for which we have data)
# Note: Since I know I'm doing this with observations from July 2017, I'm using the data HUD has available
# from third quarter 2017. It'll do.
readxl::read_excel("~/Dropbox/data/hud/ZIP_CBSA_062017.xlsx") %>%
  rename_all(tolower) %>%
  select(1:2) %>%
  mutate(cbsa = as.numeric(cbsa)) %>%
  left_join(., OESMSA17_cbsa) %>%
  # Small issue with a quick and dirty fix:
  # One CBSA can have a lot of zip codes. However, given the nature of the data, I don't think it too much a problem
  # to just take the mean of the rjp variable, by zip
  group_by(zip) %>%
  summarize(rjp = mean(rjp, na.rm=T)) -> RJP_CBSA17

saveRDS(RJP_CBSA17, "data/rjp_cbsa17.rds")

# * Now, let's do state level from 2000-onward. -------
# Don't forget: the VSG analyses are in 2017 for state-level, so don't forget 2017 for the state-level.
# 2000 is a weirder (older) file, so we'll need some care.

# First, let's read 2000 in.
readxl::read_excel("~/Dropbox/data/bls/oes/state_2000_dl.xlsx",
                             skip = 42, sheet=1) %>%
  # Give us just the 0000s in the occ_code
  filter(group == "major") %>%
  # convert tot_emp to numeric
  mutate(tot_emp = as.numeric(tot_emp)) %>%
  select(st, occ_code, tot_emp) %>%
  mutate(routine = ifelse(occ_code %in% c("00-0000", "41-0000", "43-0000",
                                          "45-0000", 
                                          # ^ There's some misgivings about farm belonging here.
                                          "47-0000", "49-0000",
                                          "51-0000", "53-0000"), 1, 0)) %>%
  group_by(st) %>%
  mutate(n = sum(tot_emp)) %>%
  filter(routine == 1) %>%
  summarize(rjp = sum(tot_emp)/max(n)) %>%
  # practice safe group_by 
  ungroup() %>%
  # Especially when you want to rename.
  rename(state = st) %>%
  mutate(year = 2000) -> OESS00

# 2004-2017 should be fairly routine. I could be able to make this a function to decrease the code.

clean_oesst <- function (data) {
  data %>%
    rename_all(tolower) %>%
    mutate(routine = ifelse(occ_code %in% c("00-0000", "41-0000", "43-0000",
                                            "45-0000", 
                                            # ^ There's some misgivings about farm belonging here.
                                            "47-0000", "49-0000",
                                            "51-0000", "53-0000"), 1, 0)) %>%
    select(st, occ_code, tot_emp, routine) %>%
    rename(state = st) %>%
    mutate(tot_emp = as.numeric(tot_emp)) %>%
    group_by(state) %>%
    filter(routine == 1) %>%
    # Because we have 00-0000, the max is the total
    mutate(n = max(tot_emp, na.rm=T)) %>%
    filter(occ_code != "00-0000") %>%
    summarize(rjp = sum(tot_emp, na.rm=T)/max(n))
}

readxl::read_excel("~/Dropbox/data/bls/oes/oesn04st/state_november2004_dl.xlsx") %>%
  clean_oesst() %>%
  mutate(year = 2004) -> OESS04

readxl::read_excel("~/Dropbox/data/bls/oes/oesm08st/state__M2008_dl.xlsx") %>%
  clean_oesst() %>%
  mutate(year = 2008) -> OESS08

readxl::read_excel("~/Dropbox/data/bls/oes/oesm12st/state_M2012_dl.xlsx") %>%
  clean_oesst() %>%
  mutate(year = 2012) -> OESS12

readxl::read_excel("~/Dropbox/data/bls/oes/oesm16st/state_M2016_dl.xlsx") %>%
  clean_oesst() %>%
  mutate(year = 2016) -> OESS16

readxl::read_excel("~/Dropbox/data/bls/oes/oesm17st/state_M2017_dl.xlsx") %>%
  clean_oesst() %>%
  mutate(year = 2017) -> OESS17

mget(ls(pattern="OESS"))  %>%
  bind_rows() -> RJP_states

saveRDS(RJP_states, "data/rjp_states.rds")
