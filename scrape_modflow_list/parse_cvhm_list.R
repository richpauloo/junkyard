##########################################################################
# The purpose of this script is to parse CVHM LIST file for CV GW budget
# Units: from DIS file, length (meters), time (days)
# recall that storages are CUMULATIVE, so they must be 
# differenced in order to regain the incremental dS per time step
##########################################################################

# packages
library(tidyverse)
library(stringr)
library(lubridate)

# read in LIST file and subset for lines with gw storage
l  <- read_lines("F:/Box Sync/Research/Pre QE Research/CVHM stuff/Rich CVHM/LIST.txt")
l1 <- l[str_detect(l, "             STORAGE =")]
l2 <- l[str_detect(l, "    INST. IB STORAGE =")]


# subset to a vector of storage, and convert to numeric
l1 <- str_sub(l1, 23, 39) %>% str_trim() %>% as.numeric()
l2 <- str_sub(l2, 23, 39) %>% str_trim() %>% as.numeric()


# create vectors for stress period, time step, storage(S_in, S_out)
sp <- rep(1:510, each = 2)         # stress periods
ts <- rep(1:2, times = 510)        # time steps
si <- l1[seq(1, length(l1), 2)]    # storage in
so <- l1[seq(2, length(l1), 2)]    # storage out
ii <- l2[seq(1, length(l2), 2)]    # subsidence storage in
io <- l2[seq(2, length(l2), 2)]    # subsidence storage out

# combine into df, and calculate cumulative dS
df <- tibble(sp, ts, si, so, ii, io) %>% 
  mutate(ds_old = (so - si) / 1E9,
         ds_new = ((so + io) - (si + ii)) / 1E9) 

# plot cumulative dS in MAF
stv <- readxl::read_xlsx("C:/Users/rpauloo/Desktop/Updated_CVHM_del_storage_SMaples_031318.xlsx",
                         skip = 1, sheet = 1)

df %>% 
  filter(ts == 2) %>% 
  mutate(dtt = seq(as.Date("1961-05-01"), length = 510, by = "1 month") - 1) %>% 
  filter(dtt >= "1961-05-01") %>% 
  mutate(stv    = stv$CVHM_All / 1000000,
         ds_old = ds_old * 810714 / 1000000,
         ds_old = ds_old - ds_old[1],
         ds_new = ds_new * 810714 / 1000000,
         ds_new = ds_new - ds_new[1]) %>% 
  select(dtt, stv, ds_old, ds_new) %>% 
  tidyr::gather(method, ds, -dtt) %>%  
  ggplot(aes(dtt, ds, color = method)) +
  geom_line()












