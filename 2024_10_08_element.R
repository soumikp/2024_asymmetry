pacman::p_load(here, tidyverse, readxl)

data_dir <- file.path("~/University of Michigan Dropbox/Soumik Purkayastha/SoumikP/reverse causality project")
setwd("~/University of Michigan Dropbox/Soumik Purkayastha/SoumikP/reverse causality project")

#### DNA methylation foliocc counts (wrong) ####
dnam_p20_w <- read_csv(file.path("P20_DNA_Methylation_112021.csv"))
## 247 patients - shouldn't we have about 80?

dnam_p01_t1_w <- read_csv(file.path("P01_DNA_Methylation_T1_202001.csv"))
## 385 patients - shouldn't we have about 500?

length(intersect(dnam_p01_t1$FOLIOCC_w, dnam_p20$FOLIOCC_w))
## 205 patients in common - unsure if these match with Jackie's counts


#### DNA methylation foliocc counts (right) ####

dnam_p20 <- read_csv(file.path("P20_EpigeneticAge_Children.csv"))
## 79 patients - shouldn't we have about 80? yes we do now. 

dnam_p01_t1 <- read_csv(file.path("P01_EpigeneticAge_children.csv"))
## 526 patients - shouldn't we have about 500? yes we do now.

length(intersect(dnam_p01_t1$foliocc, dnam_p20$foliocc))
## 62 patients in common - close to Jackie's count of 80. All good now.








#### Blood pressure foliocc counts ####
bp_cc <- read_csv("CC_Children_Blood_Pressure_(102021).csv")
# V0 has about 814 BP measurements (good)

bp_p20 <- read_csv("P20_Anthropometry_with_BMI(anthro_z).csv")
# P20 has 250 BP measurements

bp_p01_t1 <- read_csv("P01_Anthropometry_w.BMI_zscores_202203.csv")
# P01-T1 has 554 measurements, P01-T2 has 519 measurements





#### Target cohorts ####

## sex data
demo <- read_csv("~/OneDrive - University of Pittsburgh/Research/2023_enar/data/P01_CpG sites and  BP_(T1 Only)_09082022.csv") %>% 
  select(c(foliocc, AC_SexH))

## so I guess DNAm at P01_T1 has to be the pivot group. 

### BP at V0, DNAm at P01-T1, BP at P01-T2 (n = 364)
inner_join(as_tibble(x = intersect(intersect(bp_cc$foliocc, dnam_p01_t1$foliocc), 
                 bp_p01_t1$FOLIOCC[bp_p01_t1$etapa == "T2"])) %>% rename(foliocc = value), 
           demo) %>% 
  group_by(AC_SexH) %>% 
  summarise(n = n())

### BP at P20, DNAm at P01-T1, BP at P01-T2 (n = 206)
inner_join(as_tibble(x = intersect(intersect(bp_p20$FOLIOCC, dnam_p01_t1$foliocc), 
                     bp_p01_t1$FOLIOCC[bp_p01_t1$etapa == "T2"])) %>% rename(foliocc = value), 
           demo) %>% 
  group_by(AC_SexH) %>% 
  summarise(n = n())



### DNAm and BP at P20 and P01-T1 (n = 62)
length(intersect(dnam_p20$foliocc, dnam_p01_t1$foliocc))
length(intersect(bp_p20$FOLIOCC, bp_p01_t1$FOLIOCC))

inner_join(as_tibble(x = intersect(intersect(dnam_p20$foliocc, dnam_p01_t1$foliocc), 
                 intersect(bp_p20$FOLIOCC, bp_p01_t1$FOLIOCC))) %>% 
  rename(foliocc = value), 
  demo) %>% 
  group_by(AC_SexH) %>% 
  summarize(n = n())





