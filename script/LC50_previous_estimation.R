# LC50 estimation with first R1-3 experiment 
library(tidyverse)
library(xts)
library(ggplot2)
library(readxl)

# import data 

dataR1 <- readxl::read_xlsx("../20221017_R1_test/20221123_R1_donnees_larves_nymphes_adultes.xlsx", 2) %>% 
  select(c(2,4,7,8, 9))

dataR2 <- readxl::read_xlsx("../20221024_R2/20221123_R2_donnees_larves_nymphes_adultes.xlsx", 3) %>% 
select(c(2,4,7,8, 9))

dataR3 <- readxl::read_xlsx("../20221121_R3/20221123_R3_donnees_larves_nymphes_adultes.xlsx", 1) %>% 
select(c(2,4,7,8, 9))

data_all <- rbind(dataR1, dataR2, dataR3) %>% 
  mutate(passage_nymph=case_when(jour_nymphe!="NA"~1, .default= 0), conc_P=`concentration_permethrine_mg/ml`)

# prop 
# par replicat
sum_tot <- data_all  %>% 
  group_by(traitement, replicat, conc_P) %>% 
  summarise(Nnymph_tot=n())

sum_nymph <- data_all %>% 
  filter(passage_nymph==1) %>% 
  group_by(traitement, replicat,conc_P) %>% 
  summarise(Nnymph=n())

res_prop_nymph <- right_join(sum_nymph, sum_tot) %>% 
  mutate(p_nymph=Nnymph/Nnymph_tot)

res_prop_nymph <-res_prop_nymph %>% mutate(P_nymph2=replace_na(p_nymph,0)) 

ggplot(res_prop_nymph, aes(x=conc_P, y=P_nymph2, colour=traitement))+
  geom_point()+
  ylim(0,1)
ggplot(res_prop_nymph, aes(x=conc_P, y=P_nymph2, colour=replicat, shape=traitement))+
  geom_point()+
  ylim(0,1)

# Raw visualimapping = # Raw visualisation 

data_all %>% ggplot()+
  geom_point(aes(x=conc_P, y=passage_nymph ))

library(drc)

drm1 <- drm(passage_nymph~conc_P, data=data_all, 
            fct=LL.4(fixed=c(NA, 0, 1, NA),
                     names = c("Slope", "Lower Limit", "Upper Limit", "ED50")), 
            type="binomial")

summary(drm1)
plot(drm1)

plot(drm1, type="bars", log="")

## no bornes   
drm2 <- drm(passage_nymph~conc_P,replicat, data=data_all, 
              fct=LL.4(fixed=c(NA, 0, NA, NA),
                       names = c("Slope", "Lower Limit", "Upper Limit", "ED50")), 
              type="binomial")
  
summary(drm2)
plot(drm2, type="bars", log="")

# all other data 

data2 <- read_xlsx("~/Nextcloud/Partages_recus/20221017_Manip_gîtes_larvaires/KdrKIS/20230503_tous_replicats_larves_nymphes_adultes_LHT.xlsx")

data2_sub1 <- data2 %>% filter(souche=="kdr") %>% 
  mutate(conc_P=`concentration_permethrine_mg/ml`)

data2_sub2 <- data2 %>% filter(souche=="kdr") %>% 
  mutate(conc_P=`concentration_permethrine_mg/ml`) %>% filter(conc_P!=0.020)


data2_sub1 %>% ggplot()+
  geom_point(aes(x=conc_P, y=passage_ny1phe))

data2_sub2 %>% ggplot()+
  geom_point(aes(x=conc_P, y=passage_ny1phe))

# prop nymphe 

sum_tot2 <- data2_sub2  %>% 
  group_by(traitement, replicat, conc_P) %>% 
  summarise(Nnymph_tot=n())

sum_nymph2 <- data2_sub2 %>% 
  filter(passage_ny1phe==1) %>% 
  group_by(traitement, replicat,conc_P) %>% 
  summarise(Nnymph=n())

res_prop_nymph2 <- right_join(sum_nymph2, sum_tot2) %>% 
  mutate(p_nymph=Nnymph/Nnymph_tot)

res_prop_nymph2

res_prop_nymph2 %>% ggplot()+
  geom_point(aes(x=conc_P, y=p_nymph, colour=replicat, shape=traitement))+
  ylim(0,1)


# DRM 
drm3 <- drm(passage_ny1phe~conc_P,replicat, data=data2_sub, 
            fct=LL.4(fixed=c(NA, 0, NA, NA),
                     names = c("Slope", "Lower Limit", "Upper Limit", "ED50")), 
            type="binomial")

summary(drm3)
plot(drm3, type="bars", log="x")


