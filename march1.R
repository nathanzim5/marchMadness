library(dplyr)
library(tidyr)
library(ggplot2)

setwd("C:\\Users\\User\\Desktop\\ncaa")

dat_beta_m <- read.csv("NCAA_Basketball_HCA_Analysis-master\\Men\\Output\\out_tab_MBB_2019.csv")
dat_ncaa_m <- read.csv("data_NCAA_1985_2019_men.csv")
dat_sigma_m <- read.csv("NCAA_Basketball_HCA_Analysis-master\\Men\\Output\\ANOVA_tab_MBB_2019.csv")
dat_aq <- read.csv("automaticQualifiers.csv")

names(dat_aq)[1] <- "X"

#add year column to beta data
dat_beta_m$Year <- as.numeric(gsub("....-", "", dat_beta_m$Season))

# # #check for errors in aq data
# try <- merge(dat_aq, dat_beta_m, by = c("Team", "Year"), all.x = TRUE) %>%
#   filter(is.na(beta))
# 
# #check for errors in tourney data
# try <- merge(dat_ncaa_m, dat_beta_m, by.x = c("Name1", "Year"), by.y = c("Team", "Year"), all.x = TRUE) %>%
#   filter(is.na(beta))
# 
# try <- merge(dat_ncaa_m, dat_beta_m, by.x = c("Name2", "Year"), by.y = c("Team", "Year"), all.x = TRUE) %>%
#   filter(is.na(beta))

#add bracket location to tourney data
dat_ncaa_m$Region <- sub("[0-9]+", "", dat_ncaa_m$Seed1) 
dat_ncaa_m$Region[dat_ncaa_m$Round > 4] <- NA
dat_ncaa_m$Seed1 <- as.numeric(sub("[^0-9]+", "", dat_ncaa_m$Seed1))
dat_ncaa_m$Seed2 <- as.numeric(sub("[^0-9]+", "", dat_ncaa_m$Seed2))
dat_ncaa_m$Group1 <- ifelse(dat_ncaa_m$Round > 3, NA, ifelse(dat_ncaa_m$Seed1 %in% c(1,8,5,4,16,9,12,13), 1, 2))
dat_ncaa_m$Group2 <- ifelse(dat_ncaa_m$Round > 2, NA, ifelse(dat_ncaa_m$Seed1 %in% c(1,8,6,3,16,9,11,14), 1, 2))
dat_ncaa_m$Group3 <- ifelse(dat_ncaa_m$Round > 1, NA, ifelse(dat_ncaa_m$Seed1 %in% c(1,5,6,7,10,11,12,16), 1, 2))
dat_ncaa_m$Win1 <- ifelse(dat_ncaa_m$Score1 > dat_ncaa_m$Score2, 1, 0)
dat_ncaa_m$Win2 <- ifelse(dat_ncaa_m$Score2 > dat_ncaa_m$Score1, 1, 0)

#filter and format sigma data
dat_sigma2_m <- dat_sigma_m[grep("Residual", dat_sigma_m$X),]
dat_sigma2_m$Year <- gsub("\\(....-", "", dat_sigma2_m$X)
dat_sigma2_m$Year <- gsub(") Residual", "", dat_sigma2_m$Year)
dat_sigma2_m$Sigma <- sqrt(dat_sigma2_m$MS)
dat_sigma2_m <- data.frame(Year = as.character(dat_sigma2_m$Year), Sigma = dat_sigma2_m$Sigma)

#output summary of results for first round seed matchups
out_rd1_smry_by_year_and_seed <- dat_ncaa_m %>%
  filter(Round == 1) %>%
  group_by(Year, Seed1, Seed2) %>%
  summarise(winsSeed1 = sum(Win1), winsSeed2 = sum(Win2)) %>% 
  as.data.frame()

write.csv(out_rd1_smry_by_year_and_seed, file = "smry_rd1_wins_by_year_and_seed.csv")

#output summary of results for second round seed matchups
out_rd2_smry_by_year_and_seed <- dat_ncaa_m %>%
  filter(Round == 2) %>%
  group_by(Year, Seed1, Seed2) %>%
  summarise(winsSeed1 = sum(Win1), winsSeed2 = sum(Win2)) %>% 
  as.data.frame()

write.csv(out_rd2_smry_by_year_and_seed, file = "smry_rd2_wins_by_year_and_seed.csv")

#create new ncaa team table
dat_ncaa_m_teams <- rbind(dat_ncaa_m %>% 
                            filter(Round == 1) %>% 
                            dplyr::select(Year, Name1, Seed1, Region:Group3) %>%
                            dplyr::rename(Seed = Seed1,
                                          Name = Name1) %>%
                            as.data.frame(),
                          dat_ncaa_m %>% 
                            filter(Round == 1) %>% 
                            dplyr::select(Year, Name2, Seed2, Region:Group3) %>%
                            dplyr::rename(Seed = Seed2,
                                          Name = Name2) %>%
                            as.data.frame()
) %>%
  merge(dat_beta_m, by.x = c("Year", "Name"), by.y = c("Year", "Team")) %>%
  merge(dat_aq[dat_aq$Gender == "M",], by.x = c("Year", "Name"), by.y = c("Year", "Team"), all.x = TRUE) %>%
  dplyr::mutate(beta_hca = beta + ifelse(Seed <= 4, HCA, 0)) %>%
  dplyr::mutate(AQ = ifelse(Year %in% unique(dat_aq$Year), ifelse(is.na(X.y), 0, 1), NA)) %>%
  dplyr::select(Year, Region:Group3, Seed, Name, beta, HCA, beta_hca, AQ) %>%
  arrange(Year, Region, Group1, Group2, Group3, Seed)

a1 <- dat_ncaa_m_teams %>% group_by(Year) %>% dplyr::summarise(AQ = sum(AQ)) %>% as.data.frame()
a2 <- dat_ncaa_m_teams %>% group_by(Seed) %>% dplyr::summarise(AQ = sum(AQ, na.rm = TRUE)) %>% as.data.frame()

write.csv(a1, file = "aq_m_by_year.csv")
write.csv(a2, file = "aq_m_by_seed.csv")


dat_ncaa_m_rd1 <- merge(dat_ncaa_m_teams, dat_ncaa_m_teams, by = c("Year", "Region", "Group1", "Group2", "Group3")) %>% 
  filter(Name.x != Name.y) %>%
  merge(dat_sigma2_m, by = "Year") %>%
  mutate(Round = 1,
         ProbWinCond = pnorm((beta.x - beta.y) / Sigma),
         ProbWinCond_circ = pnorm((beta_hca.x - beta_hca.y) / Sigma),
         ProbReach = 1,
         ProbReach_circ = 1,
         ProbReachOpp = 1,
         ProbReachOpp_circ = 1) %>%
  dplyr::rename(Seed = Seed.x,
                Name = Name.x,
                beta = beta.x,
                beta_circ = beta_hca.x,
                SeedOpp = Seed.y,
                NameOpp = Name.y,
                betaOpp = beta.y,
                betaOpp_circ = beta_hca.y) %>%
  mutate(ProbMatchup = ProbReach * ProbReachOpp,
         ProbMatchup_circ = ProbReach_circ * ProbReachOpp_circ) %>%
  mutate(ProbWinJoint = ProbWinCond * ProbMatchup,
         ProbWinJoint_circ = ProbWinCond_circ * ProbMatchup_circ) %>%
  dplyr::select(Year, Sigma, Region:Group3, Round, Seed, Name, 
                ProbReach, ProbReach_circ, beta, beta_circ, SeedOpp, NameOpp, 
                ProbReachOpp, ProbReachOpp_circ, betaOpp, betaOpp_circ, 
                ProbMatchup, ProbMatchup_circ, ProbWinCond, ProbWinCond_circ, 
                ProbWinJoint, ProbWinJoint_circ) %>%
  arrange(Year, Region, Group1, Group2, Group3, Seed)

dat_ncaa_m_rd2 <- merge(dat_ncaa_m_teams, dat_ncaa_m_teams, by = c("Year", "Region", "Group1", "Group2")) %>% 
  filter(Group3.x != Group3.y) %>%
  merge(dat_sigma2_m, by = "Year") %>%
  mutate(Round = 2,
         ProbWinCond = pnorm((beta.x - beta.y) / Sigma),
         ProbWinCond_circ = pnorm((beta_hca.x - beta_hca.y) / Sigma)) %>%
  dplyr::rename(Group3 = Group3.x,
                Seed = Seed.x,
                Name = Name.x,
                beta = beta.x,
                beta_circ = beta_hca.x,
                SeedOpp = Seed.y,
                NameOpp = Name.y,
                betaOpp = beta.y,
                betaOpp_circ = beta_hca.y) %>%
  merge(dat_ncaa_m_rd1 %>%
          dplyr::select(Year, Name, ProbWinJoint, ProbWinJoint_circ), 
        by = c("Year", "Name")) %>%
  dplyr::rename(ProbReach = ProbWinJoint,
                ProbReach_circ = ProbWinJoint_circ) %>%
  merge(dat_ncaa_m_rd1 %>%
          dplyr::select(Year, Name, ProbWinJoint, ProbWinJoint_circ), 
        by.x = c("Year", "NameOpp"), by.y = c("Year", "Name")) %>%
  dplyr::rename(ProbReachOpp = ProbWinJoint,
                ProbReachOpp_circ = ProbWinJoint_circ) %>%
  mutate(ProbMatchup = ProbReach * ProbReachOpp,
         ProbMatchup_circ = ProbReach_circ * ProbReachOpp_circ) %>%
  mutate(ProbWinJoint = ProbWinCond * ProbMatchup,
         ProbWinJoint_circ = ProbWinCond_circ * ProbMatchup_circ) %>%
  dplyr::select(Year, Sigma, Region:Group3, Round, Seed, Name, 
                ProbReach, ProbReach_circ, beta, beta_circ, SeedOpp, NameOpp, 
                ProbReachOpp, ProbReachOpp_circ, betaOpp, betaOpp_circ, 
                ProbMatchup, ProbMatchup_circ, ProbWinCond, ProbWinCond_circ, 
                ProbWinJoint, ProbWinJoint_circ) %>%
  arrange(Year, Region, Group1, Group2, Group3, Seed)

dat_ncaa_m_prob_16 <- rbind(dat_ncaa_m_rd1, 
                         dat_ncaa_m_rd2) %>%
  group_by(Year, Region, Seed, Name, beta, Round) %>%
  dplyr::summarise(ProbWin = sum(ProbWinJoint)) %>%
  spread(Round, ProbWin) %>%
  as.data.frame()

dat_ncaa_m_prob_16_circ <- rbind(dat_ncaa_m_rd1, 
                            dat_ncaa_m_rd2) %>%
  group_by(Year, Region, Seed, Name, beta_circ, Round) %>%
  dplyr::summarise(ProbWin_circ = sum(ProbWinJoint_circ)) %>%
  spread(Round, ProbWin_circ) %>%
  as.data.frame()

dat_ncaa_m_prob_16_final <- dat_ncaa_m_prob_16 %>%
  inner_join(dat_ncaa_m_prob_16_circ, by = c("Year", "Region", "Seed", "Name")) %>%
  as.data.frame()

names(dat_ncaa_m_prob_16_final)[5:10] <- c("beta", "Rd1", "Rd2", "beta_circ", "Rd1_circ", "Rd2_circ")


dat_ncaa_m_prob_16_by_seed <- rbind(dat_ncaa_m_rd1, 
                                 dat_ncaa_m_rd2) %>%
  group_by(Year, Region, Seed, Name, beta, Round) %>%
  dplyr::summarise(ProbWin = sum(ProbWinJoint)) %>%
  group_by(Seed, Round) %>%
  dplyr::summarise(ProbWin = mean(ProbWin)) %>%
  spread(Round, ProbWin) %>%
  as.data.frame()

dat_ncaa_m_prob_16_circ_by_seed <- rbind(dat_ncaa_m_rd1, 
                                    dat_ncaa_m_rd2) %>%
  group_by(Year, Region, Seed, Name, beta_circ, Round) %>%
  dplyr::summarise(ProbWin_circ = sum(ProbWinJoint_circ)) %>%
  group_by(Seed, Round) %>%
  dplyr::summarise(ProbWin_circ = mean(ProbWin_circ)) %>%
  spread(Round, ProbWin_circ) %>%
  as.data.frame()

dat_ncaa_m_prob_16_final_by_seed <- dat_ncaa_m_prob_16_by_seed %>%
  inner_join(dat_ncaa_m_prob_16_circ_by_seed, by = c("Seed")) %>%
  as.data.frame()

names(dat_ncaa_m_prob_16_final_by_seed) <- c("Seed", "Rd1", "Rd2", "Rd1_circ", "Rd2_circ")

write.csv(dat_ncaa_m_prob_16_final_by_seed, file = "men_smry_16_by_seed_circ.csv")


dat_ncaa_m_rd3 <- merge(dat_ncaa_m_teams, dat_ncaa_m_teams, by = c("Year", "Region", "Group1")) %>% 
  filter(Group2.x != Group2.y) %>%
  merge(dat_sigma2_m, by = "Year") %>%
  mutate(Round = 3,
         ProbWinCond = pnorm((beta.x - beta.y) / Sigma)) %>%
  dplyr::rename(Group3 = Group3.x,
                Group2 = Group2.x,
                Seed = Seed.x,
                Name = Name.x,
                beta = beta.x,
                SeedOpp = Seed.y,
                NameOpp = Name.y,
                betaOpp = beta.y) %>%
  merge(dat_ncaa_m_rd2 %>%
          dplyr::select(Year, Name, ProbWinJoint) %>%
          group_by(Year, Name) %>%
          dplyr::summarise(ProbReach = sum(ProbWinJoint)), 
        by = c("Year", "Name")) %>%
  merge(dat_ncaa_m_rd2 %>%
          dplyr::select(Year, Name, ProbWinJoint) %>%
          group_by(Year, Name) %>%
          dplyr::summarise(ProbReachOpp = sum(ProbWinJoint)), 
        by.x = c("Year", "NameOpp"), by.y = c("Year", "Name")) %>%
  mutate(ProbMatchup = ProbReach * ProbReachOpp) %>%
  mutate(ProbWinJoint = ProbWinCond * ProbMatchup) %>%
  dplyr::select(Year, Sigma, Region:Group3, Round, Seed, Name, 
                ProbReach, beta, SeedOpp, NameOpp, ProbReachOpp, betaOpp, 
                ProbMatchup, ProbWinCond, ProbWinJoint) %>%
  arrange(Year, Region, Group1, Group2, Group3, Seed)

dat_ncaa_m_rd4 <- merge(dat_ncaa_m_teams, dat_ncaa_m_teams, by = c("Year", "Region")) %>% 
  filter(Group1.x != Group1.y) %>%
  merge(dat_sigma2_m, by = "Year") %>%
  mutate(Round = 4,
         ProbWinCond = pnorm((beta.x - beta.y) / Sigma)) %>%
  dplyr::rename(Group3 = Group3.x,
                Group2 = Group2.x,
                Group1 = Group1.x,
                Seed = Seed.x,
                Name = Name.x,
                beta = beta.x,
                SeedOpp = Seed.y,
                NameOpp = Name.y,
                betaOpp = beta.y) %>%
  merge(dat_ncaa_m_rd3 %>%
          dplyr::select(Year, Name, ProbWinJoint) %>%
          group_by(Year, Name) %>%
          dplyr::summarise(ProbReach = sum(ProbWinJoint)), 
        by = c("Year", "Name")) %>%
  merge(dat_ncaa_m_rd3 %>%
          dplyr::select(Year, Name, ProbWinJoint) %>%
          group_by(Year, Name) %>%
          dplyr::summarise(ProbReachOpp = sum(ProbWinJoint)), 
        by.x = c("Year", "NameOpp"), by.y = c("Year", "Name")) %>%
  mutate(ProbMatchup = ProbReach * ProbReachOpp) %>%
  mutate(ProbWinJoint = ProbWinCond * ProbMatchup) %>%
  dplyr::select(Year, Sigma, Region:Group3, Round, Seed, Name, 
                ProbReach, beta, SeedOpp, NameOpp, ProbReachOpp, betaOpp, 
                ProbMatchup, ProbWinCond, ProbWinJoint) %>%
  arrange(Year, Region, Group1, Group2, Group3, Seed)

dat_ncaa_m_prob <- rbind(dat_ncaa_m_rd1[,c("Year", "Sigma", "Region", "Group1",      
                                           "Group2", "Group3", "Round", "Seed",        
                                           "Name", "ProbReach", "beta", "SeedOpp",     
                                           "NameOpp", "ProbReachOpp", "betaOpp", "ProbMatchup", 
                                           "ProbWinCond", "ProbWinJoint")], 
                         dat_ncaa_m_rd2[,c("Year", "Sigma", "Region", "Group1",      
                                           "Group2", "Group3", "Round", "Seed",        
                                           "Name", "ProbReach", "beta", "SeedOpp",     
                                           "NameOpp", "ProbReachOpp", "betaOpp", "ProbMatchup", 
                                           "ProbWinCond", "ProbWinJoint")],
                         dat_ncaa_m_rd3,
                         dat_ncaa_m_rd4) %>%
  group_by(Year, Region, Seed, Name, beta, Round) %>%
  dplyr::summarise(ProbWin = sum(ProbWinJoint)) %>%
  spread(Round, ProbWin) %>%
  as.data.frame()
names(dat_ncaa_m_prob)[6:9] <- paste("Rd", names(dat_ncaa_m_prob)[6:9], sep = "")

dat_ncaa_m_prob_by_seed <- rbind(dat_ncaa_m_rd1[,c("Year", "Sigma", "Region", "Group1",      
                                                   "Group2", "Group3", "Round", "Seed",        
                                                   "Name", "ProbReach", "beta", "SeedOpp",     
                                                   "NameOpp", "ProbReachOpp", "betaOpp", "ProbMatchup", 
                                                   "ProbWinCond", "ProbWinJoint")], 
                                 dat_ncaa_m_rd2[,c("Year", "Sigma", "Region", "Group1",      
                                                   "Group2", "Group3", "Round", "Seed",        
                                                   "Name", "ProbReach", "beta", "SeedOpp",     
                                                   "NameOpp", "ProbReachOpp", "betaOpp", "ProbMatchup", 
                                                   "ProbWinCond", "ProbWinJoint")],
                                 dat_ncaa_m_rd3,
                                 dat_ncaa_m_rd4) %>%
  group_by(Year, Region, Seed, Name, beta, Round) %>%
  dplyr::summarise(ProbWin = sum(ProbWinJoint)) %>%
  group_by(Seed, Round) %>%
  dplyr::summarise(ProbWin = mean(ProbWin)) %>%
  spread(Round, ProbWin) %>%
  as.data.frame()
names(dat_ncaa_m_prob_by_seed)[2:5] <- paste("Rd", names(dat_ncaa_m_prob_by_seed)[2:5], sep = "")


dat_ncaa_m_results <- rbind(dat_ncaa_m %>% 
                              dplyr::select(Year, Round, Name1, Seed1, Win1, Region:Group3) %>%
                              dplyr::rename(Seed = Seed1,
                                            Name = Name1,
                                            Win = Win1) %>%
                              as.data.frame(),
                            dat_ncaa_m %>% 
                              dplyr::select(Year, Round, Name2, Seed2, Win2, Region:Group3) %>%
                              dplyr::rename(Seed = Seed2,
                                            Name = Name2,
                                            Win = Win2) %>%
                              as.data.frame()
) %>%
  dplyr::select(Year, Round, Win, Region:Group3, Seed, Name) %>%
  arrange(Year, Round, Region, Group1, Group2, Group3, Seed)

N_Tot_m <- length(dat_ncaa_m_results$Seed[dat_ncaa_m_results$Round == 1]) / 16

dat_ncaa_m_results_by_seed <- dat_ncaa_m_results %>%
  group_by(Round, Seed) %>%
  dplyr::summarise(NWin = sum(Win),
                   NTot = N_Tot_m) %>%
  mutate(PercWin = NWin / NTot) %>%
  dplyr::select(-NWin, -NTot) %>%
  spread(Round, PercWin, fill = 0) %>%
  as.data.frame()
names(dat_ncaa_m_results_by_seed)[2:7] <- paste("Rd", names(dat_ncaa_m_results_by_seed)[2:7], sep = "")

##########check team strengh model for various second round matchups#########

t1 <- dat_ncaa_m_rd1 %>%
  group_by(Year, Region, Seed, Name, SeedOpp, NameOpp) %>%
  dplyr::summarise(ProbWin = sum(ProbWinCond)) %>%
  group_by(Seed, SeedOpp) %>%
  dplyr::summarise(ProbWinSeed1 = mean(ProbWin)) %>%
  filter(Seed < SeedOpp) %>%
  as.data.frame()

tt1 <- dat_ncaa_m %>%
  filter(Round == 1) %>%
  group_by(Seed1, Seed2) %>%
  summarise(N = n(), winsSeed1 = sum(Win1), winsSeed2 = sum(Win2)) %>% 
  mutate(ActualWin = winsSeed1 / (winsSeed1 + winsSeed2)) %>%
  as.data.frame()

ttt1 <- merge(tt1, t1, by.y = c("Seed", "SeedOpp"), by.x = c("Seed1", "Seed2"))

t2 <- dat_ncaa_m_rd2 %>%
  group_by(Year, Region, Seed, Name, SeedOpp, NameOpp) %>%
  dplyr::summarise(ProbWin = sum(ProbWinCond)) %>%
  group_by(Seed, SeedOpp) %>%
  dplyr::summarise(ProbWinSeed1 = mean(ProbWin)) %>%
  filter(Seed < SeedOpp) %>%
  as.data.frame()

tt2 <- dat_ncaa_m %>%
  filter(Round == 2) %>%
  group_by(Seed1, Seed2) %>%
  summarise(N = n(), winsSeed1 = sum(Win1), winsSeed2 = sum(Win2)) %>%
  as.data.frame()

for(i in 1:length(tt2[,1])) {
  if(tt2$Seed1[i] > tt2$Seed2[i]) {
    a1 <- tt2$Seed1[i]
    a2 <- tt2$Seed2[i]
    b1 <- tt2$winsSeed1[i]
    b2 <- tt2$winsSeed2[i]
    tt2$Seed1[i] <- a2
    tt2$Seed2[i] <- a1
    tt2$winsSeed1[i] <- b2
    tt2$winsSeed2[i] <- b1
  }
}

tt2 <- rbind(tt2, data.frame(Seed1 = 8, Seed2 = 16, N = 0, winsSeed1 = 0,
                             winsSeed2 = 0)) %>%
  mutate(ActualWin = winsSeed1 / (winsSeed1 + winsSeed2)) %>%
  as.data.frame()

ttt2 <- merge(tt2, t2, by.y = c("Seed", "SeedOpp"), by.x = c("Seed1", "Seed2")) %>%
  arrange(Seed1)

write.csv(ttt1, file = "rd1_matchups_actual_vs_probwin.csv", row.names = FALSE)
write.csv(ttt2, file = "rd2_matchups_actual_vs_probwin.csv", row.names = FALSE)

#put lower seed first always

#code for counting matchups
u1 <- dat_ncaa_m

for(i in 1:length(u1[,1])) {
  if(u1$Seed1[i] > u1$Seed2[i]) {
    a1 <- u1$Seed1[i]
    a2 <- u1$Seed2[i]
    b1 <- u1$Name1[i]
    b2 <- u1$Name2[i]
    c1 <- u1$Win1[i]
    c2 <- u1$Win2[i]
    u1$Seed1[i] <- a2
    u1$Seed2[i] <- a1
    u1$Name1[i] <- b2
    u1$Name2[i] <- b1
    u1$Win1[i] <- c2
    u1$Win2[i] <- c1
  }
}

u2 <- merge(u1, dat_beta_m[, c("Year", "Team", "beta")], 
            by.x = c("Year", "Name1"), 
            by.y = c("Year", "Team")) %>%
  dplyr::rename(beta1 = beta) %>%
  merge(dat_beta_m[, c("Year", "Team", "beta")], 
        by.x = c("Year", "Name2"), 
        by.y = c("Year", "Team")) %>%
  dplyr::rename(beta2 = beta) %>%
  merge(dat_sigma2_m, by = "Year") %>%
  mutate(ProbWin1 = pnorm((beta1 - beta2) / Sigma)) %>%
  group_by(Round, Seed1, Seed2) %>%
  dplyr::summarise(N = n(),
                   ProbWinSeed1 = mean(ProbWin1),
                   winsSeed1 = sum(Win1), 
                   winsSeed2 = sum(Win2)) %>%
  mutate(ActualWinSeed1 = winsSeed1 / (winsSeed1 + winsSeed2)) %>%
  as.data.frame()

write.csv(u2, file = "matchups_actual_vs_probwin.csv", row.names = FALSE)

#############################################################################

v1 <- dat_ncaa_m %>%
  group_by(Year, Round, Seed1) %>%
  summarise(N = n()) %>%
  rename(Seed = Seed1)

v2 <- dat_ncaa_m %>%
  group_by(Year, Round, Seed2) %>%
  summarise(N = n()) %>%
  rename(Seed = Seed2)

v_combos <- expand.grid(Year = unique(v2$Year),
                        Round = unique(v2$Round),
                        Seed = 1:16)

v3 <- rbind(v1, v2) %>%
  group_by(Year, Round, Seed) %>%
  summarise(N = sum(N)) %>%
  right_join(v_combos, by = c("Year", "Round", "Seed")) %>%
  arrange(Year, Round, Seed) %>%
  as.data.frame()

v3$N[is.na(v3$N)] <- 0

write.csv(v3, file = "appearances_by_year_rd_seed.csv", row.names = FALSE)

#############################################################################


x1 <- dat_beta_m %>%
  filter(Rank_beta <= 32) %>%
  dplyr::select(Year, Name1 = Team, Rank1 = Rank_beta, beta1 = beta) %>%
  mutate(Seed1 = ceiling(Rank1 / 4)) %>%
  arrange(Year, Rank1)

x2 <- dat_beta_m %>%
  filter(Rank_beta >= 33 & Rank_beta <= 64) %>%
  dplyr::select(Year, Name2 = Team, Rank2 = Rank_beta, beta2 = beta) %>%
  mutate(Seed2 = ceiling(Rank2 / 4)) %>%
  arrange(Year, desc(Rank2)) 

dat_hypoth_ncaa_m <- cbind(x1, x2[,-1])
dat_hypoth_ncaa_m$Region <- c(1,2,3,4,4,3,2,1)

dat_hypoth_ncaa_m$Group1 <- ifelse(dat_hypoth_ncaa_m$Seed1 %in% c(1,4,5,8,9,12,13,16), 1, 2)
dat_hypoth_ncaa_m$Group2 <- ifelse(dat_hypoth_ncaa_m$Seed1 %in% c(1,3,6,8,9,11,14,16), 1, 2)
dat_hypoth_ncaa_m$Group3 <- ifelse(dat_hypoth_ncaa_m$Seed1 %in% c(1,5,6,7,10,11,12,16), 1, 2)

dat_hypoth_ncaa_m_teams <- rbind(dat_hypoth_ncaa_m %>% 
                                   dplyr::select(Year, Name1, Seed1, Region:Group3) %>%
                                   dplyr::rename(Seed = Seed1,
                                                 Name = Name1) %>%
                                   as.data.frame(),
                                 dat_hypoth_ncaa_m %>% 
                                   dplyr::select(Year, Name2, Seed2, Region:Group3) %>%
                                   dplyr::rename(Seed = Seed2,
                                                 Name = Name2) %>%
                                   as.data.frame()
) %>%
  merge(dat_beta_m, by.x = c("Year", "Name"), by.y = c("Year", "Team")) %>%
  dplyr::select(Year, Region:Group3, Seed, Name, beta) %>%
  arrange(Year, Region, Group1, Group2, Group3, Seed)


dat_hypoth_ncaa_m_rd1 <- merge(dat_hypoth_ncaa_m_teams, dat_hypoth_ncaa_m_teams, by = c("Year", "Region", "Group1", "Group2", "Group3")) %>% 
  filter(Name.x != Name.y) %>%
  merge(dat_sigma2_m, by = "Year") %>%
  mutate(Round = 1,
         ProbWinCond = pnorm((beta.x - beta.y) / Sigma),
         ProbReach = 1,
         ProbReachOpp = 1) %>%
  dplyr::rename(Seed = Seed.x,
                Name = Name.x,
                beta = beta.x,
                SeedOpp = Seed.y,
                NameOpp = Name.y,
                betaOpp = beta.y) %>%
  mutate(ProbMatchup = ProbReach * ProbReachOpp) %>%
  mutate(ProbWinJoint = ProbWinCond * ProbMatchup) %>%
  dplyr::select(Year, Sigma, Region:Group3, Round, Seed, Name, 
                ProbReach, beta, SeedOpp, NameOpp, ProbReachOpp, betaOpp, 
                ProbMatchup, ProbWinCond, ProbWinJoint) %>%
  arrange(Year, Region, Group1, Group2, Group3, Seed)

dat_hypoth_ncaa_m_rd2 <- merge(dat_hypoth_ncaa_m_teams, dat_hypoth_ncaa_m_teams, by = c("Year", "Region", "Group1", "Group2")) %>% 
  filter(Group3.x != Group3.y) %>%
  merge(dat_sigma2_m, by = "Year") %>%
  mutate(Round = 2,
         ProbWinCond = pnorm((beta.x - beta.y) / Sigma)) %>%
  dplyr::rename(Group3 = Group3.x,
                Seed = Seed.x,
                Name = Name.x,
                beta = beta.x,
                SeedOpp = Seed.y,
                NameOpp = Name.y,
                betaOpp = beta.y) %>%
  merge(dat_hypoth_ncaa_m_rd1 %>%
          dplyr::select(Year, Name, ProbWinJoint), 
        by = c("Year", "Name")) %>%
  dplyr::rename(ProbReach = ProbWinJoint) %>%
  merge(dat_hypoth_ncaa_m_rd1 %>%
          dplyr::select(Year, Name, ProbWinJoint), 
        by.x = c("Year", "NameOpp"), by.y = c("Year", "Name")) %>%
  dplyr::rename(ProbReachOpp = ProbWinJoint) %>%
  mutate(ProbMatchup = ProbReach * ProbReachOpp) %>%
  mutate(ProbWinJoint = ProbWinCond * ProbMatchup) %>%
  dplyr::select(Year, Sigma, Region:Group3, Round, Seed, Name, 
                ProbReach, beta, SeedOpp, NameOpp, ProbReachOpp, betaOpp, 
                ProbMatchup, ProbWinCond, ProbWinJoint) %>%
  arrange(Year, Region, Group1, Group2, Group3, Seed)

dat_hypoth_ncaa_m_rd3 <- merge(dat_hypoth_ncaa_m_teams, dat_hypoth_ncaa_m_teams, by = c("Year", "Region", "Group1")) %>% 
  filter(Group2.x != Group2.y) %>%
  merge(dat_sigma2_m, by = "Year") %>%
  mutate(Round = 3,
         ProbWinCond = pnorm((beta.x - beta.y) / Sigma)) %>%
  dplyr::rename(Group3 = Group3.x,
                Group2 = Group2.x,
                Seed = Seed.x,
                Name = Name.x,
                beta = beta.x,
                SeedOpp = Seed.y,
                NameOpp = Name.y,
                betaOpp = beta.y) %>%
  merge(dat_hypoth_ncaa_m_rd2 %>%
          dplyr::select(Year, Name, ProbWinJoint) %>%
          group_by(Year, Name) %>%
          dplyr::summarise(ProbReach = sum(ProbWinJoint)), 
        by = c("Year", "Name")) %>%
  merge(dat_hypoth_ncaa_m_rd2 %>%
          dplyr::select(Year, Name, ProbWinJoint) %>%
          group_by(Year, Name) %>%
          dplyr::summarise(ProbReachOpp = sum(ProbWinJoint)), 
        by.x = c("Year", "NameOpp"), by.y = c("Year", "Name")) %>%
  mutate(ProbMatchup = ProbReach * ProbReachOpp) %>%
  mutate(ProbWinJoint = ProbWinCond * ProbMatchup) %>%
  dplyr::select(Year, Sigma, Region:Group3, Round, Seed, Name, 
                ProbReach, beta, SeedOpp, NameOpp, ProbReachOpp, betaOpp, 
                ProbMatchup, ProbWinCond, ProbWinJoint) %>%
  arrange(Year, Region, Group1, Group2, Group3, Seed)

dat_hypoth_ncaa_m_rd4 <- merge(dat_hypoth_ncaa_m_teams, dat_hypoth_ncaa_m_teams, by = c("Year", "Region")) %>% 
  filter(Group1.x != Group1.y) %>%
  merge(dat_sigma2_m, by = "Year") %>%
  mutate(Round = 4,
         ProbWinCond = pnorm((beta.x - beta.y) / Sigma)) %>%
  dplyr::rename(Group3 = Group3.x,
                Group2 = Group2.x,
                Group1 = Group1.x,
                Seed = Seed.x,
                Name = Name.x,
                beta = beta.x,
                SeedOpp = Seed.y,
                NameOpp = Name.y,
                betaOpp = beta.y) %>%
  merge(dat_hypoth_ncaa_m_rd3 %>%
          dplyr::select(Year, Name, ProbWinJoint) %>%
          group_by(Year, Name) %>%
          dplyr::summarise(ProbReach = sum(ProbWinJoint)), 
        by = c("Year", "Name")) %>%
  merge(dat_hypoth_ncaa_m_rd3 %>%
          dplyr::select(Year, Name, ProbWinJoint) %>%
          group_by(Year, Name) %>%
          dplyr::summarise(ProbReachOpp = sum(ProbWinJoint)), 
        by.x = c("Year", "NameOpp"), by.y = c("Year", "Name")) %>%
  mutate(ProbMatchup = ProbReach * ProbReachOpp) %>%
  mutate(ProbWinJoint = ProbWinCond * ProbMatchup) %>%
  dplyr::select(Year, Sigma, Region:Group3, Round, Seed, Name, 
                ProbReach, beta, SeedOpp, NameOpp, ProbReachOpp, betaOpp, 
                ProbMatchup, ProbWinCond, ProbWinJoint) %>%
  arrange(Year, Region, Group1, Group2, Group3, Seed)

dat_hypoth_ncaa_m_prob <- rbind(dat_hypoth_ncaa_m_rd1, 
                                dat_hypoth_ncaa_m_rd2,
                                dat_hypoth_ncaa_m_rd3,
                                dat_hypoth_ncaa_m_rd4) %>%
  group_by(Year, Region, Seed, Name, beta, Round) %>%
  dplyr::summarise(ProbWin = sum(ProbWinJoint)) %>%
  spread(Round, ProbWin) %>%
  as.data.frame()
names(dat_hypoth_ncaa_m_prob)[6:9] <- paste("Rd", names(dat_hypoth_ncaa_m_prob)[6:9], sep = "")

dat_hypoth_ncaa_m_prob_by_seed <- rbind(dat_hypoth_ncaa_m_rd1, 
                                        dat_hypoth_ncaa_m_rd2,
                                        dat_hypoth_ncaa_m_rd3,
                                        dat_hypoth_ncaa_m_rd4) %>%
  group_by(Year, Region, Seed, Name, beta, Round) %>%
  dplyr::summarise(ProbWin = sum(ProbWinJoint)) %>%
  group_by(Seed, Round) %>%
  dplyr::summarise(ProbWin = mean(ProbWin)) %>%
  spread(Round, ProbWin) %>%
  as.data.frame()
names(dat_hypoth_ncaa_m_prob_by_seed)[2:5] <- paste("Rd", names(dat_hypoth_ncaa_m_prob_by_seed)[2:5], sep = "")


z1 <- expand.grid(g1 = c(1,16), g2 = c(8,9), g3 = c(5,12), g4 = c(4,13), 
                  g5 = c(6,11), g6 = c(3,14), g7 = c(7,10), g8 = c(2,15),
                  r1 = 0, r2 = 0, r3 = 0, r4 = 0, r5 = 0, r6 = 0, r7 = 0, r8 = 0)

for(i in 1:length(z1[,1])) {
  z1[i,9:16] <- z1[i,1:8][order(z1[i,1:8])]
}

z2 <- rbind(z1[,1:8], z1[,1:8], z1[,1:8], z1[,1:8], z1[,1:8], z1[,1:8], z1[,1:8], z1[,1:8])
z2$Seed <- c(z1$r1, z1$r8, z1$r5, z1$r4, z1$r6, z1$r3, z1$r7, z1$r2)
z2$Seed_rs <- c(rep(1, 256), rep(8, 256), rep(5, 256), rep(4, 256), rep(6, 256), rep(3, 256), rep(7, 256), rep(2, 256))
z2$SeedOpp <- c(z1$r8, z1$r1, z1$r4, z1$r5, z1$r3, z1$r6, z1$r2, z1$r7)
z2$SeedOpp_rs <- c(rep(8, 256), rep(1, 256), rep(4, 256), rep(5, 256), rep(3, 256), rep(6, 256), rep(2, 256), rep(7, 256))

dat_ncaa_m_rd2_reseed <- merge(dat_ncaa_m_teams, dat_ncaa_m_teams, by = c("Year", "Region")) %>% 
  merge(dat_sigma2_m, by = "Year") %>%
  mutate(Round = 2,
         ProbWinCond = pnorm((beta.x - beta.y) / Sigma)) %>%
  dplyr::rename(Group1 = Group1.x,
                Group2 = Group2.x,
                Group3 = Group3.x,
                Seed = Seed.x,
                Name = Name.x,
                beta = beta.x,
                SeedOpp = Seed.y,
                NameOpp = Name.y,
                betaOpp = beta.y) %>%
  merge(z2, by = c("Seed", "SeedOpp")) %>%
  merge(dat_ncaa_m_rd1 %>%
          dplyr::select(Year, Region, Seed, ProbWinJoint), 
        by.x = c("Year", "Region", "g1"), by.y = c("Year", "Region", "Seed")) %>%
  dplyr::rename(Prob_g1 = ProbWinJoint) %>%
  merge(dat_ncaa_m_rd1 %>%
          dplyr::select(Year, Region, Seed, ProbWinJoint), 
        by.x = c("Year", "Region", "g2"), by.y = c("Year", "Region", "Seed")) %>%
  dplyr::rename(Prob_g2 = ProbWinJoint) %>%
  merge(dat_ncaa_m_rd1 %>%
          dplyr::select(Year, Region, Seed, ProbWinJoint), 
        by.x = c("Year", "Region", "g3"), by.y = c("Year", "Region", "Seed")) %>%
  dplyr::rename(Prob_g3 = ProbWinJoint) %>%
  merge(dat_ncaa_m_rd1 %>%
          dplyr::select(Year, Region, Seed, ProbWinJoint), 
        by.x = c("Year", "Region", "g4"), by.y = c("Year", "Region", "Seed")) %>%
  dplyr::rename(Prob_g4 = ProbWinJoint) %>%
  merge(dat_ncaa_m_rd1 %>%
          dplyr::select(Year, Region, Seed, ProbWinJoint), 
        by.x = c("Year", "Region", "g5"), by.y = c("Year", "Region", "Seed")) %>%
  dplyr::rename(Prob_g5 = ProbWinJoint) %>%
  merge(dat_ncaa_m_rd1 %>%
          dplyr::select(Year, Region, Seed, ProbWinJoint), 
        by.x = c("Year", "Region", "g6"), by.y = c("Year", "Region", "Seed")) %>%
  dplyr::rename(Prob_g6 = ProbWinJoint) %>%
  merge(dat_ncaa_m_rd1 %>%
          dplyr::select(Year, Region, Seed, ProbWinJoint), 
        by.x = c("Year", "Region", "g7"), by.y = c("Year", "Region", "Seed")) %>%
  dplyr::rename(Prob_g7 = ProbWinJoint) %>%
  merge(dat_ncaa_m_rd1 %>%
          dplyr::select(Year, Region, Seed, ProbWinJoint), 
        by.x = c("Year", "Region", "g8"), by.y = c("Year", "Region", "Seed")) %>%
  dplyr::rename(Prob_g8 = ProbWinJoint) %>%
  mutate(ProbRow = Prob_g1 * Prob_g2 * Prob_g3 * Prob_g4 * Prob_g5 * Prob_g6 * Prob_g7 * Prob_g8) %>%
  mutate(ProbWinJoint = ProbWinCond * ProbRow) %>%
  dplyr::select(Year, Sigma, Region, Round, g1, g2, g3, g4, g5, g6, g7, g8, 
                Prob_g1, Prob_g2, Prob_g3, Prob_g4, Prob_g5, Prob_g6, Prob_g7, Prob_g8,
                ProbRow, 
                Seed_rs, Seed, Name, 
                beta, SeedOpp_rs, SeedOpp, NameOpp, betaOpp, 
                ProbWinCond, ProbWinJoint) %>%
  arrange(Year, Region, Seed, SeedOpp)

dat_ncaa_m_rd2_reseed_prob_by_seed <- dat_ncaa_m_rd2_reseed %>%
  group_by(Year, Region, Seed, Name, beta) %>%
  dplyr::summarise(ProbWin = sum(ProbWinJoint)) %>%
  group_by(Seed) %>%
  dplyr::summarise(ProbWin = mean(ProbWin)) %>%
  as.data.frame()

dat_hypoth_ncaa_m_rd2_reseed <- merge(dat_hypoth_ncaa_m_teams, dat_hypoth_ncaa_m_teams, by = c("Year", "Region")) %>% 
  merge(dat_sigma2_m, by = "Year") %>%
  mutate(Round = 2,
         ProbWinCond = pnorm((beta.x - beta.y) / Sigma)) %>%
  dplyr::rename(Group1 = Group1.x,
                Group2 = Group2.x,
                Group3 = Group3.x,
                Seed = Seed.x,
                Name = Name.x,
                beta = beta.x,
                SeedOpp = Seed.y,
                NameOpp = Name.y,
                betaOpp = beta.y) %>%
  merge(z2, by = c("Seed", "SeedOpp")) %>%
  merge(dat_hypoth_ncaa_m_rd1 %>%
          dplyr::select(Year, Region, Seed, ProbWinJoint), 
        by.x = c("Year", "Region", "g1"), by.y = c("Year", "Region", "Seed")) %>%
  dplyr::rename(Prob_g1 = ProbWinJoint) %>%
  merge(dat_hypoth_ncaa_m_rd1 %>%
          dplyr::select(Year, Region, Seed, ProbWinJoint), 
        by.x = c("Year", "Region", "g2"), by.y = c("Year", "Region", "Seed")) %>%
  dplyr::rename(Prob_g2 = ProbWinJoint) %>%
  merge(dat_hypoth_ncaa_m_rd1 %>%
          dplyr::select(Year, Region, Seed, ProbWinJoint), 
        by.x = c("Year", "Region", "g3"), by.y = c("Year", "Region", "Seed")) %>%
  dplyr::rename(Prob_g3 = ProbWinJoint) %>%
  merge(dat_hypoth_ncaa_m_rd1 %>%
          dplyr::select(Year, Region, Seed, ProbWinJoint), 
        by.x = c("Year", "Region", "g4"), by.y = c("Year", "Region", "Seed")) %>%
  dplyr::rename(Prob_g4 = ProbWinJoint) %>%
  merge(dat_hypoth_ncaa_m_rd1 %>%
          dplyr::select(Year, Region, Seed, ProbWinJoint), 
        by.x = c("Year", "Region", "g5"), by.y = c("Year", "Region", "Seed")) %>%
  dplyr::rename(Prob_g5 = ProbWinJoint) %>%
  merge(dat_hypoth_ncaa_m_rd1 %>%
          dplyr::select(Year, Region, Seed, ProbWinJoint), 
        by.x = c("Year", "Region", "g6"), by.y = c("Year", "Region", "Seed")) %>%
  dplyr::rename(Prob_g6 = ProbWinJoint) %>%
  merge(dat_hypoth_ncaa_m_rd1 %>%
          dplyr::select(Year, Region, Seed, ProbWinJoint), 
        by.x = c("Year", "Region", "g7"), by.y = c("Year", "Region", "Seed")) %>%
  dplyr::rename(Prob_g7 = ProbWinJoint) %>%
  merge(dat_hypoth_ncaa_m_rd1 %>%
          dplyr::select(Year, Region, Seed, ProbWinJoint), 
        by.x = c("Year", "Region", "g8"), by.y = c("Year", "Region", "Seed")) %>%
  dplyr::rename(Prob_g8 = ProbWinJoint) %>%
  mutate(ProbRow = Prob_g1 * Prob_g2 * Prob_g3 * Prob_g4 * Prob_g5 * Prob_g6 * Prob_g7 * Prob_g8) %>%
  mutate(ProbWinJoint = ProbWinCond * ProbRow) %>%
  dplyr::select(Year, Sigma, Region, Round, Seed_rs, Seed, Name, 
                beta, SeedOpp_rs, SeedOpp, NameOpp, betaOpp, 
                ProbRow, ProbWinCond, ProbWinJoint) %>%
  arrange(Year, Region, Seed, SeedOpp)

dat_hypoth_ncaa_m_rd2_reseed_prob_by_seed <- dat_hypoth_ncaa_m_rd2_reseed %>%
  group_by(Year, Region, Seed, Name, beta) %>%
  dplyr::summarise(ProbWin = sum(ProbWinJoint)) %>%
  group_by(Seed) %>%
  dplyr::summarise(ProbWin = mean(ProbWin)) %>%
  as.data.frame()


# write.csv(head(dat_ncaa_m_rd2_reseed, 100), file = "rd2_reseed_sample.csv")

smry_by_seed_m <- data.frame(Seed = 1:16,
                             N = N_Tot_m,
                             Rd1_ActualWin = dat_ncaa_m_results_by_seed$Rd1,
                             Rd1_ProbWin = dat_ncaa_m_prob_by_seed$Rd1,
                             Rd1_ProbWin_T64 = dat_hypoth_ncaa_m_prob_by_seed$Rd1,
                             Rd2_ActualWin = dat_ncaa_m_results_by_seed$Rd2,
                             Rd2_ProbWin = dat_ncaa_m_prob_by_seed$Rd2,
                             Rd2_ProbWin_T64 = dat_hypoth_ncaa_m_prob_by_seed$Rd2,
                             Rd2_ProbWin_RS = dat_ncaa_m_rd2_reseed_prob_by_seed$ProbWin,
                             Rd2_ProbWin_T64_RS = dat_hypoth_ncaa_m_rd2_reseed_prob_by_seed$ProbWin,
                             Rd3_ActualWin = dat_ncaa_m_results_by_seed$Rd3,
                             Rd3_ProbWin = dat_ncaa_m_prob_by_seed$Rd3,
                             Rd3_ProbWin_T64 = dat_hypoth_ncaa_m_prob_by_seed$Rd3,
                             Rd4_ActualWin = dat_ncaa_m_results_by_seed$Rd4,
                             Rd4_ProbWin = dat_ncaa_m_prob_by_seed$Rd4,
                             Rd4_ProbWin_T64 = dat_hypoth_ncaa_m_prob_by_seed$Rd4
)

write.csv(smry_by_seed_m, file = "men_smry_by_seed.csv", row.names = FALSE)

dat_m_smry_plot <- rbind(dat_ncaa_m_results_by_seed %>% 
                           dplyr::select(-Rd5, -Rd6) %>% 
                           gather(Round, Prob, Rd1:Rd4) %>% 
                           mutate(Model = "Actual"),
                         dat_ncaa_m_prob_by_seed %>% 
                           gather(Round, Prob, Rd1:Rd4) %>% 
                           mutate(Model = "Team Strength"),
                         dat_hypoth_ncaa_m_prob_by_seed %>% 
                           gather(Round, Prob, Rd1:Rd4) %>% 
                           mutate(Model = "Team Strength with Top 64"),
                         dat_ncaa_m_rd2_reseed_prob_by_seed %>% 
                           mutate(Round = "Rd2",
                                  Model = "Team Strength with Re-seeding") %>%
                           dplyr::rename(Prob = ProbWin) %>% 
                           dplyr::select(Seed, Round, Prob, Model),
                         dat_hypoth_ncaa_m_rd2_reseed_prob_by_seed %>% 
                           mutate(Round = "Rd2",
                                  Model = "Team Strength with Top 64 and Re-seeding") %>%
                           dplyr::rename(Prob = ProbWin) %>% 
                           dplyr::select(Seed, Round, Prob, Model)
)


ggplot(dat_m_smry_plot, aes(x = Seed, y = Prob, group = Model)) + geom_line(aes(color = Model)) + 
  ylab("Prob. of Winning") + facet_wrap(~ Round) + scale_x_continuous(breaks = 1:16, minor_breaks = NULL) +
  ggtitle("NCAA Men's Basketball Tournament")

# ggplot(dat_m_smry_plot %>% filter(Round == "Rd2"), aes(x = Seed, y = Prob, group = Model)) + geom_line(aes(color = Model)) + 
#   ylab("Prob. of Advancing to Sweet 16") + scale_x_continuous(breaks = 1:16, minor_breaks = NULL) +
#   ggtitle("NCAA Men's Basketball Tournament")









