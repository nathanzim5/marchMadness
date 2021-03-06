library(dplyr)
library(tidyr)
library(ggplot2)

setwd("C:\\Users\\User\\Desktop\\ncaa")

dat_beta_w <- read.csv("NCAA_Basketball_HCA_Analysis-master\\Women\\Output\\out_tab_WBB_2019.csv")
dat_ncaa_w <- read.csv("data_NCAA_1996_2019_women.csv")
dat_sigma_w <- read.csv("NCAA_Basketball_HCA_Analysis-master\\Women\\Output\\ANOVA_tab_WBB_2019.csv")
#dat_aq <- read.csv("automaticQualifiers.csv")

#names(dat_aq)[1] <- "X"

#add year column to beta data
dat_beta_w$Year <- as.numeric(gsub("....-", "", dat_beta_w$Season))

# #check for errors in aq data
# try <- merge(dat_aq, dat_beta_w, by = c("Team", "Year"), all.x = TRUE) %>%
#   filter(Year >= min(dat_beta_w$Year) & is.na(beta))

# check for errors in tourney data
# try <- merge(dat_ncaa_w, dat_beta_w, by.x = c("Name1", "Year"), by.y = c("Team", "Year"), all.x = TRUE) %>%
#   filter(Year >= min(dat_beta_w$Year) & is.na(beta))

# try <- merge(dat_ncaa_w, dat_beta_w, by.x = c("Name2", "Year"), by.y = c("Team", "Year"), all.x = TRUE) %>%
#   filter(Year >= min(dat_beta_w$Year) & is.na(beta))

#add bracket location to tourney data
dat_ncaa_w$Region <- sub("[0-9]+", "", dat_ncaa_w$Seed1) 
dat_ncaa_w$Region[dat_ncaa_w$Round > 4] <- NA
dat_ncaa_w$Seed1 <- as.numeric(sub("[^0-9]+", "", dat_ncaa_w$Seed1))
dat_ncaa_w$Seed2 <- as.numeric(sub("[^0-9]+", "", dat_ncaa_w$Seed2))
dat_ncaa_w$Group1 <- ifelse(dat_ncaa_w$Round > 3, NA, ifelse(dat_ncaa_w$Seed1 %in% c(1,8,5,4,16,9,12,13), 1, 2))
dat_ncaa_w$Group2 <- ifelse(dat_ncaa_w$Round > 2, NA, ifelse(dat_ncaa_w$Seed1 %in% c(1,8,6,3,16,9,11,14), 1, 2))
dat_ncaa_w$Group3 <- ifelse(dat_ncaa_w$Round > 1, NA, ifelse(dat_ncaa_w$Seed1 %in% c(1,5,6,7,10,11,12,16), 1, 2))
dat_ncaa_w$Win1 <- ifelse(dat_ncaa_w$Score1 > dat_ncaa_w$Score2, 1, 0)
dat_ncaa_w$Win2 <- ifelse(dat_ncaa_w$Score2 > dat_ncaa_w$Score1, 1, 0)

#filter and format sigma data
dat_sigma2_w <- dat_sigma_w[grep("Residual", dat_sigma_w$X),]
dat_sigma2_w$Year <- gsub("\\(....-", "", dat_sigma2_w$X)
dat_sigma2_w$Year <- gsub(") Residual", "", dat_sigma2_w$Year)
dat_sigma2_w$Sigma <- sqrt(dat_sigma2_w$MS)
dat_sigma2_w <- data.frame(Year = as.character(dat_sigma2_w$Year), Sigma = dat_sigma2_w$Sigma)

#output summary of results for first round seed matchups
out_rd1_smry_by_year_and_seed <- dat_ncaa_w %>%
  filter(Round == 1) %>%
  group_by(Year, Seed1, Seed2) %>%
  summarise(winsSeed1 = sum(Win1), winsSeed2 = sum(Win2)) %>% 
  as.data.frame()

#write.csv(out_rd1_smry_by_year_and_seed, file = "smry_rd1_wins_by_year_and_seed_w.csv")


#create new ncaa team table
dat_ncaa_w_teams <- rbind(dat_ncaa_w %>% 
                            filter(Round == 1) %>% 
                            dplyr::select(Year, Name1, Seed1, Region:Group3) %>%
                            dplyr::rename(Seed = Seed1,
                                          Name = Name1) %>%
                            as.data.frame(),
                          dat_ncaa_w %>% 
                            filter(Round == 1) %>% 
                            dplyr::select(Year, Name2, Seed2, Region:Group3) %>%
                            dplyr::rename(Seed = Seed2,
                                          Name = Name2) %>%
                            as.data.frame()
) %>%
  merge(dat_beta_w, by.x = c("Year", "Name"), by.y = c("Year", "Team")) %>%
  #merge(dat_aq[dat_aq$Gender == "M",], by.x = c("Year", "Name"), by.y = c("Year", "Team"), all.x = TRUE) %>%
  #dplyr::mutate(AQ = ifelse(Year %in% unique(dat_aq$Year), ifelse(is.na(X.y), 0, 1), NA)) %>%
  dplyr::select(Year, Region:Group3, Seed, Name, beta) %>%
  arrange(Year, Region, Group1, Group2, Group3, Seed)

# a1 <- dat_ncaa_w_teams %>% group_by(Year) %>% dplyr::summarise(AQ = sum(AQ)) %>% as.data.frame()
# a2 <- dat_ncaa_w_teams %>% group_by(Seed) %>% dplyr::summarise(AQ = sum(AQ, na.rm = TRUE)) %>% as.data.frame()
# 
# write.csv(a1, file = "aq_w_by_year.csv")
# write.csv(a2, file = "aq_w_by_seed.csv")


dat_ncaa_w_rd1 <- merge(dat_ncaa_w_teams, dat_ncaa_w_teams, by = c("Year", "Region", "Group1", "Group2", "Group3")) %>% 
  filter(Name.x != Name.y) %>%
  merge(dat_sigma2_w, by = "Year") %>%
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

dat_ncaa_w_rd2 <- merge(dat_ncaa_w_teams, dat_ncaa_w_teams, by = c("Year", "Region", "Group1", "Group2")) %>% 
  filter(Group3.x != Group3.y) %>%
  merge(dat_sigma2_w, by = "Year") %>%
  mutate(Round = 2,
         ProbWinCond = pnorm((beta.x - beta.y) / Sigma)) %>%
  dplyr::rename(Group3 = Group3.x,
                Seed = Seed.x,
                Name = Name.x,
                beta = beta.x,
                SeedOpp = Seed.y,
                NameOpp = Name.y,
                betaOpp = beta.y) %>%
  merge(dat_ncaa_w_rd1 %>%
          dplyr::select(Year, Name, ProbWinJoint), 
        by = c("Year", "Name")) %>%
  dplyr::rename(ProbReach = ProbWinJoint) %>%
  merge(dat_ncaa_w_rd1 %>%
          dplyr::select(Year, Name, ProbWinJoint), 
        by.x = c("Year", "NameOpp"), by.y = c("Year", "Name")) %>%
  dplyr::rename(ProbReachOpp = ProbWinJoint) %>%
  mutate(ProbMatchup = ProbReach * ProbReachOpp) %>%
  mutate(ProbWinJoint = ProbWinCond * ProbMatchup) %>%
  dplyr::select(Year, Sigma, Region:Group3, Round, Seed, Name, 
                ProbReach, beta, SeedOpp, NameOpp, ProbReachOpp, betaOpp, 
                ProbMatchup, ProbWinCond, ProbWinJoint) %>%
  arrange(Year, Region, Group1, Group2, Group3, Seed)

dat_ncaa_w_rd3 <- merge(dat_ncaa_w_teams, dat_ncaa_w_teams, by = c("Year", "Region", "Group1")) %>% 
  filter(Group2.x != Group2.y) %>%
  merge(dat_sigma2_w, by = "Year") %>%
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
  merge(dat_ncaa_w_rd2 %>%
          dplyr::select(Year, Name, ProbWinJoint) %>%
          group_by(Year, Name) %>%
          dplyr::summarise(ProbReach = sum(ProbWinJoint)), 
        by = c("Year", "Name")) %>%
  merge(dat_ncaa_w_rd2 %>%
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

dat_ncaa_w_rd4 <- merge(dat_ncaa_w_teams, dat_ncaa_w_teams, by = c("Year", "Region")) %>% 
  filter(Group1.x != Group1.y) %>%
  merge(dat_sigma2_w, by = "Year") %>%
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
  merge(dat_ncaa_w_rd3 %>%
          dplyr::select(Year, Name, ProbWinJoint) %>%
          group_by(Year, Name) %>%
          dplyr::summarise(ProbReach = sum(ProbWinJoint)), 
        by = c("Year", "Name")) %>%
  merge(dat_ncaa_w_rd3 %>%
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

dat_ncaa_w_prob <- rbind(dat_ncaa_w_rd1, 
                         dat_ncaa_w_rd2,
                         dat_ncaa_w_rd3,
                         dat_ncaa_w_rd4) %>%
  group_by(Year, Region, Seed, Name, beta, Round) %>%
  dplyr::summarise(ProbWin = sum(ProbWinJoint)) %>%
  spread(Round, ProbWin) %>%
  as.data.frame()
names(dat_ncaa_w_prob)[6:9] <- paste("Rd", names(dat_ncaa_w_prob)[6:9], sep = "")

dat_ncaa_w_prob_by_seed <- rbind(dat_ncaa_w_rd1, 
                                 dat_ncaa_w_rd2,
                                 dat_ncaa_w_rd3,
                                 dat_ncaa_w_rd4) %>%
  group_by(Year, Region, Seed, Name, beta, Round) %>%
  dplyr::summarise(ProbWin = sum(ProbWinJoint)) %>%
  group_by(Seed, Round) %>%
  dplyr::summarise(ProbWin = mean(ProbWin)) %>%
  spread(Round, ProbWin) %>%
  as.data.frame()
names(dat_ncaa_w_prob_by_seed)[2:5] <- paste("Rd", names(dat_ncaa_w_prob_by_seed)[2:5], sep = "")


dat_ncaa_w_results <- rbind(dat_ncaa_w %>% 
                              dplyr::select(Year, Round, Name1, Seed1, Win1, Region:Group3) %>%
                              dplyr::rename(Seed = Seed1,
                                            Name = Name1,
                                            Win = Win1) %>%
                              as.data.frame(),
                            dat_ncaa_w %>% 
                              dplyr::select(Year, Round, Name2, Seed2, Win2, Region:Group3) %>%
                              dplyr::rename(Seed = Seed2,
                                            Name = Name2,
                                            Win = Win2) %>%
                              as.data.frame()
) %>%
  filter(Year %in% unique(dat_beta_w$Year)) %>%
  dplyr::select(Year, Round, Win, Region:Group3, Seed, Name) %>%
  arrange(Year, Round, Region, Group1, Group2, Group3, Seed)

N_Tot_m <- length(dat_ncaa_w_results$Seed[dat_ncaa_w_results$Round == 1]) / 16

dat_ncaa_w_results_by_seed <- dat_ncaa_w_results %>%
  group_by(Round, Seed) %>%
  dplyr::summarise(NWin = sum(Win),
                   NTot = N_Tot_m) %>%
  mutate(PercWin = NWin / NTot) %>%
  dplyr::select(-NWin, -NTot) %>%
  spread(Round, PercWin, fill = 0) %>%
  as.data.frame()
names(dat_ncaa_w_results_by_seed)[2:7] <- paste("Rd", names(dat_ncaa_w_results_by_seed)[2:7], sep = "")


#code for counting matchups
u1 <- dat_ncaa_w[dat_ncaa_w$Year >= 2002,]

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

u2 <- merge(u1, dat_beta_w[, c("Year", "Team", "beta")], 
            by.x = c("Year", "Name1"), 
            by.y = c("Year", "Team")) %>%
  dplyr::rename(beta1 = beta) %>%
  merge(dat_beta_w[, c("Year", "Team", "beta")], 
        by.x = c("Year", "Name2"), 
        by.y = c("Year", "Team")) %>%
  dplyr::rename(beta2 = beta) %>%
  merge(dat_sigma2_w, by = "Year") %>%
  mutate(ProbWin1 = pnorm((beta1 - beta2) / Sigma)) %>%
  group_by(Round, Seed1, Seed2) %>%
  dplyr::summarise(N = n(),
                   ProbWinSeed1 = mean(ProbWin1),
                   winsSeed1 = sum(Win1), 
                   winsSeed2 = sum(Win2)) %>%
  mutate(ActualWinSeed1 = winsSeed1 / (winsSeed1 + winsSeed2)) %>%
  as.data.frame()

write.csv(u2, file = "matchups_actual_vs_probwin_w.csv", row.names = FALSE)

#############################################################################








x1 <- dat_beta_w %>%
  filter(Rank_beta <= 32) %>%
  dplyr::select(Year, Name1 = Team, Rank1 = Rank_beta, beta1 = beta) %>%
  mutate(Seed1 = ceiling(Rank1 / 4)) %>%
  arrange(Year, Rank1)

x2 <- dat_beta_w %>%
  filter(Rank_beta >= 33 & Rank_beta <= 64) %>%
  dplyr::select(Year, Name2 = Team, Rank2 = Rank_beta, beta2 = beta) %>%
  mutate(Seed2 = ceiling(Rank2 / 4)) %>%
  arrange(Year, desc(Rank2)) 

dat_hypoth_ncaa_w <- cbind(x1, x2[,-1])
dat_hypoth_ncaa_w$Region <- c(1,2,3,4,4,3,2,1)

dat_hypoth_ncaa_w$Group1 <- ifelse(dat_hypoth_ncaa_w$Seed1 %in% c(1,4,5,8,9,12,13,16), 1, 2)
dat_hypoth_ncaa_w$Group2 <- ifelse(dat_hypoth_ncaa_w$Seed1 %in% c(1,3,6,8,9,11,14,16), 1, 2)
dat_hypoth_ncaa_w$Group3 <- ifelse(dat_hypoth_ncaa_w$Seed1 %in% c(1,5,6,7,10,11,12,16), 1, 2)

dat_hypoth_ncaa_w_teams <- rbind(dat_hypoth_ncaa_w %>% 
                                   dplyr::select(Year, Name1, Seed1, Region:Group3) %>%
                                   dplyr::rename(Seed = Seed1,
                                                 Name = Name1) %>%
                                   as.data.frame(),
                                 dat_hypoth_ncaa_w %>% 
                                   dplyr::select(Year, Name2, Seed2, Region:Group3) %>%
                                   dplyr::rename(Seed = Seed2,
                                                 Name = Name2) %>%
                                   as.data.frame()
) %>%
  merge(dat_beta_w, by.x = c("Year", "Name"), by.y = c("Year", "Team")) %>%
  dplyr::select(Year, Region:Group3, Seed, Name, beta) %>%
  arrange(Year, Region, Group1, Group2, Group3, Seed)


dat_hypoth_ncaa_w_rd1 <- merge(dat_hypoth_ncaa_w_teams, dat_hypoth_ncaa_w_teams, by = c("Year", "Region", "Group1", "Group2", "Group3")) %>% 
  filter(Name.x != Name.y) %>%
  merge(dat_sigma2_w, by = "Year") %>%
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

dat_hypoth_ncaa_w_rd2 <- merge(dat_hypoth_ncaa_w_teams, dat_hypoth_ncaa_w_teams, by = c("Year", "Region", "Group1", "Group2")) %>% 
  filter(Group3.x != Group3.y) %>%
  merge(dat_sigma2_w, by = "Year") %>%
  mutate(Round = 2,
         ProbWinCond = pnorm((beta.x - beta.y) / Sigma)) %>%
  dplyr::rename(Group3 = Group3.x,
                Seed = Seed.x,
                Name = Name.x,
                beta = beta.x,
                SeedOpp = Seed.y,
                NameOpp = Name.y,
                betaOpp = beta.y) %>%
  merge(dat_hypoth_ncaa_w_rd1 %>%
          dplyr::select(Year, Name, ProbWinJoint), 
        by = c("Year", "Name")) %>%
  dplyr::rename(ProbReach = ProbWinJoint) %>%
  merge(dat_hypoth_ncaa_w_rd1 %>%
          dplyr::select(Year, Name, ProbWinJoint), 
        by.x = c("Year", "NameOpp"), by.y = c("Year", "Name")) %>%
  dplyr::rename(ProbReachOpp = ProbWinJoint) %>%
  mutate(ProbMatchup = ProbReach * ProbReachOpp) %>%
  mutate(ProbWinJoint = ProbWinCond * ProbMatchup) %>%
  dplyr::select(Year, Sigma, Region:Group3, Round, Seed, Name, 
                ProbReach, beta, SeedOpp, NameOpp, ProbReachOpp, betaOpp, 
                ProbMatchup, ProbWinCond, ProbWinJoint) %>%
  arrange(Year, Region, Group1, Group2, Group3, Seed)

dat_hypoth_ncaa_w_rd3 <- merge(dat_hypoth_ncaa_w_teams, dat_hypoth_ncaa_w_teams, by = c("Year", "Region", "Group1")) %>% 
  filter(Group2.x != Group2.y) %>%
  merge(dat_sigma2_w, by = "Year") %>%
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
  merge(dat_hypoth_ncaa_w_rd2 %>%
          dplyr::select(Year, Name, ProbWinJoint) %>%
          group_by(Year, Name) %>%
          dplyr::summarise(ProbReach = sum(ProbWinJoint)), 
        by = c("Year", "Name")) %>%
  merge(dat_hypoth_ncaa_w_rd2 %>%
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

dat_hypoth_ncaa_w_rd4 <- merge(dat_hypoth_ncaa_w_teams, dat_hypoth_ncaa_w_teams, by = c("Year", "Region")) %>% 
  filter(Group1.x != Group1.y) %>%
  merge(dat_sigma2_w, by = "Year") %>%
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
  merge(dat_hypoth_ncaa_w_rd3 %>%
          dplyr::select(Year, Name, ProbWinJoint) %>%
          group_by(Year, Name) %>%
          dplyr::summarise(ProbReach = sum(ProbWinJoint)), 
        by = c("Year", "Name")) %>%
  merge(dat_hypoth_ncaa_w_rd3 %>%
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

dat_hypoth_ncaa_w_prob <- rbind(dat_hypoth_ncaa_w_rd1, 
                                dat_hypoth_ncaa_w_rd2,
                                dat_hypoth_ncaa_w_rd3,
                                dat_hypoth_ncaa_w_rd4) %>%
  group_by(Year, Region, Seed, Name, beta, Round) %>%
  dplyr::summarise(ProbWin = sum(ProbWinJoint)) %>%
  spread(Round, ProbWin) %>%
  as.data.frame()
names(dat_hypoth_ncaa_w_prob)[6:9] <- paste("Rd", names(dat_hypoth_ncaa_w_prob)[6:9], sep = "")

dat_hypoth_ncaa_w_prob_by_seed <- rbind(dat_hypoth_ncaa_w_rd1, 
                                        dat_hypoth_ncaa_w_rd2,
                                        dat_hypoth_ncaa_w_rd3,
                                        dat_hypoth_ncaa_w_rd4) %>%
  group_by(Year, Region, Seed, Name, beta, Round) %>%
  dplyr::summarise(ProbWin = sum(ProbWinJoint)) %>%
  group_by(Seed, Round) %>%
  dplyr::summarise(ProbWin = mean(ProbWin)) %>%
  spread(Round, ProbWin) %>%
  as.data.frame()
names(dat_hypoth_ncaa_w_prob_by_seed)[2:5] <- paste("Rd", names(dat_hypoth_ncaa_w_prob_by_seed)[2:5], sep = "")


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

dat_ncaa_w_rd2_reseed <- merge(dat_ncaa_w_teams, dat_ncaa_w_teams, by = c("Year", "Region")) %>% 
  merge(dat_sigma2_w, by = "Year") %>%
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
  merge(dat_ncaa_w_rd1 %>%
          dplyr::select(Year, Region, Seed, ProbWinJoint), 
        by.x = c("Year", "Region", "g1"), by.y = c("Year", "Region", "Seed")) %>%
  dplyr::rename(Prob_g1 = ProbWinJoint) %>%
  merge(dat_ncaa_w_rd1 %>%
          dplyr::select(Year, Region, Seed, ProbWinJoint), 
        by.x = c("Year", "Region", "g2"), by.y = c("Year", "Region", "Seed")) %>%
  dplyr::rename(Prob_g2 = ProbWinJoint) %>%
  merge(dat_ncaa_w_rd1 %>%
          dplyr::select(Year, Region, Seed, ProbWinJoint), 
        by.x = c("Year", "Region", "g3"), by.y = c("Year", "Region", "Seed")) %>%
  dplyr::rename(Prob_g3 = ProbWinJoint) %>%
  merge(dat_ncaa_w_rd1 %>%
          dplyr::select(Year, Region, Seed, ProbWinJoint), 
        by.x = c("Year", "Region", "g4"), by.y = c("Year", "Region", "Seed")) %>%
  dplyr::rename(Prob_g4 = ProbWinJoint) %>%
  merge(dat_ncaa_w_rd1 %>%
          dplyr::select(Year, Region, Seed, ProbWinJoint), 
        by.x = c("Year", "Region", "g5"), by.y = c("Year", "Region", "Seed")) %>%
  dplyr::rename(Prob_g5 = ProbWinJoint) %>%
  merge(dat_ncaa_w_rd1 %>%
          dplyr::select(Year, Region, Seed, ProbWinJoint), 
        by.x = c("Year", "Region", "g6"), by.y = c("Year", "Region", "Seed")) %>%
  dplyr::rename(Prob_g6 = ProbWinJoint) %>%
  merge(dat_ncaa_w_rd1 %>%
          dplyr::select(Year, Region, Seed, ProbWinJoint), 
        by.x = c("Year", "Region", "g7"), by.y = c("Year", "Region", "Seed")) %>%
  dplyr::rename(Prob_g7 = ProbWinJoint) %>%
  merge(dat_ncaa_w_rd1 %>%
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

dat_ncaa_w_rd2_reseed_prob_by_seed <- dat_ncaa_w_rd2_reseed %>%
  group_by(Year, Region, Seed, Name, beta) %>%
  dplyr::summarise(ProbWin = sum(ProbWinJoint)) %>%
  group_by(Seed) %>%
  dplyr::summarise(ProbWin = mean(ProbWin)) %>%
  as.data.frame()

dat_hypoth_ncaa_w_rd2_reseed <- merge(dat_hypoth_ncaa_w_teams, dat_hypoth_ncaa_w_teams, by = c("Year", "Region")) %>% 
  merge(dat_sigma2_w, by = "Year") %>%
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
  merge(dat_hypoth_ncaa_w_rd1 %>%
          dplyr::select(Year, Region, Seed, ProbWinJoint), 
        by.x = c("Year", "Region", "g1"), by.y = c("Year", "Region", "Seed")) %>%
  dplyr::rename(Prob_g1 = ProbWinJoint) %>%
  merge(dat_hypoth_ncaa_w_rd1 %>%
          dplyr::select(Year, Region, Seed, ProbWinJoint), 
        by.x = c("Year", "Region", "g2"), by.y = c("Year", "Region", "Seed")) %>%
  dplyr::rename(Prob_g2 = ProbWinJoint) %>%
  merge(dat_hypoth_ncaa_w_rd1 %>%
          dplyr::select(Year, Region, Seed, ProbWinJoint), 
        by.x = c("Year", "Region", "g3"), by.y = c("Year", "Region", "Seed")) %>%
  dplyr::rename(Prob_g3 = ProbWinJoint) %>%
  merge(dat_hypoth_ncaa_w_rd1 %>%
          dplyr::select(Year, Region, Seed, ProbWinJoint), 
        by.x = c("Year", "Region", "g4"), by.y = c("Year", "Region", "Seed")) %>%
  dplyr::rename(Prob_g4 = ProbWinJoint) %>%
  merge(dat_hypoth_ncaa_w_rd1 %>%
          dplyr::select(Year, Region, Seed, ProbWinJoint), 
        by.x = c("Year", "Region", "g5"), by.y = c("Year", "Region", "Seed")) %>%
  dplyr::rename(Prob_g5 = ProbWinJoint) %>%
  merge(dat_hypoth_ncaa_w_rd1 %>%
          dplyr::select(Year, Region, Seed, ProbWinJoint), 
        by.x = c("Year", "Region", "g6"), by.y = c("Year", "Region", "Seed")) %>%
  dplyr::rename(Prob_g6 = ProbWinJoint) %>%
  merge(dat_hypoth_ncaa_w_rd1 %>%
          dplyr::select(Year, Region, Seed, ProbWinJoint), 
        by.x = c("Year", "Region", "g7"), by.y = c("Year", "Region", "Seed")) %>%
  dplyr::rename(Prob_g7 = ProbWinJoint) %>%
  merge(dat_hypoth_ncaa_w_rd1 %>%
          dplyr::select(Year, Region, Seed, ProbWinJoint), 
        by.x = c("Year", "Region", "g8"), by.y = c("Year", "Region", "Seed")) %>%
  dplyr::rename(Prob_g8 = ProbWinJoint) %>%
  mutate(ProbRow = Prob_g1 * Prob_g2 * Prob_g3 * Prob_g4 * Prob_g5 * Prob_g6 * Prob_g7 * Prob_g8) %>%
  mutate(ProbWinJoint = ProbWinCond * ProbRow) %>%
  dplyr::select(Year, Sigma, Region, Round, Seed_rs, Seed, Name, 
                beta, SeedOpp_rs, SeedOpp, NameOpp, betaOpp, 
                ProbRow, ProbWinCond, ProbWinJoint) %>%
  arrange(Year, Region, Seed, SeedOpp)

dat_hypoth_ncaa_w_rd2_reseed_prob_by_seed <- dat_hypoth_ncaa_w_rd2_reseed %>%
  group_by(Year, Region, Seed, Name, beta) %>%
  dplyr::summarise(ProbWin = sum(ProbWinJoint)) %>%
  group_by(Seed) %>%
  dplyr::summarise(ProbWin = mean(ProbWin)) %>%
  as.data.frame()


# write.csv(head(dat_ncaa_w_rd2_reseed, 100), file = "rd2_reseed_sample.csv")

smry_by_seed_w <- data.frame(Seed = 1:16,
                             N = N_Tot_m,
                             Rd1_ActualWin = dat_ncaa_w_results_by_seed$Rd1,
                             Rd1_ProbWin = dat_ncaa_w_prob_by_seed$Rd1,
                             Rd1_ProbWin_T64 = dat_hypoth_ncaa_w_prob_by_seed$Rd1,
                             Rd2_ActualWin = dat_ncaa_w_results_by_seed$Rd2,
                             Rd2_ProbWin = dat_ncaa_w_prob_by_seed$Rd2,
                             Rd2_ProbWin_T64 = dat_hypoth_ncaa_w_prob_by_seed$Rd2,
                             Rd2_ProbWin_RS = dat_ncaa_w_rd2_reseed_prob_by_seed$ProbWin,
                             Rd2_ProbWin_T64_RS = dat_hypoth_ncaa_w_rd2_reseed_prob_by_seed$ProbWin,
                             Rd3_ActualWin = dat_ncaa_w_results_by_seed$Rd3,
                             Rd3_ProbWin = dat_ncaa_w_prob_by_seed$Rd3,
                             Rd3_ProbWin_T64 = dat_hypoth_ncaa_w_prob_by_seed$Rd3,
                             Rd4_ActualWin = dat_ncaa_w_results_by_seed$Rd4,
                             Rd4_ProbWin = dat_ncaa_w_prob_by_seed$Rd4,
                             Rd4_ProbWin_T64 = dat_hypoth_ncaa_w_prob_by_seed$Rd4
)

write.csv(smry_by_seed_w, file = "women_smry_by_seed.csv", row.names = FALSE)

dat_w_smry_plot <- rbind(dat_ncaa_w_results_by_seed %>% 
                           dplyr::select(-Rd5, -Rd6) %>% 
                           gather(Round, Prob, Rd1:Rd4) %>% 
                           mutate(Model = "Actual"),
                         dat_ncaa_w_prob_by_seed %>% 
                           gather(Round, Prob, Rd1:Rd4) %>% 
                           mutate(Model = "Team Strength"),
                         dat_hypoth_ncaa_w_prob_by_seed %>% 
                           gather(Round, Prob, Rd1:Rd4) %>% 
                           mutate(Model = "Team Strength with Top 64"),
                         dat_ncaa_w_rd2_reseed_prob_by_seed %>% 
                           mutate(Round = "Rd2",
                                  Model = "Team Strength with Re-seeding") %>%
                           dplyr::rename(Prob = ProbWin) %>% 
                           dplyr::select(Seed, Round, Prob, Model),
                         dat_hypoth_ncaa_w_rd2_reseed_prob_by_seed %>% 
                           mutate(Round = "Rd2",
                                  Model = "Team Strength with Top 64 and Re-seeding") %>%
                           dplyr::rename(Prob = ProbWin) %>% 
                           dplyr::select(Seed, Round, Prob, Model)
)


ggplot(dat_w_smry_plot, aes(x = Seed, y = Prob, group = Model)) + geom_line(aes(color = Model)) + 
  ylab("Prob. of Winning") + facet_wrap(~ Round) + scale_x_continuous(breaks = 1:16, minor_breaks = NULL) +
  ggtitle("NCAA Women's Basketball Tournament")

# ggplot(dat_w_smry_plot %>% filter(Round == "Rd2"), aes(x = Seed, y = Prob, group = Model)) + geom_line(aes(color = Model)) + 
#   ylab("Prob. of Advancing to Sweet 16") + scale_x_continuous(breaks = 1:16, minor_breaks = NULL) +
#   ggtitle("NCAA Women's Basketball Tournament")










