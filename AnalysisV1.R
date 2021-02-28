# Analysis Script


# First things first lets just see if shit worked UGA 2020

MitesUGA.Aug2020 <- read.csv('MiteWashUGAAugust2020.csv',header=T, stringsAsFactors = F)

MitesUGA.Sep2020 <- read.csv('MiteWashUGASeptember2020.csv',header=T, stringsAsFactors = F)

UGAColonies2020 <- read.csv('UGAColonyInfo2020.csv',header=T, stringsAsFactors = F)

# Simplest analysis: change in PMI based on treatment vs control

MitesGA.20 <- UGAColonies2020
MitesGA.20 <- MitesGA.20[which(MitesGA.20$Survived == 1),]
MitesGA.20$DeltaPMI <- NA

for(H in 1:NROW(MitesGA.20)){
  
  MitesGA.20$DeltaPMI[H] <- (MitesUGA.Sep2020$Percent[which(MitesUGA.Sep2020$Colony == MitesUGA.Sep2020$Colony[H])]
                             -
                               MitesUGA.Aug2020$Percent[which(MitesUGA.Aug2020$Colony == MitesUGA.Aug2020$Colony[H])]
  )
  
}

shapiro.test(MitesGA.20$DeltaPMI)

hist(MitesGA.20$DeltaPMI)

DMMod.1 <- glm(DeltaPMI ~ Yard + Treatment, 
               family = 'gaussian',
               data = MitesGA.20)

anova(DMMod.1, test = 'F')

boxplot(MitesGA.20$DeltaPMI ~ MitesGA.20$Treatment)  

## colony health metrics

HealthUGA.Aug20 <- read.csv('UGAColonyAssessments2020August.csv',header=T, stringsAsFactors = F)

HealthUGA.Sep20 <- read.csv('UGAColonyAssessments2020September.csv',header=T, stringsAsFactors = F)

HealthUGA.Aug20$Multiplier <- HealthUGA.Aug20$FrameType == 'Deep'
HealthUGA.Aug20$Multiplier[which(HealthUGA.Aug20$Multiplier)] <- 1
HealthUGA.Aug20$Multiplier[which(!HealthUGA.Aug20$Multiplier)] <- 0.5

HealthUGA.Sep20$Multiplier <- HealthUGA.Sep20$FrameType == 'Deep'
HealthUGA.Sep20$Multiplier[which(HealthUGA.Sep20$Multiplier)] <- 1
HealthUGA.Sep20$Multiplier[which(!HealthUGA.Sep20$Multiplier)] <- 0.5

MitesGA.20$DeltaBrood <- NA
MitesGA.20$DeltaBees <- NA
MitesGA.20$DeltaFood <- NA

for(H in 1:NROW(MitesGA.20)){
  
  MitesGA.20$DeltaBrood[H] <- (sum((HealthUGA.Sep20$Brood*HealthUGA.Sep20$Multiplier)[which(HealthUGA.Sep20$Colony == MitesGA.20$Colony[H])])
                               -  
                                 sum((HealthUGA.Aug20$Brood*HealthUGA.Aug20$Multiplier)[which(HealthUGA.Aug20$Colony == MitesGA.20$Colony[H])])
  )
  
  
  
  MitesGA.20$DeltaBees[H] <- (sum((HealthUGA.Sep20$Bees*HealthUGA.Sep20$Multiplier)[which(HealthUGA.Sep20$Colony == MitesGA.20$Colony[H])])
                              -  
                                sum((HealthUGA.Aug20$Bees*HealthUGA.Aug20$Multiplier)[which(HealthUGA.Aug20$Colony == MitesGA.20$Colony[H])])
  )
  
  
  
  MitesGA.20$DeltaFood[H] <- (sum((HealthUGA.Sep20$Honey*HealthUGA.Sep20$Multiplier)[which(HealthUGA.Sep20$Colony == MitesGA.20$Colony[H])])
                              -  
                                sum((HealthUGA.Aug20$Honey*HealthUGA.Aug20$Multiplier)[which(HealthUGA.Aug20$Colony == MitesGA.20$Colony[H])])
  )
  
}


par(mfrow = c(1,3))

shapiro.test(MitesGA.20$DeltaBrood)
shapiro.test(MitesGA.20$DeltaBees)
shapiro.test(MitesGA.20$DeltaFood)

hist(MitesGA.20$DeltaBrood)
hist(MitesGA.20$DeltaBees)
hist(MitesGA.20$DeltaFood)


DBrMod.1 <- glm(DeltaBrood ~ Yard + Treatment, 
                family = 'gaussian',
                data = MitesGA.20)

anova(DBrMod.1, test = 'F')

boxplot(MitesGA.20$DeltaBrood ~ MitesGA.20$Treatment)  


DBeMod.1 <- glm(DeltaBees ~ Yard + Treatment, 
                family = 'gaussian',
                data = MitesGA.20)

anova(DBeMod.1, test = 'F')

boxplot(MitesGA.20$DeltaBees ~ MitesGA.20$Treatment)  


DHoMod.1 <- glm(DeltaFood ~ Yard + Treatment, 
                family = 'gaussian',
                data = MitesGA.20)

anova(DHoMod.1, test = 'F')

boxplot(MitesGA.20$DeltaFood ~ MitesGA.20$Treatment)  

# Auburn 2020

FullData.AU.20 <- read.csv('Auburn20.csv',header=T, stringsAsFactors = F)

MitesAU.20 <- data.frame(Colony = 
                           (unique(FullData.AU.20$Colony))[-(match(FullData.AU.20$Colony[which(FullData.AU.20$Survived == 0)] , unique(FullData.AU.20$Colony)))])

MitesAU.20$Treatment <- NA
MitesAU.20$DeltaPMI <- NA
MitesAU.20$DeltaBees <- NA
MitesAU.20$DeltaBrood <- NA
MitesAU.20$DeltaFood <- NA


for(N in 1:NROW(MitesAU.20)){
  
  MitesAU.20$Treatment[N] <- unique(FullData.AU.20$Treatment[which(FullData.AU.20$Colony == MitesAU.20$Colony[N])])
  
  MitesAU.20$DeltaPMI[N] <- (FullData.AU.20$Mites[which(FullData.AU.20$Colony == MitesAU.20$Colony[N] & FullData.AU.20$Period == max(FullData.AU.20$Period))]
                             -
                            FullData.AU.20$Mites[which(FullData.AU.20$Colony == MitesAU.20$Colony[N] & FullData.AU.20$Period == min(FullData.AU.20$Period))])
  
  MitesAU.20$DeltaBees[N] <- (FullData.AU.20$AdultWorkers[which(FullData.AU.20$Colony == MitesAU.20$Colony[N] & FullData.AU.20$Period == max(FullData.AU.20$Period))]
                              -
                                FullData.AU.20$AdultWorkers[which(FullData.AU.20$Colony == MitesAU.20$Colony[N] & FullData.AU.20$Period == min(FullData.AU.20$Period))])
  
  MitesAU.20$DeltaBrood[N] <- (FullData.AU.20$BroodCells[which(FullData.AU.20$Colony == MitesAU.20$Colony[N] & FullData.AU.20$Period == max(FullData.AU.20$Period))]
                               -
                                 FullData.AU.20$BroodCells[which(FullData.AU.20$Colony == MitesAU.20$Colony[N] & FullData.AU.20$Period == min(FullData.AU.20$Period))])
  
  MitesAU.20$DeltaFood[N] <- (FullData.AU.20$Honey[which(FullData.AU.20$Colony == MitesAU.20$Colony[N] & FullData.AU.20$Period == max(FullData.AU.20$Period))]
                              -
                                FullData.AU.20$Honey[which(FullData.AU.20$Colony == MitesAU.20$Colony[N] & FullData.AU.20$Period == min(FullData.AU.20$Period))])
  
}

par(mfrow=c(1,1))

DMMod.2 <- glm(DeltaPMI ~ Treatment, 
               family = 'gaussian',
               data = MitesAU.20)

anova(DMMod.2, test = 'F')

boxplot(MitesAU.20$DeltaPMI ~ MitesAU.20$Treatment)  

par(mfrow=c(1,3))


DBrMod.2 <- glm(DeltaBrood ~ Treatment, 
                family = 'gaussian',
                data = MitesAU.20)

anova(DBrMod.2, test = 'F')

boxplot(MitesAU.20$DeltaBrood ~ MitesAU.20$Treatment)  


DBeMod.2 <- glm(DeltaBees ~ Treatment, 
                family = 'gaussian',
                data = MitesAU.20)

anova(DBeMod.2, test = 'F')

boxplot(MitesAU.20$DeltaBees ~ MitesAU.20$Treatment)  


DHoMod.2 <- glm(DeltaFood ~ Treatment, 
                family = 'gaussian',
                data = MitesAU.20)

anova(DHoMod.2, test = 'F')

boxplot(MitesAU.20$DeltaFood ~ MitesAU.20$Treatment)  


## Auburn '19 Data next

FullData.AU.19 <- read.csv('Auburn19.csv',header=T, stringsAsFactors = F)

MitesAU.19 <- data.frame(Colony = 
                           (unique(FullData.AU.19$Colony))[-(match(FullData.AU.19$Colony[which(FullData.AU.19$Survived == 0)] , unique(FullData.AU.19$Colony)))])

MitesAU.19$Treatment <- NA
MitesAU.19$DeltaPMI <- NA
MitesAU.19$DeltaBees <- NA
MitesAU.19$DeltaBrood <- NA


for(N in 1:NROW(MitesAU.19)){
  
  MitesAU.19$Treatment[N] <- unique(FullData.AU.19$Treatment[which(FullData.AU.19$Colony == MitesAU.19$Colony[N])])
  
  MitesAU.19$DeltaPMI[N] <- (FullData.AU.19$Mites[which(FullData.AU.19$Colony == MitesAU.19$Colony[N] & FullData.AU.19$Period == max(FullData.AU.19$Period))]
                             -
                               FullData.AU.19$Mites[which(FullData.AU.19$Colony == MitesAU.19$Colony[N] & FullData.AU.19$Period == min(FullData.AU.19$Period))])
  
  MitesAU.19$DeltaBees[N] <- (FullData.AU.19$AdultWorkers[which(FullData.AU.19$Colony == MitesAU.19$Colony[N] & FullData.AU.19$Period == max(FullData.AU.19$Period))]
                              -
                                FullData.AU.19$AdultWorkers[which(FullData.AU.19$Colony == MitesAU.19$Colony[N] & FullData.AU.19$Period == min(FullData.AU.19$Period))])
  
  MitesAU.19$DeltaBrood[N] <- (FullData.AU.19$BroodCells[which(FullData.AU.19$Colony == MitesAU.19$Colony[N] & FullData.AU.19$Period == max(FullData.AU.19$Period))]
                               -
                                 FullData.AU.19$BroodCells[which(FullData.AU.19$Colony == MitesAU.19$Colony[N] & FullData.AU.19$Period == min(FullData.AU.19$Period))])
  
}

par(mfrow=c(1,1))

DMMod.2 <- glm(DeltaPMI ~ Treatment, 
               family = 'gaussian',
               data = MitesAU.19)

anova(DMMod.2, test = 'F')

boxplot(MitesAU.19$DeltaPMI ~ MitesAU.19$Treatment)  

par(mfrow=c(1,3))


DBrMod.2 <- glm(DeltaBrood ~ Treatment, 
                family = 'gaussian',
                data = MitesAU.19)

anova(DBrMod.2, test = 'F')

boxplot(MitesAU.19$DeltaBrood ~ MitesAU.19$Treatment)  


DBeMod.2 <- glm(DeltaBees ~ Treatment, 
                family = 'gaussian',
                data = MitesAU.19)

anova(DBeMod.2, test = 'F')

boxplot(MitesAU.19$DeltaBees ~ MitesAU.19$Treatment)  

# GA '19




