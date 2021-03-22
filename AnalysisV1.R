# Script by Lewis Bartlett
# lewis.bartlett@uga.edu
# Data analysis in collaboration with Jennifer Berry
# Oxalic Acid repeated sublimation study



#### Begin Analysis Script

# Analysis follows a structure whereby we analyse each site/year in isolation 
# and then undertake the 'full' technically correct composite analysis.

# We will start with the largest, arguably most important experment (UGA 2020)
# this dataset, uniquely, has 3 apiaries 
# (the remainders, UGA 2019, and Auburn 2019 + 2020, have one apiary each)

# Load in the data tables from the UGA 2020 experiments (2019 and Auburn experiments will come later)
#Read in table of mite data at start of experiment (August)
MitesUGA.Aug2020 <- read.csv(file = 'MiteWashUGAAugust2020.csv',header= T, stringsAsFactors = F)
#Read in table of mite data at end of experiment (Sept)
MitesUGA.Sep2020 <- read.csv(file = 'MiteWashUGASeptember2020.csv',header=T, stringsAsFactors = F)
# Read in colony reference table
UGAColonies2020 <- read.csv(file = 'UGAColonyInfo2020.csv',header=T, stringsAsFactors = F)

# Simplest analysis: change in PMI based on treatment vs control
# PMI = 'percent mite intensity' or 'mites per 100 bees'

# Select colonies which survived the experiment only for analysis
# (code clones the main colony reference data set then overwrites itself to only include surviver colonies)
MitesGA.20 <- UGAColonies2020
MitesGA.20 <- MitesGA.20[which(MitesGA.20$Survived == 1),]

# Add the variable of interest: change in mites per 100 bees between start and end of experiment
# 'Delta-PMI'
MitesGA.20$DeltaPMI <- NA

# Populate the DeltaPMI column using a for loop to calcuilate the metric per colony
for(H in 1:NROW(MitesGA.20)){
  
  MitesGA.20$DeltaPMI[H] <- (MitesUGA.Sep2020$Percent[which(MitesUGA.Sep2020$Colony == MitesUGA.Sep2020$Colony[H])]
                             -
                               MitesUGA.Aug2020$Percent[which(MitesUGA.Aug2020$Colony == MitesUGA.Aug2020$Colony[H])]
  )
  
}

# Look at the distribution of this data for good sense and closeness to normality
# (mind that it is models resilduals that will matter more)
shapiro.test(MitesGA.20$DeltaPMI)
hist(MitesGA.20$DeltaPMI)

# Analysis here is a Type 1 multivariate anova via a generalised linear model (Gaussian)
DMMod.1 <- glm(DeltaPMI ~ Yard + Treatment, 
               family = 'gaussian',
               data = MitesGA.20)
anova(DMMod.1, test = 'F')
# We see no significant effect of either yard or treatment

# Look at the corresponding box plot, grouped by treatment (ignore apiaries for plotting)
boxplot(MitesGA.20$DeltaPMI ~ MitesGA.20$Treatment)  

## Colony health metrics
# Exactly the same framework as above, |
# read in data tables
# calculate delta values (changes) from start to end
# and then analyse based on treatment assignment
# first we standardise data based on frame area to account for shallow vs deep frames

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

# All the above should map directly to the explanations for the delta PMI.
# We will specifically use these UGA 2020 colony health metrics in the paper
# (mite levels don't change, removing a confound in assessing treatment safety)

# Let us now repeat the above sectional analysis for the other location/year data sets
# framework should be near-identical with some subtle differences in data manipulation
# (different laboratory conventions on data formatting / storage)

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


## Auburn '19

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

## GA '19

MitesGA.19 <- read.csv('OAUGA2019.csv',header=T, stringsAsFactors = F)

par(mfrow=c(1,1))

DMMod.2 <- glm(DeltaPMI ~ Treatment, 
               family = 'gaussian',
               data = MitesGA.19)

anova(DMMod.2, test = 'F')

boxplot(MitesGA.19$DeltaPMI ~ MitesGA.19$Treatment)

# Repeated sectional analysis have been completed, some differences based on trials
# but some consistent trends - see paper write up

##########################################

##### OK TIME FOR COMBINED ANALYSIS ######

##########################################

# Add location and year to each data set ahead of time as that's easiest, 
# add 'yards' to those with only one yard for future full analysis.

MitesGA.19$Site <- 'GA'
MitesGA.19$Year <- '2019'
MitesGA.19$Yard <- 'Pond'

MitesAU.19$Site <- 'AL'
MitesAU.19$Year <- '2019'
MitesAU.19$Yard <- 'Auburn'

MitesGA.20$Site <- 'GA'
MitesGA.20$Year <- '2020'

MitesAU.20$Site <- 'AL'
MitesAU.20$Year <- '2020'
MitesAU.20$Yard <- 'Auburn'

# Combine into one large data frame for analysis

MitesAll <- na.omit(rbind(
  MitesAU.19[,c('Colony','Treatment','Yard','Site','Year','DeltaPMI')],
  MitesAU.20[,c('Colony','Treatment','Yard','Site','Year','DeltaPMI')],
  MitesGA.19[,c('Colony','Treatment','Yard','Site','Year','DeltaPMI')],
  MitesGA.20[,c('Colony','Treatment','Yard','Site','Year','DeltaPMI')]
))



## Main plot

# Quick transparency function for plotting
Transpa <- function(color, percent) {
  
  rgb.val <- col2rgb(color)
  
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100-percent)*255/100)
  
  return(t.col)
  
}

# Set plotting colours
ColRef <- data.frame(Treatment = levels(as.factor(MitesAll$Treatment)), Col =  c('purple2','orange3'))

# Makes the plots we would like
par(mar=c(5,5,2,2))

boxplot(MitesAll$DeltaPMI ~ MitesAll$Treatment*MitesAll$Year*MitesAll$Site, 
        main = NA, ylab = expression(paste(Delta, ' Percent Mite Intensity',sep='')), xlab = 'Treatment', 
        border = 'transparent', 
        cex.axis = 1.3, cex.lab = 1.5, outline = TRUE, lwd = 1.2,
        boxlty = 1, whisklty = 0, staplelty = 1, boxwex = 0.00, 
        col = sapply(X = as.character(ColRef$Col), FUN = Transpa, percent = 100))

abline(h =0 , lty = 3)

stripchart(MitesAll$DeltaPMI ~ MitesAll$Treatment*MitesAll$Year*MitesAll$Site,
           col = sapply(X = as.character(ColRef$Col), FUN = Transpa, percent = 40),
           vertical = T, add = T, pch = 4, cex = 0.65, 
           method = 'jitter', lwd = 2)

# (this is not real analysis it is for plotting purposes only)
library(emmeans)

PlotMod <- glm(MitesAll$DeltaPMI ~ MitesAll$Treatment*MitesAll$Year*MitesAll$Site,
               family = 'gaussian')
anova(PlotMod, test = 'F')

PlotCIs <- as.data.frame(emmeans(PlotMod, specs =c('Treatment','Year','Site')))

for(L in 1:NROW(PlotCIs)){
  
  segments(x0 = L, x1 = L, y0 = PlotCIs$asymp.LCL[L], y1 = PlotCIs$asymp.UCL[L],
           col = ColRef$Col[which(ColRef$Treatment == PlotCIs$Treatment[L])],
           lwd = 3)
  
}

##### Technical Full Analysis begins here

hist(MitesAll$DeltaPMI)

# Mixed model
# see https://m-clark.github.io/mixed-models-with-R/extensions.html as a good example of how nested + crossed effects work

library(afex)

# Mixed model structure reflects the crossed/partially-nested field trial data when in conglomerate

FullMiteMod <- mixed(DeltaPMI ~ Treatment + (1|Year) + (1|Site) + (1|Site:Yard),
                     data = MitesAll)

nice(FullMiteMod)

# the above 'nice()' line is the critical result of significance

# we re-make the model for checking direction(s) of significant effect(s) and for inspection of residuals

MirrorMod <- lmer(DeltaPMI ~ Treatment + (1|Year) + (1|Site) + (1|Site:Yard),
                  data = MitesAll)
summary(MirrorMod)

hist(resid(MirrorMod))
qqnorm(resid(MirrorMod))
qqline(resid(MirrorMod), col = "blue1", lwd = 2)

# Back to plotting.
## Replot main paper boxplots in appropriate colours and repeat analysis more thoroughly

DBrMod.Full <- mixed(DeltaBrood ~ Treatment + (1|Yard),
                     data = na.omit(MitesGA.20))

nice(DBrMod.Full)

DBrMod.Mirror <- lmer(DeltaBrood ~ Treatment + (1|Yard),
                     data = na.omit(MitesGA.20))

summary(DBrMod.Mirror)

hist(resid(DBrMod.Mirror))
qqnorm(resid(DBrMod.Mirror))
qqline(resid(DBrMod.Mirror), col = "blue1", lwd = 2)

DBeMod.Full <- mixed(DeltaBees ~ Treatment + (1|Yard),
                     data = na.omit(MitesGA.20))

nice(DBeMod.Full)

DBeMod.Mirror <- lmer(DeltaBees ~ Treatment + (1|Yard),
                      data = na.omit(MitesGA.20))

summary(DBeMod.Mirror)

hist(resid(DBeMod.Mirror))
qqnorm(resid(DBeMod.Mirror))
qqline(resid(DBeMod.Mirror), col = "blue1", lwd = 2)

DFoMod.Full <- mixed(DeltaFood ~ Treatment + (1|Yard),
                     data = na.omit(MitesGA.20))

nice(DFoMod.Full)

DFoMod.Mirror <- lmer(DeltaFood ~ Treatment + (1|Yard),
                      data = na.omit(MitesGA.20))

summary(DFoMod.Mirror)

hist(resid(DFoMod.Mirror))
qqnorm(resid(DFoMod.Mirror))
qqline(resid(DFoMod.Mirror), col = "blue1", lwd = 2)

par(mfrow =c (1,3))

par(mar=c(5,5,2,1))

boxplot(MitesGA.20$DeltaBrood ~ MitesGA.20$Treatment,
        main = 'Brood', ylab = expression(paste(Delta, ' Brood Coverage (Frames)',sep='')), xlab = 'Treatment', 
        border = sapply(X = as.character(ColRef$Col), FUN = Transpa, percent = 50), 
        cex.axis = 1.3, cex.lab = 1.5, outline = T, lwd = 1.2, outcol = 'white',
        boxlty = 1, whisklty = 1, staplelty = 1, boxwex = 0.7, 
        col = sapply(X = as.character(ColRef$Col), FUN = Transpa, percent = 50))
stripchart(MitesGA.20$DeltaBrood ~ MitesGA.20$Treatment,
           col = sapply(X = as.character(ColRef$Col), FUN = Transpa, percent = 40),
           vertical = T, add = T, pch = 16, cex = 1.2, 
           method = 'jitter', lwd = 2)

boxplot(MitesGA.20$DeltaBees ~ MitesGA.20$Treatment,
        main = 'Adult Bees', ylab = expression(paste(Delta, ' Bee Population (Frames)',sep='')), xlab = 'Treatment', 
        border = sapply(X = as.character(ColRef$Col), FUN = Transpa, percent = 50), 
        cex.axis = 1.3, cex.lab = 1.5, outline = T, lwd = 1.2, outcol = 'white',
        boxlty = 1, whisklty = 1, staplelty = 1, boxwex = 0.7, 
        col = sapply(X = as.character(ColRef$Col), FUN = Transpa, percent = 50))
stripchart(MitesGA.20$DeltaBees ~ MitesGA.20$Treatment,
           col = sapply(X = as.character(ColRef$Col), FUN = Transpa, percent = 40),
           vertical = T, add = T, pch = 16, cex = 1.2, 
           method = 'jitter', lwd = 2)

boxplot(MitesGA.20$DeltaFood ~ MitesGA.20$Treatment,
        main = 'Food Stores', ylab = expression(paste(Delta, ' Honey Area (Frames)',sep='')), xlab = 'Treatment', 
        border = sapply(X = as.character(ColRef$Col), FUN = Transpa, percent = 50), 
        cex.axis = 1.3, cex.lab = 1.5, outline = T, lwd = 1.2, outcol = 'white',
        boxlty = 1, whisklty = 1, staplelty = 1, boxwex = 0.7, 
        col = sapply(X = as.character(ColRef$Col), FUN = Transpa, percent = 50))
stripchart(MitesGA.20$DeltaFood ~ MitesGA.20$Treatment,
           col = sapply(X = as.character(ColRef$Col), FUN = Transpa, percent = 40),
           vertical = T, add = T, pch = 16, cex = 1.2, 
           method = 'jitter', lwd = 2)














