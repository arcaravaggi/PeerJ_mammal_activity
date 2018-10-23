setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# R script relating to the methods described in Caravaggi A, Gatta M, Vallely M-C, Hogg K, Freeman M, Fadei E, Dick J, Montgomery WI, Reid N, Tosh D. Seasonal and predator-prey effects on circadian activity of free-ranging mammals revealed by camera traps.

library(plyr)
library(dplyr)
library(scales)
library(lubridate)
library(tidyr)
library(jpeg)
library(grid)
library(ggplot2)
library(ggpubr)
library(reshape2)
library(overlap)

#Get data
allDat <- read.csv("all_data.csv", header = TRUE, stringsAsFactors = FALSE, as.is=TRUE)
unique(allDat$spp_1)

# Remove unwanted species from dataframe
allDat <- allDat[!allDat$spp_1 %in% c("Rat", "Hedgehog", "Muntjac", "Sika"), ]
unique(allDat$spp_1)

# Recode month
allDat$month <- recode(allDat$month, "1" = "01_Jan", "2" = "02_Feb", "3" = "03_Mar", "4" = "04_Apr", 
                       "5" = "05_May", "6" = "06_Jun", "7" = "07_Jul", "8" = "08_Aug", "9" = "09_Sep",
                       "10" = "10_Oct", "11" = "11_Nov", "12" = "12_Dec")

# Extract hour from time
allDat$hour <- hms(as.character(allDat$time))
allDat$hour <- hour(allDat$hour)

# Group data by species and month/season
month <- allDat %>% count(spp_1, month)
season <- allDat %>% count(spp_1, season)

# Group annual and seasonal activity
act.yr <- allDat %>%  group_by_("spp_1")  %>% count(hour) %>% mutate(freq = n / sum(n))
act.sn <- allDat %>%  group_by_("spp_1","season")  %>% count(hour) %>% mutate(freq = n / sum(n))

# Create hourly data and merge with both activity dataframes to fill in missing hours
seasons <- unique(allDat$season)
sp <- unique(allDat$spp_1)
y <- expand.grid(hour = 0:24, season = seasons, spp_1 = sp)
act.sn <- merge(act.sn, y, all = TRUE) %>% replace_na(list(n = 0, freq = 0))

# Calculate mean and SD magnitude for each month and fill in the gaps
mmag.mean <- allDat %>%  group_by_(.dots=c("spp_1","month")) %>%  summarize(x=mean(m_60))
mmag.sd <- allDat %>%  group_by_(.dots=c("spp_1","month")) %>%  summarize(x=sd(m_60))

# Merge with species and month
z <- expand.grid(spp_1 = sp, month = 1:12)
mmag.mean <- merge(mmag.mean, z, all = TRUE) %>% replace_na(list(x = 0))
mmag.sd <- merge(mmag.sd, z, all = TRUE) %>% replace_na(list(x = 0))

# Remove unnecessary objects
rm(list = ls()[!ls() %in% c("allDat", "act.sn", "act.yr", 
                            "mmag.mean", "mmag.sd", "month", "season")])
### CCFs 
#
# Create datasets for predator-prey pairs
# Fox & Rabbit
# Filter by each species
# Remove rows from dfx which don't match in dfy (& vv) according to ID column
# Bind to new dataframe
f <- allDat %>% filter(spp_1 == "Fox")
r <- allDat %>% filter(spp_1 == "Rabbit")
f2 <- f  %>% semi_join(r, by = "site") 
r2 <- r  %>% semi_join(f, by = "site")
fr <- rbind(f2,r2)

# Fox & hare
f <- allDat %>% filter(spp_1 == "Fox")
h <- allDat %>% filter(spp_1 == "Hare")
f3 <- f  %>% semi_join(h, by = "site")
h2 <- h  %>% semi_join(f, by = "site")
fh <- rbind(f3, h2)

# Fox & wood mouse
f <- allDat %>% filter(spp_1 == "Fox")
mo <- allDat %>% filter(spp_1 == "Mouse")
f4 <- f  %>% semi_join(mo, by = "site")
mo2 <- mo  %>% semi_join(f, by = "site")
fmo <- rbind(f4, mo2)

# Marten & squirrel
m <- allDat %>% filter(spp_1 == "Marten")
s <- allDat %>% filter(spp_1 == "Squirrel")
s2 <- s  %>% semi_join(m, by = "site")
m2 <- m  %>% semi_join(s, by = "site")
ms <- rbind(m2,s2)

# Marten & wood mouse
m <- allDat %>% filter(spp_1 == "Marten")
mo <- allDat %>% filter(spp_1 == "Mouse")
mo3 <- mo  %>% semi_join(m, by = "site")
m2 <- m  %>% semi_join(mo, by = "site")
mmo <- rbind(m2,mo3)

#Fox-rabbit datasets
spDATfr <- fr[fr$season=="spring",]
suDATfr <- fr[fr$season=="summer",]
auDATfr <- fr[fr$season=="autumn",]
wiDATfr <- fr[fr$season=="winter",]

DAT2fr <- dcast(fr, hour ~ spp_1)   #General dataset for that species pair (all seasons)
spDAT2fr <- dcast(spDATfr, hour ~ spp_1) 
suDAT2fr <- dcast(suDATfr, hour ~ spp_1) 
auDAT2fr <- dcast(auDATfr, hour ~ spp_1) 
wiDAT2fr <- dcast(wiDATfr, hour ~ spp_1) 

#Fox-hare datasets
spDATfh <- fh[fh$season=="spring",]
suDATfh <- fh[fh$season=="summer",]
auDATfh <- fh[fh$season=="autumn",]
wiDATfh <- fh[fh$season=="winter",]

DAT2fh <- dcast(fh, hour ~ spp_1)   #General dataset for that species pair (all seasons)
spDAT2fh <- dcast(spDATfh, hour ~ spp_1) 
suDAT2fh <- dcast(suDATfh, hour ~ spp_1) 
auDAT2fh <- dcast(auDATfh, hour ~ spp_1) 
wiDAT2fh <- dcast(wiDATfh, hour ~ spp_1) 

#Fox-mouse datasets
spDATfmo <- fmo[fmo$season=="spring",]
suDATfmo <- fmo[fmo$season=="summer",]
auDATfmo <- fmo[fmo$season=="autumn",]
wiDATfmo <- fmo[fmo$season=="winter",]

DAT2fmo <- dcast(fmo, hour ~ spp_1)   #General dataset for that species pair (all seasons)
spDAT2fmo <- dcast(spDATfmo, hour ~ spp_1) 
suDAT2fmo <- dcast(suDATfmo, hour ~ spp_1) 
auDAT2fmo <- dcast(auDATfmo, hour ~ spp_1) 
wiDAT2fmo <- dcast(wiDATfmo, hour ~ spp_1) 

#Marten-squirrel datasets
spDATms <- ms[ms$season=="spring",]
suDATms <- ms[ms$season=="summer",]
auDATms <- ms[ms$season=="autumn",]
wiDATms <- ms[ms$season=="winter",]

DAT2ms <- dcast(ms, hour ~ spp_1)   #General dataset for that species pair (all seasons)
spDAT2ms <- dcast(spDATms, hour ~ spp_1) 
suDAT2ms <- dcast(suDATms, hour ~ spp_1) 
auDAT2ms <- dcast(auDATms, hour ~ spp_1) 
wiDAT2ms <- dcast(wiDATms, hour ~ spp_1)

#Marten-mouse datasets
spDATmmo <- mmo[mmo$season=="spring",]
suDATmmo <- mmo[mmo$season=="summer",]
auDATmmo <- mmo[mmo$season=="autumn",]
wiDATmmo <- mmo[mmo$season=="winter",]

DAT2mmo <- dcast(mmo, hour ~ spp_1)   #General dataset for that species pair (all seasons)
spDAT2mmo <- dcast(spDATmmo, hour ~ spp_1) 
suDAT2mmo <- dcast(suDATmmo, hour ~ spp_1) 
auDAT2mmo <- dcast(auDATmmo, hour ~ spp_1) 
wiDAT2mmo <- dcast(wiDATmmo, hour ~ spp_1)


#function to export max lags
ccfmax <- function(a, b, e=0)
{
  d <- ccf(a, b, plot = FALSE, lag.max = length(a)/2)
  cor = d$acf[,,1]
  abscor = abs(d$acf[,,1])
  lag = d$lag[,,1]
  res = data.frame(cor, lag)
  absres = data.frame(abscor, lag)
  maxcor = max(absres$abscor)
  absres_max = res[which(absres$abscor >= maxcor-maxcor*e &
                           absres$abscor <= maxcor+maxcor*e),]
  return(absres_max)
}

#Cross-correlation calculations (all)
ccf.fh <- ccf(DAT2fh$Fox, DAT2fh$Hare, 50)
ccf.fr <- ccf(DAT2fr$Fox, DAT2fr$Rabbit, 50)
ccf.fmo <- ccf(DAT2fmo$Fox, DAT2fmo$Mouse, 50)
ccf.br <- ccf(DAT2br$Badger, DAT2br$Rabbit, 50)
ccf.ms <- ccf(DAT2ms$Marten, DAT2ms$Squirrel, 50)
ccf.mmo <- ccf(DAT2mmo$Marten, DAT2mmo$Mouse, 50)

ccfmax(DAT2fh$Fox, DAT2fh$Hare, 0.5)
ccfmax(DAT2fr$Fox, DAT2fr$Rabbit, 0.5)
ccfmax(DAT2fmo$Fox, DAT2fmo$Mouse, 0.5)
ccfmax(DAT2br$Badger, DAT2br$Rabbit, 0.5)
ccfmax(DAT2ms$Marten, DAT2ms$Squirrel, 0.5)
ccfmax(DAT2mmo$Marten, DAT2mmo$Mouse, 0.5)

0.6628007*sqrt((24-2)/(1-(0.6628007^2))) #t-test for fox-hare lag (-4:0, -2 is the max)
0.4128373*sqrt((24-2)/(1-(0.4128373^2))) #t-test for fox-rabbit lag(-10:-8, -9 lag)
0.6605976*sqrt((24-2)/(1-(0.6605976^2))) #t-test for fox-rabbit lag(-3:1, -1 lag)
0.4087796*sqrt((24-2)/(1-(0.4087796^2))) #t-test for fox-rabbit lag(8:11, 10 lag)
0.4799285*sqrt((24-2)/(1-(0.4799285^2))) #t-test for fox-mouse lag(-12:-10, -11 lag)
0.7542418 *sqrt((24-2)/(1-(0.7542418 ^2))) #t-test for fox-mouse lag(-2:3, 1 lag)
0.4678209*sqrt((24-2)/(1-(0.4678209^2))) #t-test for badger-rabbit lag(-5:1, -4 lag)
0.4369318*sqrt((24-2)/(1-(0.4369318^2))) #t-test for badger-rabbit lag(7:12, 10 lag)
0.416*sqrt((24-2)/(1-(0.416^2)))         #t-test for badger-rabbit lag(14:18, 16 lag)
0.4162788*sqrt((24-2)/(1-(0.4162788^2))) #t-test for marten-squirrel lag(-12:-7, -9 lag)
0.6211498*sqrt((24-2)/(1-(0.6211498^2))) #t-test for marten-squirrel lag(-1:4, 1 lag)
0.5359151*sqrt((24-2)/(1-(0.5359151^2))) #t-test for marten-mouse lag(-1:1, 0 lag)

lagDAT <- as.data.frame(c(ccf.fh, ccf.fr, ccf.fmo, ccf.ms, ccf.mmo))
lagDAT <- lagDAT[-c(2,3,5,6,8:12, 14:18, 20:24, 26:30)]

colnames(lagDAT) <- c("fh.acf", "lag", "fr.acf", "fmo.acf", "ms.acf", "mmo.acf")
#write.csv(lagDAT, file = "lagDAT.csv")

#Cross-correlation calculations (seasonal)
fh.ccf2.sp <- ccf(spDAT2fh$Fox, spDAT2fh$Hare, 50)
fh.ccf2.su <- ccf(suDAT2fh$Fox, suDAT2fh$Hare, 50)
fh.ccf2.au <- ccf(auDAT2fh$Fox, auDAT2fh$Hare, 50)
fh.ccf2.wi <- ccf(wiDAT2fh$Fox, wiDAT2fh$Hare, 50) #No data

fr.ccf2.sp <- ccf(spDAT2fr$Fox, spDAT2fr$Rabbit, 50)
fr.ccf2.su <- ccf(suDAT2fr$Fox, suDAT2fr$Rabbit, 50)
fr.ccf2.au <- ccf(auDAT2fr$Fox, auDAT2fr$Rabbit, 50)
fr.ccf2.wi <- ccf(wiDAT2fr$Fox, wiDAT2fr$Rabbit, 50)

fmo.ccf2.sp <- ccf(spDAT2fmo$Fox, spDAT2fmo$Mouse, 50)
fmo.ccf2.su <- ccf(suDAT2fmo$Fox, suDAT2fmo$Mouse, 50)
fmo.ccf2.au <- ccf(auDAT2fmo$Fox, auDAT2fmo$Mouse, 50)
fmo.ccf2.wi <- ccf(wiDAT2fmo$Fox, wiDAT2fmo$Mouse, 50)

ms.ccf2.sp <- ccf(spDAT2ms$Marten, spDAT2ms$Squirrel, 50)
ms.ccf2.su <- ccf(suDAT2ms$Marten, suDAT2ms$Squirrel, 50)
ms.ccf2.au <- ccf(auDAT2ms$Marten, auDAT2ms$Squirrel, 50)
ms.ccf2.wi <- ccf(wiDAT2ms$Marten, wiDAT2ms$Squirrel, 50)

mmo.ccf2.sp <- ccf(spDAT2mmo$Marten, spDAT2mmo$Mouse, 50)
mmo.ccf2.su <- ccf(suDAT2mmo$Marten, suDAT2mmo$Mouse, 50)
mmo.ccf2.au <- ccf(auDAT2mmo$Marten, auDAT2mmo$Mouse, 50)
mmo.ccf2.wi <- ccf(wiDAT2mmo$Marten, wiDAT2mmo$Mouse, 50)

ccfmax(spDAT2fh$Fox, spDAT2fh$Hare, 0.5)
ccfmax(suDAT2fh$Fox, suDAT2fh$Hare, 0.5)
ccfmax(auDAT2fh$Fox, auDAT2fh$Hare, 0.5)
ccfmax(wiDAT2fh$Fox, wiDAT2fh$Hare, 0.5) #No data

0.7010310*sqrt((24-2)/(1-(0.7010310^2))) #t-test for fox-hare spring lag (-2:3, 1 is the max)
0.3398935*sqrt((24-2)/(1-(0.3398935^2))) #t-test for fox-hare summer lag (-12:-11, -12 is the max)
0.6023056*sqrt((24-2)/(1-(0.6023056^2))) #t-test for fox-hare summer lag (-5:0, -3 is the max)

ccfmax(spDAT2fr$Fox, spDAT2fr$Rabbit, 0.5)
ccfmax(suDAT2fr$Fox, suDAT2fr$Rabbit, 0.5)
ccfmax(auDAT2fr$Fox, auDAT2fr$Rabbit, 0.5)
ccfmax(wiDAT2fr$Fox, wiDAT2fr$Rabbit, 0.5)

0.4145617*sqrt((24-2)/(1-(0.4145617^2))) #t-test for fox-rabbit spring lag (-1:2, 1 is the max)
0.7010310*sqrt((24-2)/(1-(0.7010310^2))) #t-test for fox-rabbit spring lag (11)
0.4728247*sqrt((24-2)/(1-(0.4728247^2))) #t-test for fox-rabbit summer lag (-12:-9, -10 is the max)
0.6541832*sqrt((24-2)/(1-(0.6541832^2))) #t-test for fox-rabbit summer lag (-4:1, -1 is the max)

ccfmax(spDAT2fmo$Fox, spDAT2fmo$Mouse, 0.5)
ccfmax(suDAT2fmo$Fox, suDAT2fmo$Mouse, 0.5)
ccfmax(auDAT2fmo$Fox, auDAT2fmo$Mouse, 0.5)
ccfmax(wiDAT2fmo$Fox, wiDAT2fmo$Mouse, 0.5)

0.5704926*sqrt((24-2)/(1-(0.5704926^2))) #t-test for fox-Mouse spring lag (-2:3, 1 is the max)
0.4065901*sqrt((24-2)/(1-(0.4065901^2))) #t-test for fox-Mouse summer lag (-10:-9, -10 max)
0.7611536*sqrt((24-2)/(1-(0.7611536^2))) #t-test for fox-Mouse summer lag (-1:1, 0 is the max)
0.5435222*sqrt((24-2)/(1-(0.5435222^2))) #t-test for fox-Mouse autumn lag (0:2, 2 is the max)

ccfmax(spDAT2ms$Marten, spDAT2ms$Squirrel, 0.5)
ccfmax(suDAT2ms$Marten, suDAT2ms$Squirrel, 0.5)
ccfmax(auDAT2ms$Marten, auDAT2ms$Squirrel, 0.5)
ccfmax(wiDAT2ms$Marten, wiDAT2ms$Squirrel, 0.5)

0.4696997*sqrt((24-2)/(1-(0.4696997^2))) #t-test for marten-squirrel spring lag (-10:-4, max at -6)
0.6254050*sqrt((24-2)/(1-(0.6254050^2))) #t-test for marten-squirrel spring lag (0:5, max at 1)
0.5117216 *sqrt((24-2)/(1-(0.5117216 ^2))) #t-test for marten-squirrel summer lag (-4:-1, max at -2)
0.4639708 *sqrt((24-2)/(1-(0.4639708 ^2))) #t-test for marten-squirrel summer lag (8)
0.5105618 *sqrt((24-2)/(1-(0.5105618 ^2))) #t-test for marten-squirrel winter lag (-9:-7, max at -8)
0.6649387 *sqrt((24-2)/(1-(0.6649387 ^2))) #t-test for marten-squirrel winter lag (-2:3, max at -1)
0.5525872 *sqrt((24-2)/(1-(0.5525872 ^2))) #t-test for marten-squirrel winter lag (9:11, max at 10)

ccfmax(spDAT2mmo$Marten, spDAT2mmo$Mouse, 0.5)
ccfmax(suDAT2mmo$Marten, suDAT2mmo$Mouse, 0.5)
ccfmax(auDAT2mmo$Marten, auDAT2mmo$Mouse, 0.5)
ccfmax(wiDAT2mmo$Marten, wiDAT2mmo$Mouse, 0.5)

0.4394959*sqrt((24-2)/(1-(0.4394959^2))) #t-test for marten-Mouse spring lag (7:11, max at 10)
0.4305365 *sqrt((24-2)/(1-(0.4305365 ^2))) #t-test for marten-Mouse summer lag (-3:-2, max at -2)
0.4639708 *sqrt((24-2)/(1-(0.4639708 ^2))) #t-test for marten-Mouse summer lag (8)
0.5025475 *sqrt((24-2)/(1-(0.5025475 ^2))) #t-test for marten-Mouse winter lag (2:4, max at 3)
0.4000340 *sqrt((24-2)/(1-(0.4000340 ^2))) #t-test for marten-Mouse winter lag (6:7, max at 6)


#Fox-hare seasonal data
fhDATccf <- c(fh.ccf2.sp, fh.ccf2.su, fh.ccf2.au, fh.ccf2.wi)
fhDATccf <- t(plyr::ldply(fhDATccf, rbind))
colnames(fhDATccf) = fhDATccf[1, ] 
fhDATccf = fhDATccf[-1, ]  

FHlag <-fhDATccf[ , c(1, 4, 7, 10, 13, 16, 19, 22)]

colnames(FHlag)[1] <- "sp.acf"
colnames(FHlag)[2] <- "sp.lag"
colnames(FHlag)[3] <- "su.acf"
colnames(FHlag)[4] <- "su.lag"
colnames(FHlag)[5] <- "au.acf"
colnames(FHlag)[6] <- "au.lag"
colnames(FHlag)[7] <- "wi.acf"
colnames(FHlag)[8] <- "wi.lag"
#write.csv(FHlag, file = "FHlag.csv")

#Fox-Rabbit seasonal data
frDATccf <- c(fr.ccf2.sp, fr.ccf2.su, fr.ccf2.au, fr.ccf2.wi)
frDATccf <- t(plyr::ldply(frDATccf, rbind))
colnames(frDATccf) = frDATccf[1, ] 
frDATccf = frDATccf[-1, ]  

FRlag <-frDATccf[ , c(1, 4, 7, 10, 13, 16, 19, 22)]

colnames(FRlag)[1] <- "sp.acf"
colnames(FRlag)[2] <- "sp.lag"
colnames(FRlag)[3] <- "su.acf"
colnames(FRlag)[4] <- "su.lag"
colnames(FRlag)[5] <- "au.acf"
colnames(FRlag)[6] <- "au.lag"
colnames(FRlag)[7] <- "wi.acf"
colnames(FRlag)[8] <- "wi.lag"
#write.csv(FRlag, file = "FRlag.csv")

#Fox-Mouse seasonal data
fmoDATccf <- c(fmo.ccf2.sp, fmo.ccf2.su, fmo.ccf2.au, fmo.ccf2.wi)
fmoDATccf <- t(plyr::ldply(fmoDATccf, rbind))
colnames(fmoDATccf) = fmoDATccf[1, ] 
fmoDATccf = fmoDATccf[-1, ]  

FMOlag <-fmoDATccf[ , c(1, 4, 7, 10, 13, 16, 19, 22)]

colnames(FMOlag)[1] <- "sp.acf"
colnames(FMOlag)[2] <- "sp.lag"
colnames(FMOlag)[3] <- "su.acf"
colnames(FMOlag)[4] <- "su.lag"
colnames(FMOlag)[5] <- "au.acf"
colnames(FMOlag)[6] <- "au.lag"
colnames(FMOlag)[7] <- "wi.acf"
colnames(FMOlag)[8] <- "wi.lag"
#write.csv(FMOlag, file = "FMOlag.csv")

#Marten-Squirrel seasonal data
msDATccf <- c(ms.ccf2.sp, ms.ccf2.su, ms.ccf2.au, ms.ccf2.wi)
msDATccf <- t(plyr::ldply(msDATccf, rbind))
colnames(msDATccf) = msDATccf[1, ] 
msDATccf = msDATccf[-1, ]  

MSlag <-msDATccf[ , c(1, 4, 7, 10, 13, 16, 19, 22)]

colnames(MSlag)[1] <- "sp.acf"
colnames(MSlag)[2] <- "sp.lag"
colnames(MSlag)[3] <- "su.acf"
colnames(MSlag)[4] <- "su.lag"
colnames(MSlag)[5] <- "au.acf"
colnames(MSlag)[6] <- "au.lag"
colnames(MSlag)[7] <- "wi.acf"
colnames(MSlag)[8] <- "wi.lag"
#write.csv(MSlag, file = "MSlag.csv")

#Marten-Squirrel seasonal data
mmoDATccf <- c(mmo.ccf2.sp, mmo.ccf2.su, mmo.ccf2.au, mmo.ccf2.wi)
mmoDATccf <- t(plyr::ldply(mmoDATccf, rbind))
colnames(mmoDATccf) = mmoDATccf[1, ] 
mmoDATccf = mmoDATccf[-1, ]  

MMOlag <-mmoDATccf[ , c(1, 4, 7, 10, 13, 16, 19, 22)]

colnames(MMOlag)[1] <- "sp.acf"
colnames(MMOlag)[2] <- "sp.lag"
colnames(MMOlag)[3] <- "su.acf"
colnames(MMOlag)[4] <- "su.lag"
colnames(MMOlag)[5] <- "au.acf"
colnames(MMOlag)[6] <- "au.lag"
colnames(MMOlag)[7] <- "wi.acf"
colnames(MMOlag)[8] <- "wi.lag"
#write.csv(MMOlag, file = "MMOlag.csv")


###### Models #######
# Activity variation between seasons within-species
BaDAT <- allDat[allDat$spp_1 == "Badger",]
ba1 <- aov(m_60~season, BaDAT)
summary(ba1)
TukeyHSD(ba1)

#Plots for assumptionns
plot(ba1)

#Check residuals are normally distributed
sresid <- (ba1$residuals - mean(ba1$residuals))/sd(ba1$residuals) 
hist(sresid, freq=F)
lines(density(sresid, adjust=1))
qqnorm(sresid, cex=1.8, pch=20)
qqline(sresid, lty=2, lwd=2)

#Check that residuals are homogeneous
plot(sresid~ba1$fitted.values, pch=20, cex=2, cex.lab=1.5) #Not homogeneous
plot(sresid~BaDAT$m_60)

#Check for influential observations
influenceba1 <- influence.measures(ba1)
summary(influenceba1)
CDba1 <- cooks.distance(ba1)
sresid <- resid(ba1, type="pearson")
plot(CDba1~sresid)

FaDAT <- allDat[allDat$spp_1 == "Fallow",]
fa1 <- aov(m_60~season, FaDAT)
summary(fa1)
TukeyHSD(fa1)

FoDAT <- allDat[allDat$spp_1 == "Fox",]
fo1 <- aov(m_60~season, FoDAT)
summary(fo1)
TukeyHSD(fo1)

HaDAT <- allDat[allDat$spp_1 == "Hare",]
ha1 <- aov(m_60~season, HaDAT)
summary(ha1)
TukeyHSD(ha1)

MaDAT <- allDat[allDat$spp_1 == "Marten",]
ma1 <- aov(m_60~season, MaDAT)
summary(ma1)
TukeyHSD(ma1)

MoDAT <- allDat[allDat$spp_1 == "Mouse",]
mo1 <- aov(m_60~season, MoDAT)
summary(mo1)
TukeyHSD(mo1)

RaDAT <- allDat[allDat$spp_1 == "Rabbit",]
ra1 <- aov(m_60~season, RaDAT)
summary(ra1)
TukeyHSD(ra1)

SqDAT <- allDat[allDat$spp_1 == "Squirrel",]
sq1 <- aov(m_60~season, SqDAT)
summary(sq1)
TukeyHSD(sq1)

Tba <- TukeyHSD(ba1)
Tfo <- TukeyHSD(fo1)
Tha <- TukeyHSD(ha1)
Tma <- TukeyHSD(ma1)
Tmo <- TukeyHSD(mo1)
Tra <- TukeyHSD(ra1)
Tsq <- TukeyHSD(sq1)

Tba$season
Tfo$season
Tha$season
Tma$season
Tmo$season
Tra$season
Tsq$season



###Overlap plots
#
# Using datasets that only contain matching species pairs
#
# Convert time to decimal and rescale to between 0 and 1
fr$time_adj <- sapply(strsplit(fr$time,":"), # Fox & rabbit
                      function(x) {
                        x <- as.numeric(x)
                        x[1]+x[2]/60
                      }
)
fr$time_adj <- scales:::rescale(fr$time_adj, to = c(0, 1)) 

fh$time_adj <- sapply(strsplit(fh$time,":"), # Fox & hare
                      function(x) {
                        x <- as.numeric(x)
                        x[1]+x[2]/60
                      }
)
fh$time_adj <- scales:::rescale(fh$time_adj, to = c(0, 1))

fmo$time_adj <- sapply(strsplit(fmo$time,":"), # Fox & mouse
                      function(x) {
                        x <- as.numeric(x)
                        x[1]+x[2]/60
                      }
)
fmo$time_adj <- scales:::rescale(fmo$time_adj, to = c(0, 1))

ms$time_adj <- sapply(strsplit(ms$time,":"), # Marten & squirrel
                      function(x) {
                        x <- as.numeric(x)
                        x[1]+x[2]/60
                      }
)
ms$time_adj <- scales:::rescale(ms$time_adj, to = c(0, 1)) 

mmo$time_adj <- sapply(strsplit(mmo$time,":"), # Marten & mouse
                      function(x) {
                        x <- as.numeric(x)
                        x[1]+x[2]/60
                      }
)
mmo$time_adj <- scales:::rescale(mmo$time_adj, to = c(0, 1)) 

# Convert to radians
timeRad.fr <- fr$time_adj  * 2 * pi
fox.p.fr <- timeRad.fr[fr$spp_1 == 'Fox']
rab.p.fr <- timeRad.fr[fr$spp_1 == 'Rabbit']

timeRad.fh <- fh$time_adj  * 2 * pi
fox.p.fh <- timeRad.fh[fh$spp_1 == 'Fox']
har.p.fh <- timeRad.fh[fh$spp_1 == 'Hare']

timeRad.fmo <- fmo$time_adj  * 2 * pi
fox.p.fmo <- timeRad.fmo[fmo$spp_1 == 'Fox']
mou.p.fmo <- timeRad.fmo[fmo$spp_1 == 'Mouse']

timeRad.ms <- ms$time_adj  * 2 * pi
mar.p.ms <- timeRad.ms[ms$spp_1 == 'Marten']
squ.p.ms <- timeRad.ms[ms$spp_1 == 'Squirrel']

timeRad.mmo <- mmo$time_adj  * 2 * pi
mar.p.mmo <- timeRad.mmo[mmo$spp_1 == 'Marten']
mou.p.mmo <- timeRad.mmo[mmo$spp_1 == 'Mouse']

# Overlap species pairs and estimate overlap
foxrab2est.fr <- overlapEst(fox.p.fr, rab.p.fr, type="Dhat4")
pdf(NULL) # Save plot to an object using a null PDF device
dev.control(displaylist="enable")
par(family = 'serif')
overlapPlot(fox.p.fr, rab.p.fr, main=NULL, font.lab=2 )
legend('top', c("Fox", "Rabbit"), lty=c(1,2), col=c(1,4), bty='n')
p1 <- recordPlot()
invisible(dev.off())

foxhar2est.fh <- overlapEst(fox.p.fh, har.p.fh, type="Dhat4") 
pdf(NULL) 
dev.control(displaylist="enable")
par(family = 'serif')
overlapPlot(fox.p.fh, har.p.fh, main=NULL,font.lab=2 )
legend('top', c("Fox", "Hare"), lty=c(1,2), col=c(1,4), bty='n')
p2 <- recordPlot()
invisible(dev.off())

foxmou2est.fmo <- overlapEst(fox.p.fmo, mou.p.fmo, type="Dhat4")
pdf(NULL) 
dev.control(displaylist="enable")
par(family = 'serif')
overlapPlot(fox.p.fmo, mou.p.fmo, main=NULL, font.lab=2 )
legend('top', c("Fox", "Mouse"), lty=c(1,2), col=c(1,4), bty='n')
p3 <- recordPlot()
invisible(dev.off())

marsqu2est.ms <- overlapEst(mar.p.ms, squ.p.ms, type="Dhat4") 
pdf(NULL) 
dev.control(displaylist="enable")
par(family = 'serif')
overlapPlot(mar.p.ms, squ.p.ms,  main=NULL,font.lab=2 )
legend('top', c("Marten", "Squirrel"), lty=c(1,2), col=c(1,4), bty='n')
p4 <- recordPlot()
invisible(dev.off())

marsqu2est.mmo <- overlapEst(mar.p.mmo, mou.p.mmo, type="Dhat4") 
pdf(NULL) 
dev.control(displaylist="enable")
par(family = 'serif')
overlapPlot(mar.p.mmo, mou.p.mmo,  main=NULL, font.lab=2 )
legend('top', c("Marten", "Mouse"), lty=c(1,2), col=c(1,4), bty='n')
p5 <- recordPlot()
invisible(dev.off())

png('Mag_plot.png', units="in", type = 'cairo', width=9.5, height=12, res=300)
ggarrange(p1, p2, p3, p4, p5,
          labels = c("a)", "b)", "c)", "d)", "e)"),
          vjust = 1,
          ncol = 2, nrow = 3)
dev.off() 

#Seasonal overlaps
#First check that we have enough data for each season
nrow(fr[fr$spp_1 == 'Rabbit' &  fr$season == 'spring',])  #492
nrow(fr[fr$spp_1 == 'Fox' & fr$season == 'spring',]) #106
nrow(fh[fh$spp_1 == 'Fox' & fh$season == 'spring',]) #72
nrow(fh[fh$spp_1 == 'Hare' &  fh$season == 'spring',]) #291
nrow(ms[ms$spp_1 == 'Marten' &  ms$season == 'spring',]) #124
nrow(ms[ms$spp_1 == 'Squirrel' &  ms$season == 'spring',]) #402

nrow(fr[fr$spp_1 == 'Rabbit' &  fr$season ==  'summer',]) #417
nrow(fr[fr$spp_1 == 'Fox' & fr$season == 'summer',]) #126
nrow(fh[fh$spp_1 == 'Fox' & fh$season == 'summer',]) #165
nrow(fh[fh$spp_1 == 'Hare' &  fh$season ==  'summer',]) #339
nrow(ms[ms$spp_1 == 'Marten' &  ms$season == 'summer',]) #73
nrow(ms[ms$spp_1 == 'Squirrel' &  ms$season == 'summer',]) #236

nrow(fr[fr$spp_1 == 'Rabbit' &  fr$season == 'autumn',]) #238
nrow(fr[fr$spp_1 == 'Fox' &  fr$season == 'autumn',]) #138
nrow(fh[fh$spp_1 == 'Fox' &  fh$season == 'autumn',]) #121
nrow(fh[fh$spp_1 == 'Hare' &  fh$season == 'autumn',]) #105
nrow(ms[ms$spp_1 == 'Marten' &  ms$season == 'autumn',]) #356
nrow(ms[ms$spp_1 == 'Squirrel' &  ms$season == 'autumn',]) #450

nrow(fr[fr$spp_1 == 'Rabbit' &  fr$season == 'winter',]) #28
nrow(fr[fr$spp_1 == 'Fox' &  fr$season == 'winter',]) #15
nrow(fh[fh$spp_1 == 'Fox' &  fh$season == 'winter',]) #2
nrow(fh[fh$spp_1 == 'Hare' &  fh$season == 'winter',]) #6
nrow(ms[ms$spp_1 == 'Marten' &  ms$season == 'winter',]) #97
nrow(ms[ms$spp_1 == 'Squirrel' &  ms$season == 'winter',]) #85

rab.sp.fr <- timeRad.fr[fr$spp_1 == 'Rabbit' & fr$season == 'spring']
fox.sp.fr <- timeRad.fr[fr$spp_1 == 'Fox' &  fr$season == 'spring']
foxrab.sp2est.fr <- overlapEst(fox.sp.fr, rab.sp.fr, type="Dhat4") # overlap estimate
overlapPlot(fox.sp.fr, rab.sp.fr, main=NULL)
legend('top', c("Fox (n = 106)", "Rabbit (n = 492)"), lty=c(1,2), col=c(1,4), bty='n')

rab.su.fr <- timeRad.fr[fr$spp_1 == 'Rabbit' &  fr$season == 'summer']
fox.su.fr <- timeRad.fr[fr$spp_1 == 'Fox' &  fr$season == 'summer']
foxrab.su2est.fr <- overlapEst(fox.su.fr, rab.su.fr, type="Dhat4") # overlap estimate
overlapPlot(fox.su.fr, rab.su.fr, main=NULL)
legend('top', c("Fox", "Rabbit"), lty=c(1,2), col=c(1,4), bty='n')

rab.au.fr <- timeRad.fr[fr$spp_1 == 'Rabbit' &  fr$season == 'autumn']
fox.au.fr <- timeRad.fr[fr$spp_1 == 'Fox' &  fr$season == 'autumn']
foxrab.au2est.fr <- overlapEst(fox.au.fr, rab.au.fr, type="Dhat4") # overlap estimate
overlapPlot(fox.au.fr, rab.au.fr, main=NULL)
legend('top', c("Fox", "Rabbit"), lty=c(1,2), col=c(1,4), bty='n')

rab.wi.fr <- timeRad.fr[fr$spp_1 == 'Rabbit' &  fr$season == 'winter']
fox.wi.fr <- timeRad.fr[fr$spp_1 == 'Fox' &  fr$season == 'winter']
foxrab.wi2est.fr <- overlapEst(fox.wi.fr, rab.wi.fr, type="Dhat1") # overlap estimate
overlapPlot(fox.wi.fr, rab.wi.fr, main=NULL)
legend('top', c("Fox", "Rabbit"), lty=c(1,2), col=c(1,4), bty='n')

har.sp.fh <- timeRad.fh[fh$spp_1 == 'Hare' &  fh$season == 'spring']
fox.sp.fh <- timeRad.fh[fh$spp_1 == 'Fox' &  fh$season == 'spring']
foxhar.sp2est.fh <- overlapEst(fox.sp.fh, har.sp.fh, type="Dhat4") # overlap estimate
overlapPlot(fox.sp.fh, har.sp.fh, main=NULL)
legend('top', c("Fox", "Hare"), lty=c(1,2), col=c(1,4), bty='n')

har.su.fh <- timeRad.fh[fh$spp_1 == 'Hare' &  fh$season == 'summer']
fox.su.fh <- timeRad.fh[fh$spp_1 == 'Fox' &  fh$season == 'summer']
foxhar.su2est.fh <- overlapEst(fox.su.fh, har.su.fh, type="Dhat4") # overlap estimate
overlapPlot(fox.su.fh, har.su.fh, main=NULL)
legend('top', c("Fox", "Hare"), lty=c(1,2), col=c(1,4), bty='n')

har.au.fh <- timeRad.fh[fh$spp_1 == 'Hare' &  fh$season == 'autumn']
fox.au.fh <- timeRad.fh[fh$spp_1 == 'Fox' &  fh$season == 'autumn']
foxhar.au2est.fh <- overlapEst(fox.au.fh, har.au.fh, type="Dhat4") # overlap estimate
overlapPlot(fox.au.fh, har.au.fh, main=NULL)
legend('top', c("Fox", "Hare"), lty=c(1,2), col=c(1,4), bty='n')

mar.sp.ms <- timeRad.ms[ms$spp_1 == 'Marten' &  ms$season == 'spring']
squ.sp.ms <- timeRad.ms[ms$spp_1 == 'Squirrel' &  ms$season == 'spring']
marsqu.sp2est.ms <- overlapEst(mar.sp.ms, squ.sp.ms, type="Dhat4") # overlap estimate
overlapPlot(mar.sp.ms, squ.sp.ms, main=NULL)
legend('top', c("Marten", "Squirrel"), lty=c(1,2), col=c(1,4), bty='n')

mar.su.ms <- timeRad.ms[ms$spp_1 == 'Marten' &  ms$season == 'summer']
squ.su.ms <- timeRad.ms[ms$spp_1 == 'Squirrel' &  ms$season == 'summer']
marsqu.su2est.ms <- overlapEst(mar.su.ms, squ.su.ms, type="Dhat4") # overlap estimate
overlapPlot(mar.su.ms, squ.su.ms,main=NULL)
legend('topleft', c("Marten", "Squirrel"), lty=c(1,2), col=c(1,4), bty='n')

mar.au.ms <- timeRad.ms[ms$spp_1 == 'Marten' &  ms$season == 'autumn']
squ.au.ms <- timeRad.ms[ms$spp_1 == 'Squirrel' &  ms$season == 'autumn']
marsqu.au2est.ms <- overlapEst(mar.au.ms, squ.au.ms, type="Dhat4") # overlap estimate
overlapPlot(mar.au.ms, squ.au.ms, main=NULL)
legend('top', c("Marten", "Squirrel"), lty=c(1,2), col=c(1,4), bty='n')

mar.wi.ms <- timeRad.ms[ms$spp_1 == 'Marten' &  ms$season == 'winter']
squ.wi.ms <- timeRad.ms[ms$spp_1 == 'Squirrel' &  ms$season == 'winter']
marsqu.wi2est.ms <- overlapEst(mar.wi.ms, squ.wi.ms, type="Dhat4") # overlap estimate
overlapPlot(mar.wi.ms, squ.wi.ms, main=NULL)
legend('topleft', c("Marten", "Squirrel"), lty=c(1,2), col=c(1,4), bty='n')

# Bootstrap data
foxboot.fr <- resample(fox.p.fr, 1000) # 1000 resamples
dim(foxboot.fr)

foxboot.sp.fr <- resample(fox.sp.fr, 1000) # 1000 resamples
dim(foxboot.sp.fr)

foxboot.su.fr <- resample(fox.su.fr, 1000) # 1000 resamples
dim(foxboot.su.fr)

foxboot.au.fr <- resample(fox.au.fr, 1000) # 1000 resamples
dim(foxboot.au.fr)

foxboot.wi.fr <- resample(fox.wi.fr, 1000) # 1000 resamples
dim(foxboot.wi.fr)

foxboot.fh <- resample(fox.p.fh, 1000) # 1000 resamples
dim(foxboot.fh)

foxboot.sp.fh <- resample(fox.sp.fh, 1000) # 1000 resamples
dim(foxboot.sp.fh)

foxboot.su.fh <- resample(fox.su.fh, 1000) # 1000 resamples
dim(foxboot.su.fh)

foxboot.au.fh <- resample(fox.au.fh, 1000) # 1000 resamples
dim(foxboot.au.fh)

harboot.fh <- resample(har.p.fh, 1000) 
dim(harboot.fh)

harboot.sp.fh <- resample(har.sp.fh, 1000) 
dim(harboot.sp.fh)

harboot.su.fh <- resample(har.su.fh, 1000) 
dim(harboot.su.fh)

harboot.au.fh <- resample(har.au.fh, 1000) 
dim(harboot.au.fh)

rabboot.fr <- resample(rab.p.fr, 1000) 
dim(rabboot.fr)

rabboot.sp.fr <- resample(rab.sp.fr, 1000) 
dim(rabboot.sp.fr)

rabboot.su.fr <- resample(rab.su.fr, 1000) 
dim(rabboot.su.fr)

rabboot.au.fr <- resample(rab.au.fr, 1000) 
dim(rabboot.au.fr)

rabboot.wi.fr <- resample(rab.wi.fr, 1000) 
dim(rabboot.wi.fr)

marboot.ms <- resample(mar.p.ms, 1000) 
dim(marboot.ms)

marboot.sp.ms <- resample(mar.sp.ms, 1000) 
dim(marboot.sp.ms)

marboot.su.ms <- resample(mar.su.ms, 1000) 
dim(marboot.su.ms)

marboot.au.ms <- resample(mar.au.ms, 1000) 
dim(marboot.au.ms)

marboot.wi.ms <- resample(mar.wi.ms, 1000) 
dim(marboot.wi.ms)

squboot.ms <- resample(squ.p.ms, 1000) 
dim(squboot.ms)

squboot.sp.ms <- resample(squ.sp.ms, 1000) 
dim(squboot.sp.ms)

squboot.su.ms <- resample(squ.su.ms, 1000) 
dim(squboot.su.ms)

squboot.au.ms <- resample(squ.au.ms, 1000) 
dim(squboot.au.ms)

squboot.wi.ms <- resample(squ.wi.ms, 1000) 
dim(squboot.wi.ms)

# Now we pass these two matrices to bootEst to generate estimates of overlap from each pair of samples. 
foxhar.b.fh <- bootEst(foxboot.fh, harboot.fh, type="Dhat4") # takes a few seconds
(BSmean.foxhar.b.fh <- mean(foxhar.b.fh))

foxhar.b.sp.fh <- bootEst(foxboot.sp.fh, harboot.sp.fh, type="Dhat4") # takes a few seconds
(BSmean <- mean(foxhar.b.sp.fh))

foxhar.b.su.fh <- bootEst(foxboot.su.fh, harboot.su.fh, type="Dhat4") # takes a few seconds
(BSmean.foxhar.b.sp.fh <- mean(foxhar.b.su.fh))

foxhar.b.au.fh <- bootEst(foxboot.au.fh, harboot.au.fh, type="Dhat4") # takes a few seconds
(BSmean.foxhar.b.au.fh <- mean(foxhar.b.au.fh))

foxrab.b.fr <- bootEst(foxboot.fr, rabboot.fr, type="Dhat4") # takes a few seconds
(BSmean.foxrab.b.fr <- mean(foxrab.b.fr))

foxrab.b.sp.fr <- bootEst(foxboot.sp.fr, rabboot.sp.fr, type="Dhat4") # takes a few seconds
(BSmean.foxrab.b.sp.fr <- mean(foxrab.b.sp.fr))

foxrab.b.su.fr <- bootEst(foxboot.su.fr, rabboot.su.fr, type="Dhat4") # takes a few seconds
(BSmean.foxrab.b.su.fr <- mean(foxrab.b.su.fr))

foxrab.b.au.fr <- bootEst(foxboot.au.fr, rabboot.au.fr, type="Dhat4") # takes a few seconds
(BSmean.foxrab.b.au.fr <- mean(foxrab.b.au.fr))

foxrab.b.wi.fr <- bootEst(foxboot.wi.fr, rabboot.wi.fr, type="Dhat1") # takes a few seconds
(BSmean.foxrab.b.wi.fr <- mean(foxrab.b.wi.fr))

marsqu.b.ms <- bootEst(marboot.ms, squboot.ms, type="Dhat4") # takes a few seconds
(BSmean.marsqu.b.ms <- mean(marsqu.b.ms))

marsqu.b.sp.ms <- bootEst(marboot.sp.ms, squboot.sp.ms, type="Dhat4") # takes a few seconds
(BSmean.marsqu.b.sp.ms <- mean(marsqu.b.sp.ms))

marsqu.b.su.ms <- bootEst(marboot.su.ms, squboot.su.ms, type="Dhat4") # takes a few seconds
(BSmean.marsqu.b.su.ms <- mean(marsqu.b.su.ms))

marsqu.b.au.ms <- bootEst(marboot.au.ms, squboot.au.ms, type="Dhat4") # takes a few seconds
(BSmean.marsqu.b.au.ms <- mean(marsqu.b.au.ms))

marsqu.b.wi.ms <- bootEst(marboot.wi.ms, squboot.wi.ms, type="Dhat4") # takes a few seconds
(BSmean.marsqu.b.wi.ms <- mean(marsqu.b.wi.ms))

# Extract CI
fh.annual <- bootCI(foxhar2est.fh, foxhar.b.fh)
fh.annualCI <- fh.annual[3,]
fh.spring <- bootCI(foxhar.sp2est.fh, foxhar.b.sp.fh)
fh.springCI <- fh.spring[3,]
fh.summer <- bootCI(foxhar.su2est.fh, foxhar.b.su.fh)
fh.summerCI <- fh.summer[3,]
fh.autumn <- bootCI(foxhar.au2est.fh, foxhar.b.au.fh)
fh.autumnCI <- fh.autumn [3,] 

fr.annual <- bootCI(foxrab2est.fr, foxrab.b.fr)
fr.annualCI <- fr.annual[3,]
fr.spring <- bootCI(foxrab.sp2est.fr, foxrab.b.sp.fr)
fr.springCI <- fr.spring[3,]
fr.summer <- bootCI(foxrab.su2est.fr, foxrab.b.su.fr)
fr.summerCI <- fr.summer[3,]
fr.autumn <- bootCI(foxrab.au2est.fr, foxrab.b.au.fr)
fr.autumnCI <- fr.autumn[3,]
fr.winter<- bootCI(foxrab.wi2est.fr, foxrab.b.wi.fr)
fr.winterCI <- fr.winter[3,]

ms.annual <- bootCI(marsqu2est.ms, marsqu.b.ms)
ms.annualCI <- ms.annual[3,]
ms.spring <- bootCI(marsqu.sp2est.ms, marsqu.b.sp.ms)
ms.springCI <- ms.spring[3,]
ms.summer <- bootCI(marsqu.su2est.ms, marsqu.b.su.ms)
ms.summerCI <- ms.summer[3,]
ms.autumn <- bootCI(marsqu.au2est.ms, marsqu.b.au.ms)
ms.autumnCI <- ms.autumn[3,]
ms.winter <- bootCI(marsqu.wi2est.ms, marsqu.b.wi.ms)
ms.winterCI <- ms.winter[3,]

#Create a data frame with the overlaps and the bootstraped CIs
SpeciesPair <- c(rep("Fox - Rabbit", times = 5), rep("Fox - Hare", times = 5), rep("Marten - Squirrel", 
                                                                                   times = 5))
Season <- c("Annual", "Spring", "Summer", "Autumn", "Winter")

Overlap_estimates_fr <- c(foxrab2est.fr,
                          foxrab.sp2est.fr,
                          foxrab.su2est.fr, 
                          foxrab.au2est.fr, 
                          foxrab.wi2est.fr)
Overlap_CI_fr <- c(fr.annualCI, 
                   fr.springCI, 
                   fr.summerCI ,
                   fr.autumnCI ,
                   fr.winterCI)
Lower_Overlap_CI_fr <- Overlap_CI_fr [c(1, 3, 5, 7, 9)]
Upper_Overlap_CI_fr <- Overlap_CI_fr [c(2, 4, 6, 8, 10)]

Overlap_estimates_fh <- c(foxhar2est.fh,
                          foxhar.sp2est.fh,
                          foxhar.su2est.fh,
                          foxhar.au2est.fh, 
                          "NA")
Overlap_CI_fh <- c(fh.annualCI, 
                   fh.springCI, 
                   fh.summerCI ,
                   fh.autumnCI ,
                   "NA", "NA")
Lower_Overlap_CI_fh <- Overlap_CI_fh [c(1, 3, 5, 7, 9)]
Upper_Overlap_CI_fh <- Overlap_CI_fh [c(2, 4, 6, 8, 10)]

Overlap_estimates_ms <- c(marsqu2est.ms,
                          marsqu.sp2est.ms,
                          marsqu.su2est.ms, 
                          marsqu.au2est.ms, 
                          marsqu.wi2est.ms)
Overlap_CI_ms <- c(ms.annualCI, 
                   ms.springCI, 
                   ms.summerCI ,
                   ms.autumnCI ,
                   ms.winterCI)
Lower_Overlap_CI_ms <- Overlap_CI_ms [c(1, 3, 5, 7, 9)]
Upper_Overlap_CI_ms <- Overlap_CI_ms [c(2, 4, 6, 8, 10)]

Overlap_estimates <- c(Overlap_estimates_fr, Overlap_estimates_fh, Overlap_estimates_ms)
Lower_Overlap_CI <- c(Lower_Overlap_CI_fr, Lower_Overlap_CI_fh, Lower_Overlap_CI_ms)
Upper_Overlap_CI <- c(Upper_Overlap_CI_fr, Upper_Overlap_CI_fh, Upper_Overlap_CI_ms)

OverlapTable <- data.frame(SpeciesPair, Season, Overlap_estimates,
                           Lower_Overlap_CI, Upper_Overlap_CI)

View(OverlapTable)

OverlapTable$Overlap_estimates <- as.numeric(as.character(OverlapTable$Overlap_estimates))
OverlapTable$Upper_Overlap_CI <- as.numeric(as.character(OverlapTable$Upper_Overlap_CI))
OverlapTable$Lower_Overlap_CI <- as.numeric(as.character(OverlapTable$Lower_Overlap_CI))

#Round funciton always rounds .5 down
OverlapTable[,c("Overlap_estimates", "Upper_Overlap_CI", "Lower_Overlap_CI")] <- round(OverlapTable[,
                                                                                                    c("Overlap_estimates", "Upper_Overlap_CI", "Lower_Overlap_CI")], 3)


colnames(OverlapTable) <- c("Predator-Prey pairs", "Season", "Overlap estimate", 
                            "Upper bootstrap CI", "Lower bootstrap CI")

#write.csv(OverlapTable, file = "OverlapTable.csv")


### Plots
theme_ac1 <- function(base_family = "serif", base_size_a = 12, base_size_t = 12){
  theme_bw(base_family = base_family) %+replace%
    theme(
      plot.background = element_blank(),
      panel.grid = element_blank(),   
      legend.key=element_rect(colour=NA, fill =NA),
      panel.border = element_rect(fill = NA, colour = "black", size=1),
      panel.background = element_rect(fill = "white", colour = "black"), 
      strip.background = element_rect(fill = NA)
    )
}

##### Activity of species groups
#
# Group 1 = Fox, badger, marten
# Group 2 = hare, rabbit, squirrel
# Group 3 = fallow, woodmouse
a1 <- ggplot(act.yr) +
  geom_rect(xmin = -5, xmax = 5, ymin = -1, ymax = 1.1, fill="lightgrey", linetype = 0, color = "lightgrey") +
  geom_rect(xmin = 20, xmax = 25, ymin = -1, ymax = 1.1, fill="lightgrey", linetype = 0, color = "lightgrey") +
  stat_smooth(data=subset(act.yr, spp_1 %in% c("Fox")), aes(y = rescale(freq, to = c(0.1, .9)), x = hour, linetype = "Fox"), 
              colour = "black", method = lm, formula = y ~ poly(x, 10), se = FALSE) + 
  stat_smooth(data=subset(act.yr, spp_1 %in% c("Hare")), aes(y = rescale(freq, to = c(0.1, 0.9)), x = hour, linetype = "Hare"), 
              colour = "black", method = lm, formula = y ~ poly(x, 10), se = FALSE) + 
  stat_smooth(data=subset(act.yr, spp_1 %in% c("Rabbit")), aes(y = rescale(freq, to = c(0., .9)), x = hour, linetype = "Rabbit"), 
              colour = "black", method = lm, formula = y ~ poly(x, 10), se = FALSE) + 
  theme_ac1() + 
  theme(
    axis.text.x = element_text(colour = "white"),
    axis.title.x = element_text(colour = "white"),
    axis.ticks.x = element_line(colour = "white"),
    axis.text.y = element_text(size = 12, colour = "black"),
    axis.title.y = element_text(size = 12, face = "bold", colour = "black")) +  
  scale_linetype_manual(values=c("solid", "dotted", "dotdash"), name = "Species")  +
  xlab("Hour") + ylab("Frequency of detection") +
  scale_x_continuous(limits = c(0,23.5),
                     breaks = c(0,6,12,18,24)) +
  ylim(0,1)


a2 <- ggplot(act.yr) +
  geom_rect(xmin = -5, xmax = 5, ymin = -1, ymax = 1.1, fill="lightgrey", linetype = 0, color = "lightgrey") +
  geom_rect(xmin = 20, xmax = 25, ymin = -1, ymax = 1.1, fill="lightgrey", linetype = 0, color = "lightgrey") +
  stat_smooth(data=subset(act.yr, spp_1 %in% c("Marten")), aes(y = rescale(freq, to = c(0.1, .9)), x = hour, linetype = "Marten"), 
              colour = "black", method = lm, formula = y ~ poly(x, 10), se = FALSE) + 
  stat_smooth(data=subset(act.yr, spp_1 %in% c("Squirrel")), aes(y = rescale(freq, to = c(0.1, .9)), x = hour, linetype = "Squirrel"), 
              colour = "black", method = lm, formula = y ~ poly(x, 10), se = FALSE) + 
  stat_smooth(data=subset(act.yr, spp_1 %in% c("Mouse")), aes(y = rescale(freq, to = c(0.1, .9)), x = hour, linetype = "Mouse"), 
              colour = "black", method = lm, formula = y ~ poly(x, 10), se = FALSE) + 
  theme_ac1() + 
  theme(
    axis.text.x = element_text(colour = "white"),
    axis.title.x = element_text(colour = "white"),
    axis.ticks.x = element_line(colour = "white"),
    axis.text.y = element_text(size = 12, colour = "black"),
    axis.title.y = element_text(size = 12, face="bold", colour = "black")) + 
  scale_linetype_manual(values=c("solid", "dotted", "dotdash"), name = "Species")  +
  xlab("Hour") + ylab("Frequency of detection") +
  scale_x_continuous(limits = c(0,23.5),
                     breaks = c(0,6,12,18,24)) +
  ylim(0,1)

a3 <- ggplot(act.yr) +
  geom_rect(xmin = -5, xmax = 5, ymin = -1, ymax = 1.1, fill="lightgrey", linetype = 0, color = "lightgrey") +
  geom_rect(xmin = 20, xmax = 25, ymin = -1, ymax = 1.1, fill="lightgrey", linetype = 0, color = "lightgrey") +
  stat_smooth(data=subset(act.yr, spp_1 %in% c("Badger")), aes(y = rescale(freq, to = c(0.1, .9)), x = hour, linetype = "Badger"), 
              colour = "black", method = lm, formula = y ~ poly(x, 10), se = FALSE) + 
  stat_smooth(data=subset(act.yr, spp_1 %in% c("Fallow")), aes(y = rescale(freq, to = c(0.1, .9)), x = hour, linetype = "Fallow"), 
              colour = "black", method = lm, formula = y ~ poly(x, 10), se = FALSE) + 
  theme_ac1() + 
  theme(
    axis.text = element_text(size = 12, colour = "black"),
    axis.title.y = element_text(size = 12, face="bold", colour = "black"),
    axis.title.x = element_text(size = 12, face="bold", colour = "black")) + 
  scale_linetype_manual(values=c("solid", "dotted"), name = "Species")  +
  xlab("Hour") + ylab("Frequency of detection") +
  scale_x_continuous(limits = c(0,23.5),
                     breaks = c(0,6,12,18,24)) +
  ylim(0,1)


#Export multiplot
#png('Plots/Activity_plot.png', units="in", type="cairo", width=6, height=10, res=300)
#ggarrange(a1, a2, a3,
#          labels = c("a)", "b)", "c)"),
#          ncol = 1, nrow = 3)
#dev.off() 

# Import animal silhouette graphics
# Images copyright Anthony Caravaggi. Do not use without permission
badger <- readJPEG("images/badger.jpg")
badger <- rasterGrob(badger, interpolate=TRUE)
deer <- readJPEG("images/fallow deer.jpg")
deer <- rasterGrob(deer, interpolate=TRUE)
fox <- readJPEG("images/fox.jpg")
fox <- rasterGrob(fox, interpolate=TRUE)
hare <- readJPEG("images/hare.jpg")
hare <- rasterGrob(hare, interpolate=TRUE)
marten <- readJPEG("images/pine marten.jpg")
marten <- rasterGrob(marten, interpolate=TRUE)
mouse <- readJPEG("images/wood mouse.jpg")
mouse <- rasterGrob(mouse, interpolate=TRUE)
rabbit <- readJPEG("images/rabbit.jpg")
rabbit <- rasterGrob(rabbit, interpolate=TRUE)
squirrel <- readJPEG("images/squirrel.jpg")
squirrel <- rasterGrob(squirrel, interpolate=TRUE)

# Function for reproducible magnitude plots
#
# spp = species(e.g. "Fox")
# img = species silhouette
magplot <- function(spp, img){
  ggplot(data = subset(allDat, spp_1 %in% c(spp)), aes(x = season, y = m_60)) +
    geom_rect(xmin = 0, xmax = 5, ymin = -11, ymax = 0, position = position_nudge(x = -1, y = 0),
              fill="#CCCCCC", linetype = 1, color = "#000000") + #Changed ymin=10
    #geom_violin(scale = "count", trim = TRUE) +
    #stat_summary(fun.data=data_summary, geom="crossbar", width=0.1 ) + 
    geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
    geom_point(aes(y = m_60), size = .05, alpha = 0.8, position = position_jitter(width = .15)) +
    geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5) +
    theme_ac1() + 
    theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
    xlab("Season") +
    ylab(expression(atop("Detection time", paste("relative to sunrise/set"))))+
    scale_x_discrete(labels=c("Spring", "Summer", "Autumn", "Winter")) +
    geom_hline(yintercept = mean(subset(allDat$m_60, allDat$spp_1 %in% c(spp))), linetype="dashed") +
    annotation_custom(img, xmin=3.5, xmax=4.5, ymin=6, ymax=10)+
    scale_y_continuous(limits=c(-10, 10)) #Added limits
}

b.s <- magplot("Badger", badger)
b.s <- b.s + theme(
  axis.text.x = element_text(colour = "white"),
  axis.title.x = element_text(colour = "white"),
  axis.ticks.x = element_line(colour = "white"),
  axis.text.y = element_text(size = 14),
  axis.title.y = element_text(size = 14))
d.s <- magplot("Fallow", deer)
d.s <- d.s + theme(
  axis.text.x = element_text(colour = "white"),
  axis.title.x = element_text(colour = "white"),
  axis.ticks.x = element_line(colour = "white"),
  axis.text.y = element_text(colour = "white"),
  axis.title.y = element_text(colour = "white"),
  axis.ticks.y = element_line(colour = "white"))
f.s <- magplot("Fox", fox)
f.s <- f.s + theme(
  axis.text.x = element_text(colour = "white"),
  axis.title.x = element_text(colour = "white"),
  axis.ticks.x = element_line(colour = "white"),
  axis.text.y = element_text(size = 14),
  axis.title.y = element_text(size = 14))
h.s <- magplot("Hare", hare)
h.s <- h.s + theme(
  axis.text.x = element_text(colour = "white"),
  axis.title.x = element_text(colour = "white"),
  axis.ticks.x = element_line(colour = "white"),
  axis.text.y = element_text(colour = "white"),
  axis.title.y = element_text(colour = "white"),
  axis.ticks.y = element_line(colour = "white"))
ma.s <- magplot("Marten", marten)
ma.s <- ma.s + theme(
  axis.text.x = element_text(colour = "white"),
  axis.title.x = element_text(colour = "white"),
  axis.ticks.x = element_line(colour = "white"),
  axis.text.y = element_text(size = 14),
  axis.title.y = element_text(size = 14))
r.s <- magplot("Rabbit", rabbit)
r.s <- r.s +  theme(
  axis.text.x = element_text(colour = "white"),
  axis.title.x = element_text(colour = "white"),
  axis.ticks.x = element_line(colour = "white"),
  axis.text.y = element_text(colour = "white"),
  axis.title.y = element_text(colour = "white"),
  axis.ticks.y = element_line(colour = "white"))
s.s <- magplot("Squirrel", squirrel)
s.s <- s.s + theme(
  axis.title.x = element_text(size = 14),
  axis.text.x = element_text(size = 14),
  axis.text.y = element_text(size = 14),
  axis.title.y = element_text(size = 14))
mo.s <- magplot("Mouse", mouse)
mo.s <- mo.s + theme(
  axis.text.x = element_text(size = 14),
  axis.title.x = element_text(size = 14),
  axis.text.y = element_text(colour = "white"),
  axis.title.y = element_text(colour = "white"),
  axis.ticks.y = element_line(colour = "white"))

# Export multiplot
png('Mag_plot.png', units="in", type = 'cairo', width=9.5, height=12, res=300)
ggarrange(b.s, d.s, f.s, h.s, ma.s, r.s, s.s, mo.s,
          labels = c("a)", "b)", "c)", "d)", "e)", "f)", "g)", "h)"),
          vjust = 1,
          ncol = 2, nrow = 4)
dev.off() 

