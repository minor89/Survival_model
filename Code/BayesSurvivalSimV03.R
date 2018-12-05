#########################################################################################################
# Filename: BayesSurvivalSimV03.R
# DoesWhat? At this stage, simply simmulates survival ...
#########################################################################################################
rm(list=ls())

# Set random number generator start seed if u want to get the same numbers each run (for testing)
#set.seed(123)

# Sites (x2)
Sites <- 2
# Blocks (x8)
Blocks <- 8
# Treatments (x3) Control, Low, High
Treats <- 3

#########################################################################################################
dat <- expand.grid(1:Sites,1:Blocks,1:Treats)
names(dat) <- c("Site","Block","Treat")

#########################################################################################################
# Generate truth

# Specify true effects for trees (Blocks x Reps x Treatments)
# Specify instantaneous mortality rates (per day) for each treatment

# Assumes "Control" treatment will still have some mortality
Trt.mus <- c(0.01,0.02,0.03)

# Specify true effects for blocks
# This is a multiplicative factor for the daily mortality rate for each block
sd.Blk <- 0.1 # specify how much variation associated with block
REs.Blk <- rlnorm(Blocks,0,sd.Blk)

# Specify true effects for sites
# This is a multiplicative factor for the daily mortality rate for each block
sd.Site <- 0.1  # specify how much variation associated with site
REs.Site <- rlnorm(Sites,0,sd.Site)

#########################################################################################################
# Columns for simulated survival data

# Samples sizes somewhere between 90 and 100 (I made this up)
dat$N0 <- round(100*runif(Sites*Blocks*Treats,0.9,1.0))
# Surviving at 24 days (24 chosen arbiitrarily)
dat$N24 <- NA
# Surviving at 53 days (53 chosen arbiitrarily)
dat$N53 <- NA

# Calculate survival at subsequent observation periods 
for (i in 1:nrow(dat)) {	
 # Grab the Block, Tree and Treatment data for simulating survival
 Site.pick <- dat$Site[i]
 Blk.pick <- dat$Block[i]
 Trt.pick <- dat$Treat[i]
 # Calculate the allocated mortality rate for each tree
 mu.pick <- Trt.mus[Trt.pick]*REs.Site[Site.pick]*REs.Blk[Blk.pick]
 # Calculate survival based on binomial distribution, with parameter p
 # calculated from the mortality rate and duration
 dat$N24[i] <- rbinom(1,dat$N0[i],prob=exp(-mu.pick*24))
 dat$N53[i] <- rbinom(1,dat$N24[i],prob=exp(-mu.pick*(53-24)))
}

# Have a look
dat

# Plot it
win.graph(h=7,w=14)
par(mfrow=c(1,2))
with(dat,boxplot(N24/N0 ~ Treat,ylim=c(0,1),xlab="Treatment",ylab="Survival"))
title("Survival by treatment (first observation period)")
abline(h=seq(0.2,0.8,0.2),lty=3)
with(dat,boxplot(N53/N24 ~ Treat,ylim=c(0,1),xlab="Treatment",ylab="Survival"))
title("Survival by treatment  (second observation period)")
abline(h=seq(0.2,0.8,0.2),lty=3)


#########################################################################################################
# Should wrap up in a function ... ToDo








#########################################################################################################
