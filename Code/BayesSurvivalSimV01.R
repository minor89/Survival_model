#########################################################################################################
# Filename: BayesSurvivalSimV01.R
# DoesWhat? At this stage, simply simmulates survival ...
#########################################################################################################

#test

rm(list=ls())

# Set random number generator start seed so we get the same numbers
set.seed(123)

# Blocks (x2)
Blocks <- 2
# Reps (x8)
Trees <- 8
# Treatments (x3) Control, Low, High
Treats <- 3

#########################################################################################################
dat <- expand.grid(1:Blocks,1:Trees,1:Treats)
names(dat) <- c("Block","Tree","Treat")

# Columns for simulated survival data

# Samples sizes somewhere between 90 and 100 (I made this up)
dat$N0 <- round(100*runif(Blocks*Trees*Treats,0.9,1.0))
# Surviving at 24 days
dat$N24 <- NA
# Surviving at 53 days
dat$N53 <- NA
 
#########################################################################################################
# Generate truth
# Specify instantaneous mortality rates (per day) for each treatment
# Assumes "Control" treatment will still have some mortality
mus <- c(0.01,0.02,0.03)

# Specify true effects for trees
# - this is a multiplicative factor for the daily mortality rate for each tree
REs.Trees <- replicate(Blocks,rlnorm(Trees,0,0.2))
# Specify true effects for blocks
REs.Blks <- rlnorm(Blocks,0,0.1)

# Calculate survival at subsequent observation periods 
for (i in 1:nrow(dat)) {	
 # Grab the Block, Tree and Treatment data for simulating survival
 Blk.pick <- dat$Block[i]
 Tree.pick <- dat$Tree[i]
 Trt.pick <- dat$Treat[i]
 # Calculate the allocated mortality rate for each tree
 mu.pick <- mus[Trt.pick]*REs.Trees[Tree.pick,Blk.pick]*REs.Blks[Blk.pick]
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
with(dat,boxplot(N24/N0 ~ Treat,ylim=c(0,1)))
title("Survival (first observation period)")
abline(h=seq(0.2,0.8,0.2),lty=3)
with(dat,boxplot(N53/N24 ~ Treat,ylim=c(0,1)))
title("Survival (second observation period)")
abline(h=seq(0.2,0.8,0.2),lty=3)


#########################################################################################################
