###########################################################
#File name: Population_model_trial2.R                     #
#Population model for Neodiprion sertifer                 #
#Exploring how trait-mediated effects affects the dynamics#
###########################################################

library(ggplot2)

#ADD:
#Direct consumptive efffect - ok 

#Female choice

#Possibly add something on browsing intesity (100% - have data for survival, and for fecundity although low sample size)
#Would be interesting as surivival response seems to be linear whereas fecundity seem to be hump-shaped 

#What values in the control model are needed for outbreaks - put these in and see if slarvaeB and DC can mitigate outbreaks under these conditions

###Create a function that draws random values from a beta distribution (for browsing proportions):
P.brow<-0.35
#P.brow - mean browsing level
b.var<-0.05
#b.var - sd of browsing level

#Create a function that creates alpha (shape1) from the mean and sd:
S1<-(P.brow^2-P.brow^3-P.brow*(b.var)^2)/(b.var^2)

#Create a function that creates beta (shape2) from the mean and sd
S2<-(P.brow-2*P.brow^2+P.brow^3-b.var^2+P.brow*(b.var)^2)/b.var^2

rbeta(10,S1,S2)

#OK! this works. Now: P.brow should be Btrees (0 to 1 by 0.1, or maybe skip 0 and 1?) and for each mean we should run 
#the model a set nb of times - 100 maybe?


#Model function 
years=50 #set number of years (same as in function)

sawfly.model<-function(BtreesM=0.5,
                       SlarvaeC=0.6,
                       years=50,
                       DC=0.05,
                       FecundityC=79,
                       biasB=0.5){
  #need to store the outputs in a big output matrix/df!!
  #?!?!?!?! hoe do I do this? 
  output.big<-matrix(nrow=50,ncol=100)
  for(k in 1:100){
    #put the whole thing in here - will this work? 
    N<-2 #Initial population density (cocoons per m2)
    #SlarvaeB<-0.42 #Survival of larvae on browsed trees
    #Below is the attempt to draw browsing from distr.
    b.var<-0.05
    S1<-((BtreesM^2)-(BtreesM^3)-BtreesM*(b.var)^2)/(b.var^2)
    S2<-(BtreesM-2*(BtreesM^2)+(BtreesM^3)-b.var^2+BtreesM*(b.var)^2)/b.var^2
    Btrees<-rbeta(1,S1,S2)
    Ctrees<-(1-Btrees)
    #Ctrees<-(1-BtreesM)
    sexratio<-0.50 #Survival of larvae on control trees
    Bfec<-0 #Proportion of sawflies that fed on browsed trees as larvae in previous generation - have fecundity from browsed trees
    Cfec<-1
    biasC<-(1-biasB)
    FecundityB<-FecundityC*1.09 #Fecundity of females reared on browsed trees (9% higher than controls), 15% if we use clipped
    Sbackground<-0.5 #Adult survival 
    Seggs<-0.9 #Egg survival
    SlarvaeB=SlarvaeC*0.8 #Surival of larvae on browsed trees
    #Try with SlarvaeB = rnorm(1, mean, sd) [and same for fecundity values etc.]
    
    #Make population value and lamda value matrices
    #population<-numeric(years)#Nt+1 matrix
    population<-matrix(nrow=years,ncol=2)
    pop<-numeric(years)#Nt matrix
    lambda<-numeric(years) #Lambda matrix (Lambda=Nt+1/Nt)
    ingestion<-numeric(years)
    
    
    for(j in 1:years){
      pop[j]<-N #number of pupae at time t
      s.pupae<-1-(45*(N/(625+(N^2)))) #density dependent survival of pupae (based on Larsson et al 2000, Hanski 1987)
      N<-N*s.pupae #Basically number of adults at time t
      #N<-N*s.adult #adult survival (from Olofsson or other study?)
      #If we could incorporate female choice we should do it here
      
      N<-N*Sbackground #number of adults after effects of background survival (mortality of adults)
      
      #The female choice should go here I think
      #N is nb of adults after background survival
      #NF<-N*sexratio #Number of females
      #NFC<-NF*Cfec #Number of females with c-fecundity
      #NFB<-NF*Bfec #Number of females with b-fecundity
      #Not sure what these 3 lines above are for... 
      
      
      
      
      #Old way (w/o the choice module):
      NB.eggs<-N*Bfec*FecundityB*sexratio #Number of eggs laid by females with FecundityB
      NC.eggs<-N*Cfec*FecundityC*sexratio #Number of eggs laid by females with FecundityC
      N.eggs<-NB.eggs+NC.eggs #number of eggs
      NB<-N.eggs*Btrees #Number of eggs on browsed trees
      NC<-N.eggs*Ctrees #Number of eggs on control trees
      
      #trial with density dependent DC (egg ingestion)
      
      #if we exchange alfo for DC we can explore similar to before different levels of max DC but with the d-d function
      #alfo<-0.2 #Maximum value of e.ingest (0.2 is 20%)
      beto<-50
      e.ingest=(DC*NB^2)/(beto^2+NB^2)
      
      NB<-NB - e.ingest*NB
      
      #NB<-NB - DC*NB #Number of eggs on browsed trees after direct consumptive effects
      
      
      NC<-NC #No DC effects on control trees
      
      NB.larvae<-NB*Seggs #other survival effects on eggs (independent of browsing) --> nb of larvae
      NC.larvae<-NC*Seggs #other survival effects on eggs (independent of browsing) --> nb of larvae
      
      NB<-NB.larvae*SlarvaeB #survivl of larvae on browsed trees -> nb of cocoons produced from browsed trees
      NC<-NC.larvae*SlarvaeC #survival of larvae on control trees -> nb of cocoons produced from control trees
      
      
      N<-NB+NC #Number of pupae at t+1 
      
      
      Bfec<-(NB)/N #the prop of individuals that have been reared on browsed trees 
      Cfec<-(NC)/N #the prop of individuals that have been reared on control trees 
      #Prop of induced trees (random), this can be varied to only be within a certain range or have a set value
      #Btrees<-runif(1,0,1) #Prop of trees getting browsed during winter, could be random or fixed 
      #Ctrees<-(1-Btrees)
      population[j]<-N
      population[j,2]<-j #counting - years I guess
      lambda[j]<-(population[j]/pop[j])
      ingestion[j]<-e.ingest
      
      #Population regulation (specialist natural enemy)
      lastyears<-subset(population,population[,2]<=j & population[,2]>=(j-4))
      over<-which(lastyears>1000)
      if(length(over)>=5){
        N=N*0.001
      }
      
    }
    output<-list("params" = c("BtreesM"=BtreesM,"DC"=DC,"SlarvaeC"=SlarvaeC,"FecundityC"=FecundityC,"biasB"=biasB),"population"=population,"population(t-1)"=pop,"lambda"=lambda,"ingestion"=ingestion)
    #return(output)
    output.big[,k]<-output$population[,1]
  }
  return(output.big)
  
}

#i think it works now - output big is now all the 100 runs of the same thing - only Btrees change due to rbeta()

sawfly.model()

#trial with only 1 run:
hej<-sawfly.model(BtreesM=0.9,
                  SlarvaeC=0.65,
                  years=50,
                  DC=0.05,
                  FecundityC=79,
                  biasB=0.5)
hej1<-rowMeans(hej)
hej1
plot(1:50,hej1)
lines(1:50,hej1)
lines(1:50,rep(200,50),col="lightblue")

# first, set up some ranges for a few parameters
BtreesM <- seq (from=0.3, to=0.4, by=0.1)
DC <- seq(from=0, to=0, by=0.05)
SlarvaeC<-seq(from=0.45,to=0.70,by=0.05)
FecundityC<-seq(from=65, to=95, by=1)
biasB<-seq(from=0.5,to=0.5,by=0.25)



#difference between control and browsed survival from our experiment:
#10 "procentenheter". (47% vs 37%)
#What happens if we use the 'effect size' in the model SlarvaeB is 10% lower than SlarveC


# make a data.frame with every combination of those parameters
param.args <- expand.grid(BtreesM = BtreesM, DC=DC, SlarvaeC=SlarvaeC, FecundityC=FecundityC,biasB=biasB)


# using apply, iterate across every row and pass the row values as the arguments to the tmii.func
output.list <- apply(param.args,1,function(params)sawfly.model(BtreesM=params[1],DC=params[2],SlarvaeC=params[3],FecundityC=params[4],biasB=params[5]))

param.args$run <- paste0("run_", seq_along(param.args[,1])) 
#output.df <-data.frame((matrix(ncol = length(output.list), nrow = years)))

output.df <- data.frame("Step"=seq(from=1, to=years, by=1))
for (i in 1:length(output.list)){
  output.df[i+1] <- output.list[[i]]$population[,1]
  colnames(output.df)[i+1] <- paste0("run_",i)
}

#This adds 186 columns for thresholds, maybe I can directly add 0 or 1 with ifelse ?
#Should not be 186 here - calc how many runs it is 
#186 * 12 (because of 12 levels of browsing proportions)? (186*12 = 2232)
ncol(output.df) #check nb of columns in output
colum<-ncol(output.df)
for(i in 1:colum){
  output.df[[paste0("threshold_",i)]]<-0
  
}

#Hopefully this is correct: 
for(j in 1:colum){
  for(i in 1:50){
    if(output.df[i,1+j] >= 200){
      output.df[i,(colum+1)+j]<-1
    }       
  }
}


threshold_values<-output.df[,(colum+2):ncol(output.df)] #should it really be +2 here?
#should try with +1
threshold_values
outb.sum<-colSums(threshold_values)
outb.sum<-data.frame(outb.sum)
outb.sum$fecundity<-param.args$FecundityC
outb.sum$survival<-param.args$SlarvaeC
outb.sum$btrees<-param.args$BtreesM
outb.sum$dc<-param.args$DC



outb.sum

outb.sum$proYears<-outb.sum$outb.sum/50

Obs3<-aggregate(proYears~btrees,data=outb.sum,FUN=mean)
Obs3
plot(Obs3$btrees,Obs3$proYears) #proportion of years that have outbreaks (out of the 50 years)

plot(outb.sum$survival,outb.sum$outb.sum)
plot(outb.sum$fecundity,outb.sum$outb.sum)
plot(outb.sum$btrees,jitter(outb.sum$outb.sum))
plot(outb.sum$dc,outb.sum$outb.sum)

plot(outb.sum$btrees,outb.sum$proYears)
plot(outb.sum$survival,outb.sum$proYears)
plot(outb.sum$fecundity,outb.sum$proYears)

Obs<-aggregate(outb.sum~btrees,data=outb.sum,FUN=sum)
Obs
plot(Obs$btrees,Obs$outb.sum)

Obs1<-aggregate(outb.sum~survival,data=outb.sum,FUN=sum)
Obs1
plot(Obs1$survival,Obs1$outb.sum)

Obs2<-aggregate(outb.sum~fecundity,data=outb.sum,FUN=sum)
Obs2
plot(Obs2$fecundity,Obs2$outb.sum)


Obs4<-aggregate(outb.sum~dc,data=outb.sum,FUN=sum)
Obs4
plot(Obs4$dc,Obs4$outb.sum)


plot(1:years,output.df$run_9000,ylim=c(0,15000))
lines(1:years,output.df$run_9000,ylim=c(0,15000))
lines(1:years,rep(200,50),col="salmon")


plot(1:years,output.df$run_198,ylim=c(0,15000))
lines(1:years,output.df$run_198,ylim=c(0,15000))
lines(1:years,rep(200,50),col="salmon")


plot(1:years,output.df$run_132,ylim=c(0,15000))
lines(1:years,output.df$run_132,ylim=c(0,15000))
lines(1:years,rep(200,50),col="salmon")



 #Remember to change to BtreesM everywhere (done upto here)


#Tag values > 200 in the output df
#output.df$value<-ifelse(output.df$run_1>200,"Yes","No")
#But it only needs to be one value in the run. 
#Should rather be added to a df or list of the parameter combinations 


plot.df <- reshape2::melt (output.df, id="Step")

plot.df$threshold <- 0
plot.df$threshold[plot.df$value >= 200] <- 1

param.args$threshold <-  (table (plot.df$variable, plot.df$threshold)[,2])
#if there are no values in column 2 this does not work 
param.args$threshold[param.args$threshold > 0] <- 1


library(ggplot2)
ggplot2::ggplot (plot.df, aes(Step, value,fill=variable))+
  geom_line(alpha=0.2)+
  coord_cartesian(ylim=c(0,500))+
  geom_hline(yintercept=200,linetype=2,colour="lightblue",size=1.2)+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Calculate the proportion of outbreak runs:
#Should be in some sort of loop so that values for all parameter combinations can be stored.
#Or should be devided according to parameter combinations !
th1<-plot.df[plot.df$threshold==1,]
outbreaks<-(length(th1$threshold))/length(plot.df$threshold)


#ggplot(param.args,aes(Btrees,DC))+
#geom_point(alpha=0.6,colour=ifelse(param.args$threshold>0,"darkorange","lightblue"),size=4)+
#theme_minimal()+
#theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


param.args



library(plotly)

p <- plot_ly(param.args, x = ~SlarvaeC, y = ~FecundityC, z = ~Btrees, color = ~threshold, colors = c("lightblue", "darkorange")) %>%
  add_markers() %>%
  hide_colorbar() %>%
  layout(scene = list(xaxis = list(title = 'S'),
                      yaxis = list(title = 'F'),
                      zaxis = list(title = 'B')))

p

ggplot(param.args,aes(FecundityC,SlarvaeC))+
  geom_point(alpha=0.6,colour=ifelse(param.args$threshold>0,"darkorange","lightblue"),size=4,position="jitter")+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


ggplot(param.args,aes(Btrees,SlarvaeC))+
  geom_point(alpha=0.6,colour=ifelse(param.args$threshold>0,"darkorange","lightblue"),size=4,position="jitter")+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggplot(param.args,aes(Btrees,FecundityC))+
  geom_point(alpha=0.6,colour=ifelse(param.args$threshold>0,"darkorange","lightblue"),size=4,position="jitter")+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())



#Try to do a logistic regression for the model output
OBdata<-param.args
head(OBdata)



library(car)
library(lme4)
logr.m<-glm(threshold~Btrees+FecundityC+SlarvaeC+DC+biasB,family=binomial,data=OBdata)
Anova(logr.m)
summary(logr.m)



#calculate, for each level of Btrees (i.e. 0, 0.05, ..., 1 (21 levels)), the proportion of outbreaks.
#On each Btrees level there is one run for each fecxsurvival combination and the same runs are on all levels. 
#By calc the prop of outbreaks we get a value for how Btrees affect oubreaks. 
data.sum <- aggregate(OBdata[,"threshold"] ~ Btrees+DC, data=OBdata, FUN=sum)
#can add DC and biasB to aggregate by if I use those versions of the model
data.sum
data.sum$prop<-data.sum$`OBdata[, "threshold"]`/186
data.sum
plot(data.sum$Btrees,data.sum$prop,bty="n",xlab="Proportion of browsed trees",ylab="Proportion of outbreaks")
lines(data.sum$Btrees,data.sum$prop)




ggplot(data.sum,aes(Btrees,prop,colour=c(factor(data.sum$biasB),factor(data.sum$DC)),group=c(factor(data.sum$biasB),factor(data.sum$DC))))+
  geom_point(alpha=0.6,size=4)+
  #geom_line(alpha=0.6,size=2)+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "none")

ggplot(data.sum, aes(Btrees, prop, color = factor(biasB), linetype = factor(DC))) + geom_path()+geom_point()

ggplot(data.sum,aes(Btrees, prop, group=factor(DC)))+
  geom_point(aes(size=0.5,alpha=0.6,colour = factor(DC)))+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "none")



#One plot for each level of DC: 
ggplot(data.sum[data.sum$DC==0,],aes(Btrees, prop, group=biasB))+
  geom_point(aes(size=0.5,alpha=0.6,colour = biasB))+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "none",axis.ticks = element_line(size=1,colour="grey"),axis.line = element_line(size=1,colour="grey"))+
  scale_y_continuous(breaks =seq(0.1, 0.6, by = 0.1),limits=c(0.1,0.6))

ggplot(data.sum[data.sum$DC==0.1,],aes(Btrees, prop, group=biasB))+
  geom_point(aes(size=0.5,alpha=0.6,colour = biasB))+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "none",axis.ticks = element_line(size=1,colour="grey"),axis.line = element_line(size=1,colour="grey"))+
  scale_y_continuous(breaks =seq(0.1, 0.6, by = 0.1),limits=c(0.1,0.6))

#want to increase size of axis nb

ggplot(data.sum[data.sum$DC==0.2,],aes(Btrees, prop, group=biasB))+
  geom_point(aes(size=0.5,alpha=0.6,colour = biasB))+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "none",axis.ticks = element_line(size=1,colour="grey"),axis.line = element_line(size=1,colour="grey"))+
  scale_y_continuous(breaks =seq(0.1, 0.6, by = 0.1),limits=c(0.1,0.6))


plot(jitter(OBdata$threshold,0.2)~jitter(OBdata$Btrees,1))
abline(lm(OBdata$threshold~OBdata$Btrees))
plot(jitter(OBdata$threshold,0.2)~jitter(OBdata$FecundityC,1))
abline(lm(OBdata$threshold~OBdata$FecundityC))
plot(jitter(OBdata$threshold,0.2)~jitter(OBdata$SlarvaeC,1))
abline(lm(OBdata$threshold~OBdata$SlarvaeC))
plot(jitter(OBdata$threshold,0.2)~jitter(OBdata$DC,1))
abline(lm(OBdata$threshold~OBdata$DC))
plot(jitter(OBdata$threshold,0.2)~jitter(OBdata$biasB,1))
abline(lm(OBdata$threshold~OBdata$biasB))


#'#0C4B8E', ,'#BF382A'

#Control model: 

control.model<-function(years=10,Slarvae=0.6,Fecundity=79){
  NC=2
  Sbackground=0.5
  sexratio=0.5
  Seggs=0.9
  populationC<-numeric(years)
  popC<-numeric(years)
  lambdaC<-numeric(years)
  for(i in 1:years){
    popC[i]<-NC #number of pupae at time t
    s.pupae<-1-(45*(NC/(625+(NC^2)))) #density dependent survival of pupae 
    NC<-NC*s.pupae #Basically number of adults at time t
    NC<-NC*sexratio*Fecundity #Number of eggs laid
    NC<-NC*Seggs
    NC<-NC*Sbackground #Number of larvae
    NC<-NC*Slarvae #Number of pupae
    populationC[i]<-NC
    lambdaC[i]<-(populationC[i]/popC[i])
  }
  outputC<-list("params" = c("Slarvae"=Slarvae,"Fecundity"=Fecundity),"populationC"=populationC,"populationC(t-1)"=popC,"lambdaC"=lambdaC)
  return(outputC)
}

control.model()



Slarvae<-seq(from=0.45,to=0.70,by=0.05)
Fecundity<-seq(from=65,to=95,by=1)
#limit is at 0.545

#difference between control and browsed survival from our experiment:
#10 "procentenheter". (47% vs 37%)
#What happens if we use the 'effect size' in the model SlarvaeB is 10% lower than SlarveC


# make a data.frame with every combination of those parameters
param.argsC <- expand.grid(Slarvae=Slarvae,Fecundity=Fecundity)


# using apply, iterate across every row and pass the row values as the arguments to the tmii.func
output.listC <- apply(param.argsC,1,function(params)control.model(Slarvae=params[1],Fecundity=params[2]))

param.argsC$run <- paste0("run_", seq_along(param.argsC[,1])) 
#output.df <-data.frame((matrix(ncol = length(output.list), nrow = years)))

output.dfC <- data.frame("Step"=seq(from=1, to=years, by=1))
for (i in 1:length(output.listC)){
  output.dfC[i+1] <- output.listC[[i]]$populationC
  colnames(output.dfC)[i+1] <- paste0("run_",i)
}



plot.dfC <- reshape2::melt (output.dfC, id="Step")

plot.dfC$threshold <- 0
plot.dfC$threshold[plot.dfC$value >= 200] <- 1

param.argsC$threshold <-  (table (plot.dfC$variable, plot.dfC$threshold)[,2])
#if there are no values in column 2 this does not work 
param.argsC$threshold[param.argsC$threshold > 0] <- 1

ggplot2::ggplot (plot.dfC, aes(Step, value,fill=variable))+
  geom_line(alpha=0.2)+
  coord_cartesian(ylim=c(0,500))+
  geom_hline(yintercept=200,linetype=2,colour="lightblue",size=1.2)+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())



param.argsC

ys<-rep(1,length(param.argsC$Slarvae))

ggplot(param.argsC,aes(Slarvae,ys))+
  geom_point(alpha=0.6,colour=ifelse(param.argsC$threshold>0,"darkorange","lightblue"),size=4)+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggplot(param.argsC,aes(Fecundity,Slarvae))+
  geom_point(alpha=0.6,colour=ifelse(param.argsC$threshold>0,"darkorange","lightblue"),size=4)+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


pC <- plot_ly(param.argsC, x = ~Btrees, y = ~DC, z = ~SlarvaeC, color = ~threshold, colors = c('#BF382A', '#0C4B8E')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = '% browsed trees'),
                      yaxis = list(title = 'Direct consumtion (%)'),
                      zaxis = list(title = 'Baseline larval survival (control)')))

pC


op.list<-control.model()
op.list

plot(op.list$populationC,bty="n",ylim=c(0,500))
lines(op.list$populationC)



##############################################

#QUESTIONS, LIMITATIONS, PROBLEMS, IDEAS ETC.:
##############################################

#Incorporate some direct consumption effect (% sawfly eggs accidently eaten by mammals during winter)
#Incorpotate some selectivity - female sawflies might avoid or prefer to oviposit on browsed trees
#Repeated herbivory - moose often re-browse trees - might change the level of imduced reponse
#Intensity of browsing - relationship between browsing induced reponse and the effect on sawflies might not be linear


