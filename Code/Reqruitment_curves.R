#Try to make a simpilfied script to look at reqruitmentcurves for the different 'r' values
###############
##############
#################


####################

library(ggplot2)


#Model function
#MODEL:
# N(t+1) = Nt * r (r = sexratio * survival * fecundity)



years=10 #set number of years (same as in function)

sawfly.model<-function(Btrees=0.5,
                       SlarvaeC=0.6,
                       years=10,
                       DC=0.05,
                       FecundityC=79,
                       biasB=0.5){
  N<-2 #Initial population density (cocoons per m2)
  #SlarvaeB<-0.42 #Survival of larvae on browsed trees
  Ctrees<-(1-Btrees)
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
  population<-numeric(years)#Nt+1 matrix
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
    #beto<-50
    #e.ingest=(DC*NB^2)/(beto^2+NB^2)
    
    #NB<-NB - e.ingest*NB
    
    NB<-NB - DC*NB #Number of eggs on browsed trees after direct consumptive effects
    
    
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
    lambda[j]<-(population[j]/pop[j])
    #ingestion[j]<-e.ingest
  }
  output<-list("params" = c("Btrees"=Btrees,"DC"=DC,"SlarvaeC"=SlarvaeC,"FecundityC"=FecundityC,"biasB"=biasB),"population"=population,"population(t-1)"=pop,"lambda"=lambda,"ingestion"=ingestion)
  return(output)
}


M<-sawfly.model()
M
#Plot reqruitment curves
plot(M$`population(t-1)`,M$population)
lines(M$`population(t-1)`,M$population)

#Plot lambda over the years 
plot(seq(1:years),M$lambda)
lines(seq(1:years),M$lambda)


Btrees <- seq (from=0.5, to=0.5, by=0.1)
DC <- seq(from=0, to=0, by=0.05)
SlarvaeC<-seq(from=0.5,to=0.70,by=0.1)
FecundityC<-seq(from=65, to=95, by=5)
biasB<-seq(from=0.5,to=0.5,by=0.25)



#difference between control and browsed survival from our experiment:
#10 "procentenheter". (47% vs 37%)
#What happens if we use the 'effect size' in the model SlarvaeB is 10% lower than SlarveC


# make a data.frame with every combination of those parameters
param.args <- expand.grid(Btrees = Btrees, DC=DC, SlarvaeC=SlarvaeC, FecundityC=FecundityC,biasB=biasB)


# using apply, iterate across every row and pass the row values as the arguments to the tmii.func
output.list <- apply(param.args,1,function(params)sawfly.model(Btrees=params[1],DC=params[2],SlarvaeC=params[3],FecundityC=params[4],biasB=params[5]))

param.args$run <- paste0("run_", seq_along(param.args[,1])) 
#output.df <-data.frame((matrix(ncol = length(output.list), nrow = years)))

output.df <- data.frame("Step"=seq(from=1, to=years, by=1))
for (i in 1:length(output.list)){
  output.df[i+1] <- output.list[[i]]$population
  colnames(output.df)[i+1] <- paste0("run_",i)
}


###Create an output with population at t - 1. 

op.rc<-data.frame("Step"=seq(from=1,to=years,by=1))
for(i in 1:length(output.list)){
  op.rc[i+1]<-output.list[[i]]$`population(t-1)`
  colnames(op.rc)[i+1]<-paste0("run_",i)
}

#Combine t and t -1 outputs (to be able to plot it in ggplot):

#New try
newP<- reshape2::melt (output.df, id="Step")
newR<-reshape2::melt (op.rc, id="Step")

new.out<-data.frame(newP,newR)
new.out

###Create an output with lambda

op.lambda<-data.frame("Step"=seq(from=1,to=years,by=1))
for(i in 1:length(output.list)){
  op.lambda[i+1]<-output.list[[i]]$lambda
  colnames(op.lambda)[i+1]<-paste0("run_",i)
}


length(output.df)
#Make a loop to plot all reqruitment curves (?)
plot(op.rc[,length(output.df)],output.df[,length(output.df)],type="n",ylim=c(0,70),xlim=c(0,40))

for(i in 2:length(output.df)){
  lines(op.rc[,i],output.df[,i],lty=2)
}
lines(0:40,rep(27.5,41),lty=3,col="salmon",lwd=4)

for(i in 1:length(output.df)){
  plot(op.rc[,i],output.df[,i],ylim=c(0,150),xlim=c(0,50),xlab=i,ylab="pop")
  lines(op.rc[,i],output.df[,i],lty=2)
}



plot(op.rc$run_40,output.df$run_40)
lines(op.rc$run_40,output.df$run_40,lty=2)
lines(op.rc$run_40,op.rc$run_40) #Equilibrium line (i.e. Nt+1 = Nt)
#Now - output.df gives us the population and op.rc the population at t - 1. 

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


ggplot2::ggplot (plot.df, aes(Step, value,fill=variable))+
  geom_line(alpha=0.2)+
  coord_cartesian(ylim=c(0,500))+
  geom_hline(yintercept=200,linetype=2,colour="lightblue",size=1.2)+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


plot.2<-reshape2::melt(op.rc,id="Step")

ggplot2::ggplot (plot.2, aes(Step, value,fill=variable))+
  geom_line(alpha=0.2)+
  coord_cartesian(ylim=c(0,500))+
  geom_hline(yintercept=200,linetype=2,colour="lightblue",size=1.2)+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Make lambda plots like this! 
#Here it is:
plot.L<-reshape2::melt(op.lambda,id="Step")

ggplot2::ggplot (plot.L, aes(Step, value,fill=variable))+
  geom_line(alpha=0.2)+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Data frame new.out for plot:
#Use new.out:
new.out 

#Try to add a tag to ones that 'circulates'/stabilises around 27 and have them in a diff colour
ggplot2::ggplot (new.out, aes(value.1, value,fill=variable))+
  geom_point(alpha=0.2)+
  geom_line(alpha=0.2)+
  coord_cartesian(ylim=c(10,35),xlim=c(0,40))+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "none")

#coord_cartesian(ylim=c(0,500))+
#geom_hline(yintercept=200,linetype=2,colour="lightblue",size=1.2)+



