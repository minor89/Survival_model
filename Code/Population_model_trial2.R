###########################################################
#File name: Population_model_trial2.R                     #
#Population model for Neodiprion sertifer                 #
#Exploring how trait-mediated effects affects the dynamics#
###########################################################


#Set initial variables:
######################
sexratio<-0.5 #sexratio of the population 
#There are several studies on sexratios of N. sertifer so this value could be varied

Sback<-0.4 #Background survival (egg, adult survival)
#There are several studies on mortality rates in different life stages as well as mortality rates 
#caused by differnt natural enemies (virus, ants, parasitoids etc.)
#This could be used in the model

years<-40 #number of years/generations to run the model for


#Proportion of browsed and un-browsed trees
Btrees<-0.5 #Proportion of induced (browsed) trees 
Ctrees<-0.5 #Proportion of non-induced (control) trees 

#Proportion of the sawfly population that fed on browsed/un-browsed trees last year
#Fecundity of females at time t will depend on what they fed on as larvae at time t-1. 
#Start with Cfec<-1, i.e. all individuals fed on un-browed (control) trees at time t-1. 
#The Bfec and Cfec in the next time step (t+1) will depend directly on the Btrees and Ctrees in the previous year (t) 
Bfec<-0 #Proportion of individuals that fed on induced(browsed) trees in the previous year - i.e. have browsed-fecundity value
Cfec<-1 #Proportion of individuals that fed on non-induced(control) trees in the previous year - i.e. have control-fecundity value

 

#Set fecundity values for browsed and control trees
#Fecundity = number of eggs per female 
#Could be drawn from a distribution from data ?
FecB<-85 #Fecundity of individuals that have fed on browsed trees (mean) [mean fecundity on clipped trees is 91]
FecC<-79 #Fecundity of individuals that have fed on control trees (mean)


#Set larval survival values for browsed and un-browsed trees
#Could be drawn from a distribution from data ?
SlarvaeB<-0.42 #Survival on browsed trees (mean)
SlarvaeC<-0.50 #Survival on control trees (mean)

#Set population start value
N<-2 #Number of pupae per m2 at time t=0

#Make population value and lamda value matrices
population<-numeric(years)#Nt+1 matrix
pop<-numeric(years)#Nt matrix
lambda<-numeric(years) #Lambda matrix (Lambda=Nt+1/Nt)


#Model with TMII
for(i in 1:years){
  pop[i]<-N #number of pupae at time t
  s.pupae<-1-(45*(N/(625+(N^2)))) #density dependent survival of pupae (based on Larsson et al 2000, Hanski 1987)
  N<-N*s.pupae #Basically number of adults at time t
  #N<-N*s.adult #adult survival (from Olofsson or other study?)
  #If we could incorporate female choice we should do it here
  
  #Proportion of population on different trees and with different fecundity and survival 
  #Add effect of direct consumption of the browsed trees 
  NBB<-N*Bfec*Btrees*Sback*sexratio*SlarvaeB*FecB #on browsed trees with browsed fecundity, [add direct consumption]
  NCB<-N*Bfec*Ctrees*Sback*sexratio*SlarvaeC*FecB #on control trees with browsed fecundity
  NBC<-N*Cfec*Btrees*Sback*sexratio*SlarvaeB*FecC #on browsed trees with control fecundity, [add direct consumption]
  NCC<-N*Cfec*Ctrees*Sback*sexratio*SlarvaeC*FecC #on control trees with control fecundity 
  
  N<-NBB+NCB+NBC+NCC #sum to obtain total population size
  Bfec<-(NBB+NBC)/N #the prop of individuals that have been reared on browsed trees 
  Cfec<-(NCB+NCC)/N #the prop of individuals that have been reared on control trees 
  #Prop of induced trees (random), this can be varied to only be within a certain range or have a set value
  Btrees<-runif(1,0,1) #Prop of trees getting browsed during winter, could be random or fixed 
  Ctrees<-(1-Btrees)
  population[i]<-N
  lambda[i]<-(population[i]/pop[i])
  
}
pop #Check pop size
population #Check pop siz
plot(population)
lines(population)
plot(lambda)

#Model w/o TMII
#Set population size (NC)
NC<-2
#Create matrices for population sizes and lambda 
populationC<-numeric(years)
popC<-numeric(years)
lambdaC<-numeric(years)
for(i in 1:years){
  popC[i]<-NC #number of pupae at time t
  s.pupae<-1-(45*(NC/(625+(NC^2)))) #density dependent survival of pupae 
  NC<-NC*s.pupae #Basically number of adults at time t
  NC<-NC*Sback*sexratio*SlarvaeC*FecC
  populationC[i]<-NC
  lambdaC[i]<-(populationC[i]/popC[i])
}
popC
populationC
plot(populationC)
lines(populationC)

#Plot both models 
plot(populationC, ylab="Population size", xlab="Years") #Control model
lines(populationC)
points(population,xlab="years",ylab="Population size",col="purple") #TMII model
lines(population,col="purple")
##############################################

#QUESTIONS, LIMITATIONS, PROBLEMS, IDEAS ETC.:
##############################################

#Incorporate some direct consumption effect (% sawfly eggs accidently eaten by mammals during winter)
#Incorpotate some selectivity - female sawflies might avoid or prefer to oviposit on browsed trees
#Repeated herbivory - moose often re-browse trees - might change the level of imduced reponse
#Intensity of browsing - relationship between browsing induced reponse and the effect on sawflies might not be linear


