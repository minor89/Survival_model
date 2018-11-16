##################################################################################################
#File name: General_tmii_model.R
#Build a general model, describing trait-mediated indirect interaction between mammals and insects
#Starting from a basic population model and  the tmii-models in Anderson et al 2009 
#Modifying the model with specific mammal-insect factors 
##################################################################################################

#Discrete population model
#Forest system (many herbivore generation per plant generation, do not need to model plant population, see Underwood 1999)

#Models are based on:
#Berverton-Holt model
#Anderson et al. 2009

BH_func<-function(r1,M1,r2,M2,H1,H2,generations){
  H1pop<-numeric(generations)
  H2pop<-numeric(generations)
  for(i in 1:generations){
    
    H1<-(r1*H1)/(1+(H1/M1))
    H1pop[i]<-H1
    
    H2<-(r2*H2)/(1+(H2/M2)) 
    H2pop[i]<-H2
  }
  
}
BH_func(10,10,10,10,0.1,0.1,100)

#Set initial variables for MODEL 1 (Beverton-Holt model)
M<-10 #
#Carrying capacity K = (r - 1)* M [if M = 10, r = 10. K = (10-1)*10 = 90]
r<-10 #Population growth rate
generations<-500 #Number of generations to run the model for 
H<-0.1 #Population density at time t=0 of the insect herbivore
H1<-0.1 #Population density at time t=0 of the mammal 
M1<-10 #M for mammal model
r1<-1 #r for mammal model

#MODEL 1:
#####################################
#Model w/o induced responses and no mortality/regulating factors.  
H.population2<-numeric(generations) #Population size matrix for insect
H1.pop<-numeric(generations) #Population size matrix for mammal
for(i in 1:generations){
  H<-(r*H)/(1+(H/M)) 
  H.population2[i]<-H
  H1<-(r1*H1)/(1+(H1/M1))
  H1.pop[i]<-H1
} 
plot(H.population2, col="salmon",pch=16,cex=1.3)
lines(H.population2, col="salmon",lwd=2)
points(H1.pop,col="lightblue",pch=16,cex=1.3)
lines(H1.pop, col="lightblue",lwd=2)
####################################
#MODEL 1 - population do not depend on eachother and increase to their carrying capacities. 

#Set initial varialbes for MODEL 2 (Modified Beverton-Holt)
M<-10 
r<-10 
generations<-500 
H<-0.1 
H1<-0.1
I1<-0.4 #Level of mammal induced response 
I2<-0 #Level of insect induced response  
d1<-0.75 #Decay rate of induced reponse I1 (0= no decay, 1=100% decay)
d2<-0.75 #Decay rate of induced reponse I2 (0= no decay, 1=100% decay)
y1<-0.5 #Effect of induced response on herbivore growth rate per unit of I1 (effect can be -,0,+ & weak/strong)
y2<-0.5 #Effect of induced resposne on herbivore growth rate per unit of I2 (effect can be -,0,+ & weak/strong)
M1<-10
r1<-1

alfa<-10 #Maximum per capita induced response elicitation rate
beta<-1 #Per unit reduction in elicitation rate due to plant self-limitation
ro<-numeric(1) #Elicitation by herbivory and self-limitation for I2
ro1<-numeric(1) #Elicitation by herbivory and self-limitation for I1
Q<-0.5 #Proportion of plants damaged by insects in previous generation
#I in the next generation will be I(t+1) = I(t) + ro - d*I(t) (previous level + elicitation/selflimitation - decay)

#MODEL 2:
####################################
#Model with induced reponses but each herbivore is only affected by the response they induce themselves
H.population1<-numeric(generations) #Population size matrix
H.initial<-numeric(generations)
H1.pop2<-numeric(generations)
for(i in 1:generations){
  H.initial[i]<-H
  if(I2<(alfa/beta)){
    ro<-(alfa-beta*I2)*H
  }else{
    ro<-0
  }
  
  I2<-I2+ro-d2*I2
  
  if(I1<(alfa/beta)){
    ro1<-(alfa-beta*I1)*H1
  }else{
    ro1<-0
  }
  
  I1<-I1+ro1-d1*I1
  
  #H<-(r*H)/(1+((H+y2*I2)/M)) 
  #H<-Q*((r*H)/(1+((H+y2*I2)/M))) + (1-Q)*((r*H)/(1+(H)/M) - kH) (where k is some sort of survival/mortality rate?, should this be added everywhere?)
  H<-Q*((r*H)/(1+((H+y2*I2)/M))) + (1-Q)*((r*H)/(1+(H)/M))
  H.population1[i]<-H
  H1<-(r1*H1)/(1+((H1+y1*I1)/M1))
  H1.pop2[i]<-H1
} 
points(H.population1, col="darkgreen",pch=16,cex=1.3)
lines(H.population1, col="darkgreen",lwd=2)
points(H1.pop2, col="purple",pch=16,cex=1.3)
lines(H1.pop2, col="purple",lwd=2)

plot(H.initial,H.population1)
lines(H.initial,H.population1)
####################################
#MODEL 2 - population regulation/dynamcis only depend on their own induced response 
#Insect herbivore depend on insect induced changes and mammal herbivore on mammal induced changes 



#Set initial variables for MODEL 3 

M<-10 
r<-10
M1<-10
r1<-1
generations<-500 
H<-0.1 
H1<-0.1
I1<-0.4#Induced response level by mammalian (effect on insect can be -,0,+ & weak/strong)
I2<-0 #Induced response level by insect herbivore (effect on insect can be -,0,+ & weak/strong)
d1<-0.75
d2<-0.75
y2<-(0.5) #Effect of induced resposne I2 (insect induced) (effect can be -,0,+ & weak/strong)
y3<-0 #Effect of induced resposne I1 (mammal induced) on mammals (effect can be -,0,+ & weak/strong)
#y1 - the effect of mammal induced response on insect depends on the level of I1. (see for-loop)
#The effect of mammals on insects is highest when I1 is at intermediate levels (hump-shaped relationship)

alfa<-10
beta<-1
ro<-numeric(1)
ro1<-numeric(1)

DC<-0.05 #Direct consumption effect (% of the insect population accidently eaten by the mammals)
#Would be good if this could depend on the proportion of insect-mammals, = the more insects per mammal the higher DC
P<-0.3 #Proportion of plants damaged by mammal 
Q<-0.3 #Proportion of plant damaged by insects
PQ<-0.3 #Proportion of plants damaged by both insects and mammals


#MODEL 3:
####################################
#Model with induced responses. Induced reponses of mammal affects insect population
#Specific 'direct consumption' term added (insect accidently eaten by mammals)
#Different proportions of the plant population have had previous herbivory by insects or mammals or both or neither
#Insect population is affected by mammalian induced responses but the mammal is not affected by the insect
#The mammal population is affected by its own induced responses


H.population<-numeric(generations) #Population size matrix for insect
H1.pop3<-numeric(generations)#Population size matrix for mammal
I1.mat<-numeric(generations)#Matrix to look at I1
I2.mat<-numeric(generations)#Matrix to look at I2
H.init<-numeric(generations)

for(i in 1:generations){
  
  #Generate value for y1. y1 depends on the level of mammal induced response (I1) 
  H.init[i]<-H
  if(I1<5|I1>15){
    y1<-0.2 #Per unit effect of induced resposne I1 (mammal induced) on insects (effect can be -,0,+ & weak/strong)
  }else{
    y1<-0.4
  }
  
  if(I2<(alfa/beta)){
    ro<-(alfa-beta*I2)*H
  }else{
    ro<-0
  }
  
  I2<-I2+ro-d2*I2
  I2.mat[i]<-I2
  
  if(I1<(alfa/beta)){
    ro1<-(alfa-beta*I1)*H1
  }else{
    ro1<-0
  }
  
  I1<-I1+ro1-d1*I1
  I1.mat[i]<-I1
  
  #Q - prop of trees previously affected by H (the insect)
  #P - proportion of trees affected by the mammal (H1)
  #PQ - proportion of trees affected by both
  #1-Q-P-PQ - prop of trees non-induced
  H<-P*((r*H)/(1+((H+y1*I1)/M))-DC*H)+Q*((r*H)/(1+((H+y2*I2)/M)))+PQ*((r*H)/(1+((H+y1*I1+y2*I2)/M))-DC*H)+(1-P-Q-PQ)*((r*H)/(1+((H)/M)))
    
  
  H1<-(r1*H1)/(1+((H1+y3*I1)/M1))

  H.population[i]<-H
  H1.pop3[i]<-H1
 
} 
points(H.population, col="orange",pch=16,cex=1.3)
lines(H.population, col="orange",lwd=2)
points(H1.pop3, col="yellow",pch=16,cex=1.3)
lines(H1.pop3, col="yellow",lwd=2)


####################################


#############################################
#QUESTIONS, PROBLEMS, LIMITATIONS, IDEAS ETC.
#############################################

#Need to run several insect generations per mammal generation (?!)
#Can we link mammal population density to proportion of trees that are induced/non-induced by mammals
#Should mammalian dynamics be affected by induced responses? 
#Use proportions in the mammal-dynamics part as well 
#Does the proportion thing make sense? 
#Should mammals be affected by insect induced responses?
#Selectivity of herbivores - herbivores might try to avoid induced or non-induced plants 
#Do we need to model the mammals - can we just model the mammal induced response and can we link that to proportion of trees?




