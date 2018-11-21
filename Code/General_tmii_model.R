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
  return(list(H1pop,H2pop))
}
BH_func(10,10,10,10,0.1,0.1,100)


############################################3


#MODEL 3 - TMII model
#Model with induced responses. Induced reponses of mammal affects insect population
#Specific 'direct consumption' term added (insect accidently eaten by mammals)
#Different proportions of the plant population have had previous herbivory by insects or mammals or both or neither
#Insect population is affected by mammalian induced responses but the mammal is not affected by the insect
#The mammal population is affected by its own induced responses

#H1 - population density mammal
#H2 - population density insect
#I1 - level of mammal induced resposne
#I2 - level of insect induced response
#d1 - decay rate I1
#d2 - decay rate I2
#y1 - effect of mammal induced response on insect - depends on the level of I1
#y2 - effect of insect induced response on insect (constant)
#y3 - effect of mammal induced resposne on mammal (constant)
#if y1, y2, y3 are negative - effect is positive, if they are postivie - effect is negative. 

#alfa - max rate of elicitation of induced response
#beta - self limitation of induced responses 


#DC - Direct consumption effect (% of the insect population accidently eaten by the mammals)
#Would be good if this could depend on the proportion of insect-mammals, = the more insects per mammal the higher DC
#P - Proportion of plants damaged by mammal 
#Q - Proportion of plant damaged by insects
#PQ - Proportion of plants damaged by both insects and mammals





tmii.func<-function(H1=0.1,H2=0.1,r1=1,r2=1,M1=10,M2=10,I1=0,I2=0,d1=1,d2=1,y2=0.1,y3=0.1,P=0.1,Q=0.1,PQ=0.1,DC=0.05,generations=50){
  ro1<-0
  ro2<-0
  H1pop<-numeric(generations)
  H2pop<-numeric(generations)
  I1level<-numeric(generations)
  I2level<-numeric(generations)
  alfa<-10
  beta<-1
  
  for(i in 1:generations){
  
    #Generate value for y1. y1 depends on the level of mammal induced response (I1) 
    if(I1<5|I1>15){
      y1<-0.2 #Per unit effect of induced resposne I1 (mammal induced) on insects (effect can be -,0,+ & weak/strong)
    }else{
      y1<-0.4
    }
    
    #Generate value for change in I2
    if(I2<(alfa/beta)){
      ro2<-(alfa-beta*I2)*H2
    }else{
      ro2<-0
    }
    
    #New level of I2
    I2<-I2+ro2-d2*I2
    I2level[i]<-I2
    
    #Generate value for change in I2
    if(I1<(alfa/beta)){
      ro1<-(alfa-beta*I1)*H1
    }else{
      ro1<-0
    }
    
    #New level of I1
    I1<-I1+ro1-d1*I1
    I1level[i]<-I1
    
    #Population size of insect (H2)
    H2<-P*((r2*H2)/(1+((H2+y1*I1)/M2))-DC*H2)+
    Q*((r2*H2)/(1+((H2+y2*I2)/M2)))+
    PQ*((r2*H2)/(1+((H2+y1*I1+y2*I2)/M2))-DC*H2)+
    (1-P-Q-PQ)*((r2*H2)/(1+((H2)/M2)))
    
    
    H1<-(r1*H1)/(1+((H1+y3*I1)/M1))
    
    H1pop[i]<-H1
    H2pop[i]<-H2
    
  } 
  populations<-list("H1pop"=H1pop,"H2pop"=H2pop,"I1level"=I1level,"I2level"=I2level)
  return(populations)
}

tmii.func(0.1,1,10,10,10,10,0,0,0.75,0.75,0.1,0.1,0.2,0.2,0.2,0.05,100)

output.list<-list()
for(i in seq(from=0.1,to=1,by=0.1)){
  temp<-tmii.func(r1=i)
  output.list<-list(output.list,temp)
}
output.list

#Which parameters do I want to vary? 
#d1 and d2 - if response has disappeard to the next year/generation?
#r
#M
#y1, y2, y3
#DC
#P, Q, PQ
#alfa and beta - removed


#NEXT STEPS:
#Make plot function
#Add selectivity/choice of herbivores 
#Work out a way to make DC depend on insect:mammal proportion
#Generate value for DC
#DC<-1-(H2/(H1+H2)) #direct consumptive effect. Not good! 
#Proportion of induced plants (P, Q, PQ) - could they depend on population sizes (H1,H2)?
#Repeated mammalian herbivory 
#y1 - the effect of mammal induced responses on insect - non-linear

#Model assumptions:
#Forest/tree system - many herbivore generations per plant generation
#Discrete reproduction 






