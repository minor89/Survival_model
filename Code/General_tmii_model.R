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

##################################################################################################

#Beverton-Holt model:
BH_func<-function(r1=10,M1=10,r2=10,M2=10,H1=0.1,H2=0.1,generations=10){
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
BH_func()


################################################################################################


#TMII model:
#Model with induced responses. Induced reponses of mammal affects insect population
#Specific 'direct consumption' term added (insect accidently eaten by mammals)
#Different proportions of the plant population have had previous herbivory by insects or mammals or both or neither
#Insect population is affected by mammalian induced responses but the mammal is not affected by the insect
#The mammal population is affected by its own induced responses


tmii.func<-function(H1=0.1,#H1 - population density mammal
                    H2=0.1,#H2 - population density insect
                    r1=1, #growth rate H1
                    r2=1, #growth rate H2
                    M1=10, #variable connected to carrying capacity for H1
                    M2=10, #variable connected to carrying capacity for H2
                    I1=0,#I1 - level of mammal induced resposne
                    I2=0,#I2 - level of insect induced response
                    d1=1,#d1 - decay rate I1
                    d2=1,#d2 - decay rate I2
                    y2=0.1, #y2 - effect of insect induced response on insect 
                    y3=0.1, #y3 - effect of mammal induced resposne on mammal 
                    P=0.1,#P - Proportion of plants damaged by mammal 
                    Q=0.1,#Q - Proportion of plant damaged by insects
                    PQ=0.1,#PQ - Proportion of plants damaged by both insects and mammals
                    DC=0.05, #DC - Direct consumption effect (% of the insect population accidently eaten by the mammals)
                    alfa=10, #maximum per capita induced response elicitation rate
                    beta=1, #per unit reduction in induced resposne elicitation due to plant self-inhibition
                    generations=50){
  ro1<-0 #ro1 - change in level of induced response I1 per time step, depends on I1 and H1
  ro2<-0 #ro2 - change in level of induced response I2 per time step, depends on I2 and H2
  H1pop<-numeric(generations)
  H2pop<-numeric(generations)
  I1level<-numeric(generations)
  I2level<-numeric(generations)
  
  for(i in 1:generations){
  
    #Generate value for y1.
    #y1 - effect of mammal induced response on insect - depends on the level of I1 
    if(I1<5|I1>15){
      y1<-0.2 #Per unit effect of induced resposne I1 (mammal induced) on insects (effect can be -,0,+ & weak/strong)
    }else{
      y1<-0.4
    }
    
    #Generate value for change in I2 (ro2)
    if(I2<(alfa/beta)){
      ro2<-(alfa-beta*I2)*H2
    }else{
      ro2<-0
    }
    
    #New level of I2
    I2<-I2+ro2-d2*I2
    I2level[i]<-I2
    
    #Generate value for change in I1 (ro1)
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
  # added a list of the three test parameters to show values...
  populations<-list("params" = c("d2"=d2,"r1"=r1, "y2"=y2), "H1pop"=H1pop,"H2pop"=H2pop,"I1level"=I1level,"I2level"=I2level)
  return(populations)
}


## Method 1: For looping through a single parameter at a time. Useful for smaller changes.
output.list<-list()
for(ii in seq(from=0.1,to=1,by=0.1)){
  m<- tmii.func(r1=ii)
  output.list[[paste0(ii)]] <- m
}
output.list

## method 2: using apply to use all combinations of three parameters

# first, set up some ranges for a few parameters
y2 <- seq (from=0.05, to=0.15, by=0.01)
d2 <- seq(from=0.8, to=1, by=0.1)
r1 <- seq(from=0.8, to=1.1, by=0.1)

# make a data.frame with every combination of those parameters
param.args <- expand.grid(y2 = y2, d2=d2, r1=r1)

# using apply, iterate across every row and pass the row values as the arguments to the tmii.func
output.list <- apply(param.args,1,function(params)tmii.func(y2=params[1],d2=params[2], r1=params[3]))

output.df <-data.frame((matrix(ncol = length(output.list), nrow = 50)))

output.df <- data.frame("Step"=seq(from=1, to=50, by=1))
for (i in 1:length(output.list)){
  output.df[i+1] <- output.list[[i]]$H2pop
  colnames(output.df)[i+1] <- paste0("run_",i)
}

plot.df <- reshape2::melt (output.df, id="Step")

ggplot2::ggplot (plot.df, aes(Step, value, fill=variable))+
  geom_line(alpha=0.2)+
  theme_minimal()



#NEXT STEPS:
#Make plotting function
#Add selectivity/choice of herbivores 
#Work out a way to make DC depend on insect:mammal proportion
#Proportion of induced plants (P, Q, PQ) - could they depend on population sizes (H1,H2)?
#Repeated mammalian herbivory 
#y1 - the effect of mammal induced responses on insect - non-linear 

#Model assumptions:
#Forest/tree system - many herbivore generations per plant generation
#Discrete reproduction 






