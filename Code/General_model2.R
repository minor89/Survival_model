##################################################################################################
#File name: 
#Build a general model, describing trait-mediated indirect interaction between mammals and insects
#Starting from a basic population model and  the tmii-models in Anderson et al 2009 
#Modifying the model with specific mammal-insect factors 
##################################################################################################

#Discrete population model
#Forest system (many herbivore generation per plant generation, do not need to model plant population, see Underwood 1999)

library(ggplot2)

#Make DC depend on H1 and H2
#DCt = 1-(45*(H1/(625+(H1^2)))) #We can probably use a function similar to this one 
#Need to make a positive density-dependent function (the more insect and the more mammals the higher the DC 
#DC e[0,1]

generations<-50
tmii.func<-function(H1=0.1,#H1 - population density mammal
                    H2=0.1,#H2 - population density insect
                    r1=1, #growth rate H1
                    r2=1, #growth rate H2
                    K1=2, #Carrying capacity for H1
                    K2=200, #Carrying capacity for H2
                    P=0.2,
                    Q=0.2,
                    I1=0,#I1 - level of mammal induced resposne
                    I2=0,#I2 - level of insect induced response
                    d1=1,#d1 - decay rate I1
                    d2=1,#d2 - decay rate I2
                    y1=0.1, #y1 - effect of mammal induced response on insect
                    y2=0.1, #y2 - effect of insect induced response on insect 
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
  L1<-numeric(generations)
  L2<-numeric(generations)
  h2p<-numeric(generations)
  
  for(i in 1:generations){
    
    h1<-H1 #rember value of H1 from last generation (to be able to calc lambda later)
    h2<-H2 #rember value of H2 from last generation
    h2p[i]<-h2
    
    
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
    #Which plants the herbivores are on: 
    if(H2<0){
      H2<-0
    }else{
      H2P<-H2*P
      H2Q<-H2*Q
      H21<-H2*(1-(P+Q))
      
      #Growth of herbivores on P plants (direct and indirect effects)
      H2P<-H2P-DC*H2P #first the eaten eggs disappear
      H2P<-H2P+(r2*H2P)*(1-((H2P+(y1*I1))/K2)) #The individuals that are left are affected by induced responses 
      
      #Growth of herbivores on Q plants 
      H2Q<-H2Q+(r2*H2Q)*(1-((H2Q+(y2)*I2)/K2))
      
      #Growth of herbivores on the rest
      H21<-H21+(r2*H21)*(1-(H21/K2))
      
      H2<-H2P+H2Q+H21
    }
    

    
    #Population size of mammal (H1)
    H1<-H1+(r1*H1)*(1-((H1)/K1))
    
    
    lambda1<-H1/h1 #calculate lambda
    lambda2<-H2/h2
    
    L1[i]<-lambda1
    L2[i]<-lambda2
    
    H1pop[i]<-H1
    H2pop[i]<-H2
    
    
  } 
  # added a list of the three test parameters to show values...
  populations<-list("params" = c("y1"=y1,"P"=P,"Q"=Q, "DC"=DC), "H1pop"=H1pop,"H2pop"=H2pop,"I1level"=I1level,"I2level"=I2level,"L1"=L1,"L2"=L2,"h2p"=h2p)
  return(populations)
}




## method 2: using apply to use all combinations of three parameters

# first, set up some ranges for a few parameters
y1 <- seq (from=-1, to=1, by=1)
P <- seq(from=1, to=1, by=0.1)
Q <- seq(from=0,to=0,by=0.1)
DC <- seq(from=0, to=0,by=0.05)


# make a data.frame with every combination of those parameters
param.args <- expand.grid(y1 = y1, P=P,Q=Q, DC=DC)

# using apply, iterate across every row and pass the row values as the arguments to the tmii.func
output.list <- apply(param.args,1,function(params)tmii.func(y1=params[1],P=params[2],Q=params[3],DC=params[4]))

output.df <-data.frame((matrix(ncol = length(output.list), nrow = generations)))

output.df <- data.frame("Step"=seq(from=1, to=generations, by=1))
for (i in 1:length(output.list)){
  output.df[i+1] <- output.list[[i]]$H2pop
  colnames(output.df)[i+1] <- paste0("run_",i)
}

plot.df <- reshape2::melt (output.df, id="Step")

ggplot2::ggplot (plot.df, aes(Step, value, fill=variable))+
  geom_line(alpha=0.2)+
  theme_minimal()


#What type of plots do we want to make to explore this model? 

library(plotly)

p <- plot_ly(param.args, x = ~y1, y = ~d2, z = ~DC) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'y1'),
                      yaxis = list(title = 'd2'),
                      zaxis = list(title = 'DC')))

p


##Same type of plot for L2 (lambda for H2) or I2 level (induced resposne of insect level)
output.dfL <-data.frame((matrix(ncol = length(output.list), nrow = generations)))

output.dfL <- data.frame("Step"=seq(from=1, to=generations, by=1))
for (i in 1:length(output.list)){
  output.dfL[i+1] <- output.list[[i]]$I1level
  colnames(output.dfL)[i+1] <- paste0("run_",i)
}

plot.dfL <- reshape2::melt (output.dfL, id="Step")

ggplot2::ggplot (plot.dfL, aes(Step, value, fill=variable))+
  geom_line(alpha=0.2)+
  theme_minimal()

#If I want to plot recruitment curves I need to store the t-1 population sizes as well (i.e. h1, h2)

plot(output.list[[6]]$h2p,output.list[[6]]$H2pop)
lines(output.list[[6]]$h2p,output.list[[6]]$H2pop)

plot(1:50,output.list[[5]]$H2pop)
lines(1:50,output.list[[5]]$H2pop)
#Make the plot into a function
#What we want to add in to the function is which 'output' variable we want to plot (H1, H2, I1 etc.)


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






