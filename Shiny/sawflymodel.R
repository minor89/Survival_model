sawfly.model<-function(Btrees=0.5,
                       FecundityB=85,
                       SlarvaeC=0.6,
                       years=10,
                       DC=0.05){
  N<-2 #Initial population density (cocoons per m2)
  #SlarvaeB<-0.42 #Survival of larvae on browsed trees
  Ctrees<-(1-Btrees)
  sexratio<-0.50 #Survival of larvae on control trees
  Bfec<-0 #Proportion of sawflies that fed on browsed trees as larvae in previous generation - have fecundity from browsed trees
  Cfec<-1
  FecundityC<-79 #Fecundity of females reared on control trees 
  Sbackground<-0.5 #Adult survival 
  Seggs<-0.9 #Egg survival
  SlarvaeB=(SlarvaeC-0.1)
  #Sawfly survival B - SurvivalC*0.9 or SurvivalC - 0.1 or something else? 
  #Same question goes for fecundity 
  
  #Make population value and lamda value matrices
  population<-numeric(years)#Nt+1 matrix
  pop<-numeric(years)#Nt matrix
  lambda<-numeric(years) #Lambda matrix (Lambda=Nt+1/Nt)
  
  
  for(i in 1:years){
    pop[i]<-N #number of pupae at time t
    s.pupae<-1-(45*(N/(625+(N^2)))) #density dependent survival of pupae (based on Larsson et al 2000, Hanski 1987)
    N<-N*s.pupae #Basically number of adults at time t
    #N<-N*s.adult #adult survival (from Olofsson or other study?)
    #If we could incorporate female choice we should do it here
    
    N<-N*Sbackground #number of adults after effects of background survival (mortality of adults)
    
    
    NB.eggs<-N*Bfec*FecundityB*sexratio #Number of eggs laid by females with FecundityB
    NC.eggs<-N*Cfec*FecundityC*sexratio #Number of eggs laid by females with FecundityC
    
    N.eggs<-NB.eggs+NC.eggs #number of eggs
    NB<-N.eggs*Btrees #Number of eggs on browsed trees
    NC<-N.eggs*Ctrees #Number of eggs on control trees
    
    NB<-NB - DC*NB #Number of eggs on browsed trees after direct consumptive effects
    NC<-NC #No DC effects on control trees
    
    NB.larvae<-NB*Seggs #other survival effects on eggs (independent of browsing) --> nb of larvae
    NC.larvae<-NC*Seggs #other survival effects on eggs (independent of browsing) --> nb of larvae
    
    NB<-NB.larvae*SlarvaeB #survivl of larvae on browsed trees -> nb of cocoons produced from browsed trees
    NC<-NC.larvae*SlarvaeC #survival of larvae on control trees -> nb of cocoons produced from control trees
    
    
    N<-NB+NC #Number of pupae at t+1 
    
    
    #NBB<-N*Bfec*Btrees*Sbackground*sexratio*SlarvaeB*FecundityB-N*DC #on browsed trees with browsed fecundity, [add direct consumption]
    #NCB<-N*Bfec*Ctrees*Sbackground*sexratio*SlarvaeC*FecundityB #on control trees with browsed fecundity
    #NBC<-N*Cfec*Btrees*Sbackground*sexratio*SlarvaeB*FecundityC-N*DC #on browsed trees with control fecundity, [add direct consumption]
    #NCC<-N*Cfec*Ctrees*Sbackground*sexratio*SlarvaeC*FecundityC #on control trees with control fecundity 
    
    #N<-NBB+NCB+NBC+NCC #sum to obtain total population size
    #These needs to be fixed now (think that it's ok now):
    Bfec<-(NB)/N #the prop of individuals that have been reared on browsed trees 
    Cfec<-(NC)/N #the prop of individuals that have been reared on control trees 
    #Prop of induced trees (random), this can be varied to only be within a certain range or have a set value
    #Btrees<-runif(1,0,1) #Prop of trees getting browsed during winter, could be random or fixed 
    #Ctrees<-(1-Btrees)
    population[i]<-N
    lambda[i]<-(population[i]/pop[i])
  }
  output<-list("params" = c("Btrees"=Btrees,"DC"=DC,"SlarvaeC"=SlarvaeC),"population"=population,"population(t-1)"=pop,"lambda"=lambda)
  return(output)
}
