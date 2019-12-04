#sawfly female choice
#sampling sawflies based on likelihood to encounter browsed or control trees and prefernece to oviposit on them

Nadults<-100
Ctrees<-0.5
Btrees<-0.5
biasB<-0.5
biasC<-0.5
n<-1000

C.tree.store<-numeric(Nadults)
B.tree.store<-numeric(Nadults)

for(i in 0:Nadults){
  y<-sample(c(0,1),1,prob=c(Ctrees,Btrees))
  
  for(k in 1:n){ #Now I just need to figure out how to get the right n 
    
    if(y==0){
      oviC<-sample(c(1,0),1,prob=c(biasC,biasB))
      if(oviC==1){
        #store
        C.tree.store[i]<-1
        break
      }else{
        y<-sample(c(0,1),1,prob=c(Ctrees,Btrees)) 
      }
    }
    
    if(y==1){
      oviB<-sample(c(1,0),1,prob=c(biasB,biasC))
      if(oviB==1){
        #store
        B.tree.store[i]<-1
        break
        #else - re-run
      }else{
        y<-sample(c(0,1),1,prob=c(Ctrees,Btrees)) 
      }
    }  
 }
  
  
}

C.tree.store
B.tree.store
sum(C.tree.store)
sum(B.tree.store)
sum(C.tree.store)+sum(B.tree.store)
a1c<-sum(C.tree.store)
a2b<-sum(B.tree.store)
tot<-sum(C.tree.store)+sum(B.tree.store)
eggs.Btrees<-a2b/tot #proportion of eggs on browsed trees
eggs.Ctrees<-a1c/tot #proportion of eggs on control trees
#But what about the fact that some females have higher fecundity than other - they need to be modelled separatly
#Loop one: females with fecB - % that ovipos on browsed vs control
#loop two: females with fecC - % that ovipos on browsed vs control

#SUM the 1's in each storing matrix  - then we know how many females oviposits on each type of tree (can calc. propotions if needed)



#TRY 2 
#With 'counting down' approach
Nadults<-100
Ctrees<-0.5
Btrees<-0.5
biasB<-0.5
biasC<-0.5
SC<-0 #Counting egg laying on control
SB<-0 #Counting egg laying on browsed

for(i in 1:Nadults){
  y<-sample(c(0,1),1,prob=c(Ctrees,Btrees))
    #y<-sample(c(0,1),1,prob=c(Ctrees,Btrees))
    if(y==0){
      oviC<-sample(c(1,0),1,prob=c(biasC,biasB))
      if(oviC==1){
        SC<-SC+1
      }else{
        next
      }
    }
    if(y==1){
      oviB<-sample(c(1,0),1,prob=c(biasB,biasC))
      if(oviB==1){
        SB<-SB+1
      }else{


SC
SB
SC+SB
        next
      }
    }    
    
}



Nadults<-100
Ctrees<-0.2
Btrees<-0.8
biasB<-0.8
biasC<-0.2
SC<-0 #Counting egg laying on control
SB<-0 #Counting egg laying on browsed
i<-1
while(i<=Nadults){
  y<-sample(c(0,1),1,prob=c(Ctrees,Btrees))
  if(y==0){
    oviC<-sample(c(1,0),1,prob=c(biasC,biasB))
    if(oviC==1){
      SC<-SC+1
      i=i+1
    }else{
      next
    }
  }
  if(y==1){
    oviB<-sample(c(1,0),1,prob=c(biasB,biasC))
    if(oviB==1){
      SB<-SB+1
      i=i+1
    }else{
      next
    }
  }
}
SC
SB
SC+SB
