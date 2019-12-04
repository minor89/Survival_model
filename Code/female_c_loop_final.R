#make a script with the female choice loop that can be called within the function in the other script and easily removed... 
#now that loop takes forever so no use in running it all the time 


#Choice

##Loop 1: NFC

NFC<-1000*Cfec #Fake number of females with FecundityC (based on the proportion)
NFB<-1000*Bfec #Fake number of females with FecundityB (based on the proportion)

SC<-0 #Counting nb of females egg laying on control
SB<-0 #Counting nb of females egg laying on browsed
i<-1
while(i<=NFC){
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


##Loop 2: NFB

SSC<-0 #Counting nb of females egg laying on control
SSB<-0 #Counting nb of females egg laying on browsed
i<-1
while(i<=NFB){
  y<-sample(c(0,1),1,prob=c(Ctrees,Btrees))
  if(y==0){
    oviC<-sample(c(1,0),1,prob=c(biasC,biasB))
    if(oviC==1){
      SSC<-SSC+1
      i=i+1
    }else{
      next
    }
  }
  if(y==1){
    oviB<-sample(c(1,0),1,prob=c(biasB,biasC))
    if(oviB==1){
      SSB<-SSB+1
      i=i+1
    }else{
      next
    }
  }
}

#proportions of females that have different fecundities and will oviposit on different types of trees:
P1<-SC/1000 #Proportion of females with fecundityC laying on Ctrees
P2<-SB/1000 #Proportion of females with fecundityC laying on Btrees
P3<-SSC/1000 #Proportion of females with fecundityB laying on Ctrees
P4<-SSB/1000 #Proportion of females with fecundityB laying on Btrees


#Number of females on the differnt trees with the different fecundities:
NCC.fem<-P1*NF #number of females egg laying on control trees
NCB.fem<-P2*NF #number of females egg laying on briowsed trees
NBC.fem<-P3*NF #number of females egg laying on control trees
NBB.fem<-P4*NF #number of females egg laying on briowsed trees

NC.eggs<-NCC.fem*FecundityC + NBC.fem*FecundityB #all eggs on C trees
NB.eggs<-NCB.fem*FecundityC + NBB.fem*FecundityB #all eggs on B trees

NC<-NC.eggs
NB<-NB.eggs - DC*NB.eggs #Direct consumptive effects 


