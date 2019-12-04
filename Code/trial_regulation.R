#Specialist natural enemy

#if(N>X )


if(population[j]>X & population[j-1]>X& population[j-2]>X& population[j-3]>X& population[j-4]>X){
  N=0.1*N
}


years=50
control.model<-function(years=50,Slarvae=0.6,Fecundity=79){
  NC=2
  Sbackground=0.5
  sexratio=0.5
  Seggs=0.9
  #populationC<-numeric(years)
  populationC<-matrix(nrow=years,ncol=2)
  popC<-numeric(years)
  lambdaC<-numeric(years)
  X=1000
  for(i in 1:years){
    popC[i]<-NC #number of pupae at time t
    s.pupae<-1-(45*(NC/(625+(NC^2)))) #density dependent survival of pupae 
    NC<-NC*s.pupae #Basically number of adults at time t
    NC<-NC*sexratio*Fecundity #Number of eggs laid
    NC<-NC*Seggs
    NC<-NC*Sbackground #Number of larvae
    NC<-NC*Slarvae #Number of pupae
    populationC[i,1]<-NC
    populationC[i,2]<-i
    lambdaC[i]<-(populationC[i]/popC[i])
    #if(populationC[i]>X & populationC[i-1]>X& populationC[i-2]>X& populationC[i-3]>X& populationC[i-4]>X){
      #NC=NC*0.1
    #}
    lastyears<-subset(populationC,populationC[,2]<=i & populationC[,2]>=(i-4))
    over<-which(lastyears>1000)
    if(length(over)>=5){
      NC=NC*0.001
    }
  }
  outputC<-list("params" = c("Slarvae"=Slarvae,"Fecundity"=Fecundity),"populationC"=populationC,"populationC(t-1)"=popC,"lambdaC"=lambdaC)
  return(outputC)
  plot(populationC[,2],populationC[,1])
  lines(populationC[,2],populationC[,1])
  }

control.model()

#OK! now the population regulation works:
#now- need to figure out a way to change the rest of the code so that it matches the function because:
#populationC is now a matrix and not a vector 

Slarvae<-seq(from=0.45,to=0.70,by=0.05)
Fecundity<-seq(from=65,to=95,by=1)
#limit is at 0.545


# make a data.frame with every combination of those parameters
param.argsC <- expand.grid(Slarvae=Slarvae,Fecundity=Fecundity)


# using apply, iterate across every row and pass the row values as the arguments to the tmii.func
output.listC <- apply(param.argsC,1,function(params)control.model(Slarvae=params[1],Fecundity=params[2]))

param.argsC$run <- paste0("run_", seq_along(param.argsC[,1])) 
#output.df <-data.frame((matrix(ncol = length(output.list), nrow = years)))

output.dfC <- data.frame("Step"=seq(from=1, to=years, by=1))
for (i in 1:length(output.listC)){
  output.dfC[i+1] <- output.listC[[i]]$populationC[,1] #This is now changed to fit that it's now a matrix and not a vector
  colnames(output.dfC)[i+1] <- paste0("run_",i)
}

#Want to register every 'outbreak' i.e. every time pop is > the threshold (200)


#This adds 186 columns for thresholds, maybe I can directly add 0 or 1 with ifelse ?
output.dfC
for(i in 1:186){
    output.dfC[[paste0("threshold_",i)]]<-0
         
}



#Hopefully this is correct: 
for(j in 1:186){
  for(i in 1:50){
    if(output.dfC[i,1+j] >= 200){
      output.dfC[i,187+j]<-1
    }       
  }
}


threshold_values<-output.dfC[,188:373]
threshold_values
outb.sum<-colSums(threshold_values)
outb.sum<-data.frame(outb.sum)
outb.sum$fecundity<-seq(from=65,to=95,by=1)
outb.sum$survival<-seq(from=0.45,to=0.70,by=0.05)

outb.sum

plot(outb.sum$survival,outb.sum$outb.sum)
plot(outb.sum$fecundity,outb.sum$outb.sum)

library(plotly)

nb.plot <- plot_ly(outb.sum, x = ~survival, y = ~fecundity, z = ~outb.sum) %>%
  add_markers() %>%
  hide_colorbar() %>%
  layout(scene = list(xaxis = list(title = 'S'),
                      yaxis = list(title = 'F'),
                      zaxis = list(title = 'Nb')))

nb.plot


#Make a 3d plot with hight of bar accoring to nb of years with outbreak densities (fec and surv on the other axis)


outb<-which(output.dfC$run_70>=200) #gives me which are over 200
outb

plot(1:years,output.dfC$run_70,ylim=c(0,15000))
lines(1:years,output.dfC$run_70,ylim=c(0,15000))
lines(1:years,rep(200,50),col="salmon")


plot(1:years,output.dfC$run_10,ylim=c(0,15000))
lines(1:years,output.dfC$run_10,ylim=c(0,15000))
lines(1:years,rep(200,50),col="salmon")


plot(1:years,output.dfC$run_186,ylim=c(0,15000))
lines(1:years,output.dfC$run_186,ylim=c(0,15000))
lines(1:years,rep(200,50),col="salmon")


ifelse(output.dfC[i,1+i]>=200,1,0)

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

#add another column to param.argsC which 'counts' every outbreak

param.argsC$number<-0
if()


#add one that registers the length of the outbreks? (duration)


ys<-rep(1,length(param.argsC$Slarvae))

ggplot(param.argsC,aes(Slarvae,ys))+
  geom_point(alpha=0.6,colour=ifelse(param.argsC$threshold>0,"darkorange","lightblue"),size=4)+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggplot(param.argsC,aes(Fecundity,Slarvae))+
  geom_point(alpha=0.6,colour=ifelse(param.argsC$threshold>0,"darkorange","lightblue"),size=4)+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

