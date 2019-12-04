#Egg ingestion - the rate of egg ingestion depends on the insect population size
#Type III functional response curve


#trial

a<-0.2
b<-50
x<-seq(1,400,2)

f=(a*x^2)/(b^2+x^2)
plot(x,f)

#Values for a and b are currently arbitrary 

#f is the direct consumption/egg ingestion rate
#x is the insect population size
#a is the maximum value that f can take 

alfo<-0.2
beto<-2000
e.ingest=(alfo*x^2)/(beto^2+x^2)


#I guess that b needs to be low - i.e. if beta is 50 the DC starts increasing at that density (50 cocoons per m2).
#When you set dc to increase at beta = 2000 that's already above outbreak levels -> the model will behave like the control model