rf=0.06
S0=12089
K=12200
T=50
#from last class

nT=10
nS=10

delT=T/nT
sig=0.15


assprice=matrix(0,nrow=nS, ncol=nT)
intrval=matrix(0,nrow=nS, ncol=nT)
updatval=matrix(0,nrow=nS, ncol=nT)




#simulation

for(i in 1:nS){
  assprice[i,1]=S0
  intrval[i,1]=max(assprice[i,1]-K,0)
  for(t in 2:nT){
    assprice[i,t]=assprice[i,(t-1)]*exp(((rf-(sig*sig)/2)*delT)+ (sig*sqrt(delT)*rnorm(1)))
    intrval[i,t]=max((assprice[i,t]-K),0)

  }
}


updatval[,nT]=intrval[,nT]
# 3 vectors
for(t in (nT-1):2){
  
  indic=rep(0,nS) #indicator 
  PVW=rep(0,nS)
  ind1=rep(0,nS) # for P
  ind2=rep(0,nS) # for P^2
  count=1 # where ever > count ++ 
  
  for(i in 1:nS){
    if(intrval[i,t]>0){
       indic[i]=1
       
       for(tt in (t+1):nT){
         PVW[count]= PVW[count] + updatval[i,tt]*exp(-rf*(tt-t)*delT)
         
       }
       ind1[count] = assprice[i,t]
       ind2[count] = assprice[i,t]^2
       count=count+1
       
    }
  }
  


count = count -1

newcount=1
# now regression , linear model  

regr= lm(PVW[1:count]~ind1[1:count]+ind2[1:count]) 
EPVW=fitted(regr)

for(i in 1:nS){
  # check when indicator == 1 
  if(indic[i]!=0){
        if(intrval[i,t] > EPVW[newcount]){ # iterate over count 
            updatval[i,t]=intrval[i,t]
            updatval[i,(t+1):nT]=0
            
        }
        else{
              updatval[i,t]=0
        }
    newcount = newcount+1
  } 
 
  
}

#sum= rep(0,nS)
sum=0
for (i in 1:nS){
  for (t in 2: nT){
    sum=sum + updatval[i,t]*exp(-rf*(t-1)*delT)
  }
  
}

avg = sum /nS



indic=rep(0,nS) #indicator 
PVW=rep(0,nS)
ind1=rep(0,nS) # for P
ind2=rep(0,nS) # for P^2




}










