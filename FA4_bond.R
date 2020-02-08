library(readxl)
df <- read.csv(file.choose(),header=T)
'Input interes rate file'
df2 <- read.csv(file.choose(),header=T) 
'The Bond data file'

'Function to calculate bond price'

BondPrice=function(Fv,Cpn_Rate,N,w,rates,f){
  pv=0
  for(i in 1:N){
    pv=pv+Fv*Cpn_Rate/(f*100*((1+rates[i]/(100*f))^(w+i-1)))
  }
  pv=pv+Fv/((1+rates[N]/(100*f))^(N+w-1))
  return(pv)
}

'For YTM use Newton Raphson'
'For f(y)'

bp_fun=function(Fv,Cpn_Rate,N,w,y,f){
  pv=0
  for(i in 1:N){
    pv=pv+Fv*Cpn_Rate/(f*100*((1+y/(100*f))^(w+i-1)))
  }
  pv=pv+Fv/((1+y/(100*f))^(N+w-1))
  return(pv)
}

'For f_(y)'

derivf=function(Fv,Cpn_Rate,N,w,rates,f){
  pv=0
  for(i in 1:N){
    pv=pv+Fv*((w+i-1)/f)*Cpn_Rate/(f*100*((1+y/(100*f))^(w+i)))
  }
  pv=pv+Fv*((N+w-1)/f)/((1+y/(100*f))^(N+w))
  return(pv)
}


no_of_cpns = as.numeric(unlist(df2[,8]))  
cpn_rates = as.numeric(unlist(df2[,3])) 
NDNCs = as.numeric(unlist(df2[,10])) 
Maturity = as.numeric(unlist(df2[,9])) 
Maturity = Maturity/125
'Have to scale down Maturity to fit in the curve for comparision'

YTMS=c()
for(i in 1:length(no_of_cpns)){ 
  N <- no_of_cpns[i]
  Fv <- 100
  Cpn_Rate <- cpn_rates[i]
  NDNC <- NDNCs[i]
  f <- 2
  w <- NDNC/(365/f)
  irs <- as.numeric(unlist(df[,2]))
  'Interest Rate Calculation'
  rates=c()
  for(j in 1:(length(irs)-1)){
    rates[j] <-irs[j] + (irs[j+1]-irs[j])*w 
  }
  dp=BondPrice(Fv,Cpn_Rate,N,w,rates,f)
  'newton raphson method'
  'initial guess'
  y <- 10 
  error=(dp-bp_fun(Fv,Cpn_Rate,N,w,y,f))/derivf(Fv,Cpn_Rate,N,w,rates,f)
  while(error > 1/1000000){
    y=y-error
    error=(dp-bp_fun(Fv,Cpn_Rate,N,w,y,f))/derivf(Fv,Cpn_Rate,N,w,rates,f)
  }
  YTMS[i] <- y
}  

rates=df[,2] 

Time_to_maturity=df[,1] 

'ZCYC curve'
plot(Time_to_maturity,rates,col="red", type="l") 

'YTMs against the maturity date'
lines(Maturity,YTMS,col="green", type="l")

'Compared'
