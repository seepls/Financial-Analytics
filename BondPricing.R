FV=100
Cpn_Rate=0.0737
N=7
NoDaysTillNextCpn=78
f=2
w=78/(365/f)

ir_input=read.csv(file.choose(),header=T)
'Interest Rate Calculation'
r=rep(0,N)
rate=ir_input[,1]

for (i in 1:N) 
  {
  r[i]= rate[i]+(rate[i+1]-rate[i])*w
  }
#Function to calculate Bond price

BondPrice=function(F,cr, n ,w,r,f){
  r=r/100
  price=0
  for (i in 1:n) {
    price = price +  (cr*F/f)/(1+r[i]/f)**(i-1+w)
  }
  price = price + F/(1+r[n]/f)**(n-1+w)
  return (price)
}
BP = BondPrice(FV,Cpn_Rate,N,w,r,f)
# This BP is Dirty Price

#Accrued interest = (1-w)*C/f
#Quoted Price = Dirty Price-Accrued Interest
#Quoted price is what is quoted on the sites but we add accrued interest to quoted price to get 
# dirty price which is what we sell it at.
BP

#We try to get the YTM by interating again and again by using Newtop Raphson Method
#We start by taking YTM as avg of all the spot rates 
#y_n+1 = y_n - f(y_n)/f'(y_n)

AI = (1-w)*(Cpn_Rate*FV/f)
AI
Quotedprice = BP - AI

y=mean(r)

fy_der=function(F,n,cr,f,w,y){
  fy_d = 0
  for (i in 1:n) {
    fy_d = fy_d + (cr*F/f)*((w-1+i)/f)/(1+y/f)**(i+w)
  }
  fy_d= fy_d + (F/f)*((w-1+n)/f)/(1+y/f)**(n+w)
  return(fy_d)
}

fy_deriv= fy_der(FV,N,Cpn_Rate,f,w,y)
fy_deriv

'fy_value = function(F,cr, n ,w,f,dp,y){
  fy = 0
  for(i in 0:n-1){
    fy = fy - (cr*F/f)/(1+y/f)**(i+w)
  }
  fy = fy + F/(1+y/f)**(n-1+w)
  return(fy)
}'

'fy_val = fy_value(FV,Cpn_Rate,N,w,f,BP,y)
fy_val 
'

threshold = 0.0001
y=mean(r)
y1= rep(y,N)
error= BP - BondPrice(FV,Cpn_Rate,N,w,y1,f)
while(abs(error) > threshold){
   y=y-error 
   y1= rep(y,N)
  error = (BP - BondPrice(FV,Cpn_Rate,N,w,y1,f))/fy_der(FV,N,Cpn_Rate,f,w,y/100)
  
}
y
