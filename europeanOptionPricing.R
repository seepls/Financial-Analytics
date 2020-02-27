t = 50
n = 5000
delT = t/n
rf = 0.05
sig = 0.13
s0= 12089
K=12200
nsim= 100

st= rep(0,n)
payoff = rep(0,n)
sum=0

for(s in 1:nsim){

st=s0
for(i in 1:n){
  st = st*exp(((rf-(sig^2)/2)*delT)+(sig*sqrt(delT)*rnorm(1)))
}
payoffc = max(st-K,0)
payoffp = max(K-st,0)

sumc = sumc + payoffc
sump = sump + payoffp
}

payoffc = sumc/nsim
payoffp = sump/nsim
call = payoffc*exp(-rf*t)
put = payoffp*exp(-rf*t)
