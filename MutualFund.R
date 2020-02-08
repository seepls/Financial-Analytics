library(quadprog)
data=read.csv(file.choose(),header=TRUE)

prices=data[,2:6]
n=nrow(prices)
ret=(prices[1:(n-1),]/prices[2:n,])-1
head(ret)
ave_ret=colMeans(ret)
varcovar=cov(ret)
# Unknown variables are my weights
# But after selection of stock I know my vairance and my covariance
# AIm: Minimize variance of portfolio
# condition : Summation of Weights = 1 (Obvious)

num_assets=ncol(prices)
cons_mx=rbind(rep(1,num_assets),ave_ret)
'Shortselling constraints'
cons_wos_mx=rbind(rep(1,num_assets),ave_ret,diag(num_assets))
#constraint matrix without shortselling
#AMAT : Constraint Matrix
#prep Bvector

rhat=mean(ave_ret)

dvec=c(rep(0,num_assets))
#solve.QP.compact(Dmat, dvec, Amat, Aind, bvec, meq=0, factorized=FALSE)
#DMAT : Cov matrix


inc=((max(ave_ret)-min(ave_ret)))/20
ret_seq=seq(min(ave_ret), max(ave_ret),inc)

risk=rep(0,21)
risk_wos = rep(0,21)
for(i in 1:length(ret_seq)){
bvec=c(1,ret_seq[i]) 
bvec_wos=c(1,ret_seq[i],rep(0,num_assets)) 
model=solve.QP(Dmat=varcovar, dvec=dvec,Amat=t(cons_mx),bvec=bvec,meq=2)
model_wos=solve.QP(Dmat=varcovar, dvec=dvec,Amat=t(cons_wos_mx),bvec=bvec_wos,meq=2)
#meq remain same , rest inequality , default is greater than
risk[i]=sqrt(model$value)
risk_wos[i]=sqrt(model_wos$value)
}

plot(risk, ret_seq , col="red")
lines(risk_wos, ret_seq, col="blue" )