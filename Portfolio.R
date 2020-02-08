install.packages("quadprog")
#https://cran.r-project.org/web/packages/quadprog/quadprog.pdf
library(quadprog)
data = read.csv(file.choose(),header = TRUE)
head(data)
prices = data[,2:6]
n=nrow(prices)
ret=(prices[1:(n-1),]/prices[2:n,])-1
head(ret)
avg_ret=colMeans(ret)
varcovar = cov(ret)

num_assets = ncol(prices)
constraint_mtrx=rbind(rep(1,num_assets),avg_ret)

#Shortselling constraints _wos
cons_wos_mtrx = rbind(rep(1,num_assets),avg_ret,diag(num_assets))


rhat=mean(avg_ret)
dvec = c(rep(0,num_assets))

increment= ((max(avg_ret))-(min(avg_ret)))/20
ret_seq = seq(min(avg_ret),max(avg_ret),increment)  

risk=rep(0,21)
risk_wos=rep(0,21)
maxsr=0
maxsr_wos=0
rf=((1.05)^(1/252))-1  
#sharpe ratio = (R_p-Rf) / sigma_p
for (i in 1:length(ret_seq)) {
  bvec = c(1,ret_seq[i])
  bvec_wos=c(1,ret_seq[i],rep(0,num_assets))
  model= solve.QP(Dmat=varcovar,dvec=dvec, Amat = t(constraint_mtrx),bvec = bvec,meq = 2)
  model_wos= solve.QP(Dmat=varcovar,dvec=dvec, Amat = t(cons_wos_mtrx),bvec = bvec_wos,meq = 2)
  
  risk[i]=sqrt(model$value)
  risk_wos[i]=sqrt(model_wos$value)
  sr=(ret_seq[i]-rf)/risk[i]
  sr_wos=(ret_seq[i]-rf)/risk_wos[i]
  if(sr>maxsr){
    maxsr=sr
    optwt=model$solution
  }
}
plot(risk, ret_seq,col="red")  
lines(risk_wos,ret_seq,col="blue")
model$solution

