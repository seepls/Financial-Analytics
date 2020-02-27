rf=0.06
S0=12089
K=12200
T=50/365
nT=100
delT=T/nT

sig=0.15


u= exp(sig*sqrt(delT))

d = 1/u

p = (exp(rf*delT)-d)/(u-d)

#MATRIX 

binaryMatrix = matrix(0,nrow=nT, ncol=nT)
binaryMatrix[1,1]=S0
for ( i in 2:nT){ # i columns 
 
  for (j in 1:i){ # j rows [r,c]
    if(j==1  ){ binaryMatrix[1,i] = binaryMatrix[1,i-1]*u} # done 
    if((j>1)){
      binaryMatrix[j,i] = binaryMatrix[j-1,i-1]*d
    }
    
  }
}



#spot prices 

# back calculation 
AssetMatrix = matrix(0,nrow=nT, ncol=nT)
#payoff 
for ( i in 1:nT){
AssetMatrix[i,nT]= max(binaryMatrix[i,nT]-K,0)
}


for(i in (nT-1):1){ # i column
  for(j in 1:i){ # j rows
    AssetMatrix[j,i] = (AssetMatrix[j,i+1]*p +AssetMatrix[j+1,i+1]*(1-p))*exp(-rf*delT)
  }
}

'
for(i in 1:nT-1){ # i column
  for(j in 1:i){ # j rows
    AssetMatrix[nT-j,nT-i] = AssetMatrix[nT-j,nT-i+1]*p +AssetMatrix[nT-j+1,nT-i+1]*(1-p)
  }
}
'

