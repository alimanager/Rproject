###################################################################################
#            THE GAP STATISTICS : DETRMINING THE NUMBER OF CLASSES IN K-means.
###################################################################################

kmeansGAP=function(data, Nmax , B,...){

WK=function(data,k){ #function to compute the wk for k.
    dk=W=wk_b=c()
      
    km=kmeans(data,centers=k,...) # k-means with the argument 
    
      for(j in 1:length(km$size)){
      # the sum of intra-cluster distances between points in a given cluster
      dk[j]=sum(dist(data[km$cluster==j,])) # the eucldian distance within-cluster
      # W[j]=(1/2*km$size[j])*dk[j] 
      }
      W=sum(dk/(2*km$size)) #the normalized intra-cluster sums of squares
      return(W)
}

wK_copies=c()
GAP=SD=SK=wk=moy_B=c()
min_range=apply(data,2,function(x){min(x)})  
max_range=apply(data,2,function(x){max(x)})
min_max=as.data.frame(rbind(max_range,min_range)) #the range(min,max) for each feature for bootstrapping

for (i in seq(Nmax)){
  wk[i]=WK(data,i)
  for( b in seq(B) ){ # B : the number of sampling for each cluster
      #monte carlo sampling
      don=as.data.frame(apply(min_max,2, function(x){ sample(runif(nrow(data), min=x[2] ,max= x[1]))})) 
      wK_copies[b]=WK(don,i)
      
  }
  
  moy_B[i]=(sum(log(wK_copies))/B) #  the average of B copies log W^*_k
  GAP[i]= moy_B[i]  - log(wk[i]) # the Gap
  SD[i] = sqrt(sum(( log(wK_copies) - moy_B[i] )^2)/B ) #  standard deviation 
  SK[i] = sqrt(1+(1/B))*SD[i] # quantity of the simulation error sd
  
}  
number_k=GAP[1:9]-(GAP[2:10]-SK[2:10]) #the number of clusters as the smallest k
  
      par( mfrow = c( 1, 3 ), oma = c( 0, 0, 2, 0 ) )
     
      matplot(y = data.frame(log(wk),moy_B), type="o",pch=c(1,2),col=c("blue","red"),xlab="K",ylab="Log(wk) & exp{log(wk*)}")
      legend("bottomleft",legend = c("log(wk)","exp{log(wk*)}"),col = c("blue","red"),pch=c(2,1),box.lty = 0,cex = 0.75,text.width = 1,title.adj = T)
      barplot(number_k,main = "The Number of Cluster ") # the firt postive is the number of cluster.
      plot(GAP,type="b",xlab="k") #Gap plot
  
      return(list(Gap=GAP,WK=log(wk),moy_B=moy_B,SK=SK))
}
