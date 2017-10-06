bestMap<-function(s1,s2){
  # this function computes the best matching of two clustering outcomes s1 and s2, using Hungarian algorithm.
  if(!is.vector(s1)){
    s1<-as.numeric(s1);
  }
  if(!is.vector(s2)){
    s2<-as.numeric(s2);
  }
  if(length(s1)!=length(s2)){
    print("length doesn't match, exit.");
    return(NULL);
  }
  Label1 = unique(s1);
  nClass1 = length(Label1);
  Label2 = unique(s2);
  nClass2 = length(Label2);
  nClass = max(nClass1,nClass2);
  G = matrix(0,nClass,nClass);
  for(i in 1:nClass1){
    for(j in 1:nClass2){
      G[i,j] = length(which(s1 == Label1[i] & s2 == Label2[j]));
    }
  }
  hung<-solve_LSAP(G,maximum=TRUE);
  news2<-rep(0,length(s2));
  for(i in 1:nClass2){
    news2[s2==Label2[i]]<-Label1[hung[i]]
  }
  misrate<-1-sum(news2==s1)/length(s1);

  return(news2);
}

