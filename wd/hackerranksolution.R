L<-scan("stdin")
N<-L[1]
L<-L[2:length(L)]

k<-1
j<-0
while(k<N){
  if(any(is.na(L[k+2]), L[k+2]==1)){
    k<-k+1
  }else{
    k<-k+2
  }
  j<-j+1
}
cat(j)

