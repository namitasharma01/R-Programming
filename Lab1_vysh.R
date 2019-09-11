name="Vyshnavi Pisupati"
liuid="vyspi394"

#1.1.1
my_num_vector <-function () {
  c(log10(11),cos(pi/5),exp(pi/3),1173%%7/19)
}

#1.1.2
filter_my_vector <- function(x,leq){
  replace(x,which(x>=leq),NA)
}

#1.1.3
dot_prod<-function(a,b){
  sum(a*b)
}

#1.1.4
approx_e<-function(N){
  sum(1/factorial(0:N))
}

#1.2.1
my_magic_matrix<-function(){
  matrix(c(4,3,8,9,5,1,2,7,6),nrow=3,ncol=3)
}

#1.2.2
calculate_elements<-function(A){
  nrow(A)*ncol(A)
}

#1.2.3
row_to_zero<-function(A,i){
  A[i,]<-0
  return(A)
}

#1.2.4
add_elements_to_matrix<-function(A,x,i,j){
  A[i,j]=A[i,j]+x
  return(A)
}

#1.3.1
my_magic_list<-function(){
  list("info"="my own list",my_num_vector(),my_magic_matrix())
}


#1.3.2
change_info<-function(x,text){
  x["info"]<-text
  return(x)
}

#1.3.3
add_note<-function(x,note){
  x["note"]<-note
  return(x)
}

#1.3.4
sum_numeric_parts<-function(x){
  p=sum(as.numeric(unlist(x)),na.rm=TRUE)
  return(p)
}

#1.4.1.
my_data.frame<-function(){
  data.frame("id"=1:3,"name"=c("John","Lisa","Azra"),"income"=c(7.30,0.00,15.21),"rich"=c(FALSE,FALSE,TRUE))
}

#1.4.2
sort_head<-function(df,var.name,n){
  df=head(df[order(df[,var.name],decreasing=TRUE),],n)
  return(df)
}

#1.4.3
add_median_variable<-function(df,j){
  m=median(df[,j])
  df$compared_to_median<-"Median"
  df$compared_to_median[which(df[,j]>m)]<-"Greater"
  df$compared_to_median[which(df[,j]<m)]<-"Smaller"
  return(df)
}

#1.4.4
data("iris")
df<-iris
analyze_columns<-function(df,j){
  a=setNames(c(mean(df[,j[1]]),median(df[,j[1]]),sd(df[,j[1]])),c("mean","median","sd"))
  b=setNames(c(mean(df[,j[2]]),median(df[,j[2]]),sd(df[,j[2]])),c("mean","median","sd"))
  v=cor(df[j])
  t=list(a,b,v)
  names(t)<-c(colnames(df[j]),"correlation_matrix")
  return(t)
}

