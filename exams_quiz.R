func1 <- function(x,y){
  a<- x+y
  b<-x-y
  c<-x*y
  d<- x/y
  e<- x^y
  f<- log(x)
  g<-exp(y)
  k<-c(a,b,c,d,e,f,g)
  return (k)
}
func1(7,8)
#excercise2 
func2<-function(a,b,c){
  roots1<- (((-b)+(sqrt(b*b-4*a*c)))/2*a)
  roots2<- (((-b)-(sqrt(b*b-4*a*c)))/2*a)
  result<-c(roots1,roots2)
  return (result)
}
func2(2,5,2)
