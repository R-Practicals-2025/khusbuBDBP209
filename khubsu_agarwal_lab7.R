#deleting col from a dataframe
data=read.csv("/home/ibab/Downloads/BrainCancer.csv")
data$time<-NULL# modifies the orginal dataset
print(dim(data))
print(colnames(data))
install.packages("readxl")
library(readxl)
data2=read_excel("/home/ibab/Downloads/pone.0148733.s001.xlsx",1)
print(names(data2))
print(colnames(data2))
a=5.0
b=10.0
if (a<b)
  print("a is less than b")
a=5.0
b=10.0
c=15.0
d=20.0
# if loop
if(a>b)
{
  print("a is greater than b")
}
#if and else loop
if(a>b)
{
  print("a>b")
}else if (b<c)
{
  print("b<c")
}
a=2
b=3
if(a>b) print ("a>b")
for (i in 1:5)print(i*2)
# for loop
j=0
k=-1
for (i in 1:5){
  j<-j+1
  k<-k+2
  print(paste(j,k))
}

# different ways of calculating factorials
fn1<-function(x)x*2
fac1<-function(x){
  f<-1
  if(x<2)return(1)
  for(i in 2:x){
    f<-f*i
    f}
    return (f)}
print(fac1(4))
print(sapply(0:5,fac1))
#while loop it is more effective soometimes it is true if sometimes excueted

fac2<-function(x){
  f<-1
  t<-x
  while(t>1){
    f<-f*t
    t<-t-1
  }
  return (f)
}
print(fac2(5))
#command repeat it will continue untill it meets with break statement
fac3<-function(x){
  f<-1
  t<-x
  repeat{
    if(t<2)break
    f<-f*t
    t<-t-1
  }
  return(f)
  }
fac3(3)
cumprod(1:4)
fac4<-function(x) max(cumprod(1:x))
fac4(4)
fac5<-function(x)gamma(x+1)
fac5(3)
factorial((5))

#timed the functions
pd<-proc.time()
result1<-fac1(11234)
proc.time() - pd
#uniform distribution
x<-runif(1000000000)
pc<-proc.time()
cmax<-x[1]
for(i in 1000000000){
  if(x[i]>cmax)cmax<-x[i]
}
proc.time()-pc



