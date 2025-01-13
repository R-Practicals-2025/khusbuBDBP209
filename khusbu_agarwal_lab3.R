#lab3
#EXCERCISE4
#1
vec<-c(4,7,6,5,6,7)
class(vec)
#specifies the class which is numeric
length(vec)
#gives the legth of the vector
min(vec)
#gives minimum value
max(vec)
#gives maximum value
#2
vec <-scan()
# it takes input from the command line
#3
vec[4]
vec[5]
#IN R index actually starts from 1 rather than 0
#4
ind <-c(2,3,6)
vec[ind]
#it shows NA for 6th element index because the length of the vector is 5
vec[(c(2,3,6))]
#it is same for both the above function
#5
vec[-1]
#it pops out the first element
vec[c(-3,-4)]
#it removes the middle element
#6
vec[-length(vec)]
#it pops out the last element

#7

vector <-c(5,6,87,4,32,39,21,34,56)
vector

func1 <- function(x) sort(x)[(3:(length(x)-2))]
func1(vector)

trim <-function(x) sort(x) [c(3:(length(x)-2))]
trim(vector)

#8
vec[1:3]
vec[seq(2,length(vec),2)]
#produces a sequence which starts with 2nd index and skips 2 in the length(vec)
vec[1:length(vec)%%2==0]
#9
x <- 0:10
x[x<5]
sum(x[x<5])
#alternative way
sum

#10
sorted <-sort(x,decreasing=TRUE)
sum(sorted[1:3])
'#11'
which.max(x)
which.min(x)
#shows the index of maximum and minimum number
#12
cbind(1:10,10:1)
cbind(1:10,10:8)

rbind(1:10,10:1)
#13
x<-c(1:10)
x
y<-c(1:10*5)
y
x*y
x+y
x/y
x^y
log(x)
exp(y)

#EXCERCISE5 5
y <-1:24
dim(y)<-c(2,4,3)
y
#it secifies 2 rows, 4 columns and 3 dimension
#1
x <-matrix (c(1,0,0,0,1,0,0,0,1),nrow=3)
x <-matrix(1:20,nrow=3)
x
# it doesnotcreate a matrix because 20 is not a submultiple of 3
x <-matrix(1:18,nrow=3)
x
vector<-c(7,5,4,3,1,2,1,2)
v <-matrix(vector,byrow=T,nrow=2)
v
v <-matrix(vector,byrow=F,nrow=2)
v
#2
vector<-c(1,2,3,4,4,3,2,1)
v<-matrix(vector,byrow=T,nrow=2)
v
dim(vector)<-c(4,2)
vector
#the above function shows bycolumn function
is.matrix(vector)


x<-c (1,4,5,7,"khusbu","agarwal",76,54)
dim(x)<-c(1,2,4,1)
x
x[,,1,1]

#3
#1
x<-matrix(1:24,nrow=3)
x
min(x)
#it shows the minimum number
max(x)
#it shows the maximum number
sum(x)
#it shows the sum between the numbers
range(x)
#it shows the range between the numbers
sort(x)
#it shows the sorting 
#2
colMeans(x)
#3
z<- x[1:4] %o%y[1:3]
z
yox <-y[1:3] %o% x[1:4]
yox
t(z)
#it shows the transpose of a vector
t(yox)
#it shows the teanspose of y outerproduct of x
x_subset <-x[1:3]
y_subset <-x[1:3]
x_subset %*% y_subset
#it shows the dot product between the  two numbers


sum(x_subset*y_subset)
#it is the another way of the dot product 

crossprod(x_subset[1:4],z)
#it shows the crossproduct 
diag(4)
#it shows the identity matrix of 4 rows and 4 columns
class(x_subset)
#it shows the class of the matrix which is actually the integer

