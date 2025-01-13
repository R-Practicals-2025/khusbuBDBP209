# excercise1
#1)
round(12.1343,digits=3)
#2
round(123.12344,digits=3)
#3
round(1234.12344,digits=3)
#4
round(12345.12344,digits=3)
#R actually trucante and format the output for the accuracy and precision ,Now we can look upon to different ways to handle this situation.options(digits=15)
#5
options(digits=15)
round(12345.12344,digits=3)
# with the help of option command we can see the digit is perfectly been rounded to 3
#6
formatC(round(12345.12344,digits=3),format="f",digits=3)
#with the help of format C command it is also concatenating and rounding it to 3 digit places
#7
print(1234.12344)
#when we are putting with any digits it is printing the exact no
#8
print(1234.723,digits=3)
#when we are writing digits=3 or digits that refers to the number of significant digits.if the number is less than or equal to the number of digits before the decimal point then it retains the nearest round number of digits before the decimal point
print(1234.723,digits=5)
#when we are puttings digits 5 it is alctually giving 5 significant figures
#9
round(123456788.123,digits=3)
#10
print(round(123456788.123,digits=2),digits=20)
#it shows significant figures till 20 
#11
print(round(123456789.1234,digits=4),digits=20)
#it first shows significant features till digit 4 and then add zero makes the significant digits till20
#12
paste("hello world")
paste("hello","world")
#13
paste(1:10)
paste(1:10)[4]
#14
as.numeric(paste(1:10))
#15
paste(1:10,collapse=".")
paste(1:10,sep="-")

#16
paste(c("hello","world"),1:10,sep="-")
print(paste("hello",1:10,sep="-"))

#EXCERCISE-2
#1
0:10
seq(0:10)
#when we seq function it starts from 1 rather than 0
#2
15:5
#
#3
seq(0,1.5,0.1)
#it starts from 0 to 1.5 with a decrease of 0.1
#4
seq(6,4,-0.2)
#it starts from 6 to 4 with a decrease of 0.2 minus sign is important as we are going reverse
#5
N <- c(55,76,92,103,84,88,121,91,65,77,99)
print(N)
#It just actually concatenate the numbers
#6
seq(from=0.04,by=0.01,length=11)
#it started from 0.04 to length 11 by increasing 0.01
seq(0.04,by=0.01,along=N)
# it have automatically taken N as a length of 11
#7
seq(from=0.04,to=0.14,along=N)
#it is actually starting from 0.04 to 0.014 as alength of 11
#8
sequence(c(4,3,4,4,4,5))
#it will show sequence from 1 to 4 and then 1 to 3 and then again 1 to 4 at the end from 1 to 5
seq(-2)
#actually for seq fuction R starts from 1

seq(0)
#9
rep(9,5)
#it creates repeat of number 9 5 times it creates an array
rep(1:4,2)
#it creates repeat from 1 to 4 twice
rep(1:4,each=2)
rep(1:4,each=2,times=3)
#the number starts from 1 in a pair  to 4 and repeating 3 times
rep(1:4,1:4)
#10
rep(1:4,c(4,1,4,2))
#it states that number starts from 1 to 4 and it concatenates the no o f times it has been provided
rep(c("cat","dog","goldfish","rat"))
c(2,3,2,1,3)
# it just simple concatenate
#11
seq(-1,1,by=0.1)
seq(-1,1,0.1)
#12
seq(-1,1,length=7)
#13
N<- -1 +(0:20)*0.1
print(N)
sequence(c(4,3,4))
sequence(c(4,3,4,5))
sequence(4,3,4)
sequence(4,3,4,5,6)
#EXCERCISE-3
#1
3/0
0/0
Inf/Inf
#2
full.frame[! is.na(full.frame),]
exp(-Inf)
#exponential to infintity is 0 
#3
(0:3)**Inf
#4
0/0
#5
Inf-Inf
#6
Inf/Inf
#7
is.finite(10)
#it true because numbers are always finite
#8
is.infinite(10)
#it is false because numbers are finite
#9
is.infinite(Inf)
#it is true because infinite is infinite
#10
y<-c(4,NA,7)
y=="NA"
is.na(y)
y==7
#it is actually equating and seeing where na is there it returns true
#11
y[! is.na(y)]
#it is removing na from the y
#12
c1<-c(1,2,3,NA)
c2<-c(5,6,NA,8)
c3<-c(9,NA,11,12)
c4<-c(NA,14,15,16)
full.frame<-data.frame(c1,c2,c3,c4)
full.frame
reduced.frame<-full.frame[! is.na(full.frame),]
reduced.frame
reduced.frame<-full.frame[! is.na(full.frame$c1),]
reduced.frame
x<-c(1,2,3,4,NA)
mean(x)
mean(x,na.rm=TRUE)
#13
v<-c(1:6,NA,NA,9:12)
seq(along=v)[is.na(v)]
which(is.na(v))











