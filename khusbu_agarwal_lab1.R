#lab1 jan3 2025
#1.1
x <- 2.7/2
print(x)
#1.2
x <- 2.7 %/% 2
print(x)
#1.3
print(10+5i/2)
x <-10+5i/2
print(x)
#1.4
print(round(2.5))
#1.5
print(round(-2.5))
#1.6
print(2 %/% 4-1)
#1.7
print(3*2**2)
#1.8
print(3**2*2)
#1.9
print(7 %/% 4)
#1.10
print(7 %% 4)
#1.11
print(-7 %% 4)
print(-9 %% 4)
#1.12
print(trunc(5.7))
#1.13
print(trunc(-5.7))
#2
x <- 6+0.5
floor(x)
#3
a=1
b=2
c=4
print(a & b)
#returns TRUE as both a and b are non-zero
print(!a < b|c > b)
#(!a<b)is FALSE and c>b is TRUE therefore AND(|) gives TRUE

#4.1
#default datatype for number vector is numeric and not integer
x <-c(5,3,7,8)
#4.2
is.integer(x)
#4.3
is.numeric(x)
#4.4
x <- integer
print(x)
# converts the numeric datatype to integer datatype
#4.5
x <- c(5,3,7,8)
is.integer(x)
x <-as.integer(x)
#checks if it integer datatype
is.integer(x)
is.numeric(x)
 

#5.1
#assigns the square root value of 2 to x
x <- sqrt(2)
print(x)
#5.2
x*x ==2 
#returns false because x*x is slighlty less than 2 and not exactly 2

#5.3
x*x - 2
#returns the total rounding value
#5.4
#all.equal specifically used for compairing real numbers
all.equal(x*x,2)










