
data=read.csv("/home/ibab/Downloads/BrainCancer.csv",header=TRUE)
print(data$sex)
print(class(data$sex))
#5.1
data$sex <-factor(data$sex,levels=c("Male" ,"Female"))
print(class(data$sex))
print(is.factor(data$sex))
#5.2
print(nlevels(data$sex))
print(levels(data$sex))
#5.3
print(levels(data$diagnosis))

#generating levels using gl() function
print(gl(4,3))
print(gl(4,3,24))
Temp <-gl(2,2,24,labels=c("Low","High"))
print(Temp)
#6.1
Temperature<-gl(3,3,24,labels=c("Hot","Cold","Lukewarm"))
print(Temperature)
#6.2
new_data<-data.frame(Temperature,Temp)
print(new_data)
Soft <-gl(3,8,24,labels=c("Hard","medium","Soft"))
print(Soft)
fac_df<-data.frame(Temp,Soft)
print(fac_df)

#7
tapply(data$gtv,data$ki,mean)
tapply(data$gtv,data$ki,mean,trim=0.1)
#

subset<-c(data$gtv,data$ki==70)
print(subset)
vector<-sort(subset)
print(vector)
mean(vector)
mean(vector,trim=0.1)





#8
#pmin,pmax
print(pmin(data$gtv,data$time,data$ki))
print(pmax(data$gtv,data$time,data$ki))




#9.1
#diff between rank , sort and order
ranks<-rank(data$gtv)
sorted <-sort(data$gtv)
ordered <- order(data$gtv)
view <- data.frame(data$gtv,ranks,sorted,ordered)
print(view)
print(data$time[ordered])
#9.2
new_d<-data.frame(data$gtv,data$diagnosis,ordered)
print(new_d)


write.csv(new_d,file="lab4_ordered_data.csv")
#10.1
# data frame into matrix
filter1=data[1:6,3:8]
#10.2
filter1mat=as.matrix(filter1)
print(filter1mat)
print(class(filter1mat))
print(mode(filter1mat))
print(attributes(filter1mat))
#10.3
newcol=data$ki+data$gtv+data$time
print(newcol)
#10.4
newcoladded=data.frame(data,newcol)
print(newcoladded)
print(colnames(newcoladded))
#10.5
newcoladded2=cbind(data,newcol)
print(colnames(newcoladded2))
#10.6

new_rows <- data[c(26, 35), ]

updated_data <- rbind(data, new_rows)
print(updated_data)

print(dim(updated_data))

                

filter2=data[c(1,3,8,8),]
newrowadded=rbind(data,filter2)
print(newrowadded)

filter=data[c(8),]
newrowadded=rbind(data,filter)
print(newrowadded)

#names of row andcols
X<-matrix(c(1,0,2,5,3,1,1,3,1,3,3,1,0,2,2,1,0,2,1,0),nrow=4)
print(X)
rownames(X)<-rownames(X,do.NULL = FALSE,prefix="Trial.")
print(X)
          
drugs.names<-c("aspirin","paracetmol","nurofen","hedex","placebo")
colnames(X)<-drugs.names
print(X)

dimnames(X)<-list(NULL,paste("drug",1:5,sep=""))
print(X)

#calculation
print(mean(X[,5]))
print(var(X[4,]))
print(rowSums(X))#method 1 to get sum along the columns
print(colSums(X))
#method 2 to get the sum along the columns and rows
print(apply(X,1,sum))#1 over here states the sum of the row
print(apply(X,2,sum))#2states over the sum of the column
print(apply(X,2,sqrt))#sqrt doesnot care about the row or column
print(apply(X,1,function(X)X^2+X))
print(rowMeans(X))
print(colMeans(X))
print(apply(X,1,mean))
group=c("A","B","B","A")
print(rowsum(X,group))
print(row(X))
print(col(X))
print(tapply(X,list(group[row(X)],col(X)),sum))
print(aggregate(X,list(group),sum))