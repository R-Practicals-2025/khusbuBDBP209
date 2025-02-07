data=read.csv("/home/ibab/Downloads/Heart.csv",header=TRUE)

print(data$Sex)
print(class(data$Sex))
#5.1
data$sex <-factor(data$Sex,levels=c("0 ","1"))
print(class(data$sex))
print(is.factor(data$sex))
#5.2
print(nlevels(data$Sex))
print(levels(data$Sex))
#5.3
print(levels(data$Chol))

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
tapply(data$Chol,data$RestECG,mean)
tapply(data$Chol,data$RestECG,mean,trim=0.1)
#8
#pmin,pmax
print(pmin(data$Chol,data$RestBP,data$RestECG))
print(pmax(data$Chol,data$RestBP,data$RestECG))


#9.1
#diff between rank , sort and order
ranks<-rank(data$Chol)
sorted <-sort(data$Chol)
ordered <- order(data$Chol)
view <- data.frame(data$Chol,ranks,sorted,ordered)
print(view)
print(data$Chol[ordered])
#9.2
new_d<-data.frame(data$Chol,data$RestECG,ordered)
print(new_d)


write.csv(new_d,file="lab4_heart_ordered_data.csv")
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
newcol=data$Chol+data$RestBP+data$RestECG
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



filter2=data[c(1,3,8),]
newrowadded=rbind(data,filter2)
print(newrowadded)



