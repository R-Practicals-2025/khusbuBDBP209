eg_sweep=data.frame(data$ki,data$gtv,data$time)
#method 1 to perform sweep action
cols=apply(eg_sweep,2,mean)
print(cols)
col.means=matrix(rep(cols,rep(dim(eg_sweep)[1],dim(eg_sweep)[2])),nrow=dim(eg_sweep)[1])
print(col.means)
eg_sweep_alt=eg_sweep-col.means
print(eg_sweep_alt)
eg_sweep_alt2=sweep(eg_sweep,2,cols)
print(eg_sweep_alt2)

#sapply for vectors
eg_sapply=sapply(3:7,seq)
print(attributes(eg_sapply))
print(class(eg_sapply))

pgdata=read.table("/home/ibab/Downloads/pgfull.txt")
print(pgdata)
print(names(pgdata))
species=pgdata[,1:54]
print(max.col(species))
print(names(species)[max.col(species)])
print(table(names(species)[max.col(species)]))
print(max.col(-species))


#lists
apples <- c(4,4.5,4.2, 5.1,3.9)
oranges <- c(TRUE,TRUE,FALSE)
chalk <- c("limestone","marl","oolite","CaCO3")
cheese <- c(3.2-4.5i,12.8+2.2i)
items <- list(apples, oranges, chalk, cheese)
print(items)
data.frame(apples,oranges,chalk) #different rows so data frame is not possible
#subscripts on lists have double square brackets
print(items[[3]])
print(items[[3]][3])
print(items[3])
print(items[[1]][2])
print(items[1][2])

items <- list(first=apples,second=oranges,third=chalk,fourth=cheese)
print(names(items))
print(items$fourth)
print(class(items))
print(lapply(items,length))
print(lapply(items,class))
print(lapply(items,mean)) #Predict the output first
print(summary((items)))
print(str(items))
#difference between class and mode


