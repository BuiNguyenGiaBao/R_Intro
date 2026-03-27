column1 <- c(1:3)
column2 <- c('b','c','a')
column3 <- c(T,F,F)

dataset <- data.frame(column1, column2, column3)
#hiệm thị ra consoul thì print(dataset)
dataset
# view 
View(dataset)
#  column name 
colnames(dataset)
#change columns name 
colnames(dataset)[2]<- 'name'
dataset
#change many columns name 
colnames(dataset)<-c("#",'name', 'check')
dataset

# add more row for datframe 
newrows<- c('4','batman','T')
dataset2 <-rbind(dataset,newrows)

# add more columns
newcolumn<- c('q','p','e','b')
dataset3 <- cbind(dataset2, newcolumn)
dataset3

dataset3$newcolumn2<-c(1,2,3,4)

#take data from dataframe
dataset3[1,2] #left is row right is columns  and you can change it by name if only one row or columns is will take all in 
# the row or column 
# head show fist few line or col
# tail show last few line or col
#string show structure of dataset
# summary is all of that
#change data type for column
dataset3$check<- as.logical(dataset3$check)

data()
CO2
View(CO2)
str(CO2)
summary(CO2)
head(CO2)
tail(CO2)
#data wrangling
# mege 
# innerjoin (chỉ lấy phần chung 2 set)
#set<- merge(set1,set2,by="ten cot")
#outerjoin (lấy tất cả cái nào thiếu thì null )
#set<- merge(set1,set2,by="ten cot", all= true)
# left and right join y như vậy nhưng all.x là pahur all.y là trái
# sắp sếp dữ liệu dùng sort còn muốn cao hơn hay thấp hơn thì in hay decrease
# how to filtering 
#set[set$region =='tphcm'$ is.na(set4$region)]
# kiểm tra data có bị na
is.na()




