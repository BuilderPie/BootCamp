# Day 2
# Correlation


#source("https://bioconductor.org/biocLite.R") #run if necessary
#biocLite("curl") #run if necessary
library(curl)
my_data <- read.table(curl("http://tcga-data.nci.nih.gov/docs/publications/gbm_exp/unifiedScaledFiltered.txt"),sep="\t",header=1)


##Data Handling
#Problem A
dim(my_data)

vec1 <- c(1:5)
vec2 <- seq(2, 10, 2)

#Problem A
cor(x = vec1, y = vec2)
help(cor)

q <- cor(my_data)
head(q)

w <- cor(t(my_data))  #transpose
dim(w)

#Problem B
range <- max(w) - min(w)
max(w)
min(w)

#Problem C
which.max(q)

# w3 <- as.table(w)
w2 <- as.data.frame(as.table(w))

class(w)
class(w2)

head(w2)

# loc <- w2[,3] != 1
# head(loc)
# 
# w4 <- w2[,3][loc]
# Gene1_names <- w2[,1][loc]
# Gene2_names <- w2[,2][loc]
# 
# head(Gene1_names)
# 
# Gene1_names_vec <- as.vector(Gene1_names)
# Gene2_names_vec <- as.vector(Gene2_names)
# 
# dim(Gene1_names_vec)
# class(Gene1_names_vec)
# 
# max(abs(w4))
# which.max(abs(w4))
# Gene1_names_vec[which.max(w4)]
# Gene2_names_vec[which.max(w4)]

#Best Solution by classmates()

w3 <- w2[w2$Var1 != w2$Var2,]
head(w3)

w4 <- w3[order(abs(w3$Freq),decreasing=TRUE),]
head(w4)

# Problem D, Spearson method
w5 <- cor(t(my_data), method="spearman")  #transpose
w6 <- as.data.frame(as.table(w5))

w7 <- w6[w6$Var1 != w6$Var2,]
head(w7)

w8 <- w7[order(abs(w7$Freq),decreasing=TRUE),]
head(w8)



#Problem E
library(corrplot)
top20_gene1 <- w4[1:10, 1]
top20_gene2 <- w4[1:10, 2]
# head(top20_gene2)


my_data_20 <- rbind(my_data[top20_gene2,], my_data[top20_gene1,])
# head(my_data_20)

q2 <- cor(t(my_data_20))


corrplot(q2, method="circle")

##Clustering

#Problem A
class(q2)
dim(q2)

hc <- hclust(dist(q2))
plot(hc)

#Problem B




