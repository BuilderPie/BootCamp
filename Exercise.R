#Data handling


#source("https://bioconductor.org/biocLite.R") #run if necessary
#biocLite("curl") #run if necessary
library(curl)
# my_data <- read.table(curl("http://tcga-data.nci.nih.gov/docs/publications/gbm_exp/unifiedScaledFiltered.txt"),sep="\t",header=1)


##Data Handling
#Problem A
dim(my_data)

#Problem B
head(apply(my_data, 1, mean))

head(apply(my_data, 2, mean))

#Problem C
head(apply(my_data, 1, min))

head(apply(my_data, 1, max))

head(apply(my_data, 2, min))

head(apply(my_data, 2, max))

#Problem D
max_sd <- which.max(apply(my_data,1,sd))
max_sd

sd(my_data[max_sd,])

#Problem E
# overall_max <- max(max(my_data))
# overall_max
# my_data[my_data==overall_max]
# 
# my_data==overall_max
max(my_data)

which.max(apply(my_data,2,max))
which.max(apply(my_data,1,max))

##plotting
#Problem A
EGFR_data <- my_data["EGFR",]
class(EGFR_data)


hist(data.matrix(EGFR_data),30, main="Histogram of EGFR expression", xlab="EGFR Expression", ylab="Frequency (# of samples)")

#Problem B
IDH1_data <- my_data["IDH1",]
plot(data.matrix(IDH1_data), data.matrix(EGFR_data), main="EGFR versus IDH1", xlab="EGFR Expression", ylab="IDH1 Expression")

#Problem C
ZNF_LOC <- grep("ZNF", rownames(my_data))
ZNF_LOC

rownames(my_data)[ZNF_LOC]

ZNF_Vector <- as.vector(apply(my_data[ZNF_LOC,], 2, mean))
ALL_Data_Vector <- as.vector(apply(my_data, 2, mean))


boxplot(ZNF_Vector, ALL_Data_Vector, outline=FALSE, notch = TRUE, main="ZNF gene expression versus all genes", names=c("Average ZNF","Average of ALL Genes"), ylab="Expression")

##Functions

test=matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow=3, ncol=3)
test_2 <- as.vector(test)
test_2 <- as.vector(test_2)

min_func <- function(x){
  min_output <- x[1]
  min_loc_output <- 1
  i_loc <- 1
  for (i in x){
    if (i < min_output){
      min_output <-  i;
      min_loc_output <-  i_loc;
    }
    i_loc <- i_loc + 1
  }
  output <- c(min_output, min_loc_output)
  return(output)
}

test <- min_func(c(4,1,2,6,7,-9))
test

# swap_func <- function (x, y){
#   temp = x
#   x = y
#   y = temp
#   return(c(x, y))
# }
# 
# swap_func(5,2)

sort_func <- function(x){
  x_reshape <- as.vector(x)
  x_original_loc <- c(1:length(x_reshape))
  x_original_loc_2 <- c(1:length(x_reshape))
  
  for (i in x_original_loc){
    temp <- min_func(x_reshape[i:length(x_reshape)])
    
    # x_sorted[i] <- temp[1]
    # x_sorted_loc[i] <- temp[2] + i - 1
    
    temp2 <- x_reshape[i]
    x_reshape[i] <- x_reshape[i:length(x_reshape)][temp[2]]
    x_reshape[i:length(x_reshape)][temp[2]] <- temp2
    
    temp3 <- x_original_loc_2[i]
    x_original_loc_2[i] <- x_original_loc_2[i:length(x_reshape)][temp[2]]
    x_original_loc_2[i:length(x_reshape)][temp[2]] <- temp3
  }
  
  return (c(x_reshape, x_original_loc_2))
}

my_vector <- c(10,5,2,6,8,4,1,9,3,7);
print_out <- sort_func(my_vector)
print_out[1: (0.5 * length(print_out))]
print_out[(0.5 * length(print_out)+1) : length(print_out)]



sort_func(data.matrix(my_data))
