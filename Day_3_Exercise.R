d <- list(one=1, two=2, three=3, four=4, new=list(name='John'))
str(d)
(unlist(d))

runif(10)
str(rbind(runif(10),runif(10),runif(10)))

n.genes <- 5; n.samples <- 10
x <- lapply(1:n.genes, function(i) runif(n.samples))
str(x)
str(do.call(rbind, lapply(n.genes, function(i) runif(n.samples))))

set.seed(10)  #control state of random number generator
runif(5)

set.seed(0)
m <- do.call(rbind, lapply(1:n.genes, function(i) runif(n.samples)))
str(m)
unlist(lapply(1:ncol(m),function(i) mean(m[,i])))

1:nrow(m) %% 2== 0
ifelse(1:nrow(m) %% 2 == 0, "even", "odd")
ifelse(1:nrow(m) %% 2 == 0, paste('even', 1:nrow(m)), "odd")





##speeding up by parallel

rpois(n=10, lambda=500)  #poison distribution
1024 %% 1000
n.rounds <- 500
x <- lapply(1:n.rounds, function(i) {sum(rpois(n=1e4, lambda=500)) %% 1000})
str(unlist(x))

system.time(x <- lapply(1:n.rounds, function(i) {sum(rpois(n=1e4, lambda=500)) %% 1000}))

library(parallel)
system.time(x <- mclapply(1:n.rounds, function(i) {sum(rpois(n=1e4, lambda=500)) %% 1000}, mc.cores=5))


#function variable scope

x <- 0

counter <- function(){
  x <<- x+1
  x
}

counter()
counter()

x <- 0
counter <- function() {
  current.env <- environment()
  e <- parent.env(current.env);
  assign('x',x+1,envir=e)
  get('x',envir=e)
}
counter()
counter()

##debugging

traceback()
options(error=dump.frames)



#recurssion

k <- 10; a <- 3
sum(a + 0:k)
#sanity check
(k+1)*(a + a+k)/2


f <- function(k, a=3){
  if(k==0){
    return(a)
  }else{
    a+k + f(k-1, a=a)
  }
}

f(k)

simpleSum <- function (x,y){x+y}



##Alignment




