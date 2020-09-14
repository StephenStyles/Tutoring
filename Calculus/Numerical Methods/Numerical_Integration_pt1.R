integral <- function(x){
  return(cos(x)/x)
}


trap <- function(a,b,n){
  delta = (b-a)/n
  list1 = seq(a,b,delta)
  list2 = c(1,rep(2,n-1),1)
  functionout = integral(list1)
  return(delta/2*(sum(functionout*list2)))
}

trap(1,5,100000)


simp <- function(a,b,n){
  if(n%%2 == 1){
    stop("n needs to be even")
  } else if(n == 2){
    list2 = c(1,4,1)
  } else{
    list2 =c(1,rep(c(4,2),(n-2)/2),4,1)
  }
  delta = (b-a)/n
  list1 = seq(a,b,delta)
  functionout = integral(list1)
  return(delta/3*(sum(functionout*list2)))
}

simp(1,5,10000)


mid <- function(a,b,n){
  delta = (b-a)/n
  midpoints = seq(a+0.5*delta,a+(n-0.5)*delta,delta)
  functionout = integral(midpoints)
  return(sum(functionout)*delta)
}

mid(1,5,10000)
