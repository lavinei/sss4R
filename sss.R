library(sets)

drop1 <- function(A){
  Map(function(x){cset_difference(A,x)},sapply(A,identity))
}
add1 <- function(Omega, A){
  Ac=sapply(cset_difference(Omega,A),identity)
  Map(function(x){cset_union(x,A)},Ac)
}
swap1 <- function(Omega,A){
  if(cset_is_empty(A)){
    list()
  }else{
    Ac=sapply(cset_difference(Omega,A),identity)
    Aless1=drop1(A)
    unlist(Map(function(y){Map(function(x){cset_union(x,y)},Ac)},Aless1),recursive=FALSE)
  }
}

neighbours <- function(Omega,A) list(dropped=drop1(A),added=add1(Omega,A),swapped=swap1(Omega,A))

nextmodel <- function(score,constraint,Omega,A){
  #Obtain neighbourhood sets "added", "dropped" and "swapped"
  neigh=neighbours(Omega,A)
  #filter empty neighbourhood sets
  neigh=neigh[sapply(neigh,function(x){!length(x)==0})]
  #filter infeasible sets
  neigh=Map(function(i){neigh[[i]][sapply(neigh[[i]],constraint)]},names(neigh)) #pheasible
  #filter empty neighbourhood sets
  neigh=neigh[sapply(neigh,function(x){!length(x)==0})]
  #Sample one model each from added,dropped,swapped so long as these are not empty
  candidates=unlist(Map(function(v){sample(v,size=1,prob=Map(score,v))},neigh),recursive=FALSE)
  #sample from this set of 3 models
  sample(candidates,size=1,prob=Map(score,candidates))[[1]]
}

addmap <- function(f){
  leaderboard=list()
  function(set){
    result <- f(set)
    leaderboard[[paste("Model:",toString(set))]]<<-result
    result
  }
}

sss <- function(objective,constraint,initialmodel,Omega,niter){
  score=addmap(objective)
  model=initialmodel
  for(i in seq(0,niter)) model=nextmodel(score,constraint,Omega,model)
  leaderboard=environment(score)$leaderboard
  sortedlb=leaderboard[sort(unlist(leaderboard),decreasing=TRUE,index.return=TRUE)$ix]
}

objective <- function(set){
  total=0
  if(cset_contains_element(set,1)) total=total+0.3
  if(cset_contains_element(set,2)) total=total-0.1
  if(cset_contains_element(set,3)) total=total+0.6
  if(cset_contains_element(set,4)) total=total-0.5
  pnorm(total,0,1)
}

constraint <- function(set){
  if(cset_contains_element(set,3)){
    TRUE
  }else{
    FALSE
  }
}


Omega=set(1,2,3,4,5,6)
A=set(1,2)
niter=100
sss(objective,constraint,A,Omega,niter)
