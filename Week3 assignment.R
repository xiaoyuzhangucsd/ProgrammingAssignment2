makeCacheMatrix<-function(m=matrix()){
    i<-NULL
  set<-function(f){
          m<<-f
          i<<-NULL
  }
  get<-function()m
  setinverse<-function(inverse)i<<-inverse
  getinverse<-function()i
  list(set=set,
       get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}

cacheSolve<-function(m,...){
  i<-m$getinverse()
  if(!is.null(i)){
      message("getting cached data")
      return(i)
  }
  data<-m$get()
  i<-solve(data,...)
  m$setinverse(i)
  i
}
