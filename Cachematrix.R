#Creating a cache variable for matrix inversion

##makeCacheMatrix will create a "special" matrix that returns a list containing the functions to set and visualize the vector and its inverse

makeCacheMatrix<-function(x=matrix()){
        m<-NULL
        set <-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setinv<-function(inv) m<<-inv
        getinv<-function() m
        list(set=set,get=get,setinv=setinv,getinv=getinv)
}

##cacheSolve computes the inverse of the special matrix, if not already avaiable, or retrieves the cached value of the inverse

cacheSolve<-function(x,...){
        m<-x$getinv()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
                
}