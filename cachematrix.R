




# makeCacheMatrix- creates a list of functions that will:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse





makeCacheMatrix <- function(x = matrix()) {

	#m is created as a blank holder
	m<-NULL
	
	#set is a function within a function that sets up the matrix
	set<-function(y){
		x<<-y
		m<<-NULL
			
	}

	#Gets the matrix
	get<-function() x
	
	#Sets the inverse
	setinverse<-function(inverse) m<<-inverse
	
	#Then gets the inverse
	getinverse <-function() m
	
	#Returns the matrix outside of the function
	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}



#cacheSolve- calculates the inverse of a matrix. If the inverse already exsists, it returns the cached inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #Get the inverse of the matrix
        m<-x$getinverse()
        
        # If the inverse already exists, then just return it
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }



    # If the inverse does not exsist yet, calculate it
    data <- x$get()
    m <- solve(data, ...)

    # Cache the inverse
    x$setinverse(m)

    # Return the inverse
    m
}

