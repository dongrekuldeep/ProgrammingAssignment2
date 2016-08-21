## makeCacheMatrix expects a square matrix input 
## it caches the input and the inverse of the matrix 
## Inverse of the matrix is computed using solve function


makeCacheMatrix <- function(gsqrMatrix = matrix()) {
  
  # check if the matrix input is a square matrix
  if(!nrow(gsqrMatrix) == ncol(gsqrMatrix)){
    print ("Expect a square matrix")
    return()
  }

  #store the sqr matrix locally (cache)
  cacheMat <<- gsqrMatrix
  
  # compute and cache the inverse of the matrix
  gInvMatrix <<- solve(gsqrMatrix)

}


# cacheSolve computes the inverse of a matrix using solve function.
# it first checks if the inverse is already computed in the cache 
# in cacheMatrix. 
# If the matrix input and matrix for which the inverse is required 
# are different, then it recomputes the inverse.
# if the inverse is already computed it extracts it from the cache.
# the interactive print statements indicate whether the inverse is from cache
# or is recomputed.

cacheSolve <- function(ginsqrMatrix = matrix()){
  
  # check if the sqr matrix is the same that we've calculated the inv for.
  
  if (identical(cacheMat,ginsqrMatrix) && dim(cacheMat) == dim(ginsqrMatrix)) {
    # get the matrix inverse from the cache
    print ("Inverse from cache..")
    gInvMatrix
  } else {
    # compute the inverse anyways
    if(!nrow(ginsqrMatrix) == ncol(ginsqrMatrix)){
      print ("Expect a square matrix")
      return()
    } 
    
    # compute and cache the inverse of the matrix
    # print 
    print("calculations done again as matrix is different from cached matrix")
    ginInvMatrix <<- solve(ginsqrMatrix)
    ginInvMatrix
  }
  
}

# the testing of use cases is done below using two cases.

# Case 1 - Illustrates the use of cache to show sourcing inverse from cache

# Showing the use of cached inverse being used and recomputation not done
set.seed(10)
inmat <- matrix(data=runif(9,10,23),nrow=3, ncol=3)
makeCacheMatrix(inmat)
cacheSolve(inmat)

# Case 2 - Illustrate the recomputation of a matrix inverse as cache cannot be used 

# Illustration to show that inverse computation is done again not using cache
set.seed(20)
inmat <- matrix(data=runif(9,20,23),nrow=3, ncol=3)
inmat2 <- matrix(data=rnorm(9),nrow=3,ncol=3)
makeCacheMatrix(inmat)
cacheSolve(inmat2)


