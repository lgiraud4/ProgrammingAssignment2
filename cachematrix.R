  ## x une matrice carree inversible
  ## retourne une liste avec les fonctions
  ##               set  , get  , setinverse, getinverse
  ##         cette liste est un input à chacheSolve()

makeCacheMatrix <- function(x = matrix()) {
 
  inv1 <- NULL
  set <- function(y) {
    x <<- y
    inv1 <<- NULL
  }
  get<- function() x
  setinverse <- function(inverse) inv1 <<- inverse 
  getinverse <- function() inv1
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
  ## x: output de makeCacheMatrix()
  ## retourne: l'inverse de la matrice en input de makeCacheMatrix()
cacheSolve <- function(x, ...) {
  inv1 <- x$getinverse()
  
  # si deja calculé
  if (!is.null(inv1)){
    # prends le dans le cache et évite les calculs. 
    message("getting cached data")
    return(inv1)
  }
  
  # sinon calcul l'inverse
  matrice.data <- x$get()
  inv1 <- solve(matrice.data, ...)
  
  #met la valeur dans la liste initiale: en "cache" via la fonction setinverse 
  
  x$setinverse(inv1)
  
  return(inv1)
}

