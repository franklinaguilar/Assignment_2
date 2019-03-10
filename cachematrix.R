## ESTA FUNCION CREA UNA UNA MATRIZ Y CAPTURA SU INVERSA.
## SI LA LA MATRIZ YA SE CALCULO, ENTONCES PARA.
## PERO SI NO LO HIZO, ENTONCES CAPTURA SU MATRIZ

## ESTA FUNCION CREA UNA MATRIZ QUE CAPTURA SU INVERSA
## (SIEMPRE Y CUANDO NO LA HALLA CAPTURADO PREVIAMENTE)
## SI LA LA MATRIZ YA SE CALCULO,
## ENTONCES DEVUELVE LA INVERSA
makeCacheMatrix <- function(x = matrix()) {
        
        # SON LOS INSUMOS DE LA LATRIZ
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # SE OBTIENE LOS INSUMOS
        get <- function() x
        
        # SE PREPARA LA INVERSA
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## ESTA FUNCION CREA UNA UNA MATRIZ Y CAPTURA SU INVERSA.
cacheSolve <- function(x, ...) {
        
        ## VERIFICACMOS QUE NO SE HAYA CAPTURADO
        ## LA INVERSA PREVIAMENTE
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # SI NO SE HA CALCULALO LA INVERSA PREVIAMENTE
        # ENTONCES SE CALCULA SU INVERSA
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
