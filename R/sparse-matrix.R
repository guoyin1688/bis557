#' Construct a sparse.matrix object
#' 
#' @description This function constructs a sparse.matrix object. 
#' @param i row indices of non-zero elements
#' @param j column indices of non-zero elements
#' @param x values of non-zero elements
#' @param dims dimension of the sparse matrix
#' @return a sparse.matrix object
#' @export 
sparse.matrix <- function(i, j, x, dims = c(max(i), max(j))) {
  if (length(i) != length(j) || length(j) != length(x))
    stop("Incorrect dimensions.")
  
  s <- order(i) 
  i <- i[s]
  j <- j[s]
  x <- x[s]
  
  sm <- list(mat = data.frame(i = i, j = j, x = x), dims = dims)
  class(sm) <- "sparse.matrix"
  sm
}

#' Add sparse matrices
#' 
#' @description This function adds two sparse matrices.  
#' @param a a sparse.matrix object
#' @param b a sparse.matrix object
#' @return a sparse.matrix object
#' @examples
#' a <- sparse.matrix(i = c(1, 2), j = c(1, 3), x = c(3, 1))
#' b <- sparse.matrix(i = c(1, 2), j = c(3, 1), x = c(4.4, 1.2))
#' sparse_add(a, b)
#' @export 
sparse_add <- function(a, b) {
  if (!inherits(b, "sparse.matrix"))
    stop ("b is not a sparse.matrix type.")
  
  if (any(a$dims != b$dims) == TRUE)
    stop ("Incorrect dimensions for addition.")
  
  amat <- a$mat
  bmat <- b$mat
  
  c <- merge(amat, bmat, by = c("i", "j"), all = TRUE, suffixes = c("1", "2"))
  c$x1[is.na(c$x1)] <- 0
  c$x2[is.na(c$x2)] <- 0
  c$x <- c$x1 + c$x2
  c <- c[, c("i", "j", "x")]
  
  sm <- list(mat = c, dims = a$dims) 
  class(sm) <- "sparse.matrix" 
  sm
}

#' Multiply sparse matrices
#' 
#' @description This function multiplies two sparse matrices.  
#' @param a a sparse.matrix object
#' @param b a sparse.matrix object
#' @return a sparse.matrix object
#' @examples
#' a <- sparse.matrix(i = c(1, 2), j = c(1, 3), x = c(3, 1))
#' b <- sparse.matrix(i = c(1, 3), j = c(2, 1), x = c(4.4, 1.2))
#' sparse_multiply(a, b)
#' @export
sparse_multiply <- function(a, b) {
  if (!inherits(b, "sparse.matrix"))
    stop ("b is not a sparse.matrix type.")
  
  if (a$dims[2] != b$dims[1])
    stop ("Incorrect dimensions for multiplication.")
  
  amat <- a$mat
  bmat <- b$mat
  
  colnames(bmat) <- c("i2", "j2", "x2")
  c <- merge(amat, bmat, by.x = "j", by.y = "i2", all = FALSE, 
             suffixes = c("1", "2"))
  c$x <- c$x * c$x2
  c$key <- paste(c$i, c$j2, sep = "-")
  x <- tapply(c$x, c$key, sum)
  key <- strsplit(names(x), "-")
  d <- data.frame(i = sapply(key, getElement, 1),
                  j = sapply(key, getElement, 2),
                  x = as.numeric(x))
  d$i <- as.numeric(d$i)
  d$j <- as.numeric(d$j)
  
  sm <- list(mat = d, dims = c(a$dims[1], b$dims[2])) 
  class(sm) <- "sparse.matrix" 
  sm
}

#' Transpose a sparse matrix
#' 
#' @description This function transposes a sparse matrix.  
#' @param a a sparse.matrix object
#' @return a sparse.matrix object
#' @examples
#' a <- sparse.matrix(i = c(1, 2), j = c(1, 3), x = c(3, 1))
#' sparse_transpose(a)
#' @export 
sparse_transpose <- function(a) {
  mat <- a$mat
  
  tmp <- mat$i
  mat$i <- mat$j
  mat$j <- tmp
  
  sm <- list(mat = mat, dims = c(a$dims[2], a$dims[1])) 
  class(sm) <- "sparse.matrix" 
  sm
}

`+.sparse.matrix` <- function(x, y) {
  sparse_add(x, y)
}

`%*%.default` <- .Primitive("%*%")
`%*%` <- function(x, y) {
  UseMethod("%*%", x)
}
`%*%.sparse.matrix` <- function(x, y) {
  sparse_multiply(x, y)
}

`t.sparse.matrix` <- function(x) {
  sparse_transpose(x)
}