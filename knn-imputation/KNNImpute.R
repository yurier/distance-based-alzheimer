kNNImpute = function(x, k, x.dist = NULL, impute.fn, verbose=T) {
  if (!is.matrix(x)) stop("x should be a numeric data matrix")
  if(k >= nrow(x)) stop("k must be less than the number of rows in x")
  
  prelim = impute.prelim(x)
  if (prelim$numMissing == 0) return (x)
  missing.matrix = prelim$missing.matrix
  x.missing = prelim$x.missing
  missing.rows.indices = prelim$missing.rows.indices
  
  if (missing(impute.fn)) 
    impute.fn = function(values, distances, k) {
      ranks = order(distances)
      smallest.distances = distances[ranks]
      #values corresponding to smallest distances
      knn.values = values[ranks][1:k]
      knn.weights = 1 - (smallest.distances / max(distances)) [1:k]
      weighted.mean(knn.values, knn.weights)
    }
  
  if (verbose) print("Computing distance matrix...")
  if (is.null(x.dist)) x.dist = dist(x)
  if (verbose) print("Distance matrix complete")
  
  x.missing.imputed = t(apply(x.missing, 1, function(i) {
    rowIndex = as.numeric(i[1])
    i.original = unlist(i[-1])
    if(verbose) print(paste("Imputing row", rowIndex,sep=" "))
    missing.cols = which(missing.matrix[rowIndex,])
    if(length(missing.cols) == ncol(x))
      warning( paste("Row",rowIndex,"is completely missing",sep=" ") )
    imputed.values = sapply(missing.cols, function(j) {
      #find neighbors that have data on the jth column
      neighbor.indices = which(!missing.matrix[,j])
      #lookup the distance to these neighbors
      #order the neighbors to find the closest ones
      if (!is.null(x.dist)) {
        indices.1d = .dist.2dto1d(rowIndex, neighbor.indices, nrow(x))
        knn.dist = x.dist[indices.1d]
      }
      else knn.dist = pdist(x, indices.A = rowIndex,
                            indices.B = neighbor.indices)@dist
      impute.fn(x[neighbor.indices,j], knn.dist, k)
    })
    i.original[missing.cols] = imputed.values
    i.original
  }))
  x[missing.rows.indices,] = x.missing.imputed
  
  #Things that were not able to be imputed are set to 0
  missing.matrix2 = is.na(x)
  x[missing.matrix2] = 0
  
  return (list(
    x=x,
    missing.matrix=missing.matrix
  ))
}