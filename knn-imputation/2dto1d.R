.dist.2dto1d = function(i,j,n) {
  ret = rep(0, length(j))
  j.larger.indices = which(j > i)
  j.smaller.indices = which(j < i)
  if (length(j.larger.indices) > 0) {
    j.larger = j[j.larger.indices]
    ret[j.larger.indices] = (i-1)*n - i^2/2 + j.larger - i/2
  }
  if (length(j.smaller.indices) > 0) {
    j.smaller = j[j.smaller.indices]
    ret[j.smaller.indices] = (j.smaller-1)*n - j.smaller^2/2 + i - j.smaller/2
  }
  ret
}