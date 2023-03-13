
remove_local_duplicates=function(vector1, vector2) {
  # build empty lists
  res1=c()
  res2=c()
  vector1=unlist(vector1)
  vector2=unlist(vector2)
  # see vector1
  for (i in 1:(length(vector1)-1)) {
    #if the next element is not the same as current,then put them in to results
    if (vector1[i] != vector1[i+1]) {
      res1=c(res1, vector1[i])
      res2=c(res2, vector2[i])
    }
  }
  # put the last element in to results
  res1=c(res1, vector1[length(vector1)])
  res2=c(res2, vector2[length(vector2)])
  # return results
  return(list(res1, res2))
}









