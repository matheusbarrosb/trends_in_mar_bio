irregular_bin_mean = function(vec, intervals, na.rm = TRUE) {
  means = numeric(length(intervals))
  start = 1
  
  for (i in seq_along(intervals)) {
    end = start + intervals[i] - 1
    if (end > length(vec)) {
      end = length(vec)
    }
    means[i] = mean(vec[start:end], na.rm = na.rm)
    start = end + 1
  }
  
  means
}
