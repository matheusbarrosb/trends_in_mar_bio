split_at_intervals = function(vec, intervals) {
  split_vec = list()
  start = 1
  
  for (i in seq_along(intervals)) {
    end = start + intervals[i] - 1
    if (end > length(vec)) {
      end = length(vec)
    }
    split_vec[[i]] = vec[start:end]
    start = end + 1
  }
  
  split_vec
}
