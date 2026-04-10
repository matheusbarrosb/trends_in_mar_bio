extract_indices = function(parameter_name, par = NULL) {
  
  indices = gsub("pred\\[|\\]", "", parameter_name)
  
  return(strsplit(indices, ",")[[1]])
  
}
