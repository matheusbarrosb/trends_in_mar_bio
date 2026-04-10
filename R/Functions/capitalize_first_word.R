capitalize_first_word = function(species) {
  words = strsplit(species, " ")[[1]]
  words[1] = paste(toupper(substring(words[1], 1, 1)), tolower(substring(words[1], 2)), sep = "")
  if (length(words) > 1) {
    words[2:length(words)] = tolower(words[2:length(words)])
  }
  paste(words, collapse = " ")
}