#Description: Split FTS flow rows by a chosen column
#Original Author: Dan Walton
#Creation date: Feb 2021
#Modified by: Alex Miller
#Last revision: Jan 2025
#Notes: value.cols will be split evenly across splits

fts_split_rows <- function(data, value.cols = "amountUSD", split.col = "destinationObjects_UsageYear.name", split.pattern = "; ", remove.unsplit = T){
  split.pattern <- trimws(split.pattern)
  temp <- data[, .(trimws(unlist(strsplit(as.character(get(split.col)), split.pattern))), as.numeric(get(value.cols))/(1+ifelse(is.na(get(split.col)), 0, nchar(get(split.col))-nchar(gsub(split.pattern, "", get(split.col)))))), by=list(rownames(data))]
  if(remove.unsplit){
    names(temp) <- c("rownames", split.col, value.cols)
    data[, (split.col) := NULL]
    data[, (value.cols) := NULL]
  } else {
    names(temp) <- c("rownames", paste0(split.col, ".split"), paste0(value.cols, ".split"))
  }
  data <- merge(data[, rownames := rownames(data)], temp, by = "rownames")
  data[, rownames := NULL]
  return(data)
}
