list.of.packages <- c("XML", "data.table", "httr", "jsonlite")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)


####OECD SDMX-JSON function####
OECD <- function(url){
  rawContent <- content(GET(url))
  rawData <- rawContent$dataSets[[1]]$observations
  rawStructure <- rawContent$structure
  dimensions <- rawStructure$dimensions$observation
  attributes <- rawStructure$attributes$observation
  datnames <- c(sapply(dimensions, "[[", 2),sapply(attributes, "[[", 1),"value")
  ndim <- length(sapply(dimensions, "[[", 2))
  natt <- length(sapply(attributes, "[[", 1))
  ncol <- ndim+natt
  data <- matrix(ncol=ncol+1,nrow=length(rawData))
  for(i in 1:length(rawData)){
    row <- rawData[i]
    rawDimensions <- names(row)
    splitDimensions <- strsplit(rawDimensions,":")[[1]]
    # Dimensions
    for(j in 1:ndim){
      dimensionReference <- dimensions[[j]]$values
      dimensionIndex <- as.integer(splitDimensions[j])+1
      dimensionValue <- dimensionReference[[dimensionIndex]][[2]]
      data[i,j] <- dimensionValue
    }
    # Attributes
    for(j in 1:natt){
      attributeReference <- attributes[[j]]$values
      rawAttIndex <- row[[1]][[j+1]]
      if(is.null(rawAttIndex)){
        attributeValue <- NA
      }else{
        attributeIndex <- as.integer(rawAttIndex+1)
        attributeValue <- attributeReference[[attributeIndex]][[2]]
      }
      data[i,ndim+j] <- attributeValue
    }
    # Observations
    obs = unlist(row[[1]][[1]])
    data[i,ncol:ncol+1] = obs
  }
  data <- setNames(data.frame(data,stringsAsFactors=FALSE),datnames)
  names(data)[which(names(data)=="Year")] <- "obsTime"
  return(data)
}
