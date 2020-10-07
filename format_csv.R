library(jsonlite)
library(dplyr)
new_cards <- read.csv('FA.csv')

cards <- lapply(1:nrow(new_cards), function(i){
  row <- new_cards[i,]
  # turn dice sides into a list
  if(!is.na(row$side1)){
    sides <- list(row$side1, row$side2, row$side3,
                  row$side4, row$side5, row$side6)
  } else {
    sides <- NA
  }
  # turn subtypes into a list
  if(!is.na(row$subtype1)){
    subtypes <- list(row$subtype1, row$subtype2, row$subtype3)
    subtypes <- subtypes[!is.na(subtypes)]
  } else {
    subtypes <- NA
  }

  # turn remained of columns into a list
  cols <- lapply(1:ncol(new_cards), function(j){
    cell_value <- row[,j]
    gsub('^ ','',cell_value)
  })
  names(cols) <- colnames(new_cards)
  
  # overwrite side1 and subtype1 with side and subtype lists
  names(cols)[names(cols)=='side1'] <- 'sides'
  names(cols)[names(cols)=='subtype1'] <- 'subtypes'
  cols$sides <- sides
  cols$subtypes <- subtypes
  # remove all the extra side and subtype items
  cols <- cols[!grepl('[0-9]',names(cols))]
  cols <- cols[!grepl('BoFpoints', names(cols))]
  cols <- cols[!is.na(cols)]
  cols <- cols[order(names(cols))]
  # return the finished list
  if('deck_limit' %in% names(cols)) if(!is.na(cols$deck_limit)) cols$deck_limit <- as.numeric(cols$deck_limit)
  if('health' %in% names(cols)) if(!is.na(cols$health)) cols$health <- as.numeric(cols$health)
  if('position' %in% names(cols)) if(!is.na(cols$position)) cols$position <- as.numeric(cols$position)
  if('cost' %in% names(cols)) if(!is.na(cols$cost)) cols$cost <- as.numeric(cols$cost)
  cols
})

json_file <- jsonlite::toJSON(cards, auto_unbox = TRUE) %>% prettify()
write(gsub('<\\\\','<\\',json_file) %>% gsub('\\\\n','\\n',.) %>% gsub('\\\\"','',.) %>%
        gsub('"FALSE"','false',.) %>% gsub('"TRUE"','true',.),
      file = 'FA.json')