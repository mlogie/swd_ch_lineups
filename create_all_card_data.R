swdb <- 'C:/Users/marlog/Documents/swdestinydb-json-data-ml/set'
library(rjson)
library(dplyr)
library(pbapply)
files <- list.files(swdb,full.names = TRUE)

allcards <- pblapply(files, FUN = function(file){
  cards <- fromJSON(file = file)
  lapply(cards, FUN = function(card){
    cd <- data.frame(card, stringsAsFactors = FALSE)
    if('subtypes' %in% names(card)){
      cd$subtype1 <- ifelse(length(card$subtypes)>0,cd$subtypes[1],NA)
      cd$subtype2 <- ifelse(length(card$subtypes)>1,cd$subtypes[2],NA)
      cd$subtype3 <- ifelse(length(card$subtypes)>2,cd$subtypes[3],NA)
      cd$subtype4 <- ifelse(length(card$subtypes)>3,cd$subtypes[4],NA)
      cd <- cd %>% select(-subtypes)
    }
    if(card$has_die){
      cd$side1 <- cd$sides[1]
      cd$side2 <- cd$sides[2]
      cd$side3 <- cd$sides[3]
      cd$side4 <- cd$sides[4]
      cd$side5 <- cd$sides[5]
      cd$side6 <- cd$sides[6]
      cd <- cd %>% select(-sides)
    }
    cd <- cd[1,]
    cd
  }) %>% bind_rows()
}) %>% bind_rows()

# Combine with BotF data
balance <- fromJSON(file = file.path(swdb,'../formats.json'))
inf <- balance[[3]]
balance <- inf$data$balance
balance <- data.frame(code = names(balance),
                      BoFpoints = lapply(balance, FUN = function(b){b}) %>% unlist(),
                      stringsAsFactors = FALSE)
allcards <- merge(allcards, balance, by.x = 'code', by.y = 'code', all.x = TRUE)
allcards$BoFpoints[is.na(allcards$BoFpoints)] <- allcards$points[is.na(allcards$BoFpoints)]

firstnames <- c('code', 'affiliation_code', 'set_code', 'name', 'subtitle', 'type_code', 'points', 'BoFpoints',
                'cost', 'health', 'side1', 'side2', 'side3', 'side4', 'side5', 'side6', 'faction_code', 'text')
allcards <- allcards %>% select(all_of(firstnames), names(allcards)[!(names(allcards) %in% firstnames)])
allcards$points[grepl('/',allcards$points)] <- paste0(' ', allcards$points[grepl('/',allcards$points)])
allcards$BoFpoints[grepl('/',allcards$BoFpoints)] <- 
  paste0(' ', allcards$BoFpoints[grepl('/',allcards$BoFpoints)])
allcards <- allcards %>% write.csv('allSWD.csv')
getwd()
