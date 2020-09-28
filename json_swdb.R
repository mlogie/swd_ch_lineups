getwd()
# First off, clone the github swdestinydb repository to your working directory
# Repository is here:
#   https://github.com/fafranco82/swdestinydb-json-data
# Then set that location of the set folder here
list.files()
swdb <- 'C:/Users/marlog/Documents/swdestinydb-json-data-ml/set'
files <- list.files(swdb,full.names = TRUE)

# Run this is packages not yet installed:
# install.packages(c('rjson','dplyr','pbapply'))
library(rjson)
library(dplyr)
library(pbapply)
calclineups <- TRUE
# This is my deck target
point_target <- 30
# Here's your multiplier dataframe, to value various sides
multiplier <- data.frame(sides = c('MD','RD','ID','F','Dr',
                                   'Sh','Dc','R','Sp','-'),
                         values = c(1,1,.7,1,.5,
                                    .8,.5,2,2,0))
# Here's what you multiply modifier sides by to devalue them
modifier_down <- .7

# Here's my sets of interest, subset files to these sets
setofinterest <- c('AoN','CONV','CM','SoH','TR')
setskeeping <- lapply(basename(files), FUN = function(fi){
  any(lapply(setofinterest, FUN = function(soi){grepl(soi,fi)}) %>% unlist())
}) %>% unlist()
files <- files[setskeeping]

# Here's a function to calculate the values of all characters in a
# swdb json file
ch_values <- pblapply(files, FUN = function(file){
  result <- fromJSON(file = file)
  # Subset to characters only
  result <-
    result[lapply(result, function(r){r$type_code=='character'}) %>% unlist()]

  # result is a list of json objects, each of which is a card from the set
  values <- lapply(result, FUN = function(r){
    if(r$has_die){
      # We're looking at a character with a die, so we can assign a value to it
      value <- lapply(1:6,FUN = function(side){
        s <- r$sides[side]
        # If it is a modifier, remove the + to get the value (we'll downgrade it later)
        s <- ifelse(grepl('\\+',s),substr(s,2,nchar(s)),s)
        # Work out what multiplier we need for each side value
        m <- multiplier$values[which(stringr::str_replace_all(s,'[0-9]+','') == multiplier$sides)]
        # Work out the side values as numeric values
        v <- stringr::str_extract(s,'^[0-9]+') %>% as.numeric()
        # Multiply multiplier by side value (or 0 if this can't be done)
        vm <- ifelse(!is.na(v*m),v*m,0)
        # Apply special value to special sides
        vm <- ifelse(grepl('Sp',r$sides[side]),multiplier$values[multiplier$sides=='Sp'],vm)
          if(!is.na(stringr::str_extract(s,'[0-9]+$'))){
          # The side is a pay side. We'll have to downgrade its value.
          # Work out how much it costs to resolve,
          # and subtract that * the value of a resource side
          vm <- vm-(((stringr::str_extract(s,'[0-9]+$')) %>% as.numeric())*
                    multiplier$values[multiplier$sides=='R'])
        }
        if(grepl('\\+',r$sides[side])){
          # Downgrade side as it's a modifier side
          vm <- vm*modifier_down
        }
        vm
      }) %>% unlist() %>% sum()
      # 'value' is now the sum of the 'values' of all the die sides
      return <- data.frame(a = substr(r$affiliation_code,1,1),
                           set = basename(file) %>% substr(1,nchar(.)-5),
                           code = r$code, character = r$name, points = r$points %>% as.character(),
                           health = r$health, unique = r$is_unique,
                           side1 = r$sides[1], side2 = r$sides[2], side3 = r$sides[3],
                           side4 = r$sides[4], side5 = r$sides[5], side6 = r$sides[6],
                           value = value,
                           stringsAsFactors = FALSE)
    } else {
      # Diceless character.  Just give it a value of 5.
      return <- data.frame(a = substr(r$affiliation_code,1,1),
                           set = basename(file) %>% substr(1,nchar(.)-5),
                           code = r$code, character = r$name, points = r$points %>% as.character(),
                           health = r$health, unique = r$is_unique,
                           side1 = '', side2 = '', side3 = '',
                           side4 = '', side5 = '', side6 = '',
                           value = 5,
                           stringsAsFactors = FALSE)
    }
    # Return the values dataframe
    return(return)
  }) %>% bind_rows()
  # Return the values dataframe
  return(values)
}) %>% bind_rows()

# Combine with BotF data - this is a bit tricky - we're using Infinite Holocron here
balance <- fromJSON(file = file.path(swdb,'../formats.json'))
inf <- balance[[3]]
balance <- inf$data$balance
balance <- data.frame(code = names(balance),
                      points = lapply(balance, FUN = function(b){b}) %>% unlist())
# Merge our balance of the force with the values dataframe
ch_values <- merge(ch_values, balance, by.x = 'code', by.y = 'code', all.x = TRUE)
ch_values$points <- ch_values$points.x
# For all characters where there is a BoF value, overwrite the old value
ch_values$points[!is.na(ch_values$points.y)] <-
  ch_values$points.y[!is.na(ch_values$points.y)] %>% as.character()
# Remove the placeholder merge columns
ch_values <- ch_values[,!(names(ch_values) %in% c('points.x','points.y'))]

# Find eValues (points and value)
ch_evalues <- ch_values[grepl('\\/',ch_values$points),]
ch_evalues$points <- stringr::str_extract(ch_evalues$points,'[0-9]+$') %>% as.numeric()
# Double the points for eCharacters (as two dice)
ch_evalues$value <- ch_evalues$value*2
ch_evalues$character <- paste0('e',ch_evalues$character)
# Turn original dataframe into a non-elite dataframe
ch_values$points <- stringr::str_extract(ch_values$points,'^[0-9]+') %>% as.numeric()
# Bind
ch_values <- bind_rows(ch_values, ch_evalues)

# Increase values of some characters that have abilities that are worth something.
# This list is currently pretty arbitrary
ch_values$value[grepl('Kylo Ren',ch_values$character)&ch_values$set=='TPG'] <- 
  ch_values$value[grepl('Kylo Ren',ch_values$character)&ch_values$set=='TPG'] + 6
ch_values$value[grepl('Snoke',ch_values$character)&ch_values$set=='WotF'] <- 
  ch_values$value[grepl('Snoke',ch_values$character)&ch_values$set=='WotF'] + 4
ch_values$value[grepl('Cassian',ch_values$character)&ch_values$set=='WotF'] <- 
  ch_values$value[grepl('Cassian',ch_values$character)&ch_values$set=='WotF'] + 5*multiplier$values[multiplier$sides=='ID']
ch_values$value[grepl('eCassian',ch_values$character)&ch_values$set=='WotF'] <- 
  ch_values$value[grepl('eCassian',ch_values$character)&ch_values$set=='WotF'] + 5*multiplier$values[multiplier$sides=='ID']
ch_values$value[grepl('Yoda',ch_values$character)&ch_values$set=='LEG'] <- 
  ch_values$value[grepl('Yoda',ch_values$character)&ch_values$set=='LEG'] + 2
ch_values$value[grepl('eYoda',ch_values$character)&ch_values$set=='LEG'] <- 
  ch_values$value[grepl('eYoda',ch_values$character)&ch_values$set=='LEG'] + 4
ch_values$value[grepl('Jabba',ch_values$character)&ch_values$set=='WotF'] <- 
  ch_values$value[grepl('Jabba',ch_values$character)&ch_values$set=='WotF'] - 1
ch_values$value[grepl('Anakin',ch_values$character)&ch_values$set=='Tr'] <- 
  ch_values$value[grepl('Anakin',ch_values$character)&ch_values$set=='Tr'] + 1

# Calculate 'relative value' = value/points
ch_values$rel_value <- (ch_values$value/ch_values$points) %>% round(2)
# Multiply this value by health
ch_values$health_adj_value <- (ch_values$rel_value*ch_values$health) %>% round(2)
#ch_values <- ch_values[!(ch_values$set %in% c('AW','SoR','EaW')),]

ch_values$rel_rank <- NA
ch_values$health_adj_rank <- NA
# Rank the characters by these values
ch_values$rel_rank[order(ch_values$rel_value,ch_values$health_adj_value)] <- nrow(ch_values):1
ch_values$health_adj_rank[order(ch_values$health_adj_value,ch_values$rel_value)] <- nrow(ch_values):1
ch_values$rank <- ch_values$rel_rank+ch_values$health_adj_rank
(ch_values %>% arrange(-rel_value))[1:20,-1]
(ch_values %>% arrange(-health_adj_value))[1:20,-1]
(ch_values %>% arrange(rank))[1:20,-1]

# set ranks
set_ranks <- lapply(unique(ch_values$set), FUN = function(s){
  tmp_df <- ch_values %>% filter(set == s)
  tmp_df <- tmp_df[!grepl('^[e]',tmp_df$character),]
  top10 <- tmp_df[tmp_df$health_adj_rank %in% (tmp_df$health_adj_rank %>% sort())[1:8],]
  data.frame(set = s, av_rank = sum(tmp_df$health_adj_rank)/nrow(tmp_df), av_pwr = sum(tmp_df$health_adj_value)/nrow(tmp_df),
             av_rank_10 = sum(top10$health_adj_rank)/nrow(top10), av_pwr_10 = sum(top10$health_adj_value)/nrow(top10),
             stringsAsFactors = FALSE)
}) %>% bind_rows()
set_ranks %>% arrange(av_rank)
set_ranks %>% arrange(av_rank_10)
ch_values %>% filter(set == 'CONV') %>% arrange(health_adj_rank)

if(calclineups){
  # This is a mission: create all possible lineups and score them!  Long winded
  all_lineups <- lapply(c('v','h'), FUN = function(aff){
    # Create temp dataframe of all characters != to affiliation (to include neutrals)
    df <- ch_values[ch_values$a!=aff,]
    cat('Affiliation:',ifelse(aff=='v','Hero','Villain'),'\n')
    pblapply(1:nrow(df), FUN = function(single){
      tmp_df <- df[(1:nrow(df))>=ifelse(df$unique,single+1,single),]
      # remove characters that plus our currents character are great than 30
      tmp_df <- tmp_df[tmp_df$points<=(point_target-df$points[single]),]
      # Remove the character with a name equal to eThis Character or vice versa to prevent e/non-e repeats
      tmp_df <- tmp_df[tmp_df$character!=substr(tmp_df$character[single],2,nchar(tmp_df$character[single])),]
      tmp_df <- tmp_df[tmp_df$character!=paste0('e',tmp_df$character[single]),]
      if(nrow(tmp_df)>0){
        pairs <- lapply(1:nrow(tmp_df), FUN = function(second){
          tmptmp <- tmp_df[(1:nrow(tmp_df))>=ifelse(tmp_df$unique,second+1,second),]
          # Remove impossible three wide partners and e and non-e copies of character 2
          tmptmp <- tmptmp[tmptmp$points<=(point_target-df$points[single]-tmp_df$points[second]),]
          tmptmp <- tmptmp[tmptmp$character!=substr(tmptmp$character[second],2,nchar(tmptmp$character[second])),]
          tmptmp <- tmptmp[tmptmp$character!=paste0('e',tmp_df$character[second]),]
          if(nrow(tmptmp)>0){
            triples <- lapply(1:nrow(tmptmp), FUN = function(third){
              data.frame(lineup = paste0(df$character[single],'-',tmp_df$character[second],'-',tmptmp$character[third]),
                         points = (df$points[single]+tmp_df$points[second]+tmptmp$points[third]),
                         value = (df$value[single]+tmp_df$value[second]+tmptmp$value[third]),
                         health = (df$health[single]+tmp_df$health[second]+tmptmp$health[third]),
                         healthvalue = (df$value[single]*df$health[single]+tmp_df$value[second]*tmp_df$health[second]+tmptmp$value[third]*tmptmp$health[third]),
                         type = 'three-wide',
                         stringsAsFactors = FALSE)
            }) %>% bind_rows()
          } else {
            triples <- NULL
          }
          bind_rows(data.frame(lineup = paste0(df$character[single],'-',tmp_df$character[second]),
                               points = (df$points[single]+tmp_df$points[second]),
                               value = (df$value[single]+tmp_df$value[second]),
                               health = (df$health[single]+tmp_df$health[second]),
                               healthvalue = (df$value[single]*df$health[single]+tmp_df$health[second]*tmp_df$value[second]),
                               type = 'two-wide',
                               stringsAsFactors = FALSE),
                    triples)
        }) %>% bind_rows()
      } else {
        pairs <- NULL
      }
      pairs
    }) %>% bind_rows()
  })

  names(all_lineups) <- c('hero','villain')
  all_lineups$hero <- all_lineups$hero[all_lineups$hero$points>24,]
  all_lineups$hero$healthvalue[grepl('3PO',all_lineups$hero$lineup)&grepl('R2',all_lineups$hero$lineup)] <- 
    all_lineups$hero$healthvalue[grepl('3PO',all_lineups$hero$lineup)&grepl('R2',all_lineups$hero$lineup)] + 6*8
  all_lineups$villain <- all_lineups$villain[all_lineups$villain$points>24,]
  #AL <- all_lineups$villain
  all_lineups$hero$affiliation <- 'hero'
  all_lineups$villain$affiliation <- 'villain'
  AL <- bind_rows(all_lineups$hero,all_lineups$villain)
  names(AL)[names(AL)=='healthvalue'] <- 'score'
  
  top100_all <- (AL %>% arrange(-score))[1:100,]
  top100_twowide <- (AL %>% arrange(-score) %>% filter(type == 'two-wide'))[1:100,]
  top100_threewide_adj <- (AL %>% arrange(-score) %>% filter(type == 'three-wide'))[1:500,]
  top100_twowide_adj <- (AL %>% arrange(-score) %>% filter(type == 'two-wide'))[1:100,]
  write.csv(top100_twowide_adj[,!(names(top100_twowide_adj) %in% c('value','type'))],
            file.path(swdb,'../../twowide.csv'))
  write.csv(top100_threewide_adj[,!(names(top100_threewide_adj) %in% c('value','type'))],
            file.path(swdb,'../../threewide.csv'))
  write.csv(top100_all[,!(names(top100_threewide_adj) %in% c('value','type'))],
            file.path(swdb,'../../allwide.csv'))
}