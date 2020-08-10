
##### DATA TYPES


sales <- data.frame(order_id = c(1,2,3,4,5,6),
                    revenue = c("5,454","5,668","4,062","3,936","1,067","6,635"),
                    quantity = c(494,334,259,15,307,235))

library(dplyr)
glimpse(sales)
#$ order_id <dbl> 1, 2, 3, 4, 5, 6
#$ revenue  <chr> "5,454", "5,668", "4,062", "3,936", "1,067", "6,635"
#$ quantity <dbl> 494, 334, 259, 15, 307, 235

sales %>% summarise(mean(revenue))
#  mean(revenue)
#1            NA

# this happens because revenue is saveed as char because of its comma

library(stringr)
revenue_USD <- str_remove(sales$revenue, ",")
revenue_USD <- as.numeric(revenue_USD)

sales<- sales %>% select(order_id,quantity) %>% 
  mutate( revenue_USD = as.numeric( str_remove(sales$revenue, ",")))
sales %>% summarise(mean(revenue_USD))          
#  mean(revenue_USD)
#1          4470.333


#### UNIQUENESS CONSTRAINTS

## FULL DUPLICATES == ALL ROWS ARE SAME
credit_scores <- data.frame( First.Name = c("harper","miriam","eagan","miriam","katell","katell"),
                             Last.Name = c("taylor","day","schmitt","day","roy","roy"),
                             address = c("P.O. Box 212, 6557 nunc road","6042 sollicitudin avenue",
                                         "507-6740 curus avenue","6042 sollicitudin avenue",
                                         "ap #434-4081 mi av.","ap #434-4081 mi av."),
                             Credit.Score = c(655,313,728,313,455,455))

duplicated(credit_scores)
# [1] FALSE FALSE FALSE TRUE FALSE  TRUE

sum(duplicated(credit_scores))
# [1]   2

credit_scores %>% filter(duplicated(credit_scores))
#    First.Name Last.Name          address         Credit.Score
#1     miriam       day 6042 sollicitudin avenue          313
#2     katell       roy      ap #434-4081 mi av.          455

# removing duplicates :

credit_scores_unique <- distinct(credit_scores)
sum(duplicated(credit_scores_unique))
# [1] 0


## PARTIAL DUPLICATES == FEW ENTRIES MAY BE SAME

credit_scores2 <- data.frame( First.Name = c("harper","eagan","tamekeh","tamekeh","xandra","xandra"),
                             Last.Name = c("taylor","schmitt","forbes","forbes","barrett","barrett"),
                             address = c("P.O. Box 212, 6557 nunc road","507-6740 curus avenue",
                                         "P.O. Box 147,511 velil street","P.O. Box 147,511 velil street",
                                         "P.O. Box 309, 2462 Pharetrard.","P.O. Box 309, 2462 Pharetrard."),
                             Credit.Score = c(655,728,356,342,620,636))

dup_ID <- credit_scores2 %>% count(First.Name, Last.Name) %>% 
                    filter(n>1)
#  First.Name Last.Name n
#1    tamekeh    forbes 2
#2     xandra   barrett 2

credit_scores2 %>% filter( First.Name %in% dup_ID$First.Name, Last.Name %in% dup_ID$Last.Name)
#   First.Name Last.Name        address                  Credit.Score
#1    tamekeh    forbes  P.O. Box 147,511 velil street          356
#2    tamekeh    forbes  P.O. Box 147,511 velil street          342
#3     xandra   barrett P.O. Box 309, 2462 Pharetrard.          620
#4     xandra   barrett P.O. Box 309, 2462 Pharetrard.          636

## drop partial duplicates

credit_scores2_unique <- credit_scores2 %>% distinct(First.Name,Last.Name, .keep_all = TRUE)
credit_scores2_unique
#    First.Name Last.Name      address                   Credit.Score
#1     harper    taylor   P.O. Box 212, 6557 nunc road          655
#2      eagan   schmitt          507-6740 curus avenue          728
#3    tamekeh    forbes  P.O. Box 147,511 velil street          356
#4     xandra   barrett P.O. Box 309, 2462 Pharetrard.          620

## we can also summarise them ie. sum/mean/...

credit_scores2 %>% group_by(First.Name,Last.Name) %>%
                    mutate(mean_score = mean(Credit.Score)) %>%
                      distinct(First.Name,Last.Name, .keep_all = TRUE) %>%
                        select(First.Name,Last.Name,mean_score)

#First.Name Last.Name mean_score
#  <chr>      <chr>          <dbl>
#1 harper     taylor           655
#2 eagan      schmitt          728
#3 tamekeh    forbes           349
#4 xandra     barrett          628                     
