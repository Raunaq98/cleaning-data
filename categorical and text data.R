#### CATEGORICAL VARIABLES HAVE A FIXED AND KNOWN SET OF POSSIBLE VALUES

# data                          labels                        numeric representation      outlier
# marriage status       married / unmarried                   1 / 2                       divorced
# income category       0-20k/ 20-40k/...                     1 / 2 / ...                 10-30k
# T-shirt size          S / M / L / XL                        1 / 2 / 3 / 4                 XS


study_data <- data.frame( name = c( "Beth","Ignatius","Paul","Helen","Jennifer","Kenny","Keith"),
                          birthday = c("2019-10-20","2020-07-08","2019-08-12","2019-03-17",
                                       "2019-12-17","2020-04-27","2019-04-19"),
                          blood_type = c("B-","A-","O+","O-","Z+","A+","AB+"))
# study of blood types of new borns

blood_types <- data.frame( blood_type = c("O-","O+","A-","A+","B+","B-","AB+","AB-"))
# possible blood types

# jennifer has blood type z+ which is not a member

#### anti-join will return the outlier row
library(dplyr)
study_data %>% anti_join(blood_types, 
                         by=c("blood_type"="blood_type"))
#     name   birthday   blood_type
#1 Jennifer 2019-12-17         Z+


#### semi-join will return the corrected table
study_data %>% semi_join(blood_types, 
                         by = c("blood_type" = "blood_type"))

#      name   birthday   blood_type
#1     Beth 2019-10-20         B-
#2 Ignatius 2020-07-08         A-
#3     Paul 2019-08-12         O+
#4    Helen 2019-03-17         O-
#5    Kenny 2020-04-27         A+
#6    Keith 2019-04-19        AB+

#### inconsistency within a category

animals <- data.frame( animal_id = c(12,23,6634,23,65,2342,4634,23,42,6,65,3,23,46,345,3),
                       type = c("mammal","bird"," mammal", "MAMMAL", "fish" , "bird" , "mammal ","bird","bird",
                                " mammal "," MAMMAL ", "fish", "BIRD","FISH"," fish","fish"))
animals$type <- as.factor((animals$type))


animals %>% count(type)
#type n
#1      fish 1
#2    mammal 1
#3   mammal  1
#4   MAMMAL  1
#5      bird 4
#6      BIRD 1
#7      fish 3
#8      FISH 1
#9    mammal 1
#10   MAMMAL 1
#11  mammal  1

library(stringr)

# fixing case inconsistency
animals <- animals %>% mutate(type_lower = str_to_lower(type))
animals %>% count(type_lower)
#type_lower n
#1       fish 1
#2     mammal 1
#3    mammal  2
#4       bird 5
#5       fish 4
#6     mammal 2
#7    mammal  1

animals <- animals %>% select( animal_id,type_lower)

# fixing spaces before and after the level char
animals <- animals %>% mutate(type_trimmed = str_trim(type_lower))
animals <- animals %>% select( animal_id, type_trimmed)
animals %>% count(type_trimmed)
#   type_trimmed n
#1         bird 5
#2         fish 5
#3       mammal 6

##### categories with small freq : collapse them

animals2 <- data.frame( animal_id = c(12,23,6634,23,65,2342,4634,23,42,6,65,3,23,46,345,3),
                       type = c("mammal","mammal"," mammal", "MAMMAL", "mammal" , "bug" , "mammal ","fish","bird",
                                " mammal "," MAMMAL ", "fish", "BIRD","FISH"," fish","fish"))
animals2 <- animals2 %>% mutate(type_low = str_to_lower(type)) %>% 
                            mutate( type_trim = str_trim(type_low)) %>% 
                              select( animal_id,type_trim) 
animals2 %>% count(type_trim)
#  type_trim n
#1      bird 2
#2       bug 1
#3      fish 5
#4    mammal 8

other_categories = c("bird","bug")

library(forcats)
animals2 <- animals2 %>% mutate( type_collapsed = fct_collapse(type_trim,other = other_categories))
## fct_collapse(x,y) ---> work on x factor column and collapse using y

animals2 %>% count(type_collapsed)
#  type_collapsed n
#1          other 3
#2           fish 5
#3         mammal 8


####### TEXT INCONSISTENCIES

customers<- data.frame( name = c("A","B","C","D","E","F"),
                        number = c("123-456-789","234 343 453", "234 345 345", 
                                    "345-756-345","856-374-363","234 454 658"))

# this data has dirty text as numbers have different formats

# detecting dirty data

customers %>%  filter( str_detect(number, "-"))
#   name      number
#1    A 123-456-789
#2    D 345-756-345
#3    E 856-374-363

# replacing dirty text

customers <- customers %>% mutate( number = str_replace_all(number,"-" , " "))
#   name      number
#1    A 123 456 789
#2    B 234 343 453
#3    C 234 345 345
#4    D 345 756 345
#5    E 856 374 363
#6    F 234 454 658

####  removing spaces
# using another approach

number_clean <- customers$number %>%
                  str_remove_all("-") %>%
                    str_remove_all(" ")
customers <- customers %>% mutate( number = number_clean)
#  name    number
#1    A 123456789
#2    B 234343453
#3    C 234345345
#4    D 345756345
#5    E 856374363
#6    F 234454658

### finding if any number isnt 9 numbers long
customers %>% filter(str_length(number) != 9)
#   [1] name   number
# <0 rows> (or 0-length row.names)
# hence, all entries are valid

