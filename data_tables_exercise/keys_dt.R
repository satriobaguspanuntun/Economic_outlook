# Keys and fast binary search based subset
library(data.table)

# flights data
head(flights)

# 1 Keys
# a. what is key
set.seed(1L)
DF <- data.frame(ID1 = sample(letters[1:2], 10, TRUE),
                 ID2 = sample(1:3, 10, TRUE),
                 val = sample(10),
                 stringsAsFactors = FALSE,
                 row.names = sample(LETTERS[1:10]))
DF

# we can subset a particular row using its row name as shown below
DF["C", ]

# each row is limited to exactly one row name
# and row names should be unique
# convert it to data.table
DT <- as.data.table(DF)
DT

# b set, get and use keys on a data.table
# Hpw can we set the column origin as key in the data.table flights?
flights <- data.table(flights)
setkey(flights, origin)
head(flights)

