library(data.table)

input <- if (file.exists("flights14.csv")) {
  "flights14.csv"
} else {
  "https://raw.githubusercontent.com/Rdatatable/data.table/master/vignettes/flights14.csv"
}

flights <- fread(input)
flights

# data.table structure
# x[i, j, by]

# 1 Basics
# get all flights with "JFK" as the origin airport in the month of june
ans <- flights[origin == "JFK" & month == 6L]
ans

# get the first two rows from flights
ans <- flights[1:2]
ans

# sort flights by column origin in ascending order, and then by dest in decending
# order
flights[order(origin, -dest)]

# select arr_delay column, but return as a data.table instead 
flights[, .(arr_delay)] 

# or :
flights[, list(arr_delay)]

# select both arr_delay dep_delay columns
flights[, .(arr_delay, dep_delay)]

# select both arr_delay and dep_delay columns and rename them to delay_arr and
# delay_dep
flights[, .(delay_arr = arr_delay, delay_dep = dep_delay)]

## compute or do in j
# how many trips haave had total delay < 0
flights[, sum((arr_delay + dep_delay) < 0)]

## subset in i and do in j
# calculate the average arrival and departure delay for all flights with "JFK" 
# as the origin airport in the month of june
flights[origin == "JFK" & month == 6L, 
        .(m_arr = mean(arr_delay), m_dep = mean(dep_delay))]

# How many trips have been made in 2014 from "JFK" airport in the month of june
flights[origin == "JFK" & month == 6L , length(dest)]

## Handle non-existing elements in r
# what happens when querying for non-existing elements?
setkeyv(flights, "origin")

# - key based subsettings dt["d"]
flights["XYZ"]

# - logical substetting dt[x == "d"]
flights[origin == "XYZ"]

# - exact match using nonmatch = NULL
flights["XYZ", nomatch = NULL]

## special symbols .N
flights[origin == "JFK" & month == 6L, .N]

## How can i refer to columns by names in j like ddata.frame
# - select both arr_delay and dep_delay columns the data.frame way
flights[, c("arr_delay", "dep_delay")]

# - select columns named in a variable using the .. prefix
select_cols <- c("arr_delay", "dep_delay")
flights[, ..select_cols]

# - select columns named in variable using with = false
flights[ , select_cols, with = FALSE]

DF <-  data.frame(x = c(1,1,1,2,2,3,3,3), y = 1:8)

# (1) normal way
DF[DF$x > 1, ]

# (2) using with
DF[with(DF, x > 1), ]

# you can also deselect columns using - or ! (just like select() and base R subsetting)
flights[, !c("arr_delay", "dep_delay")]

# or 
flights[, -c("arr_delay", "dep_delay")]

### 2. Aggregations
## a. Grouping using by
# how can we get the number of trips corresponding to each origin airport



