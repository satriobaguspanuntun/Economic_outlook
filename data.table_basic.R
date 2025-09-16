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
flights[, .(.N), by = .(origin)]

# how can we calculate the number of trips for each origin airport for carrier
# code "AA"
flights[carrier == "AA", .N, by = origin]

# total number of trips for each origin, dest pair for carrier code "AA"
flights[carrier == "AA", .N, by = .(origin, dest)]

# average arrival and departure delay for each orig, dest pair for each
# month for carrier code "AA"
flights[carrier == "AA",
        .(mean(arr_delay), mean(dep_delay)), 
        by = .(origin, dest, month)]

## b. sorted by: keyby
flights[carrier == "AA", 
        .(mean(arr_delay), mean(dep_delay)),
        keyby = .(origin, dest, month)]

## c. chaining
# how can we order ans using the columns origin in ascending order, 
# and dest in descending order
flights[carrier == "AA", .N, by = .(origin, dest)][order(origin, -dest)]

## d. expressions in by
# can by accept expressions as well or does it just take columns
flights[, .N, .(dep_delay > 0, arr_delay > 0)]

## multiple columns in j - SD.
# how to specify just the columns we would like to compute the (mean)
flights[carrier == "AA",
        lapply(.SD, mean),
        by = .(origin, dest, month),
        .SDcols = c("arr_delay", "dep_delay")]

## .f subset .SD for each group
# how can we return the first two rows for each month
flights[, head(.SD, 2), by = month]

## .g why keep j so flexible
# do long distance flights cover up departure delay more than short
# distance flights?
# does cover up vary by month?
flights[, `:=`(makeup = dep_delay - arr_delay)]

makeup.models <- flights[, .(fit = list(lm(makeup ~ distance))), by = .(month)]
makeup.models[, .(coefdist = coef(fit[[1]])[2], rsq = summary(fit[[1]])$r.squared), by = .(month)]

setDF(flights)
flights.split <- split(flights, f = flights$month)
makeup.models.list <- lapply(flights.split, function(df) c(month = df$month[1], fit = list(lm(makeup ~ distance, data = df))))
makeup.models.df <- do.call(rbind, makeup.models.list)
data.frame(t(sapply(
  makeup.models.df[, "fit"],
  function(model) c(coefdist = coef(model)[2L], rsq = summary(model)$r.squared)
)))

