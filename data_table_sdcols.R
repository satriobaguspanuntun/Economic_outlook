library(data.table)
library(Lahman)
library(ggplot2)

# load Lahman data
teams <- Lahman::Teams
setDT(teams)
pitching <- Lahman::Pitching
setDT(pitching)
teams
pitching

# column subsetting .SDcols
pitching[ , .SD, .SDcols = c("W", "L", "G")]

# column type conversion
fkt = c("teamIDBR", "teamIDlahman45", "teamIDretro")
str(teams[ , ..fkt])

teams[ , names(.SD) := lapply(.SD, factor), .SDcols = patterns('teamID')]
head(unique(teams[[fkt[1L]]]))

# convert all factor columns to character
fct_idx <- teams[ , which(sapply(.SD, is.factor))]
str(teams[[fct_idx[1L]]])

teams[ , names(.SD) := lapply(.SD, as.character), .SDcols = is.factor]
str(teams)

# models
extra_var <- c("yearID", "teamID", "G", "L")
models <- unlist(
  lapply(0L:length(extra_var), combn, x = extra_var, simplify = FALSE),
  recursive = FALSE
)

col16 <- c('#e6194b', '#3cb44b', '#ffe119', '#0082c8',
           '#f58231', '#911eb4', '#46f0f0', '#f032e6',
           '#d2f53c', '#fabebe', '#008080', '#e6beff',
           '#aa6e28', '#fffac8', '#800000', '#aaffc3')

par(oma = c(2, 0, 0, 0))
lm_coef <- sapply(models, function(rhs) {
  pitching[, coef(lm(ERA ~ ., data = .SD))["W"], .SDcols = c("W", rhs)]
})
barplot(lm_coef, names.arg = sapply(models, paste, collapse = "/"),
        main = "Wins Coefficient/nWith Various Covariates",
        col = col16,
        las = 2L,
        cex.names = 0.8)






