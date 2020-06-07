library(tidyverse)
library("Rlab")

# Exercise 1

dat_e1 <- map_df(mtcars, function(col) {
  prop <- runif(1, min = 0, max = 0.5)
  col[as.logical(rbern(length(col), prop))] <- NA

  prop <- runif(1, min = 0, max = 0.1)
  col[as.logical(rbern(length(col), prop))] <- -88

  prop <- runif(1, min = 0, max = 0.1)
  col[as.logical(rbern(length(col), prop))] <- -99

  return(col)
})

write_csv(dat_e1, "dat_e1.csv")


# Exercise 3

indeps <- list(
  indep_1 = rnorm(1000, 10, 3),
  indep_2 = rnorm(1000, 10, 3),
  indep_3 = rnorm(1000, 10, 3)
) %>% as_tibble()

deps <- with(indeps, list(
  dep_1 = ifelse(indep_1 + indep_2 + rnorm(1000, 1, 0) > 20, 1, 0),
  dep_2 = sample(c("A", "B", "C"), size = 1000, replace = T),
  dep_3 = indep_1 + indep_2 + rnorm(1000, 1, 0)
)) %>% as_tibble()

dat_e3 <- bind_cols(indeps, deps)
dat_e3[1:500, ] <- NA
write_csv(dat_e3, "./dat_e3.csv")

names <- c("Bacon", "Omelette", "Sausages", "Mushrooms", "Milk")
dat <- list(rbern(1500, 0.2), rbern(1500, 0.2), rbern(1500, 0.2), rbern(1500, 0.2), rbern(1500, 0.2)) %>%
  setNames(names)
dat_e4 <- as_tibble(dat)
dat_e4 <- map2_df(dat_e4, names(dat_e4), function(col, name) {
  col[col == 1] <- name
  col[col == 0] <- paste("No", name)
  return(col)
})

write_csv(dat_e4, "./dat_e4.csv")

name_map <- read_delim("./Data/dat_e5.txt", delim = " ", col_names = T)

test_name <- function(dat, from) {
  to <- colnames(dat)[colnames(dat) != from]
  table <- dat %>%
    group_by(!!sym(from)) %>%
    filter(length(unique(!!sym(to))) != 1)

  rep_oldnames <- unique(table[[from]])
  map(rep_oldnames, function(x) {
    unique(table[[to]][table[[from]] == x])
  }) %>%
    setNames(rep_oldnames) %>%
    return()
}

test_name(name_map, from = "old_name")

a <- "old_name"
name_map %>%
  group_by(!!sym(a)) %>%
  filter(length(unique(new_name)) == 2)

mtcars %>%
  group_by(gear) %>%
  summarise()

name_map <- read_delim("./Data/dat_e5.txt", delim = " ")
name_map[, 2] <- tolower(name_map[[2]])
name_map[2][30, ] <- name_map[2][40, ]
write_delim(name_map, "./Data/dat_e5_demo.txt")
name_map[2][17, ] <- name_map[2][23, ]
name_map[2][7, ] <- name_map[2][18, ]
write_delim(name_map, "./Data/dat_e5.txt")

test_name(name_map, 2)
