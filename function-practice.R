# first functions

# define the function for adding
birddog_sum <- function(birdct, dogct) {
  pets <- birdct + dogct
  return(pets)
}

# use or "call" the function
total_pets <- birddog_sum(birdct = 23, dogct = 67)

# other function for doubling
double_it_and_give_it_to_the_next_person <- function(x) {
  print(2 * x)
}

double_it_and_give_it_to_the_next_person(34.5)

# function with conditionals
animal_age <- function(animal, age) {
  if (!is.numeric(age)) { # numbers for age
    stop("bruh, numbers only for age")
  }
  if (age <= 0) { # positive numbers for age
    stop("Your zawg has to be more than 0 years old bruh")
  }
  if (!animal %in% c("dog", "goat")) { # if animal is NOT dog or goat
    stop("Please go buy a zawg or LEBRON JAMES")
  }
  if (age > 20) { # age cap
    warning("Your zawg literally cannot be that old")
  }
  if (animal == "dog") {
    return(paste("Your zawg is", age * 7, "in human years"))
  } else if (animal == "goat") {
    return(paste("Your LEBRON JAMES is", age * 4.7, "in human years"))
  } 
}

animal_age("goat", 20)

# functions with loops

# function to iterate over columns
df_means <- function(dataframe) {
  if (!is.data.frame(dataframe)) {
    stop("Please enter a dataframe")
  }
  for (i in 1:ncol(dataframe)) {
    if (!is.numeric(dataframe[[i]])) {
      print("Column isn't numeric")
    } else {
      col_name <- colnames(dataframe[i]) # extracts name of column i
      col_means <- mean(dataframe[[i]], na.rm = TRUE) # calculates mean
      print(paste("The mean value of", col_name, "is", col_means))
    }
  }
}
df_means(penguins)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------------- LOGISTIC GROWTH EXAMPLE-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# logistic growth function
logistic_growth <- function(K, N0, r, time) {
  Nt <- K / (1 + ((K - N0) / N0) * exp(-r * time))
  return(Nt)
}
# test
logistic_growth(6000, 100, 0.2, 40)

# Continue building: time vec
time_vec <- seq(from = 0, to = 40, by = 0.1)

pop_time_40 <- logistic_growth(6000, 100, 0.2, time_vec)

# can create df
pop_time_df <- data.frame(time_vec, pop_time_40)

# plot!
library(tidyverse)
ggplot(data = pop_time_df, aes(x = time_vec, y = pop_time_40)) +
  geom_line()

# using for loop (inside loop)
pop_vec <- vector(mode = "numeric", length = length(time_vec))
for (i in 1:length(time_vec)) { # stepping through time steps
  pop_size <- logistic_growth(6000, 100, 0.27, time = time_vec[i])
  pop_vec[i] <- pop_size
}

# need rate vec
rate_vec <- seq(from = 0.2, to = 0.4, by = 0.01)

# build whole matrix
matrix_out <- matrix(nrow = length(time_vec), ncol = length(rate_vec))

# nest dat for loop
for (i in 1:length(rate_vec)) { 
  for (j in 1:length(time_vec)) {
    pop <- logistic_growth(6000, 100, rate_vec[i], time_vec[j])
    matrix_out[j, i] <- pop
  }
}

#wrangling for da plot, not gonna run these lol
out_df <- data.frame(matrix_out, time = time_vec)

# update col names
colnames(out_df) <- c(paste0("gr_", rate_vec), "time")

# pivot longer 
out_df_long <- out_df |> 
  pivot_longer(cols = -time, 
               names_to = "growth_rate", 
               values_to = "population")
# plot
ggplot(out_df_long, aes(x = time, y = population)) +
  geom_line(aes(color = growth_rate)) +
  theme_light()
