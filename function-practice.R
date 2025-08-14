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
  if (age > 20) {
    warning("Your zawg literally cannot be that old")
  }
  if (animal == "dog") {
    return(paste("Your zawg is", age * 7, "in human years"))
  } else if (animal == "goat") {
    return(paste("Your LEBRON JAMES is", age * 4.7, "in human years"))
  } 
}

animal_age("goat", 20)
