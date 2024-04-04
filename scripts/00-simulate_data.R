#### Preamble ####
# Purpose: Simulates the voter preferences dataset.
# Author: Raghav Bhatia 
# Date: 4 April 2024
# Contact: raghav.bhatia@mail.utoronto.ca
# License: MIT


#### Workspace setup ####
library(tidyverse)
library(testthat)

#### Simulate data ####
set.seed(100) 
# for reproducibility

# Number of entries
num_entries <- 100

# Simulating the data
simulated_data <- tibble(
  respondent_id = 1:num_entries,
  voted = sample(
            c(
              "Democrat", "Republican"),
              num_entries,
              replace = TRUE,
              prob = c(0.5, 0.5)
              ),
  gender = sample(
            c(
              "Male", "Female"),
              num_entries,
              replace = TRUE,
              prob = c(0.49, 0.51)
              ),
  highest_education = sample(
                        c(
                          "No HS",
                          "High school graduate",
                          "Some college",
                          "2-year",
                          "4-year",
                          "Post-grad"
                          ),
                        num_entries,
                        replace = TRUE
                        ),
  Income_group = sample(
                      c(
                        "Less than $10,000",
                        "$10,000 - $19,999",
                        "$20,000 - $29,999",
                        "$30,000 - $39,999",
                        "$40,000 - $59,999",
                        "$60,000 - $69,999",
                        "$70,000 - $79,999",
                        "$80,000 - $99,999",
                        "$100,000 - $119,999",
                        "$120,000 - $149,999",
                        "$150,000 - $199,999",
                        "$200,000 - $249,999"
                        ),
                      num_entries,
                      replace = TRUE
                      ),
  age_group = sample(
                    c(
                      "18-28",
                      "29-39",
                      "40-50",
                      "51-60",
                      "Above 60"
                      ),
                    num_entries,
                    replace = TRUE
                    )
)

simulated_data <- simulated_data |>
  mutate(
    voted = as_factor(voted),
    gender = as_factor(gender),
    highest_education = as_factor(highest_education),
    Income_group = as_factor(Income_group),
    age_group = as_factor(age_group)
  )


# Viewing the first few rows of the simulated data
head(simulated_data)

#Saving the simulated data

write_parquet(simulated_data, "data/cleaned_data/voter_preferences")

# Testing the simulated table

# Test if the dataset has 100 entries
test_that("Dataset has 100 entries", {
  expect_equal(nrow(simulated_data), 100)
})

# Test if 'gender' only contains 'Male' and 'Female'
test_that("Gender variable", {
  expect_true(all(simulated_data$gender %in% c('Male', 'Female')))
})

# Test if 'education' contains the correct levels
test_that("Education variable", {
  expect_true(all(simulated_data$education %in% c(
                                              'No HS',
                                              'High school graduate',
                                              'Some college',
                                              '2-year',
                                              '4-year',
                                              'Post-grad'
                                              )))
})

# Test if 'Income_group' contains the correct categories
test_that("Income variable", {
  expect_true(all(simulated_data$Income_group %in% c(
    "Less than $10,000",
    "$10,000 - $19,999",
    "$20,000 - $29,999",
    "$30,000 - $39,999",
    "$40,000 - $59,999",
    "$60,000 - $69,999",
    "$70,000 - $79,999",
    "$80,000 - $99,999",
    "$100,000 - $119,999",
    "$120,000 - $149,999",
    "$150,000 - $199,999",
    "$200,000 - $249,999"
  )))
})

# Test if 'age_group' contains the correct categories
test_that("Age Group variable", {
  expect_true(all(simulated_data$age_group %in% c(
    "18-28",
    "29-39",
    "40-50",
    "51-60",
    "Above 60"
  )))
})

# Test if 'voted' variable contains the correct categories
test_that("Voted variable is correct", {
  expect_true(all(simulated_data$voted %in% c(
    "Democrat", "Republican")))
})

#Test if respondent_id is unique and actually sequential
test_that("respondent_id is unique and sequential", {
  expect_equal(unique(simulated_data$respondent_id), 1:num_entries) 
})

# Testing for missing values
test_that("No missing values present", {
  expect_false(anyNA(simulated_data))
})

#At least 40% of our dataset has males as the average should be 49%
test_that("Male is atleast 40%", {
  expect_true(simulated_data |>
                filter(gender == "Male") |>
                nrow() > 40)
})

#At least 40% of our dataset has democrat as the average should be 50%
test_that("Democrat is atleast 40%", {
  expect_true(simulated_data |>
                filter(voted == "Democrat") |>
                nrow() > 40)
})


