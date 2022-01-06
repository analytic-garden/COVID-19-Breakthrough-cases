#' plot_breakthroughs
#'     Plot cases and deaths by age group from CDC data.
#'
#' @param df - a dataframe of data downloaded from 
#'             https://data.cdc.gov/Public-Health-Surveillance/Rates-of-COVID-19-Cases-or-Deaths-by-Age-Group-and/3rge-nu2a
#'
#' @return a list of 2 dataframes
#'      df_cases - a summary of case percents of relevant populations, vaccinated and unvaccinated, by date and age group
#'      df_deaths - a summary of case percents of relevant populations, vaccinated and unvaccinated, by date and age group
#'      
#' @requires epical from https://rdrr.io/github/chrismerkord/epical/
#'       
plot_breakthroughs_ages <- function(df) {
  require(tidyverse)
  require(epical)
  
  # convert MMWR.week to dates
  df <- df %>%
    mutate(Date = epi_week_date(MMWR.week, "2021", system = "cdc")) %>%
    mutate(Age.group = as.factor(Age.group))
  
  # suumararize 
  df_cases <- df %>%
    filter(outcome == "case") %>%
    group_by(Date, Age.group) %>%
    summarize_at(c("Vaccinated.with.outcome", "Fully.vaccinated.population", 
                   "Unvaccinated.with.outcome", "Unvaccinated.population" ), sum) %>%
    mutate(Vacinated = (Vaccinated.with.outcome / Fully.vaccinated.population) * 100) %>%
    mutate(Unvacinated = (Unvaccinated.with.outcome / Unvaccinated.population) * 100) %>%
    pivot_longer(cols = c(Vacinated, Unvacinated),   # pivot to summarize groups
                 names_to = "Vacinated",
                 values_to = "Pct") %>%
    select(Date, Age.group, Vacinated, Pct)
  
  p <- ggplot(df_cases, aes(x = Date, y = Pct, color = Age.group, shape = Vacinated)) +
    geom_point() + geom_line() +
    labs(y = "Cases Percent", color = "") +
    ggtitle("2021 Cases All Vaccines")
  print(p)
  
  df_deaths <- df %>%
    filter(outcome == "death") %>%
    group_by(Date, Age.group) %>%
    summarize_at(c("Vaccinated.with.outcome", "Fully.vaccinated.population", 
                   "Unvaccinated.with.outcome", "Unvaccinated.population" ), sum) %>%
    mutate(Vacinated = (Vaccinated.with.outcome / Fully.vaccinated.population) * 100) %>%
    mutate(Unvacinated = (Unvaccinated.with.outcome / Unvaccinated.population) * 100) %>%
    pivot_longer(cols = c(Vacinated, Unvacinated),
                 names_to = "Vacinated",
                 values_to = "Pct") %>%
    select(Date, Age.group, Vacinated, Pct)
  
  p2 <- ggplot(df_deaths, aes(x = Date, y = Pct, color = Age.group, shape = Vacinated)) +
    geom_point() + geom_line() +
    labs(y = "Deaths Percent", color = "") +
    ggtitle("2021 Deaths All Vaccines")
  print(p2)

  return(list(df_cases = df_cases, df_deaths = df_deaths))
}