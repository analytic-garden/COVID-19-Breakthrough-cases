#' plot_breakthroughs
#'     Plot cases and deaths from CDC data.
#'
#' @param df - a dataframe of data downloaded from 
#'             https://data.cdc.gov/Public-Health-Surveillance/Rates-of-COVID-19-Cases-or-Deaths-by-Age-Group-and/3rge-nu2a
#'
#' @return a list of 2 dataframes
#'      df_cases - a summary of case percents of relevant populations, vaccinated and unvaccinated, by date
#'      df_deaths - a summary of case percents of relevant populations, vaccinated and unvaccinated, by date
#'      
#' @requires epical from https://rdrr.io/github/chrismerkord/epical/
#'       
plot_breakthroughs <- function(df) {
  require(tidyverse)
  require(epical)
  
  # convert MMWR.week to dates
  # updated because CDC changed MMWR.week format
  df <- df %>%
    mutate(MMWR.week = as.character(MMWR.week)) %>%
    mutate(Date = epi_week_date(str_sub(MMWR.week, 5), str_sub(MMWR.week, 1, 4), system = "cdc"))
  
  # summarize cases by vaccine status
  df_cases <- df %>%
    filter(outcome == "case") %>%
    group_by(Date) %>%
    summarize_at(c("Vaccinated.with.outcome", "Fully.vaccinated.population", 
                   "Unvaccinated.with.outcome", "Unvaccinated.population" ), sum) %>%
    mutate(Vacinated_with_case_pct = (Vaccinated.with.outcome / Fully.vaccinated.population) * 100) %>%
    mutate(Unvacinated_with_case_pct = (Unvaccinated.with.outcome / Unvaccinated.population) * 100)
  
  p <- ggplot(df_cases) +
    geom_point(aes(x = Date, y = Vacinated_with_case_pct, col = 'Vacinated')) + 
    geom_line(aes(x = Date, y = Vacinated_with_case_pct, col = 'Vacinated')) + 
    geom_point(aes(x = Date, y = Unvacinated_with_case_pct, col = 'Unvacinated')) +
    geom_line(aes(x = Date, y = Unvacinated_with_case_pct, col = 'Unvacinated')) +
    scale_color_manual(values = c("red", 'blue'),
                       labels = c("Unvacinated", "Vacinated")) +
    labs(y = "Cases Percent", color = "") +
    ggtitle("2021 Cases All Ages, All Vaccines")
  print(p)
  
  df_deaths <- df %>%
    filter(outcome == "death") %>%
    group_by(Date) %>%
    summarize_at(c("Vaccinated.with.outcome", "Fully.vaccinated.population", 
                   "Unvaccinated.with.outcome", "Unvaccinated.population" ), sum) %>%
    mutate(Vacinated_with_case_pct = (Vaccinated.with.outcome / Fully.vaccinated.population) * 100) %>%
    mutate(Unvacinated_with_case_pct = (Unvaccinated.with.outcome / Unvaccinated.population) * 100)
  
  p2 <- ggplot(df_deaths) +
    geom_point(aes(x = Date, y = Vacinated_with_case_pct, col = 'Vacinated')) + 
    geom_line(aes(x = Date, y = Vacinated_with_case_pct, col = 'Vacinated')) + 
    geom_point(aes(x = Date, y = Unvacinated_with_case_pct, col = 'Unvacinated')) +
    geom_line(aes(x = Date, y = Unvacinated_with_case_pct, col = 'Unvacinated')) +
    scale_color_manual(values = c("red", 'blue'),
                       labels = c("Unvacinated", "Vacinated")) +
    labs(y = "Deaths Percent", color = "") +
    ggtitle("2021 Deaths All Ages, All Vaccines")
  print(p2)
  
  return(list(df_cases = df_cases, df_deaths = df_deaths))
}