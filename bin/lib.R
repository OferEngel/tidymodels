library(tidyverse)
library(janitor)
library(broom)
library(tidymodels)  # Includes the workflows package
tidymodels::tidymodels_prefer()
library(tidyverse)
library(glue)
library(themis)
library(skimr) # use to skim(data)
library(colino)
library(vip)
library(naniar)
library(glue)
library(ggrepel)
library(textrecipes)
library(bonsai)
library(finetune)
library(future)
library(ggcorrplot)
library(embed)



options(
  pillar.advice = FALSE, 
  pillar.min_title_chars = Inf
)

theme_set(theme_minimal())
update_geom_defaults("point", list(color = "midnightblue", alpha = .8))
update_geom_defaults("line", list(color = "midnightblue", alpha = .8))




#
# Missing data: 
# childcare_costs |> 
#   slice_sample(n = 500) |> 
#   vis_miss()

# Continuous
get_cors <- function(df, out_var) {
  outcome <- as.numeric(df[[out_var]])

  df[, names(df) != out_var] |> 
    select(where(is.numeric)) |> 
    map(cor.test, y = outcome) |> 
    map_dfr(tidy, .id = "predictor") |> 
    arrange(desc(abs(estimate)))
}

plot_cors <- function(df, out_var, size = 12) {
  
    get_cors(df, out_var) |> 
    ggplot(aes(x = fct_reorder(predictor, estimate))) + 
    geom_point(aes(y = estimate)) + 
    geom_errorbar(
      aes(ymin = conf.low, ymax = conf.high), 
      width = .1
    ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = NULL, y = glue("Correlation with {out_var}")) + 
    coord_flip() + 
    theme(axis.text.y = element_text(size = size))
}

# Nominal predictors
get_nom_preds <- function(df, out_var) {
  nom_predictors <- df |> 
    select(where(~ !is.numeric(.x))) |> 
    names()
  
  nom_predictors |> 
    set_names() |> 
    map(~ reformulate(.x, intercept = FALSE, response = out_var)) |> 
    map(~ lm(.x, data = df)) |> 
    map(tidy) |> 
    list_rbind(names_to = "names") |> 
    filter(term != "(Intercept)") |> 
    mutate(
      term = str_remove(term, names),
      p.value = p.adjust(p.value)
    ) |> 
    arrange(desc(abs(estimate))) 
}

plot_nom_preds <- function(df, out_var, .keepnames = FALSE) {
  
  raw_preds <- get_nom_preds(df, out_var)  |> 
    filter(p.value < 0.05) 
  
  min.p <- raw_preds |> 
    filter(p.value > 0) |> 
    summarise(min = min(p.value)) |> 
    pull(min)

  
  raw_preds |> 
    mutate(
      keepnames = .keepnames,
      label = if_else(
        keepnames, 
        glue("{names}.{term}"), 
        term
        )
      ) |> 
    mutate(
      p.value = ifelse(p.value <= 0, min.p, p.value)
      ) |> 
    ggplot(aes(estimate, p.value)) +
    geom_point(aes(color = names)) +
    geom_smooth(alpha = .2, color = "grey70", se = FALSE) +
    geom_text_repel(aes(label = label, color = names)) +
    scale_y_log10(limits = c(min.p, .05)) +
    # scale_color_viridis_d() + 
    labs(
      color = NULL, 
      x = "Effect size", 
      title = glue("Regressing {out_var} against nominals")
      ) + 
    theme(legend.position = "bottom")
}



# res |> 
#   metric_set(rmse, rsq, mae, mape)(
#     truth = ..,
#     estimate = ..
#   )




plot_performance <- function(res, truth, estimate = .pred) {
  
  res |> 
    ggplot(aes({{ truth }}, {{ estimate }})) + 
    geom_point() + 
    geom_abline(color = "blue", linetype = "dashed") + 
    coord_obs_pred()
}


# last_fit |> 
#   collect_predictions() |> 
#   plot_performance(imdb_rating, .pred)

