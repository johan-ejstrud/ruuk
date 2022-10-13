

library("tidyverse")
library("statgl")

get_px <- function(px, ...){
  px |>  
    statgl_url(lang = "da") |> 
    statgl_fetch(..., .col_code = TRUE) |> 
    as_tibble()
}

# ------------------------------------------------------------------------------
# primo befolkingen ------------------------------------------------------------
# ------------------------------------------------------------------------------

BEXSTA <- "BEXSTA" |> 
  get_px(age = px_all(), time = px_top(), gender = c("M", "K")) |> 
  mutate(
    age   = as.integer(age),
    value = ifelse(gender == "Mænd", value, -value)
    )

# ------------------------------------------------------------------------------
# andel af befolkningen med en indkomst 50/60% under medianindkosmten i 3 sammenhængende år
# ------------------------------------------------------------------------------

SOXOU01 <- "SOXOU01" |> 
  get_px("inventory variable" = px_all("Andel*")) |> 
  rename("variable" = `inventory variable`, "time" = year) |> 
  mutate(variable = variable |> str_to_sentence())

  
# ------------------------------------------------------------------------------
# stemmeprocent, folketingsvalg (Grønland) --------------------------------------
# ------------------------------------------------------------------------------

SAXFOLK <- "SAXFOLK" |> 
  get_px(municipality = 0, "votes cast" = c(12, 16)) |> 
  rename("variable" = "votes cast") |> 
  mutate(variable = ifelse(variable == "Stemmeberettigede", "nævner", "tæller")) |> 
  spread(variable, value) |> 
  mutate(
    time  = lubridate::dmy(time) |> lubridate::year(),
    value = tæller / nævner
    ) |> 
  select(time, value) |> 
  arrange(time)