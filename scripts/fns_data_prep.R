
## Standardize factor levels/labels for throughfall dataset
## Ensures consistent ordering of identifiers and treatments across figures and models:
## - P/N/K addition is ordered as Rain (rain sample), FALSE (throughfall sample without focal nutrient addition), 
##   or TRUE (throughfall sample with focal nutrient addition)
## - Type is ordered Rain or Tfall
## - Treatment codes are relabeled to readable factor levels (e.g., "C" → "Control", "NPK" → "+NPK")
## Note: Collection and Type are columns in the raw throughfall data but not the plot means
## Note: Plot is not in present in the collection means (data averaged across rain events)
factorize_tfall_columns <- function(df) {
  df <- df %>%
    mutate(across(c(Replicate, Block), as.factor),
           across(c(P, N, K), ~ factor(.x, levels = c("Rain", FALSE, TRUE))),
           #Type = factor(Type, levels = c("Rain", "Tfall")),
           Treatment = factor(Treatment, levels = c("Rain", "C", "K", "N", "NK", "P", "PK", "NP", "NPK"),
                              labels = c("Rain", "Control", "+K", "+N", "+NK", "+P", "+PK", "+NP", "+NPK")))
  if("Plot" %in% colnames(df)) {
    df <- df %>% mutate(Plot = as.factor(Plot))
  }
  if("Collection" %in% colnames(df)) {
    df <- df %>% mutate(Collection = as.factor(Collection))
  }
  if("Type" %in% colnames(df)) {
    df <- df %>% mutate(Type = factor(Type, levels = c("Rain", "Tfall")))
  }
  df
}

## Standardize factor levels/labels for litterfall dataset
## Matches Treatment ordering and labels used in throughfall analyses and sets P/N/K addition to FALSE or TRUE
factorize_litter_columns <- function(df) {
  df %>%
    mutate(across(c(Plot, Replicate, Block), as.factor),
           across(c(P, N, K), ~ factor(.x, levels = c(FALSE, TRUE))),
           Treatment = factor(Treatment, levels = c("Control", "+K", "+N", "+NK", "+P", "+PK", "+NP", "+NPK")))
}


