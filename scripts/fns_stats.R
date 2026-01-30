
## Create LMER model, with optional log-transform of the response
make_lmer <- function(dat, resp_var, rhs, log_TF = FALSE) {
  lhs <- if (isTRUE(log_TF)) paste0("log(", resp_var, ")") else resp_var
  lmerTest::lmer(reformulate(rhs, response = lhs), data = dat)
}

## Pull a single fitted model from a specs table by response variable name
get_model <- function(df, var) {
  df %>% filter(resp_var == var) %>% pull(model) %>% .[[1]]
}

## Extract and format marginal and conditional R² from a fitted mixed-effects model
## (using MuMIn::r.squaredGLMM), returned as a display-ready character string
r2_txt <- function(df, var) {
  r2 <- df %>% get_model(var) %>% MuMIn::r.squaredGLMM()
  m <- as.numeric(r2[1, "R2m"])
  c <- as.numeric(r2[1, "R2c"])
  sprintf("R\u00b2m = %.3f; R\u00b2c = %.3f", m, c)
}

## Merge coefficient table with ANOVA output table (by Effect name)
merge_coefs <- function(df, df2) {
  df %>% rename(Effect = Coef) %>%
    mutate(Effect = gsub("TRUE", "", Effect),
           Effect = gsub("FALSE", "", Effect)) %>%
    full_join(df2, by = "Effect") %>%
    mutate(`P<0.01` = ifelse(as.numeric(P_value) < 0.01, "*", ""))
}

## Extract fixed-effect estimates and SEs (formatted for tables)
printCoefs <- function(x) {
  cf <- coefficients(summary(x)) %>% data.frame()
  cf %>% mutate(Coef = rownames(cf)) %>%
    select(Coef, Est = Estimate, SE = Std..Error) %>%
    mutate(Est = formatC(Est, digits = 2, flag = "#"),
           SE = formatC(SE, digits = 2, flag = "#"))
}

## Extract LAI slopes by Treatment from an LMER with LAI × fertilization terms
## Returns:
## - Treatment: factor with 8 factorial treatment levels (Control, +N, +P, +K, +NP, +NK, +PK, +NPK)
## - Estimate: LAI slope for that treatment (in log space)
## - int: intercept (in log space)
## - Vol: coefficient on log(Volume_mL) if present
parms.LAIxNut <- function(model) {
  fe <- lme4::fixef(model)
  
  ## Move coefficient for volume to the end, if volume is included in the model
  if(length(fe) > 8) fe <- c(fe[-3], fe[3])
  
  vals <- c(
    Control=fe[2][[1]],           ## Control
    `+N`=fe[2][[1]]+fe[3][[1]],   ## Control + N
    `+P`=fe[2][[1]]+fe[4][[1]],   ## Control + P
    `+K`=fe[2][[1]]+fe[5][[1]],   ## Control + K
    `+NP`=fe[2][[1]]+fe[3][[1]]+fe[4][[1]]+fe[6][[1]],   ## Control + N + P + NP
    `+NK`=fe[2][[1]]+fe[3][[1]]+fe[5][[1]]+fe[7][[1]],   ## Control + N + K + NK
    `+PK`=fe[2][[1]]+fe[4][[1]]+fe[5][[1]]+fe[8][[1]],   ## Control + P + K + PK
    `+NPK`=fe[2][[1]]+fe[3][[1]]+fe[4][[1]]+fe[5][[1]]+fe[6][[1]]+fe[7][[1]]+fe[8][[1]]    ## Control + N + P + K + NP + NK + PK
  )
  
  df <- tibble::enframe(vals, name = "Treatment", value = "Estimate") %>%
    mutate(int = fe[1][[1]])
  
  ## Get coefficient for volume if it appears in the model
  if(length(fe) > 8) df <- df %>% mutate(Vol = fe[9][[1]])
  
  mutate(df, Treatment = factor(Treatment))
}


## Model diagnostic panel plot + Shapiro–Wilk test on residuals
## - Prints Shapiro–Wilk test for residual normality (often very sensitive for large n)
## - Produces three stacked facets:
##   (1) Histogram of the observed response (xvar) colored by fillvar (from `df`)
##   (2) Residuals vs fitted
##   (3) Q–Q plot with fitted reference line (computed from quartiles)
## Arguments:
## - model: fitted model object
## - xvar: response variable in `df` (used only for the histogram facet)
## - fillvar: grouping variable in `df` used only for histogram fill
## - df: data used for the histogram facet (defaults to dat_litter)
diagnostic_plot <- function(model, xvar, fillvar, df = dat_litter) {
  ## Residuals & fitted from model
  res <- resid(model)
  fit <- fitted(model)
  
  ## Print Shapiro-Wilks test
  print(shapiro.test(res))
  
  ## Histogram data
  hist_df <- transform(df, plot = "Histogram")
  
  ## Residuals vs Fitted data
  rf_df <- data.frame(fitted = fit, resid  = res, plot = "Residuals vs Fitted")
  
  ## Q–Q points
  qq <- qqnorm(res, plot.it = FALSE)
  qq_df <- data.frame(fitted = qq$x, resid  = qq$y, plot = "Q–Q plot")
  
  ## Compute quartiles
  q_theoretical <- quantile(qq$x, c(0.25, 0.75))
  q_sample <- quantile(qq$y, c(0.25, 0.75))
  
  ## Get slope and intercept for Q-Q plot
  slope <- diff(q_sample) / diff(q_theoretical)
  intercept <- q_sample[1] - slope * q_theoretical[1]
  qq_df <- transform(qq_df, slope = slope, intercept = intercept)
  
  ## Make plot 
  ggplot() +
    ## Facet 1: Histogram
    geom_histogram(data = hist_df,
                   aes(x = {{xvar}}, fill = {{fillvar}}), bins = 15, color = "black") +
    ## Facet 2: Residuals vs Fitted
    geom_point(data = rf_df, aes(x = fitted, y = resid)) +
    geom_hline(data = rf_df, aes(yintercept = 0), color = "red") +
    ## Facet 3: Q–Q plot (manual)
    geom_point(data = qq_df, aes(x = fitted, y = resid)) +
    geom_abline(data = qq_df, aes(slope = slope, intercept = intercept), color = "red") +
    ## Facet and theme settings
    facet_wrap(~ plot, scales = "free", ncol = 1) +
    theme_minimal() +
    theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)) +
    guides(fill = "none") +
    labs(x = NULL, y = NULL, fill = NULL)
}

