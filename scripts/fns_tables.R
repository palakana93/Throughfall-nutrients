

## Format Type-II/III ANOVA output from lmerTest::anova() for reporting tables
## - Keeps Effect, numerator/denominator df, F, and P
## - Rounds df_den and F; formats P (<0.01 in scientific notation)
## - Coerces all columns to character for downstream table joins / Word export
## - Re-labels "sp" to "Species" (legacy label used in some models)
make_ANOVA_table <- function(anova_model) {
  test <- anova_model %>% 
    rownames_to_column("Effect") %>%
    tibble() %>% select(Effect, df_num = NumDF, df_den = DenDF, F_value = `F value`, P_value = `Pr(>F)`) %>%
    mutate(df_den = round(df_den, 2),
           F_value = round(F_value, 2),
           P_value =   ifelse(P_value < 0.01,
                              formatC(P_value, format = "e", digits = 1),
                              signif(P_value, 2)),
           Effect = ifelse(Effect == "sp", "Species", Effect))
  test <- test %>% mutate(across(everything(), as.character))
  test
}


## Summary stats helper: mean, standard error, and n (non-missing)
## Returns a 1-element list containing a named numeric vector:
##   c(mean = <mean>, se = <SE>, n = <n>)
## This structure is convenient for mapping and later extraction of "mean"/"se"
stat <- function(x) {
  mean <- mean(x, na.rm = TRUE)
  se <- sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
  n <- sum(!is.na(x))
  list(c(mean = mean, se = se, n = n))
}

## Compute throughfall nutrient transfer (throughfall minus rainfall nutrient deposition) 
## and propagate SE
## - Input: list of stat() outputs (mean/se) where the FIRST element is Rain
## - Transfer mean = mean_TFall − mean_Rain
## - Transfer SE   = sqrt(SE_TFall^2 + SE_Rain^2) (assumes independence)
## - Returns character strings formatted as "mean ± SE" via format_mean_se()
stat_transfer <- function(x) {
  means <- map_dbl(x, \(ms) unname(ms["mean"]))
  ses <- map_dbl(x, \(ms) unname(ms["se"]))
  
  # Means and SEs from rainfall (first row)
  rain_mean <- means[1]
  rain_se   <- ses[1]
  
  # Get means and SEs of nutrient transfers (throughfall flux - rainfall flux)
  diff_means <- means - rain_mean
  comb_ses   <- sqrt(ses^2 + rain_se^2)
  
  map2_chr(diff_means, comb_ses, format_mean_se)
}


## Format a mean ± SE string with consistent, publication-style rounding
## - Mean rounded to 2 significant figures (trailing zeros preserved)
## - SE rounded to match mean’s rounding scale; if SE is smaller, keep 1 sig fig
format_mean_se <- function(mean, se) {
  
  # --- Mean to 2 significant figures ---
  mean_sf2 <- signif(mean, digits = 2)
  
  # --- Rounding scale based on mean (for SE) ---
  # Example: mean = 180 → scale = 10; mean = 0.054 → scale = 0.001
  if (mean_sf2 == 0) {
    scale <- 1  # Arbitrary, won't matter much if mean is exactly 0
  } else {
    scale <- 10^(floor(log10(abs(mean_sf2))) - 1)
  }
  
  # --- SE rounding logic ---
  if (se >= scale) {
    # Standard rounding to same scale
    se_rounded <- round(se / scale) * scale
  } else {
    # SE is smaller than mean’s rounding scale → keep 1 significant figure
    se_rounded <- signif(se, digits = 1)
  }
  
  # --- Formatting the mean with *2 significant figures*, preserving trailing zeros ---
  if (mean_sf2 == 0) {
    dec_places_mean <- 2  # e.g. show "0.00"
  } else {
    order <- floor(log10(abs(mean_sf2)))     # e.g. 0.2 → -1; 180 → 2; 0.054 → -2
    dec_places_mean <- max(0, 2 - 1 - order) # formula for # of decimals for 2 sf
  }
  mean_str <- formatC(mean_sf2, format = "f", digits = dec_places_mean)
  
  # SE: use whatever decimals are implied by se_rounded itself
  se_str <- format(se_rounded, scientific = FALSE, trim = TRUE)
  
  # Return mean ± SE, rounded appropriately
  paste0(mean_str, " ± ", se_str)
}


## Create a section header row for stacked tables
## - First column contains the section label (e.g., nutrient name)
## - All other columns are NA so downstream formatting can detect/merge section rows
section_row <- function(col_names, first_col_label) {
  vals <- rep(NA_character_, length(col_names))
  vals[1] <- first_col_label
  tibble::as_tibble_row(stats::setNames(as.list(vals), col_names))
}


## Add a formatted flextable to a Word document (officer)
## - Keeps/renames columns to match `col_names` and desired order
## - Detects section header rows as rows with NA in the 2nd column, then:
##   * bolds + shades them
##   * merges across all columns
##   * adds a thin horizontal separator line
## - Optionally adds vertical lines at columns `vline_j`
## Returns: updated `doc` object with title + table + trailing blank paragraph
add_table <- function(doc, df, col_names, widths, title, vline_j = NULL) {
  
  # keep only the columns we actually want, in the order we want
  df <- df %>%
    dplyr::select(dplyr::all_of(names(col_names)))
  
  # rows that are section titles (have NA in the first numeric column)
  col_to_check <- names(col_names)[2]    # second column in the table
  is_section   <- is.na(df[[col_to_check]])
  sec_rows   <- which(is_section)
  
  # a subtle border for section separators
  sec_border <- fp_border(color = "black", width = 0.5)
  
  ft <- df %>%
    flextable() %>%
    set_header_labels(values = col_names) %>%
    fontsize(size = 10, part = "all") %>%
    font(fontname = "Times New Roman", part = "all") %>%
    theme_booktabs() %>%
    #autofit() %>%
    width(j = 1:ncol(df), width = widths) %>%             
    set_table_properties(#layout = "autofit",
      align  = "left") %>%
    # section title rows: bold + merged across all columns
    bold(i = is_section, bold = TRUE, part = "body") %>%
    bg(i = is_section, j = 1:ncol(df), bg = "#F2F2F2", part = "body") %>%
    padding(i = is_section, padding.top = 3, padding.bottom = 2,
            part = "body") %>%
    align(i = is_section, j = 1, align = "left", part = "body")
  
  if (!is.null(vline_j)) {
    ft <- flextable::vline(ft, j = vline_j, border = officer::fp_border(width = 0.5), part = "all")
  }
  
  # Merge each section row across all columns
  if (length(sec_rows) > 0) {
    for (r in sec_rows) {
      ft <- merge_at(ft, i = r, j = 1:ncol(df), part = "body")
    }
    # thin line above each section (skip first if you prefer)
    ft <- hline(ft, i = sec_rows, border = sec_border, part = "body")
  }
  
  doc %>%
    body_add_fpar(
      fpar(ftext(title, fp_text(font.family = "Times New Roman", bold = TRUE, font.size = 12)))
    ) %>%
    #body_add_par(title, style = "Normal") %>%
    body_add_flextable(ft) %>%
    body_add_par("")
}

