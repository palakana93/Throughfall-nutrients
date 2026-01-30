
## Map a nutrient column name to its corresponding fertilization factor (P, N, or K)
## Used to select the correct treatment indicator for grouping/plotting (e.g., "PO4_ppm" → "P", "NO3_ppm" → "N")
## Note: this assumes nutrient column names begin with "P", "N", or "K"
nutrient_to_factor <- function(nutrient_chr) {
  if (startsWith(nutrient_chr, "P")) {
    "P"
  } else if (startsWith(nutrient_chr, "N")) {
    "N"
  } else if (startsWith(nutrient_chr, "K")) {
    "K"
  } else {
    stop("No fertilization factor mapping for nutrient: ", nutrient_chr)
  }
}

## Treatment-effect boxplot for a response variable.
## Plots nutrient values by Treatment; point layer shows individual observations (jittered),
## while boxplots summarize the distribution. Color is mapped to the corresponding fertilization
## factor (P, N, or K) via nutrient_to_factor().
treatmentEffectPlot <- function(data, resp_var, point.size = 1) {
  data %>% filter(!is.na(.data[[resp_var]])) %>%
    ggplot(aes(x = Treatment, y = .data[[resp_var]], color = .data[[nutrient_to_factor(resp_var)]])) + 
    geom_boxplot(outlier.shape = NA) + 
    geom_point(position = position_jitter(width = 0.2), pch = 1, size = point.size, show.legend = FALSE) +
    theme_bw() + 
    labs(x = NULL, y = NULL, color = NULL) + 
    theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1, color = "black", size = 10),
          axis.text.y = element_text(color = "black", size = 10),
          legend.position = "none")
}

## Convenience wrapper to apply a standardized color scale and optional legend formatting.
## Adds the supplied scale_color_manual (scm) object and, if requested, formats legend placement:
## - legend.pos = NULL: no legend (default)
## - legend.pos = "right": legend placed at right
## - legend.pos = "inside": legend placed inside plot at legend.coord = c(x, y)
add_theme <- function(p, scm, legend.pos = NULL, legend.coord = NULL) {
  if(is.null(legend.pos)) {
    p + scm
  } else if (legend.pos == "right") {
    p + scm +
      theme(legend.position = "right",
            legend.title = element_blank(),
            legend.key.size = unit(0.75, 'cm'), 
            legend.text = element_text(size = 10))
  } else if (legend.pos == "inside" && !is.null(legend.coord) && length(legend.coord) == 2) {
    p + scm +
      theme(legend.position = "inside",
            legend.position.inside = c(legend.coord[1], legend.coord[2]),
            legend.title = element_blank(),
            legend.key.size = unit(0.75, 'cm'), 
            legend.text = element_text(size = 10))
  } else {
    stop("Invalid legend.pos or legend.coord. See add_theme() documentation.")
  }
}

## Breaks for log10 scales that look reasonable across narrow vs wide ranges
## Used for Fig. 2 and Fig. S3 (LAI x )
log_adaptive_breaks <- function(x) {
  rng <- range(x, na.rm = TRUE)
  rng <- rng[rng > 0]  # log-scale requires positive values
  if (length(rng) < 2) return(NULL)
  
  span <- rng[2] / rng[1]
  if (!is.finite(span)) return(NULL)
  
  if (span < 31.6) {   # < 1.5 orders of magnitude: use powers of 2 for line breaks
    k_min <- floor(log(rng[1], base = 2))
    k_max <- ceiling(log(rng[2], base = 2))
    2^(k_min:k_max)
  } else {             # ≥ 1.5 orders of magnitude: use powers of 10 for line breaks
    e_min <- floor(log10(rng[1]))
    e_max <- ceiling(log10(rng[2]))
    10^(e_min:e_max)
  }
}
