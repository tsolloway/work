# Generate example_data_brand and example_data_brand_dictionary
# Simulated consumer brand perception survey data (5-point Likert scale)
# 2000 respondents, ~30 brand perception attributes in 6 thematic clusters,
# 2 outcome variables (ltr, overall_satisfaction), 3 demographic subgroups

set.seed(42)
library(MASS)
library(tibble)
library(uuid)

n <- 2000

# --- Define attribute clusters ---
clusters <- list(
  quality = c("bp_high_quality", "bp_reliable", "bp_durable",
              "bp_well_made", "bp_consistent", "bp_premium_ingredients"),
  innovation = c("bp_innovative", "bp_cutting_edge", "bp_creative",
                  "bp_modern", "bp_trendsetting"),
  trust = c("bp_trustworthy", "bp_transparent", "bp_ethical",
            "bp_honest", "bp_reputable"),
  value = c("bp_good_value", "bp_affordable", "bp_worth_the_price",
            "bp_fair_pricing", "bp_cost_effective"),
  emotional = c("bp_exciting", "bp_inspiring", "bp_fun",
                "bp_makes_me_happy", "bp_cool"),
  social = c("bp_environmentally_friendly", "bp_socially_responsible",
             "bp_gives_back", "bp_sustainable")
)

outcomes <- c("ltr", "overall_satisfaction")
all_vars <- c(unlist(clusters, use.names = FALSE), outcomes)
p <- length(all_vars)

# --- Build correlation matrix ---
# Base between-cluster correlation
cor_mat <- matrix(0.25, nrow = p, ncol = p)
diag(cor_mat) <- 1

# Within-cluster correlations (r ~ 0.45-0.55)
cluster_sizes <- vapply(clusters, length, integer(1))
cluster_ranges <- list()
start <- 1
for (nm in names(clusters)) {
  end <- start + cluster_sizes[nm] - 1
  idx <- start:end
  cluster_ranges[[nm]] <- idx
  for (i in idx) {
    for (j in idx) {
      if (i != j) cor_mat[i, j] <- runif(1, 0.45, 0.55)
    }
  }
  start <- end + 1
}

# Bridge correlations between conceptually linked clusters
# These create realistic cross-battery signal for structure learning
bridges <- list(
  c("quality", "trust"),       # quality and trust naturally co-occur
  c("quality", "value"),       # quality perception ties to value
  c("innovation", "emotional"),# innovation drives emotional appeal
  c("trust", "social"),        # trust and social responsibility linked
  c("emotional", "innovation"),# excitement tied to innovation
  c("value", "quality"),       # value perception reinforces quality
  c("social", "emotional"),    # social responsibility feels good
  c("trust", "quality"),       # trust reinforces quality perception
  c("innovation", "value")     # innovation can signal value
)

for (bridge in bridges) {
  idx_a <- cluster_ranges[[bridge[1]]]
  idx_b <- cluster_ranges[[bridge[2]]]
  for (i in idx_a) {
    for (j in idx_b) {
      val <- runif(1, 0.30, 0.40)
      cor_mat[i, j] <- val
      cor_mat[j, i] <- val
    }
  }
}

# Outcome correlations with attributes
outcome_idx <- (p - 1):p
# Quality and trust clusters correlate higher with outcomes
high_outcome_clusters <- c("quality", "trust")
for (oi in outcome_idx) {
  for (nm in names(clusters)) {
    r_range <- if (nm %in% high_outcome_clusters) c(0.35, 0.50) else c(0.20, 0.35)
    for (ai in cluster_ranges[[nm]]) {
      val <- runif(1, r_range[1], r_range[2])
      cor_mat[oi, ai] <- val
      cor_mat[ai, oi] <- val
    }
  }
}

# Outcome-to-outcome correlation
cor_mat[outcome_idx[1], outcome_idx[2]] <- 0.55
cor_mat[outcome_idx[2], outcome_idx[1]] <- 0.55

# Make positive definite (small eigenvalue nudge)
eigen_decomp <- eigen(cor_mat, symmetric = TRUE)
if (any(eigen_decomp$values < 1e-6)) {
  eigen_decomp$values <- pmax(eigen_decomp$values, 1e-6)
  cor_mat <- eigen_decomp$vectors %*% diag(eigen_decomp$values) %*% t(eigen_decomp$vectors)
  # Re-normalize to correlation matrix
  d <- sqrt(diag(cor_mat))
  cor_mat <- cor_mat / (d %o% d)
}

# --- Demographic subgroups ---
gen_probs <- c(Gen_Z = 0.30, Millennials = 0.40, Gen_X = 0.30)
gen_labels <- sample(
  rep(names(gen_probs), times = round(gen_probs * n)),
  size = n,
  replace = FALSE
)
# Ensure exact n
if (length(gen_labels) < n) {
  gen_labels <- c(gen_labels, sample(names(gen_probs), n - length(gen_labels), replace = TRUE))
} else if (length(gen_labels) > n) {
  gen_labels <- gen_labels[1:n]
}

# --- Mean shifts by generation ---
base_means <- rep(3.0, p)
names(base_means) <- all_vars

gen_shifts <- list(
  Gen_Z = setNames(rep(0, p), all_vars),
  Millennials = setNames(rep(0, p), all_vars),
  Gen_X = setNames(rep(0, p), all_vars)
)

# Gen Z: higher on innovation + emotional
for (v in clusters$innovation) gen_shifts$Gen_Z[v] <- 0.3
for (v in clusters$emotional) gen_shifts$Gen_Z[v] <- 0.25
for (v in clusters$social) gen_shifts$Gen_Z[v] <- 0.2

# Gen X: higher on quality + value
for (v in clusters$quality) gen_shifts$Gen_X[v] <- 0.25
for (v in clusters$value) gen_shifts$Gen_X[v] <- 0.3
for (v in clusters$trust) gen_shifts$Gen_X[v] <- 0.15

# Millennials: slight bump on trust + social
for (v in clusters$trust) gen_shifts$Millennials[v] <- 0.15
for (v in clusters$social) gen_shifts$Millennials[v] <- 0.15

# --- Generate data per generation, then combine ---
all_data <- list()
for (gen in names(gen_probs)) {
  n_gen <- sum(gen_labels == gen)
  mu <- base_means + gen_shifts[[gen]]
  raw <- mvrnorm(n = n_gen, mu = mu, Sigma = cor_mat)
  all_data[[gen]] <- raw
}

raw_matrix <- do.call(rbind, all_data)

# Reorder to original respondent order
reorder_idx <- integer(n)
reorder_idx[gen_labels == "Gen_Z"] <- 1:sum(gen_labels == "Gen_Z")
reorder_idx[gen_labels == "Millennials"] <- (sum(gen_labels == "Gen_Z") + 1):(sum(gen_labels == "Gen_Z") + sum(gen_labels == "Millennials"))
reorder_idx[gen_labels == "Gen_X"] <- (sum(gen_labels == "Gen_Z") + sum(gen_labels == "Millennials") + 1):n
raw_matrix <- raw_matrix[order(reorder_idx), ]

# --- Bin to 1-5 Likert scale using quantile thresholds ---
likert_mat <- matrix(NA_integer_, nrow = n, ncol = p)
colnames(likert_mat) <- all_vars

for (j in seq_len(p)) {
  x <- raw_matrix[, j]
  breaks <- quantile(x, probs = c(0, 0.15, 0.35, 0.65, 0.85, 1))
  breaks[1] <- -Inf
  breaks[length(breaks)] <- Inf
  likert_mat[, j] <- as.integer(cut(x, breaks = breaks, labels = 1:5, include.lowest = TRUE))
}

# --- Brand assignment ---
brand_names <- c("Apex", "Vero", "Kinetic", "Solace", "Drift", "Ember")
brand_probs <- c(0.30, 0.10, 0.17, 0.15, 0.13, 0.15)
brand_ids <- sample(
  seq_along(brand_names),
  size = n,
  replace = TRUE,
  prob = brand_probs
)

# --- Assemble final tibble ---
example_data_brand <- tibble::tibble(
  resp_id = vapply(seq_len(n), function(i) uuid::UUIDgenerate(), character(1)),
  Brand = brand_names[brand_ids],
  Brand_id = brand_ids,
  weight = round(runif(n, min = 0.3, max = 3.0), 2),
  Total = 1L,
  Gen_Z = as.integer(gen_labels == "Gen_Z"),
  Millennials = as.integer(gen_labels == "Millennials"),
  Gen_X = as.integer(gen_labels == "Gen_X")
)

for (v in all_vars) {
  example_data_brand[[v]] <- likert_mat[, v]
}

# --- Swap field data between selected pairs ---
# Field names stay the same; the underlying data values are exchanged
# between each pair, so each column inherits the other's distribution.
# Used to deliberately introduce label/data mismatch for testing label
# integrity in downstream pipelines.
.swap_pairs <- list(
  c("bp_trustworthy", "bp_reputable"),
  c("bp_honest", "bp_environmentally_friendly"),
  c("bp_high_quality", "bp_affordable"),
  c("bp_high_quality", "bp_worth_the_price"),
  c("bp_cool", "bp_honest")
)
for (pair in .swap_pairs) {
  a <- pair[1]; b <- pair[2]
  tmp <- example_data_brand[[a]]
  example_data_brand[[a]] <- example_data_brand[[b]]
  example_data_brand[[b]] <- tmp
}
rm(.swap_pairs, pair, a, b, tmp)

# --- Dictionary ---
labels_map <- c(
  bp_high_quality = "High Quality",
  bp_reliable = "Reliable",
  bp_durable = "Durable",
  bp_well_made = "Well Made",
  bp_consistent = "Consistent",
  bp_premium_ingredients = "Premium Ingredients",
  bp_innovative = "Innovative",
  bp_cutting_edge = "Cutting Edge",
  bp_creative = "Creative",
  bp_modern = "Modern",
  bp_trendsetting = "Trendsetting",
  bp_trustworthy = "Trustworthy",
  bp_transparent = "Transparent",
  bp_ethical = "Ethical",
  bp_honest = "Honest",
  bp_reputable = "Reputable",
  bp_good_value = "Good Value",
  bp_affordable = "Affordable",
  bp_worth_the_price = "Worth the Price",
  bp_fair_pricing = "Fair Pricing",
  bp_cost_effective = "Cost Effective",
  bp_exciting = "Exciting",
  bp_inspiring = "Inspiring",
  bp_fun = "Fun",
  bp_makes_me_happy = "Makes Me Happy",
  bp_cool = "Cool",
  bp_environmentally_friendly = "Environmentally Friendly",
  bp_socially_responsible = "Socially Responsible",
  bp_gives_back = "Gives Back",
  bp_sustainable = "Sustainable",
  ltr = "Likelihood to Recommend",
  overall_satisfaction = "Overall Satisfaction"
)

example_data_brand_dictionary <- tibble::tibble(
  var = names(labels_map),
  label = unname(labels_map)
)

# --- Save ---
usethis::use_data(example_data_brand, overwrite = TRUE)
usethis::use_data(example_data_brand_dictionary, overwrite = TRUE)
