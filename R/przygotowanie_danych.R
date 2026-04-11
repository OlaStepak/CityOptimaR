#' Prepare data for fuzzy MCDA analysis
#'
#' Prepares raw data for multi-criteria decision analysis in fuzzy environment.
#' - Normalizes data to Saaty scale (1-9)
#' - Transforms values to Triangular Fuzzy Numbers (TFN)
#'
#' @param data Data frame with raw data
#' @param criteria_model Optional vector of criteria names (e.g., c("Cost", "Safety"))
#' @return List with two elements:
#'   - saaty: data frame with values in 1-9 scale
#'   - tfn: 3D matrix (alternatives x criteria x 3) with TFN values
#' @export
prepare_data <- function(data, criteria_model = NULL) {

  if(!is.data.frame(data)) stop("Data must be a data.frame")

  criteria_cols <- setdiff(names(data), c("City", "Country"))
  if(!is.null(criteria_model)) {
    # if model provided, use only specified columns
    missing_cols <- setdiff(criteria_model, criteria_cols)
    if(length(missing_cols) > 0) stop(
      "Brak kolumn w danych: ", paste(missing_cols, collapse = ", ")
    )
    criteria_cols <- criteria_model
  }
  criteria_data <- data[, criteria_cols, drop = FALSE]

  saaty_data <- as.data.frame(lapply(criteria_data, function(x) {
    rng <- range(x, na.rm = TRUE)
    if(rng[1] == rng[2]) return(rep(5, length(x))) # no variance -> 5
    1 + 8 * (x - rng[1]) / (rng[2] - rng[1])
  }))

  n_alt <- nrow(saaty_data)
  n_crit <- ncol(saaty_data)
  tfn_array <- array(NA, dim = c(n_alt, n_crit, 3),
                     dimnames = list(NULL, criteria_cols, c("l", "m", "u")))

  for(j in seq_len(n_crit)) {
    tfn_array[, j, ] <- t(sapply(saaty_data[[j]], function(v) c(v-0.5, v, v+0.5)))
  }

  return(list(
    saaty = saaty_data,
    tfn = tfn_array
  ))
}

utils::globalVariables(c("City", "Country"))

#' @title Internal MCDA Syntax Parser
#' @description Helper function to interpret user-defined model syntax.
#' Converts text "Criterion =~ var1 + var2" into R list structure.
#' @keywords internal
.parse_mcda_syntax <- function(syntax) {
  clean_syntax <- gsub("\n", "", syntax)
  lines <- strsplit(clean_syntax, ";")[[1]]

  mapping <- list()
  for (line in lines) {

    parts <- strsplit(line, "=~")[[1]]
    if (length(parts) == 2) {
      criterion_name <- trimws(parts[1])
      elements <- trimws(strsplit(parts[2], "\\+")[[1]])
      mapping[[criterion_name]] <- elements
    }
  }
  return(mapping)
}

#' @title Internal Saaty Scaler
#' @description Transforms any scale (e.g., Likert 1-5, continuous values)
#' to Saaty scale 1-9.
#' @keywords internal
.scale_to_saaty <- function(vector) {
  if (any(vector < 0, na.rm = TRUE))
    stop("Negative values detected in input data.")

  vector[is.na(vector) | vector == 99] <- 0

  valid_mask <- vector > 0
  values <- vector[valid_mask]

  if (length(values) == 0) return(vector)

  min_v <- min(values)
  max_v <- max(values)

  if (min_v == max_v) {
    vector[valid_mask] <- 1
  } else {
    # Formula: 1 + (x - min) * (8 / (max - min))
    vector[valid_mask] <- 1 + (values - min_v) * (8 / (max_v - min_v))
  }
  return(vector)
}

#' @title Internal Fuzzifier Function
#' @description Converts crisp number to Triangular Fuzzy Number (TFN).
#' TFN is a triple (l, m, u), where m = x, l = x-1, u = x+1.
#' @keywords internal
.fuzzify_vector <- function(vector) {
  l <- pmax(1, vector - 1)
  m <- vector
  u <- pmin(9, vector + 1)

  is_zero <- (vector == 0)
  l[is_zero] <- 0
  m[is_zero] <- 0
  u[is_zero] <- 0

  return(cbind(l, m, u))
}

#' Prepare MCDA Data with Advanced Syntax
#'
#' @description Transforms raw survey data into fuzzy decision matrix.
#' Calculates composite scores based on syntax, scales to 1-9 interval,
#' aggregates expert responses (if applicable), and performs fuzzification.
#'
#' @param data Data frame containing raw variables
#' @param syntax String defining criteria (e.g., "Cost =~ k1 + k2")
#' @param alternative_column Name of column identifying alternatives.
#'   If NULL, each row is treated as separate alternative.
#' @param aggregation_function Function used to merge expert opinions (default: mean)
#' @return Matrix of dimensions (m x 3n), where m is number of alternatives
#' @importFrom stats aggregate
#' @export
#' @examples
#' \dontrun{
#' # Example with city data
#' syntax <- "Quality =~ Safety + Healthcare; Cost =~ Cost_Living + Housing_Price"
#' result <- prepare_mcda_data(city_data, syntax, alternative_column = "City")
#' }
prepare_mcda_data <- function(data,
                              syntax,
                              alternative_column = NULL,
                              aggregation_function = mean) {

  if (!is.data.frame(data))
    stop("Argument 'data' must be a data frame.")

  mapping <- .parse_mcda_syntax(syntax)
  criterion_names <- names(mapping)

  temp_results <- data.frame(row_id = 1:nrow(data))

  for (crit in criterion_names) {
    variables <- mapping[[crit]]

    missing <- variables[!variables %in% names(data)]
    if (length(missing) > 0)
      stop(paste("Missing variables in data:", paste(missing, collapse=", ")))

    if (length(variables) > 1) {
      raw_score <- rowMeans(data[, variables, drop = FALSE], na.rm = TRUE)
    } else {
      raw_score <- data[[variables]]
    }

    temp_results[[crit]] <- .scale_to_saaty(raw_score)
  }

  if (!is.null(alternative_column)) {
    if (!alternative_column %in% names(data))
      stop("Alternative column not found in data.")

    temp_results$Alternative_ID <- data[[alternative_column]]

    aggregated_data <- aggregate(. ~ Alternative_ID,
                                 data = temp_results[, -1],
                                 FUN = aggregation_function)

    aggregated_data <- aggregated_data[order(aggregated_data$Alternative_ID), ]
    row_names <- aggregated_data$Alternative_ID
    result_matrix <- as.matrix(aggregated_data[, criterion_names])
  } else {
    result_matrix <- as.matrix(temp_results[, criterion_names])
    row_names <- 1:nrow(result_matrix)
  }

  decision_list <- list()
  for (i in seq_along(criterion_names)) {
    crit <- criterion_names[i]
    decision_list[[crit]] <- .fuzzify_vector(result_matrix[, i])
  }

  final_matrix <- do.call(cbind, decision_list)
  rownames(final_matrix) <- row_names

  attr(final_matrix, "criterion_names") <- criterion_names

  return(final_matrix)
}
