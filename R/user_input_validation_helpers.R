# AI-GENIE Checks Helpers ----


#' Validate and Clean User Input Data Frame
#'
#' Validates a user-provided data frame (`item_examples`) that should contain item definitions.
#' The input can be `NULL`, in which case it is returned as-is. If it is a data frame, it must contain
#' the required columns: `attribute`, `type`, and `statement` (all character type, no missing values),
#' and may optionally include `answer`. Rows are trimmed for whitespace, deduplicated (case-insensitively),
#' and validated against a known list of `attributes`.
#'
#' @param item_examples A data frame or `NULL`. If a data frame, must contain columns `attribute`, `type`, and `statement`.
#' @param attributes A named list of character vectors. Each name corresponds to a valid `type`, and each vector contains valid `attribute` names for that type.
#'
#' @return A cleaned and validated data frame with fixed casing and no duplicates, or `NULL` if `item_examples` is `NULL`.
#'
#' @throws An error if validation fails at any step (e.g., missing columns, non-character types, invalid `type` or `attribute` values).
validate_item_examples_df <- function(item_examples, attributes) {
  # If input is NULL, return NULL silently
  if (is.null(item_examples)) return(NULL)

  # Check that input is a data.frame
  if (!is.data.frame(item_examples)) {
    stop("`item.examples` must be a data frame (recommended), list, or NULL (if choosing to omit).")
  }

  # Required columns
  required_cols <- c("attribute", "type", "statement")

  # Check required columns exist
  missing_cols <- setdiff(required_cols, names(item_examples))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns in `item.examples`:", paste(missing_cols, collapse = ", ")))
  }

  # Check that all columns are character type
  non_char_cols <- names(item_examples)[!sapply(item_examples, is.character)]
  if (length(non_char_cols) > 0) {
    stop(paste("All columns in `item.examples` must be of type character. The following are not:", paste(non_char_cols, collapse = ", ")))
  }

  # Trim whitespace in all cells
  trim <- function(x) gsub("^\\s+|\\s+$", "", x)
  item_examples[] <- lapply(item_examples, trim)

  # Check for missingness (NA, empty string, or whitespace-only)
  has_missing <- function(x) any(is.na(x) | x == "")
  if (any(sapply(item_examples, has_missing))) {
    stop("`item.examples` contains missing values (NA or empty strings) after trimming.")
  }

  # Deduplicate rows (case-insensitive, whitespace-trimmed)
  to_lower_df <- function(df) {
    as.data.frame(lapply(df, function(col) tolower(trimws(col))), stringsAsFactors = FALSE)
  }
  deduped <- !duplicated(to_lower_df(item_examples))
  item_examples <- item_examples[deduped, , drop = FALSE]

  # Fix casing of `type` column to match names(attributes)
  type_lookup <- setNames(names(attributes), tolower(names(attributes)))
  item_examples$type_lower <- tolower(item_examples$type)
  unmatched_types <- setdiff(item_examples$type_lower, names(type_lookup))
  if (length(unmatched_types) > 0) {
    stop(paste("Invalid `type` values found in `item.examples`:\n", paste(unique(unmatched_types), collapse = ", ")))
  }
  item_examples$type <- type_lookup[item_examples$type_lower]
  item_examples$type_lower <- NULL

  # Now fix casing of `attribute` column based on type
  corrected_attributes <- mapply(function(attr, type) {
    valid_attrs <- attributes[[type]]
    attr_match <- valid_attrs[tolower(valid_attrs) == tolower(attr)]
    if (length(attr_match) == 0) {
      stop(paste0("Invalid `attribute` or `difficulty` value in `item.examples`: '", attr, "' not found under type '", type, "'."))
    }
    return(attr_match[1])  # Return with correct casing
  }, attr = item_examples$attribute, type = item_examples$type, USE.NAMES = FALSE)

  item_examples$attribute <- corrected_attributes

  if (nrow(item_examples) > 40){
    warning("You have many rows in `item.examples`. You may run into context window limitations.")
  }

  return(item_examples)
}





#' Validate String Vector
#'
#' Checks if the provided input is a vector where all elements are strings (character type).
#' @param x The input to be validated.
#' @return A logical value: \code{TRUE} if \code{x} is a vector of strings, \code{FALSE} otherwise.
validate_string_vector <- function(x) {
  is.vector(x) && all(is.character(x))
}


#' Validate Title or Domain
#'
#' Ensures that the provided scale title or sub-domain is a string. If \code{input} is \code{NULL}, a default value is assigned.
#' @param input The input string to validate (e.g., scale title or sub-domain).
#' @param input_name A string specifying the name of the input parameter (used in error messages).
#' @return A trimmed string with leading and trailing whitespace removed.
validate_title_or_domain <- function(input, input_name) {
  if (!is.character(input) && !is.null(input)) {
    stop(paste("The", input_name, "must be a string, if specified."))
  }
  if(is.null(input) & input_name == "scale title"){
    input <- "Networks Before vs After AI-GENIE"
  }

  if(is.character(input)){
    input <- trimws(input)

    if(input == ""){
      input <- NULL
    }
  }

  return(input) # remove leading/trailing white space
}


#' Validate Item Attributes
#'
#' Ensures that the provided \code{item.attributes} is a named list or data frame with item type labels, that each item type has at least two attributes (unless \code{items.only = TRUE}), and that there are no duplicate stemmed item characteristics.
#'
#' @param item.attributes A named list or data frame where each element is a character vector of attributes corresponding to each item type.
#' @param items.only Logical; indicates if only items are to be generated without further processing.
#' @return A list containing:
#' \describe{
#'   \item{\code{labels}}{A character vector of item type labels.}
#'   \item{\code{attribute}}{A named list of attributes for each item type.}
#' }
validate_item_attributes <- function(item.attributes, items.only) {

  # Initialize variables
  labels <- NULL
  attribute <- NULL

  if(is.list(item.attributes) && length(item.attributes) >= 1){


    # Handle named list input
    if (is.null(names(item.attributes)) || any(names(item.attributes) == "")) {
      stop("All elements in 'item.attributes' must be named with item type labels.")
    }

    labels <- names(item.attributes)
    labels <- sapply(labels, trimws)
    labels <- sapply(labels, tolower)



    if(length(labels) < length(unique(labels))){
      stop("All item labels in `item.attributes` must be unique after trimming whitespace and converting to lowercase.")
    }

    names(item.attributes) <- labels

    for(i in seq_along(item.attributes)) {
      if(all(sapply(item.attributes[[i]], is.list))){
        stop("All elements in your 'item.attributes' list MUST be a list.")
      }
      for(j in seq_along(item.attributes[[i]])){
      if(is.character(item.attributes[[i]][[j]])){
        item.attributes[[i]][[j]] <- trimws(item.attributes[[i]][[j]])
        if(item.attributes[[i]][[j]] == ""){
        stop("All elements within the lists in 'item.attributes' must not be an empty string after whitespace trimming.")}
      } else {
        stop("All elements within the lists in 'item.attributes' must be strings.")
      }
      }
    }


    # Ensure each element is a character vector
    if(!all(sapply(item.attributes, is.character))) {
      stop("Each element in 'item.attributes' must be a character vector of attributes.")
    }


    # Enforce minimum attribute count if items.only is FALSE
    if (!items.only) {
      invalid_traits <- names(item.attributes)[sapply(item.attributes, length) < 2]
      if (length(invalid_traits) > 0) {
        stop(paste("For a meaningful analysis and to ensure non-redundancy, item types must have at least two attributes. The following item types have insufficient attributes:",
                   paste(invalid_traits, collapse = ", ")))
      }
    }

    invalid_traits <- names(item.attributes)[sapply(item.attributes, function(x){length(x) < length(unique(x))})]
    if (length(invalid_traits) > 0) {
      stop("For a meaningful analysis and to ensure non-redundancy, remove duplicates from your item attributes.")
    }

    attribute <- item.attributes
    names(attribute) <- NULL

    for(i in seq_along(attribute)){
      attr <- c()
      for(j in seq_along(attribute[[i]])){

        # Check for duplicate labels within and across item types
        attr <- c(tolower(gsub("[[:punct:]]", "", attribute[[i]][[j]])), attr)
        }

      if (any(duplicated(attr))) {
        stop("Ensure that all of your attributes are unique for each item type after removing punctuation and converting to lowercase.")
      }
    }


  } else {
    stop("The 'item.attributes' argument must be a named list with at least one element.")
  }

  # Final check to ensure labels and attributes are aligned
  if (length(labels) != length(attribute)) {
    stop("The number of labels does not match the number of attribute sets.")
  }

  item.attributes <- lapply(item.attributes, tolower)
  item.attributes <- lapply(item.attributes, trimws)
  names(item.attributes) <- labels



  return(item.attributes)
}

#' Validate Item Examples
#'
#' Validates and processes the \code{item.examples} input, ensuring it is a list or vector of strings. Checks for formatting issues, special characters, and character limits based on the specified model. Returns a concatenated string of example items or \code{NULL} if none are provided.
#'
#' @param item.examples An optional list or vector of well-crafted example item strings.
#' @param model A character string specifying the model being used (e.g., "gpt3.5", "gpt4o").
#' @return A single concatenated string of example items separated by newlines, or \code{NULL} if \code{item.examples} is \code{NULL} or empty.
validate_item_examples <- function(item.examples, model) {
  if (!is.null(item.examples)) {
    if (length(item.examples) == 0) {
      return(NULL)
    } else {
      if (!is.list(item.examples) && !is.atomic(item.examples)) {
        stop("'item.examples' must either be a data frame (recommended), list, or NULL (if choosing to omit).")
      }

      if (any(sapply(item.examples, function(x){!is.character(x)}))) {
        stop("All elements in 'item.examples' must be strings.")
      }

      item.examples <- sapply(item.examples, trimws)
      item.examples.str <- paste0(item.examples, collapse = "")

      special_chars <- c("{", "}", "\\", "#", "*", "%", "_", "=", "+", ":", "[", "]", "<", ">", "~", "|", "^", "@")
      contains_char <- function(char, string) grepl(char, string, fixed = TRUE)
      result <- sapply(special_chars, function(char) {
        all(sapply(item.examples, contains_char, char = char))
      })
      if (any(result)) {
        warning("Formatting detected for example items. Please ensure your example items are plainly provided and are not formatted/labeled/numbered in any way.")
      }

      numbering_pattern <- "\\b[0-9]+[\\.):]\\s?"
      if (grepl(numbering_pattern, item.examples.str)) {
        warning("Numbering detected in example items. Please ensure your example items are plainly provided and are not formatted/labeled/numbered in any way.")
      }

      max_nchar_gpt <- 8000
      max_nchar_open_source <- 50000
      max_nchar <- ifelse((grepl("gpt", model) || grepl("o1", model) || grepl("o2", model)), max_nchar_gpt, max_nchar_open_source)
      char <- nchar(item.examples.str)
      if (char > max_nchar) {
        warning(paste("Lengthy 'item.examples' detected. You may run into context window limitations."))
      }

      return(paste0(item.examples, collapse = "\n"))
    }
  }
}


#' Validate Target Sample Size
#'
#' Validates the \code{target.N} parameter, ensuring it is a non-negative integer or a list/vector of non-negative integers. Distributes the total target sample size across item types and checks for minimum required items per type.
#'
#' @param target.N A required integer or list/vector of integers specifying the target number of items to generate per item type.
#' @param labels A character vector of item type labels.
#' @param items.only A logical flag indicating whether only items are being generated (\code{TRUE}) without further analysis.
#' @return An integer vector specifying the target number of items for each item type.
validate_target_N <- function(target.N, labels, items.only) {
  if (is.null(target.N) || (is.atomic(target.N) && length(target.N) == 0) ||
    (is.list(target.N) && length(list) == 0)) {
    stop("'target.N' must not be NULL or empty. Please provide a valid integer or list of integers.")
  }

  if ((is.atomic(target.N) && length(target.N) > 1) || (is.list(target.N) && length(target.N) > 1)) {
    target.N <- as.integer(unlist(target.N))

    if (any(target.N < 0)) {
      stop("All elements in the 'target.N' vector must be non-negative integers. Negative values found.")
    }

    if(length(target.N) != length(labels)) {
      stop("If specifying multiple target sample sizes, ensure you have specified exactly one sample size per item type.")
    }

    min_items_per_type <- 15
    if(any(sapply(target.N, function(x) {x < min_items_per_type})) && !items.only){
      stop(paste("Please specify a target sample size of at least", min_items_per_type, "per each item type for a meaningful analysis."))
    }

  } else {
    target.N <- as.integer(target.N)
    if (target.N < 0) {
      stop("'target.N' should be a non-negative whole number or a list/vector of non-negative whole numbers.")
    }

    even_distribution <- ceiling(target.N / length(labels))
    target.N <- rep(even_distribution, length(labels))
  }

  min_N <- 15
  if(sum(target.N) < min_N && !items.only){
    stop(paste0("For a more meaningful analysis, please generate at least ", min_N, " total items."))
  }
  return(target.N)
}


#' Validate EGA Model
#'
#' Validates the \code{EGA.model} parameter to ensure it is either \code{"tmfg"}, \code{"glasso"}, or \code{NULL}.
#'
#' @param EGA.model A character string specifying the model to be used with EGA, one of \code{"tmfg"}, \code{"glasso"}, or \code{NULL}.
#' @return EGA.model string in lowercase; the function stops with an error message if validation fails.
validate_EGA_model <- function(EGA.model){
  if(!is.null(EGA.model)){
    if(is.character(EGA.model)){
    EGA.model <- trimws(tolower(EGA.model))
    if(EGA.model != "tmfg" && EGA.model != "glasso"){
      stop("EGA.model must either be 'tmfg', 'glasso', or NULL.")
    }
    } else {
      stop("EGA.model must be a character string.")
    }

  }
  return(EGA.model)
}

#' Validate Embedding Model
#'
#' Validates the \code{embedding.model} parameter to ensure it is either \code{"text-embedding-3-small"}, \code{"text-embedding-3-large"}, or \code{"text-embedding-ada-002"}.
#'
#' @param embedding A character string specifying the model to be used when embedding, one of  \code{"text-embedding-3-small"}, \code{"text-embedding-3-large"}, or \code{"text-embedding-ada-002"}.
#' @return embedding string in lowercase; the function stops with an error message if validation fails.
validate_embedding <- function(embedding){
  if(is.character(embedding)){
    embedding <- trimws(tolower(embedding))
    if(embedding != "text-embedding-3-small" && embedding != "text-embedding-3-large" && embedding != "text-embedding-ada-002"){
      stop("embedding.model must either be 'text-embedding-3-small', 'text-embedding-3-large', or 'text-embedding-ada-002'.")
    }
  } else {
    stop("embedding.model must be a character string.")
  }

  return(embedding)
}


#' Validate OpenAI API Key
#'
#' Validates the \code{openai.API} parameter to ensure it is a non-empty string.
#'
#' @param openai.API A required character string of your OpenAI API key.
#' @return A trimmed version of the OpenAI API key.
validate_openai <- function(openai.API) {
  if (is.null(openai.API) || openai.API == "") {
    stop("An OpenAI API key is required. Please provide a valid OpenAI key.")
  }
  if (!is.character(openai.API)) {
    stop("'openai.API' must be a string.")
  }

  openai.API <- trimws(openai.API)

  return(openai.API)
}


#' Validate API Keys
#'
#' Validates the provided API keys for OpenAI and Groq. Ensures that the appropriate keys are provided based on the selected model.
#'
#' @param openai.API A required character string of your OpenAI API key.
#' @param groq.API An optional character string of your Groq API key. Required if using an open-source model.
#' @param model A character string specifying the model being used (e.g., "gpt3.5", "llama3").
#' @return A list containing:
#' \describe{
#'   \item{\code{openai.API}}{The trimmed OpenAI API key.}
#'   \item{\code{groq.API}}{The trimmed Groq API key or \code{NULL}.}
#' }
validate_api_keys <- function(openai.API, groq.API, model) {
  if (is.null(openai.API) || openai.API == "") {
    stop("An OpenAI API key is required. Please provide a valid OpenAI key.")
  }
  if (!is.character(openai.API)) {
    stop("'openai.API' must be a string.")
  }

  openai.API <- trimws(openai.API)

  if(!is.null(groq.API)){
  if (groq.API == "") {
    groq.API <- NULL
  }}

  if (!is.null(groq.API)) {
    if (!is.character(groq.API)) {
      stop("'groq.API' must be a string.")
    }
    groq.API <- trimws(groq.API)
  } else if (!grepl("gpt", model, ignore.case = TRUE) && !grepl("o3", model, ignore.case = TRUE) && !grepl("o1", model, ignore.case = TRUE)) {
    stop("A Groq API key is required if using an open-source model. Either specify a 'groq.API' key or use a GPT model.")
  }

  return(list(openai.API = openai.API, groq.API = groq.API))
}



#' Validate Model
#'
#' Validates the \code{model} parameter to ensure it is one of the supported models.
#'
#' @param model A character string specifying the model to be used (e.g., "gpt3.5", "gpt4o", "llama3").
#' @return The normalized model name as a string.
validate_model <- function(model, silently) {
  supported_models <- c("gpt3.5", "gpt4o", "llama3", "mixtral", "gemma2", "deepseek")
  if(is.character(model)){
    model <- tolower(model)
    model <- gsub("\\s+", "", model)
  if (!(model %in% supported_models) && !silently) {
    cat(paste("Your 'model' argument is not recognized. \nEnsure you are using an appropriate model string recognized by the API. \nOtherwise, choose from:", paste(supported_models, collapse = ", ")))
    cat("\n")
    cat("\n")
  }} else {
    stop("Model must be a character string.")
  }
  return(model)
}




#' Validate Temperature and Top P
#'
#' Validates the \code{temperature} and \code{top.p} parameters to ensure they are numeric and within the valid range.
#'
#' @param temperature A numeric value between 0 and 2 used to set the temperature of the language model.
#' @param top.p A numeric value between 0 and 1 used to set the top-p sampling of the language model.
#' @return A list containing validated \code{temperature} and \code{top.p} values.
validate_temperature_top.p <- function(temperature, top.p) {
  if (!is.numeric(temperature)) {
    stop("Temperature must be a numeric value between 0 and 2.")
  }
  if (!is.numeric(top.p)) {
    stop("Top p must be a numeric value between 0 and 1.")
  }

  if (temperature > 2 || temperature < 0) {
    stop("Temperature must be between 0 and 2.")
  }

  if (top.p > 1 || top.p < 0) {
    stop("Top P must be between 0 and 1.")
  }

  return(list(temperature = temperature, top.p = top.p))
}









# GENIE Check Helpers ----


#' Validate Item Data Type
#'
#' Ensures that the provided item data is either a data frame or a matrix. Converts a matrix to a data frame if necessary.
#'
#' @param item.data The item data to validate.
#' @param string A string used in error messages to specify the name of the data.
#' @return A data frame version of the item data.
validate_item_data_type <- function(item.data, string) {
  if (!is.data.frame(item.data) && !is.matrix(item.data)) {
    stop(paste("Ensure", string, "is either a data frame or a matrix."))
  }

  if (is.matrix(item.data)) {
    item.data <- as.data.frame(item.data) # Convert matrix to data frame
  }

  return(item.data)
}



#' Validate No Missing Data
#'
#' Checks for missing values in the provided item data and stops with an error if any are found.
#'
#' @param item.data The item data to check for missing values.
#' @return No return value; the function stops with an error message if missing data is found.
validate_no_missing_data <- function(item.data) {
  if (any(is.na(item.data))) {
    stop("Missing data found in your data. Please ensure there are no missing values in your data.")
  }
}


#' Validate Columns
#'
#' Ensures that the item data has exactly two columns and that at least one of them is of character type.
#'
#' @param item.data The item data to validate.
#' @param string A string used in error messages to specify the name of the data.
#' @return No return value; the function stops with an error message if validation fails.
validate_columns <- function(item.data, string) {
  if (ncol(item.data) != 3) {
    stop(paste("Ensure", string, "has exactly three columns: ", "\n",
               "one for items (called `statement`), ", "\n",
               "one for item types/labels (called `type`),", "\n",
               "and one for item attributes (called `attribute`)."))
  }

  validate_cols <- sapply(colnames(item.data), function(x){x %in% c("statement", "type", "attribute")})

  if(!all(validate_cols)){
    stop(paste("Ensure the coloumns of", string, "are named `statement`, `type`, and `attribute`."))
  }

  col1 <- item.data[,1]
  col2 <- item.data[,2]
  col3 <- item.data[,3]

  validate_cols <- sapply(list(col1, col2, col3), is.character)

  if(!all(validate_cols)){
    stop(paste("Ensure all columns of", string, "are character type."))
  }

}



#' Clean Item Data
#'
#' Trims whitespace and converts all columns in the item data to character type.
#'
#' @param item.data The item data to clean.
#' @param string A string used in error messages to specify the name of the data.
#' @return A cleaned data frame with all columns as character type.
clean_item_data <- function(item.data, string) {
  item.data <- as.data.frame(lapply(item.data, function(x) as.character(trimws(x))), stringsAsFactors = FALSE)
  return(item.data)
}


#' Validate Non-Empty Items and Labels
#'
#' Checks that all item statements and labels are non-empty strings after trimming whitespace.
#'
#' @param items A character vector of item statements.
#' @param item.labels A character vector of item labels.
#' @param string A string used in error messages to specify the name of the data.
#' @param item.attributes A character vector of the item attributes
#' @return No return value; the function stops with an error message if validation fails.
validate_non_empty_items_labels <- function(items, item.labels, item.attributes, string) {
  if (any(nchar(items) == 0)) {
    stop(paste("All items in", string, "must be non-empty strings and not just whitespace."))
  }

  if (any(nchar(item.labels) == 0)) {
    stop(paste("All item type labels in",string,"must be non-empty strings after conversion from numeric types."))
  }

  if (any(nchar(item.attributes) == 0)) {
    stop(paste("All item attribute labels in",string,"must be non-empty strings after conversion from numeric types."))
  }
}


#' Deduplicate Item Data
#'
#' Removes duplicate rows from the item data based on item statements and labels.
#'
#' @param item.data The item data to deduplicate.
#' @param string A string used in error messages to specify the name of the data.
#' @return A deduplicated data frame of item data.
deduplicate_item_data <- function(item.data, string) {
  item.data <- unique(data.frame("statement" = item.data[["statement"]], "type" = item.data[["type"]],
                                 "attribute" = item.data[["attribute"]],stringsAsFactors = FALSE))
  return(item.data)
}


#' Validate No Duplicate Items
#'
#' Checks for duplicate item statements across different labels and stops with an error if any are found.
#'
#' @param item.data The item data to validate.
#' @param string A string used in error messages to specify the name of the data.
#' @return No return value; the function stops with an error message if duplicates are found.
validate_no_duplicate_items <- function(item.data, string) {
  duplicate_items <- duplicated(item.data$statement)
  if (any(duplicate_items)) {
    dup_items <- unique(item.data$statement[duplicate_items])
    dup_labels <- sapply(dup_items, function(x) paste(unique(item.data$type[item.data$statement == x]), collapse = ", "))
    stop(paste0("Duplicate items detected in", string,". Ensure each item is unique. Duplicates found for items: ",
               paste(dup_items, "with labels:", dup_labels, collapse = "; "), "."))
  }
}


#' Validate Items Per Type
#'
#' Ensures that there are enough items per item type for meaningful analysis.
#'
#' @param item.data The item data to validate.
#' @param min_items_per_type An integer specifying the minimum required items per type. Defaults to 5.
#' @return No return value; the function stops with an error message if validation fails.
validate_items_per_type <- function(item.data, min_items_per_type = 30, min_per_attribute = 10) {
  item_counts <- table(item.data$type)
  if (any(item_counts < min_items_per_type)) {
    insufficient_types <- names(item_counts)[item_counts < min_items_per_type]
    stop(paste("Please provide at least", min_items_per_type, "items per item type. Found fewer items for the following types:",
               paste(insufficient_types, collapse = ", "), "."))
  }

  item_counts <- table(item.data$attribute)
  if (any(item_counts < min_per_attribute)) {
    insufficient_types <- names(item_counts)[item_counts < min_per_attribute]
    stop(paste("Please provide at least", min_per_attribute, "items per item attribute. Found fewer items for the following attributes:",
               paste(insufficient_types, collapse = ", "), "."))
  }
}


#' Validate Total Items
#'
#' Ensures that the total number of items provided meets the minimum requirement for analysis.
#'
#' @param item.data The item data to validate.
#' @param min_items An integer specifying the minimum total number of items required. Defaults to 50.
#' @return No return value; the function stops with an error message if validation fails.
validate_total_items <- function(item.data, min_items = 50) {
  if (nrow(item.data) < min_items) {
    stop(paste("Please provide at least", min_items, "items to analyze. Currently, there are only", nrow(item.data), "items."))
  }
}








# GENIE Checks Helpers ----

#' Validate System Role
#'
#' Validates the \code{system.role} parameter, ensuring it is a string within the character limit or \code{NULL}.
#'
#' @param system.role An optional character string describing the language model's role.
#' @param max_char An integer specifying the maximum allowed characters. Defaults to 800.
#' @return The trimmed \code{system.role} string or \code{NULL}.
validate_system_role <- function(system.role, max_char = 800) {

if(!is.null(system.role)) {
  # check if the system role is set to NULL in a defacto way
  if(system.role == "") {
      system.role <- NULL

      } else if (is.character(system.role)) { # case when user provided their own system role

        # trim whitespace
        system.role <- trimws(system.role)

        # check to ensure that system role is not too long
        if(nchar(system.role) > max_char) {
          warning("Lengthy system role detected. You may run into context length limitations.")

        } }else { # case when system role is not a string nor a null object
          stop("System role must be a string")
        }
    }

      return(system.role)
  }


#' Validate Item Types
#'
#' Validates the \code{item.types} parameter, ensuring it is a list or vector of unique, non-empty strings.
#'
#' @param item.types A required list or vector of item type labels.
#' @param items.only A logical flag indicating whether only items are being generated (\code{TRUE}) without further analysis.
#' @return A character vector of validated item type labels.
validate_item_types <- function(item.types, items.only){
  if(!is.list(item.types) && !is.atomic(item.types)){
    stop("'item.type' must be a list or a vector.")
  }

  # ensure it isn't empty
  if(length(item.types) == 0){
    stop("'item.type' cannot be empty.")
  }

  # check elements in item.types
  if(any(sapply(item.types, function(x){!is.character(x)}))) {
    stop("All elements in 'item.types' must be strings.")
  }

  # trim white space
  item.types <- sapply(item.types, trimws)

  if(any(sapply(item.types, function(x){x==""}))) {
    stop("Elements in 'item.types' cannot be empty strings after removing leading/trailing white space.")
  }

  # All elements should be unique
  if(length(unique(item.types)) < length(item.types)) {
    stop("Duplicate item types detetced. To avoid repetition in your item pool, all item types should be unique. If oversampling of a particular item type is desired, see documentation for the target.N argument.")
  }

  min_item_types <- 2
  if((length(item.types) < min_item_types && !items.only)){
    stop("For a meaningful network analysis, include at least two distinct item types.")
  }

  return(item.types)
}



#' Validate User Prompts
#'
#' Validates the \code{user.prompts} parameter, ensuring it is a named list of unique, non-empty strings.
#'
#' @param user.prompts A required named list or vector of custom prompt strings for each item type.
#' @return A character vector of validated user prompts.
validate_user_prompts <- function(user.prompts) {

  if(is.list(user.prompts) && length(user.prompts) >= 1){
    if(!all(sapply(user.prompts, is.character))) {
      stop("All elements in 'user.prompt' argument must be a string.")
    }

    user.prompts <- sapply(user.prompts, trimws)

    if(any(sapply(user.prompts, function(x){x==""}))){
      stop("Ensure all elements in 'user.prompts' are non-empty strings.")
    }

    if(is.null(names(user.prompts))){
      stop("'user.prompts' must be a named list in which the names are non-empty item type labels.")
    }

    labels <- names(user.prompts)
    labels <- sapply(labels, trimws)

    if(length(labels) != length(user.prompts)){
      stop("Ensure all labels of 'user.prompts' are non-whitespace strings.")
    }

    names(user.prompts) <- labels

  } else{
    stop("'user.prompts' argument must be a named list of at least one element.")
  }

  if(length(unique(unlist(user.prompts))) != length(unlist(user.prompts))){
    warning("Duplicate prompts found in 'user.prompts'. Item pool will be redundant.")
  }
  return(user.prompts)
}




#' Validate Target N for Custom Prompts
#'
#' Validates the \code{target.N} parameter when custom prompts are used, ensuring appropriate sample sizes for meaningful analysis.
#'
#' @param target.N A required integer or list/vector of integers specifying the target number of items to generate per item type.
#' @param labels A character vector of item type labels.
#' @return An integer vector specifying the target number of items for each item type.
validate_target_N_custom <- function(target.N, labels){
  if (is.null(target.N) || (is.list(target.N) && length(target.N) == 0) ||
      (is.vector(target.N) && length(target.N) == 0)) {
    stop("'target.N' must not be NULL or empty. Please provide a valid integer, list, or vector.")
  }

  if (is.list(target.N) || (is.atomic(target.N) && length(target.N) > 1)) {
    target.N <- as.integer(unlist(target.N))

    if (any(target.N < 0)) {
      stop("All elements in the 'target.N' list/vector must be non-negative integers. Negative values found.")
    }


    if(length(target.N) != length(labels)) {
      stop("If specifying multiple target sample sizes, ensure you have specified exactly one sample size per item type.")
    }

    min_items_per_type <- 15
    if(any(sapply(target.N, function(x) {x<min_items_per_type}))){
      stop(paste("Please specify a target sample size of at least", min_items_per_type, "per each item type for a meaningful analysis."))
    }

  } else {
    # Single value for target.N
    target.N <- as.integer(target.N)
    if (target.N < 0) {
      stop("'target.N' should be a non-negative whole number or a list/vector of non-negative whole numbers.")
    }

    # Distribute the total evenly across the number of labels
    even_distribution <- ceiling(target.N / length(labels))
    target.N <- rep(even_distribution, length(labels))

  }

  min_items <- 60
  if(sum(target.N) < min_items) {
    stop(paste("Please indicate a total of at least", min_items, "items for a meaningful analysis."))
  }
  return(target.N)
}


#' Validate Custom Parameters
#'
#' Validates the parameters based on whether custom prompts are being used (\code{custom = TRUE}) or not. Ensures all required parameters are provided and appropriately sets others to \code{NULL} if necessary.
#'
#' @param custom Logical; a flag passed to the function to determine if the user added their own custom prompts
#' @param item.attributes A named list or data frame containing item type labels and their corresponding attributes. The list or data frame must have names or identifiers representing item types. Each element should be a character vector of attributes for that item type.
#' @param user.prompts A list of custom prompt strings for each item type.
#' @param cleaning.fun A function provided by the user to clean and parse the model's output.
#'
#' @return The validated \code{custom} flag.
validate_custom <- function(custom, item.attributes, user.prompts, cleaning.fun) {
  if (custom) {
    # Ensure user.prompts and cleaning.fun are provided
    if (is.null(user.prompts)) {
      stop("When 'custom' is TRUE, 'user.prompts' must be provided. If you do not want to provide custom prompts and a cleaning function, set 'custom' to FALSE.")
    }
    if (is.null(cleaning.fun)) {
      stop("When 'custom' is TRUE, 'cleaning.fun' must be provided. If you do not want to provide custom prompts and a cleaning function, set 'custom' to FALSE.")
    }
    if (!identical(tolower(sort(names(item.attributes))), tolower(sort(names(user.prompts))))){
      stop("Ensure there is exactly one prompt specified for each item type provided in the `item.attributes` object.")
    }
  }

  return(custom)
}


#' Check for NA Values in Parameters
#'
#' Checks provided parameters for any \code{NA} values and stops with an error if any are found.
#'
#' @param ... Parameters to be checked for \code{NA} values.
#' @return No return value; the function stops with an error message if any \code{NA} values are found.
check_no_na <- function(...) {
  params <- list(...)
  param_names <- names(params)

  for (i in seq_along(params)) {
    param <- params[[i]]
    param_name <- param_names[i]


    if (identical(param, NA)) {
      stop(sprintf("Error: The parameter '%s' is set to NA. Please provide a valid value or leave it as NULL.", param_name))
    }


    has_na <- function(x) {
      if (is.atomic(x) || is.factor(x)) {
        return(any(is.na(x)))
      } else if (is.list(x) || is.data.frame(x)) {
        return(any(sapply(x, has_na)))
      } else {
        return(FALSE)
      }
    }

    # If the parameter is a list, data frame, or similar, check for NA within its elements
    if (is.list(param) || is.data.frame(param)) {
      if (has_na(param)) {
        stop(sprintf("Error: The parameter '%s' contains NA values within its elements. Please ensure all elements are non-NA.", param_name))
      }
    } else if (is.atomic(param) && length(param) > 1) {
      # For atomic vectors (e.g., numeric, character), check if any element is NA
      if (any(is.na(param))) {
        stop(sprintf("Error: The parameter '%s' contains NA values. Please ensure all elements are non-NA.", param_name))
      }
    }

  }

  # If all checks pass, return invisibly
  invisible(TRUE)
}



#' Check for Item Type Definitions in Parameters
#'
#' Checks provided parameters for any \code{NA} values and stops with an error if any are found.
#'
#' @param item.type.definitions A character list of the item type definitions
#' @param item.types A characater list of the item type labels
#' @return No return value; the function stops with an error message if any \code{NA} values are found.
validate_item_type_definitions <- function(item.type.definitions, item.types) {
  if (is.null(item.type.definitions)) {
    return(NULL)
  }

  # Ensure it's a named list or data frame
  if (!(is.list(item.type.definitions) || is.data.frame(item.type.definitions))) {
    stop("The 'item.type.definitions' parameter must be a named list or data frame.")
  }

  # Convert data frame to named list if necessary
  if (is.data.frame(item.type.definitions)) {
    if (ncol(item.type.definitions) != 2) {
      stop("The data frame for 'item.type.definitions' must have exactly two columns.")
    }
    if (!all(sapply(item.type.definitions, is.character))) {
      stop("Both columns in 'item.type.definitions' must be character type.")
    }
    item.type.definitions <- setNames(
      as.list(item.type.definitions[[2]]),
      item.type.definitions[[1]]
    )
  }

  # Ensure the list is named
  if (is.null(names(item.type.definitions)) || any(names(item.type.definitions) == "")) {
    stop("All elements in 'item.type.definitions' must be named with item type labels.")
  }

  # Validate definitions
  for (type in names(item.type.definitions)) {
    if (!(type %in% item.types)) {
      stop(paste("Definition provided for unknown item type:", type))
    }
    definition <- item.type.definitions[[type]]
    if (!is.character(definition) || length(definition) != 1) {
      stop(paste("Definition for item type", type, "must be a single character string."))
    }
    if (nchar(definition) > 250) {
      warning(paste("Definition for item type", type, "exceeds 250 characters and will be truncated."))
      item.type.definitions[[type]] <- substr(definition, 1, 250)
    }
  }

  return(item.type.definitions)
}


#' Check the User provided dataframe and then extract the needed columns
#'
#' Checks that the provided data frame has the correct columns (type, attribute, and statement)
#'
#' @param df A data frame that the user provided containing the type, attribute, and statement
#' @return A list of the validated item types and attributes
validate_and_extract_attributes <- function(df) {

  # Perform validation checks

  # Check 1: Ensure the input is a data frame
  if (!is.data.frame(df)) {
    stop("Input must be a data frame.")
  }

  # Check 2: Ensure there are exactly three columns named "statement", "type", and "attribute"
  required_columns <- c("attribute", "statement", "type")
  if (!all(sort(names(df)) == required_columns)) {
    stop("Data frame must have exactly three columns named 'statement', 'type', and 'attribute' in that order.")
  }

  # Check 3: Ensure all columns are character type
  if (!all(sapply(df, is.character))) {
    stop("All columns must be of character type.")
  }

  # Check 4: Ensure there are no missing values
  if (anyNA(df)) {
    stop("Data frame must not contain missing values.")
  }

  # Check 6: Ensure at least 10 rows for each combination of `type` and `attribute`
  combination_counts <- table(df$type, df$attribute)

  indices <- c()
  for (i in 1:length(combination_counts)){
    if(combination_counts[[i]] != 0) {indices <- c(i, indices)}
  }

  if (any(combination_counts[indices] < 10)) {
    stop("Each combination of 'type' and 'attribute' must have at least 10 rows.")
  }

  # Create the named list where each `type` is an element with a list of its unique `attributes`
  type_attribute_list <- split(df$attribute, df$type)
  type_attribute_list <- lapply(type_attribute_list, unique) # Ensure unique attributes per type

  # Return the list
  return(type_attribute_list)
}


#' Check the User provided API keys
#'
#' Checks that the provided at least one valid API string
#'
#' @param openai.API A string that the user provided that should correspond to an OpenAI API string
#' @param groq.API A string that the user provided that should correspond to an Groq API string
#' @param model A string of the LLM model that user provided to generate items
#' @return A cleaned list of the API string(s)
validate_apis <- function(openai.API, groq.API, model){
  if(is.null(openai.API) && is.null(groq.API)){
    stop("Please provide at least one API.")
  }

  if(is.null(openai.API) && (model=="gpt4o" | model=="gpt3.5")){
    stop("You need to provide an openAI API key to use the GPT models.")
  }

  if(is.null(groq.API) && (model=="gemma2" | model=="mixtral" | model=="llama3")){
    stop("You need to provide an Groq API key to use the Gemma, Mixtral, or Llama models.")
  }

  if(!is.null(groq.API)){
    if(!is.character(groq.API)){
      stop("Your Groq API key must be a valid string.")
    }

    groq.API <- trim.default(groq.API)
  }

  if(!is.null(openai.API)){
    if(!is.character(openai.API)){
      stop("Your OpenAI API key must be a valid string.")
    }

    openai.API <- trim.default(openai.API)
  }

  return(list(openai.API = openai.API, groq.API = groq.API))
}

#' Validate EGA Algorithm
#'
#' Checks if the provided EGA input is a string and is one of the accpted EGA algorithms
#' @param EGA.algorithm The input to be validated.
#' @return A cleaned version of the EGA algorithm string
validate_EGA_algorithm <- function(EGA.algorithm){
  if(!is.character(EGA.algorithm)){
    stop("EGA.algorithm must be a valid string. One of 'walktrap', 'louvain', or 'leiden'.")
  } else {
    EGA.algorithm <- tolower(EGA.algorithm)
    EGA.algorithm <- gsub("\\s+", "", EGA.algorithm)

    if(EGA.algorithm != "louvain" && EGA.algorithm != "walktrap" && EGA.algorithm != "leiden"){
      stop("EGA.algorithm must be one of 'walktrap', 'louvain', or 'leiden'.")
    }

  }
  return(EGA.algorithm)
}



#' Validate Flat Character Columns
#'
#' Ensures that the `statement`, `type`, and `attribute` columns are atomic character vectors (not lists or factors).
#'
#' @param item.data A data frame containing the item pool with expected columns: `statement`, `type`, and `attribute`.
#' @param string A descriptor for the error message context (e.g., "your item pool")
#' @return Stops with an informative error if validation fails.
validate_flat_character_columns <- function(item.data, string = "your data") {
  expected <- c("statement", "type", "attribute")
  for (col in expected) {
    x <- item.data[[col]]
    if (!is.character(x) || !is.atomic(x) || is.list(x)) {
      stop(sprintf("Column `%s` in %s must be a flat character vector (not a list, factor, or expression).", col, string))
    }
  }
}

#Performance AIGENIE Checks----

#' Validate Item Difficulty
#'
#' Validates and standardizes the `item.difficulty` parameter for performance-based assessments in the `p_AIGENIE` function.
#'
#' The input should be a list that specifies the difficulty levels for each item type. There are two supported formats:
#' \itemize{
#'   \item \strong{Named list:} The names of the list represent the item types, and each element is a character vector of difficulty values.
#'         Each difficulty value must be one of the accepted synonyms (case insensitive) which are mapped as follows:
#'         \itemize{
#'           \item "very easy", "very simple", "very basic" → "VERY LOW"
#'           \item "easy", "simple", "basic" → "LOW"
#'           \item "average", "standard" → "MEDIUM"
#'           \item "hard", "difficult", "challenging" → "HIGH"
#'           \item "very hard", "very difficult", "very challenging" → "VERY HIGH"
#'         }
#'         Each sublist must contain at least 2 unique difficulty values.
#'
#'   \item \strong{Unnamed list:} If `item.difficulty` is not named (i.e., just a simple list of item type labels),
#'         the function assumes the user wants to assign the default difficulty vector of `c("LOW", "MEDIUM", "HIGH")`
#'         to each item type.
#' }
#'
#' @param item.difficulty A list specifying the difficulty levels for each item type. For a named list, the names serve as item type labels and the elements should be character vectors. For an unnamed list, each element is treated as an item type label.
#'
#' @return A named list with the same item type names as the input, where each element is a character vector of validated
#' and standardized difficulty values in all caps.
#'
#' @examples
#' # Example 1: Named list with synonyms
#' validate_item_difficulty(list(
#'   fractions = c("average", "HARD"),
#'   exponents = c("easy", "VERY easy")
#' ))
#' # Returns: list(fractions = c("MEDIUM", "HIGH"), exponents = c("LOW", "VERY LOW"))
#'
#' # Example 2: Unnamed list (using default difficulties)
#' validate_item_difficulty(list("fractions", "exponents"))
#' # Returns: list(fractions = c("LOW", "MEDIUM", "HIGH"), exponents = c("LOW", "MEDIUM", "HIGH"))
validate_item_difficulty <- function(item.difficulty) {
  # If the provided list is not named (or all names are empty), assume each element is an item type label
  if (is.null(names(item.difficulty)) || all(names(item.difficulty) == "")) {
    # Check that each element is a character string of length 1
    if (!all(sapply(item.difficulty, function(x) is.character(x) && length(x) == 1))) {
      stop("When item.difficulty is not a named list, each element must be a single string representing an item type label.")
    }
    # Use the provided values as the item type names and assign the default difficulties
    default_difficulties <- c("LOW", "MEDIUM", "HIGH")
    item.difficulty <- setNames(
      replicate(length(item.difficulty), default_difficulties, simplify = FALSE),
      unlist(item.difficulty)
    )
  }

  # Now, item.difficulty is a named list. Check that names are nonempty.
  if (any(names(item.difficulty) == "")) {
    stop("All elements in item.difficulty must have nonempty names representing item types.")
  }

  # Define the mapping dictionary (keys in lower case)
  mapping <- c(
    "easy" = "LOW",
    "simple" = "LOW",
    "basic" = "LOW",
    "low" = "LOW",
    "average" = "MEDIUM",
    "medium" = "MEDIUM",
    "moderate" = "MEDIUM",
    "standard" = "MEDIUM",
    "hard" = "HIGH",
    "difficult" = "HIGH",
    "challenging" = "HIGH",
    "high" = "HIGH"
  )

  # Process each sublist in item.difficulty
  validated <- lapply(item.difficulty, function(diff_vec) {
    if (!is.character(diff_vec)) {
      stop("Each element in item.difficulty must be a character vector.")
    }

    # Trim whitespace and convert to lower case
    diff_vec <- tolower(trimws(diff_vec))

    # Map each difficulty using the mapping dictionary
    mapped <- sapply(diff_vec, function(x) {
      if (!x %in% names(mapping)) {
        stop(paste("Difficulty value", x, "is not recognized. Acceptable synonyms include 'very easy', 'average', 'hard', etc."))
      }
      mapping[[x]]
    }, USE.NAMES = FALSE)

    # Check that there are at least 2 difficulty values per item type
    if (length(mapped) < 2) {
      stop("Each item type in item.difficulty must contain at least 2 difficulty values.")
    }

    # Check for duplicates within the sublist
    if (any(duplicated(mapped))) {
      stop("Duplicate difficulty values found within an item type. Ensure each sublist contains unique difficulty values.")
    }

    return(mapped)
  })

  return(validated)
}

#' Validate Item Examples
#'
#' Validates and standardizes the `item.examples` data frame for performance-based assessments in the `p_AIGENIE` function.
#'
#' The input must be a data frame with exactly four columns: `type`, `difficulty`, `statement`, and `answer`. The function performs the following checks:
#' \itemize{
#'   \item Ensures that the `type` column values are present in the valid item types provided (typically the names from `item.difficulty`).
#'   \item Verifies that each entry in the `difficulty` column is one of the acceptable values (case-insensitive and whitespace trimmed), and maps them to their canonical forms:
#'         \itemize{
#'           \item "very easy", "very simple", "very basic" → "VERY LOW"
#'           \item "easy", "simple", "basic" → "LOW"
#'           \item "average", "standard" → "MEDIUM"
#'           \item "hard", "difficult", "challenging" → "HIGH"
#'           \item "very hard", "very difficult", "very challenging" → "VERY HIGH"
#'         }
#'   \item Checks that the `statement` column is not empty or missing.
#'   \item Checks that the `answer` column is provided (it may be numeric or any other type but must not be missing).
#' }
#' If all checks pass, the function returns a standardized data frame with the `difficulty` values converted to their canonical forms (in all caps).
#'
#' @param item.examples A data frame containing columns: `type`, `difficulty`, `statement`, and `answer`.
#' @param valid.types A character vector of valid item types (e.g., the names from the validated `item.difficulty` list).
#'
#' @return A standardized data frame where the `difficulty` column has been validated and converted to its canonical form.
validate_item_examples_p <- function(item.examples, valid.types) {
  # Check that item.examples is a data frame
  if (!is.data.frame(item.examples)) {
    stop("item.examples must be a data frame.")
  }

  # Check that the data frame contains exactly the required columns
  required_cols <- c("type", "difficulty", "statement", "answer")
  if (!all(required_cols %in% colnames(item.examples))) {
    stop(paste("item.examples must contain the following columns:", paste(required_cols, collapse = ", ")))
  }

  # Define the mapping dictionary (keys in lower case)
  mapping <- c(
    "easy" = "LOW",
    "simple" = "LOW",
    "basic" = "LOW",
    "low" = "LOW",
    "average" = "MEDIUM",
    "medium" = "MEDIUM",
    "moderate" = "MEDIUM",
    "standard" = "MEDIUM",
    "hard" = "HIGH",
    "difficult" = "HIGH",
    "challenging" = "HIGH",
    "high" = "HIGH"
  )

  # Process each row
  for (i in seq_len(nrow(item.examples))) {
    row <- item.examples[i, ]

    # Check that 'type' is among valid.types (case-insensitive comparison)
    type_val <- tolower(trimws(as.character(row[["type"]])))
    valid_lower <- tolower(valid.types)
    if (!(type_val %in% valid_lower)) {
      stop(paste("Row", i, " of `item.examples` has an invalid 'type' value. It must be one of:", paste(valid.types, collapse = ", ")))
    }


    # Check the difficulty value
    diff_val <- tolower(trimws(as.character(row[["difficulty"]])))
    if (!diff_val %in% names(mapping)) {
      stop(paste("Row", i, "in item.examples has an unrecognized difficulty value:", row[["difficulty"]],
                 ". Acceptable synonyms include 'easy', 'average', 'hard', etc."))
    }
    # Map the difficulty value to its canonical form
    canonical_diff <- mapping[[diff_val]]
    item.examples[i, "difficulty"] <- canonical_diff

    # Check that 'statement' is not missing or empty (after trimming)
    statement_val <- as.character(row[["statement"]])
    if (is.na(statement_val) || trimws(statement_val) == "") {
      stop(paste("Row", i, "in `item.examples` has an empty or missing 'statement'."))
    }

    # Check that 'answer' is provided (it may be numeric, but not missing)
    answer_val <- row[["answer"]]
    if (is.null(answer_val) || (is.character(answer_val) && trimws(answer_val) == "") || (is.na(answer_val))) {
      stop(paste("Row", i, "in `item.examples` has an empty or missing 'answer'."))
    }
  }

  item.examples[["type"]] <- sapply(item.examples[["type"]], function(x) {
    idx <- which(tolower(valid.types) == tolower(trimws(x)))
    if (length(idx) > 0) valid.types[idx] else x
  })

  item.examples <- data.frame(lapply(item.examples, as.character), stringsAsFactors = FALSE)

  return(item.examples)
}



#' Validate Level Description
#'
#' Validates and standardizes the `level.description` data frame for performance-based assessments in the `p_AIGENIE` function.
#'
#' The input must be a data frame with at least the following columns (case insensitive): `type`, `difficulty`, and `description`. The function performs the following checks:
#' \itemize{
#'   \item If both `level.description` and `item.examples` are NULL, a message is printed (subject to the `silently` flag) warning that results may be poor.
#'   \item Confirms that the data frame contains at least the required columns: `type`, `difficulty`, and `description`.
#'   \item Verifies that every value in these columns is a non-empty character string, trimming any leading or trailing whitespace.
#'   \item Drops duplicate rows.
#'   \item Ensures that every value in the `type` column appears in the names of `item.difficulties`.
#'   \item Maps the values in the `difficulty` column using a case-insensitive mapping (e.g., "easy", "simple", "basic", "low" → "LOW"; "average", "medium", "moderate", "standard" → "MEDIUM"; "hard", "difficult", "challenging", "high" → "HIGH") and checks that every mapped value is one of `c("LOW", "MEDIUM", "HIGH")`.
#' }
#'
#' If all checks pass, the function returns a standardized data frame with column names in lowercase and with the `difficulty` values converted to their canonical forms.
#'
#' @param level.description An optional data frame containing at least the columns: `type`, `difficulty`, and `description`.
#' @param item.examples An optional data frame of item examples. If both this and `level.description` are NULL, a warning is printed.
#' @param item.difficulties A named vector or list representing the valid item types (the names must match the values in the `type` column).
#' @param silently A logical flag; if TRUE, suppresses warning messages.
#'
#' @return A standardized data frame with validated and trimmed `type`, `difficulty`, and `description` columns.
validate_level_description <- function(level.description, item.examples, item.difficulties, silently) {

  # 1. Warn if both level.description and item.examples are NULL.
  if (is.null(level.description) && is.null(item.examples)) {
    if (!silently) message("Warning: Neither 'level.description' nor 'item.examples' was provided. Results may be poor.")
    # If desired, you might continue (with level.description = NULL) or assign a default.
  }

  # If level.description is NULL, return NULL.
  if (is.null(level.description)) {
    return(NULL)
  }

  # 2. Validate that level.description is a data frame with required columns.
  req_cols <- c("type", "difficulty", "description")
  # Convert column names to lowercase for checking.
  names(level.description) <- tolower(names(level.description))

  missing_cols <- setdiff(req_cols, names(level.description))
  if (length(missing_cols) > 0) {
    stop(paste("level.description is missing required column(s):", paste(missing_cols, collapse = ", ")))
  }

  # 3. Ensure all required columns are character strings and non-empty; also trim whitespace.
  for (col in req_cols) {
    if (!all(sapply(level.description[[col]], is.character))) {
      stop(paste("All values in column", col, "must be character strings."))
    }
    # Trim whitespace from each value
    level.description[[col]] <- trimws(level.description[[col]])
    # Check for empty strings
    if (any(level.description[[col]] == "")) {
      stop(paste("Column", col, "contains empty strings."))
    }
  }

  # 4. Drop duplicate rows (without warning)
  level.description <- level.description[!duplicated(level.description), ]

  # 5. Every value in the 'type' column must appear in names(item.difficulties).
  valid_types <- names(item.difficulties)
  if (!all(level.description$type %in% valid_types)) {
    invalid <- unique(level.description$type[!(level.description$type %in% valid_types)])
    stop(paste("The following types in level.description are not in names(item.difficulties):", paste(invalid, collapse = ", ")))
  }

  # 6. Map the values in the 'difficulty' column.
  mapping <- c(
    "easy" = "LOW",
    "simple" = "LOW",
    "basic" = "LOW",
    "low" = "LOW",
    "average" = "MEDIUM",
    "medium" = "MEDIUM",
    "moderate" = "MEDIUM",
    "standard" = "MEDIUM",
    "hard" = "HIGH",
    "difficult" = "HIGH",
    "challenging" = "HIGH",
    "high" = "HIGH"
  )

  # Create a helper function to map a difficulty value (case insensitive).
  map_difficulty <- function(val) {
    val_lower <- tolower(val)
    if (val_lower %in% names(mapping)) {
      return(mapping[[val_lower]])
    } else {
      return(NA_character_)
    }
  }

  mapped_difficulties <- sapply(level.description$difficulty, map_difficulty, USE.NAMES = FALSE)
  level.description$difficulty <- mapped_difficulties

  # Check that all mapped values are valid.
  if (any(is.na(level.description$difficulty))) {
    invalid_diff <- level.description$difficulty[is.na(level.description$difficulty)]
    stop(paste("Some difficulty values could not be mapped. Allowed values are: LOW, MEDIUM, HIGH."))
  }

  # 7. Return the validated and cleaned data frame.
  return(level.description)
}
