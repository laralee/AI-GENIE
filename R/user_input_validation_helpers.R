# AI-GENIE Checks Helpers ----


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
  if(is.null(input)){
    input <- "Networks Before vs After AI-GENIE"
  }
  return(trimws(input)) # remove leading/trailing white space
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



    if(length(labels) < length(unique(labels))){
      stop("All item labels in `item.attributes` must be unique after trimming whitespace.")
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


    # Check for duplicate stemmed labels within and across item types
    stemmed_labs <- tolower(gsub("[[:punct:]]", "", labels))
    stemmed_labs <- tm::stemDocument(stemmed_labs)

    if (any(duplicated(stemmed_labs))) {
      stop("Ensure that your item type labels are unique after word stemming.")
    }

    attribute <- item.attributes
    names(attribute) <- NULL

    for(i in seq_along(attribute)){
      for(j in seq_along(attribute[[i]])){

        # Check for duplicate stemmed labels within and across item types
        stemmed_attr <- tolower(gsub("[[:punct:]]", "", attribute[[i]][[j]]))
        stemmed_attr <- tm::stemDocument(stemmed_attr)

        if (any(duplicated(stemmed_attr))) {
          stop("Ensure that all of your item attributes are unique after word stemming.")
        }

      }
    }


  } else {
    stop("The 'item.attributes' argument must be a named list with at least one element.")
  }

  # Final check to ensure labels and attributes are aligned
  if (length(labels) != length(attribute)) {
    stop("The number of labels does not match the number of attribute sets.")
  }

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
        stop("'item.examples' must either be a list or a vector.")
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

      max_nchar_gpt <- 800
      max_nchar_open_source <- 1000
      max_nchar <- ifelse(grepl("gpt", model), max_nchar_gpt, max_nchar_open_source)
      char <- nchar(item.examples.str)
      if (char > max_nchar) {
        stop(paste("'item.examples' must be less than", max_nchar, "characters for the", model, "model. The combination of all item strings provided has a total of", char, "characters."))
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
#' @return No return value; the function stops with an error message if validation fails.
validate_EGA_model <- function(EGA.model){
  if(!is.null(EGA.model)){
    if(EGA.model != "tmfg" && EGA.model != "glasso"){
      stop("EGA.model must either be 'tmfg', 'glasso', or NULL.")
    }
  }
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
  } else if (!grepl("gpt", model, ignore.case = TRUE)) {
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
validate_model <- function(model) {
  supported_models <- c("gpt3.5", "gpt4o", "llama3", "mixtral", "gemma2")
  if (!(model %in% supported_models)) {
    stop(paste("Unsupported model. Choose from:", paste(supported_models, collapse = ", ")))
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
