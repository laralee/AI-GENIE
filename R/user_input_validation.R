

#' AIGENIE Input Validation
#'
#' Validates and processes the input parameters for the \code{AIGENIE} function. This function ensures that all required parameters are provided, correctly formatted, and meet the necessary conditions for the AI-GENIE pipeline to run successfully. It handles both default and custom modes.
#'
#' @param item.attributes A named list or data frame containing item type labels and their corresponding attributes.
#' @param openai.API A required character string of your OpenAI API key.
#' @param groq.API (Required when using open-source models) A character string of your Groq API key.
#' @param custom Logical; indicates whether custom prompts and a custom cleaning function are used. Defaults to \code{FALSE}.
#' @param user.prompts (Required when \code{custom = TRUE}) A list of custom prompt strings for each item type.
#' @param item.type.definitions A named list or data frame providing definitions for each item type. Each definition should be a character string not exceeding 250 characters. This helps the language model understand the item types better. Definitions are included at the beginning of the prompts for their corresponding item types.
#' @param cleaning.fun (Required when \code{custom = TRUE}) A function provided by the user to clean and parse the model's output.
#' @param system.role An optional character string describing the language model's role.
#' @param scale.title An optional character string specifying the name of your inventory.
#' @param sub.domain An optional character string specifying the inventory's sub-domain or specialty.
#' @param model A character string specifying the model to use for item generation.
#' @param item.examples An optional character vector of example item strings.
#' @param target.N An integer or integer vector specifying the target number of items to generate.
#' @param temperature Numeric; controls the randomness of the model's output.
#' @param top.p Numeric; controls the diversity of the model's output.
#' @param items.only Logical; if \code{TRUE}, only items are generated without further analysis.
#' @param adaptive Logical; if \code{TRUE}, uses adaptive prompting to avoid generating redundant items.
#' @param EGA.model An optional character string specifying the EGA model to use (\code{"tmfg"} or \code{"glasso"}).
#' @param EGA.algorithm A character string specifying the clustering algorithm for EGA (default: \code{"walktrap"}).
#' @param embedding.model A character string specifying the OpenAI embedding model that should be used. The options are `"text-embedding-3-small"`, `"text-embedding-3-large"`, or `"text-embedding-ada-002"`. Defaults to `"text-embedding-3-small"`.
#' @param keep.org Logical; if \code{TRUE}, includes the original items in the returned results.
#' @param plot Logical; if \code{TRUE}, displays the network plots.
#' @param plot.stability Logical; Specifies whether to display the secondary network stability plots.
#' @param calc.final.stability Logical; defaults to `FALSE`. Specifies whether to compute the stability of the item pool before and after item reduction.
#' @param silently Logical; if \code{TRUE}, suppresses console output.
#' @param ... Additional arguments (currently not used).
#' @return A list of validated and processed parameters ready for use in the \code{AIGENIE} function.
AIGENIE_checks <- function(item.attributes, openai.API, groq.API, custom,
                           user.prompts, item.type.definitions, cleaning.fun, system.role,
                           scale.title, sub.domain, model, item.examples,
                           target.N, temperature, top.p, items.only, adaptive, EGA.model, EGA.algorithm, embedding.model,
                           keep.org, plot, plot.stability, calc.final.stability, silently, ...) {

  # Check for missing arguments (no NA values)
  check_no_na(item.attributes, openai.API, groq.API, custom,
              user.prompts, item.type.definitions, cleaning.fun, system.role,
              scale.title, sub.domain, model, item.examples,
              target.N, temperature, top.p, items.only, adaptive, EGA.model, EGA.algorithm, embedding.model,
              keep.org, plot, plot.stability, calc.final.stability, silently)

  # Validate that 'silently' is a boolean
  if (!is.logical(silently) || length(silently) != 1) {
    stop("'silently' must be a boolean.")
  }

  # Simple checks for boolean parameters
  if (!is.logical(items.only) || length(items.only) != 1) {
    stop("'items.only' must be a boolean.")
  }
  if (!is.logical(keep.org) || length(keep.org) != 1) {
    stop("'keep.org' must be a boolean.")
  }
  if (!is.logical(plot) || length(plot) != 1) {
    stop("'plot' must be a boolean.")
  }
  if (!is.logical(adaptive) || length(adaptive) != 1) {
    stop("'adaptive' must be a boolean.")
  }
  if (!is.logical(plot.stability) || length(plot.stability) != 1) {
    stop("'plot.stability' must be a boolean.")
  }
  if (!is.logical(calc.final.stability) || length(calc.final.stability) != 1) {
    stop("'calc.final.stability' must be a boolean.")
  }

  # Validate EGA.model
  EGA.model <- validate_EGA_model(EGA.model)

  # Validate EGA.algorithm
  EGA.algorithm <- validate_EGA_algorithm(EGA.algorithm)

  # Validate embedding.model
  embedding.model <- validate_embedding(embedding.model)

  # Validate scale.title and sub.domain
  scale.title <- validate_title_or_domain(scale.title, "scale title")
  sub.domain <- validate_title_or_domain(sub.domain, "sub domain")

  # Validate item.attributes
  item.attributes <- validate_item_attributes(item.attributes, items.only)

  # Validation when custom = FALSE
  if (!custom) {
    labels <- names(item.attributes)
    attribute <- item.attributes
    names(attribute) <- NULL
  }

  # Validation when custom = TRUE
  if (custom) {
    user.prompts <- validate_user_prompts(user.prompts)
    labels <- names(user.prompts)
    if (!is.function(cleaning.fun)) {
      stop("The 'cleaning.fun' must be a function when 'custom = TRUE'.")
    }
  }

  # Validate item.type.definitions
  item.type.definitions <- validate_item_type_definitions(item.type.definitions, labels)

  # Validate system.role
  system.role <- validate_system_role(system.role)

  # Validate model
  model <- validate_model(model, silently)

  # Validate item.examples
  if(is.data.frame(item.examples)){
    item.examples <- validate_item_examples_df(item.examples, item.attributes)
  } else {
    item.examples <- validate_item_examples(item.examples, model)
  }

  # Validate target.N
  target.N <- validate_target_N(target.N, labels, items.only)

  # Validate API keys and model
  api_keys <- validate_api_keys(openai.API, groq.API, model)
  openai.API <- api_keys$openai.API
  groq.API <- api_keys$groq.API

  # Validate temperature and top.p
  params <- validate_temperature_top.p(temperature, top.p)
  temperature <- params$temperature
  top.p <- params$top.p

  # Validate that the correct combination of inputs has been supplied
  custom <- validate_custom(custom, item.attributes, user.prompts, cleaning.fun)

  # Return everything
  return(list(
    item.attributes = item.attributes, openai.API = openai.API, groq.API = groq.API, custom = custom,
    user.prompts = user.prompts, item.type.definitions=item.type.definitions, cleaning.fun=cleaning.fun, system.role=system.role,
    scale.title=scale.title, sub.domain=sub.domain, model=model, item.examples=item.examples,
    target.N=target.N, temperature=temperature, top.p=top.p, items.only=items.only, adaptive=adaptive, EGA.model=EGA.model,EGA.algorithm=EGA.algorithm, embedding.model=embedding.model,
    keep.org=keep.org, plot=plot, plot.stability=plot.stability, calc.final.stability=calc.final.stability, silently=silently

    ))
}








# GENIE checks ----
#' GENIE Input Validation
#'
#' Validates and processes the input parameters for the \code{GENIE} function. This function ensures that the provided item data is correctly formatted, contains no missing or duplicate values, and meets the requirements for a meaningful network analysis.
#'
#' @param item.data A required data frame containing your item statements and item type labels.
#' @param openai.API A required character string of your OpenAI API key.
#' @param EGA.model An optional character string specifying the EGA model to use (\code{"tmfg"} or \code{"glasso"}).
#' @param EGA.algorithm A character string specifying the clustering algorithm for EGA (default: \code{"walktrap"}).
#' @param embedding.model A string containing the embedding model alias to be used
#' @param plot Logical; if \code{TRUE}, displays the network plots.
#' @param silently Logical; if \code{TRUE}, suppresses console output.
#' @return A list containing:
#' \describe{
#'   \item{\code{items}}{A cleaned and validated data frame of your item data.}
#'   \item{\code{openai.API}}{Your validated OpenAI API key.}
#' }
GENIE_checks <- function(item.data, openai.API, EGA.model, EGA.algorithm,
                         embedding.model, plot, plot.stability, calc.final.stability, silently) {

  # Ensure there is no missingness
  check_no_na(item.data, openai.API, EGA.model, EGA.algorithm, embedding.model,
              plot, plot.stability, calc.final.stability, silently)

  # Quickly validate booleans
  if(!(plot %in% c(TRUE, FALSE))) stop("'plot' must be a boolean.")
  if(!(silently %in% c(TRUE, FALSE))) stop("'silently' must be a boolean.")
  if(!(plot.stability %in% c(TRUE, FALSE))) stop("'plot.stability' must be a boolean.")
  if(!(calc.final.stability %in% c(TRUE, FALSE))) stop("'calc.final.stability' must be a boolean.")

  # Validate the EGA model and algorithm
  EGA.model <- validate_EGA_model(EGA.model)
  EGA.algorithm <- validate_EGA_algorithm(EGA.algorithm)

  # Validate the embedding model
  embedding.model <- validate_embedding(embedding.model)

  # Validate the API key
  openai.API <- validate_openai(openai.API)

  # Ensure item.data is a data.frame or matrix
  item.data <- validate_item_data_type(item.data, "your item data")

  # Check for missing data
  validate_no_missing_data(item.data)

  # Structural column check (names + count)
  validate_columns(item.data, "your item data")

  # Ensure `statement`, `type`, `attribute` are flat character vectors
  validate_flat_character_columns(item.data, "your item data")

  # Extract and validate attributes
  item.attributes <- validate_and_extract_attributes(item.data)

  string <- "your provided data"
  item.labels <- item.data[["type"]]
  item.attribute.labels <- item.data[["attribute"]]
  items <- item.data[["statement"]]

  # Content-level checks
  validate_non_empty_items_labels(items, item.labels, item.attribute.labels, string)
  item.data <- deduplicate_item_data(item.data, string)
  validate_no_duplicate_items(item.data, string)
  validate_items_per_type(item.data)
  validate_total_items(item.data)

  # Attribute alignment check
  item.attributes <- validate_item_attributes(item.attributes, FALSE)
  test <- unlist(item.attributes)
  names(item.attributes) <- NULL
  validate_item_attribute_labels <- sapply(test, function(x){x %in% item.attribute.labels})
  validate_item_type_labels <- sapply(names(item.attributes), function(x){x %in% item.labels})

  if (!all(validate_item_attribute_labels)) {
    stop("Ensure the labels in your `item.attributes` object align with the labels in your `attribute` column of the provided data frame.")
  }

  if (!all(validate_item_type_labels)) {
    stop("Ensure the names of the item types in your `item.attributes` object align with the labels in your `type` column of the provided data frame.")
  }

  return(list(
    items = item.data,
    openai.API = openai.API,
    item.attributes = item.attributes,
    embedding.model = embedding.model,
    EGA.model = EGA.model,
    EGA.algorithm = EGA.algorithm
  ))
}



#AI GENIE Custom----
#' Validate Return Object from Cleaning Function (List Version)
#'
#' Validates the output of the user-provided text cleaning function (\code{cleaning.fun}) to ensure it returns a list of cleaned items. This function checks that the output is a list of character strings, contains no missing or empty values, and correctly formats the item data by associating each item with its corresponding type.
#'
#' @param output The output object returned by the user-provided \code{cleaning.fun} function.
#' @param n_empty The number of times the cleaning function failed to return any valid items
#' @param item_attributes A named list containing item type labels and their corresponding attributes.
#' @param performance A flag denoting whether we are in performance mode or not
#' @return A list containing the character vector of cleaned item statements, the character vector containing the cleaned item attributes, and the number of items the cleaning function failed to return viable output consecutively.
#AI GENIE Custom----
#' Validate Return Object from Cleaning Function (List Version)
#'
#' Validates the output of the user-provided text cleaning function (\code{cleaning.fun}) to ensure it returns a list of cleaned items. This function checks that the output is a list of character strings, contains no missing or empty values, and correctly formats the item data by associating each item with its corresponding type.
#'
#' @param output The output object returned by the user-provided \code{cleaning.fun} function.
#' @param n_empty The number of times the cleaning function failed to return any valid items
#' @param item_attributes A named list containing item type labels and their corresponding attributes.
#' @param performance a logical flag indicating whether we are in performance mode
#' @return A list containing the character vector of cleaned item statements, the character vector containing the cleaned item attributes (and the item answers in performance mode), and the number of items the cleaning function failed to return viable output consecutively.
validate_return_object <- function(output, n_empty, item_attributes, performance) {
  string <- "output of the provided text cleaning function"

  # Check if output is a data frame
  if (!is.data.frame(output)) {
    stop(paste("The", string, "must be a data frame of cleaned items."))
  }

  if (performance) {
    # In performance mode, expect three columns: item, difficulty, and answer.
    if (ncol(output) != 3 ||
        !("item" %in% colnames(output)) ||
        !("difficulty" %in% colnames(output)) ||
        !("answer" %in% colnames(output))) {
      stop(paste("The", string, "must be a data frame with three columns: one named `item`, one named `difficulty`, and one named `answer`."))
    }

    names(output)[names(output) == "difficulty"] <- "attribute"

  } else {
    # Regular mode: expect two columns: item and attribute.
    if (ncol(output) != 2 ||
        !("item" %in% colnames(output)) ||
        !("attribute" %in% colnames(output))) {
      stop(paste("The", string, "must be a data frame with two columns: one named `item` and one named `attribute`."))
    }
  }

  # Convert numeric values to strings in performance mode for both item and answer columns
  if (performance) {
    if (!all(sapply(output$item, is.character))) {
      output$item <- as.character(output$item)
    }
    if (!all(sapply(output$answer, is.character))) {
      output$answer <- as.character(output$answer)
    }
  }

  # Ensure all elements in the relevant columns are character strings
  if (!all(sapply(output$item, is.character)) || !all(sapply(output$attribute, is.character))) {
    stop(paste("All cells in the", string, "must be character strings."))
  }

  # For performance mode, also ensure all cells in the answer column are character strings
  if (performance && !all(sapply(output$answer, is.character))) {
    stop(paste("All cells in the answer column of the", string, "must be character strings."))
  }

  # Identify and clean the item attributes
  found_item_attributes <- output$attribute
  stemmed_found_attributes <- tm::stemDocument(trimws(tolower(gsub("[[:punct:]]", "", found_item_attributes))))

  names(item_attributes) <- NULL
  item_attributes <- unlist(item_attributes)
  item_attributes <- trimws(tolower(gsub("[[:punct:]]", "", item_attributes)))
  stemmed_attributes <- tm::stemDocument(item_attributes)

  valid_indices <- c()
  for(i in seq_len(nrow(output))){
    item <- output$item[[i]]
    attribute <- stemmed_found_attributes[[i]]

    if(attribute %in% stemmed_attributes && !is.null(item) && !is.na(item) && item != ""){
      valid_indices <- c(valid_indices, i)
    }
  }

  # Flatten the list into a character vector
  items <- unlist(output$item[valid_indices])

  if (length(items) == 0 && n_empty < 50) {
    n_empty <- n_empty + 1
  } else if (length(items) > 0){
    n_empty <- 0
  }

  if (n_empty >= 50){
    stop("No valid items were returned by the cleaning function after 50 consecutive attempts. Check function logic.")
  }

  items <- output$item[valid_indices]
  item_attributes <- stemmed_found_attributes[valid_indices]

  if(performance){
  names(output)[names(output) == "attribute"] <- "difficulty"
  }

  return(list(items = items, item_attributes = item_attributes, n_empty = n_empty))
}


#' GENIE Input Validation
#'
#' Validates and processes the input parameters for the \code{custom_prompt} function. This function ensures that the provided item data is correctly formatted, contains no missing or duplicate values, and meets the requirements.
#'
#' @param openai.API Optional (Required if using a GPT model). A character string containing your OpenAI API key.
#' @param groq.API Optional (Required if using a Groq model). A character string containing your Groq API key.
#' @param user.prompts A named list of custom prompt strings for each item type.
#' @param N.runs An integer specifying the number of times to run the prompt for preview; defaults to \code{3}.
#' @param model A character string specifying the language model to use. Options include \code{"gpt3.5"}, \code{"gpt4o"},
#'              \code{"llama3"}, \code{"mixtral"}, \code{"deepseek"}, or \code{"gemma2"}. Defaults to \code{"gpt3.5"}.
#' @param top.p Numeric; defaults to \code{1}. Sets the top-p sampling parameter for the language model.
#' @param temperature Numeric; defaults to \code{1}. Controls the randomness of the model's output (valid range: 0–2).
#' @param system.role Optional. A character string defining the role of the language model (e.g., "an expert methodologist").
#' @param silently Logical; defaults to \code{FALSE}. If \code{TRUE}, suppresses console output.
#'
#' @return A named list containing all of the cleaned and validated user parameters
validate_promt_inputs <- function(openai.API, groq.API, user.prompts, N.runs, model,
                                  top.p, temperature, system.role, silently) {
  # check that there are no NAs
  check_no_na(openai.API, groq.API, user.prompts, N.runs, model,
              top.p, temperature, system.role, silently)

  # check that there is exactly ONE API
  if(is.null(groq.API) && is.null(openai.API)){
    stop("Please provide at least one API.")
  }

  # Validate the boolean
  if(!(silently == TRUE | silently == FALSE)){
    stop("Silently must either be set to TRUE or FALSE.")
  }

  # Validate the model
  model <- validate_model(model, silently)

  # Validate system.role
  system.role <- validate_system_role(system.role)

  # Validate the APIs
  apis <- validate_apis(openai.API, groq.API, model)
  groq.API <- apis$groq.API
  openai.API <- apis$openai.API

  # Validate user prompts
  user.prompts <- validate_user_prompts(user.prompts)

  # Validate numbers
  if(!is.numeric(N.runs)){
    stop("N.runs must be a number.")
  }
  if(!is.numeric(top.p)){
    stop("Top.p must be a number.")
  }
  if(!is.numeric(temperature)){
    stop("Temperature must be a number.")
  }

  return(list(openai.API=openai.API, groq.API=groq.API, user.prompts=user.prompts, model=model, system.role=system.role))
}


#Performance AI-GENIE ----

#' p_AIGENIE Input Validation (Internal)
#'
#' Validates and processes the input parameters for the p_AIGENIE function, which is designed for generating
#' performance or ability-based assessment items (e.g., math items). This function mirrors the behavior of AIGENIE_checks
#' but replaces item.attributes with item.difficulty and expects item.examples to be a data frame with columns:
#' type, difficulty, statement, and answer. Additionally, the new parameter audience (if provided) is validated as a non-empty string.
#'
#' The validations performed include:
#' - item.difficulty: Must be either a named list or an unnamed list. For an unnamed list, each element is assumed to be
#'   an item type label and the default difficulty vector (c("LOW", "MEDIUM", "HIGH")) is assigned. For a named list,
#'   each element must be a character vector of difficulty values (synonyms are allowed) which are mapped to their canonical
#'   all-caps forms. Each sublist must contain at least 2 unique values.
#' - item.examples: If provided, it must be a data frame with columns type, difficulty, statement, and answer. The type
#'   values must be among the valid item types (i.e., the names from item.difficulty), and the difficulty values are mapped
#'   to their canonical forms.
#' - audience: If provided, must be a non-empty string describing the intended audience of the scale.
#' - All other parameters (openai.API, groq.API, system.role, scale.title, sub.domain, model, etc.) are validated using
#'   the existing helper functions.
#'
#' @param item.difficulty A list specifying difficulty levels for each item type.
#' @param level.description A data frame containing the columns `type`, `difficulty`, and `description`.This data frame defines what make an easy item easy, and moderate item moderate, and a challenging item challenging.
#' @param openai.API A character string of your OpenAI API key.
#' @param groq.API A character string of your Groq API key.
#' @param custom A boolean indicating whether custom prompts and a cleaning function are used.
#' @param user.prompts A named list of custom prompt strings (required if custom is TRUE).
#' @param item.type.definitions An optional named list or data frame with definitions for each item type.
#' @param cleaning.fun A function to clean and parse the language model's output (required if custom is TRUE).
#' @param system.role A character string describing the role the language model should assume.
#' @param scale.title A character string specifying the scale's title.
#' @param audience (Optional) A non-empty string describing the intended audience.
#' @param sub.domain A character string specifying the sub-domain or specialty.
#' @param model A character string specifying the language model to use.
#' @param item.examples An optional data frame with columns type, difficulty, statement, and answer.
#' @param target.N An integer or vector of integers specifying the target number of items to generate.
#' @param temperature A numeric value controlling the randomness of the language model's output.
#' @param top.p A numeric value controlling the diversity of the language model's output.
#' @param items.only A boolean indicating whether only items are generated without further analysis.
#' @param adaptive A boolean indicating whether previously generated items are incorporated into subsequent prompts.
#' @param EGA.model An optional character string specifying the EGA model to use.
#' @param EGA.algorithm A character string specifying the clustering algorithm for EGA.
#' @param embedding.model A character string specifying the OpenAI embedding model.
#' @param keep.org A boolean indicating whether the original generated item pool and embeddings are retained.
#' @param plot A boolean indicating whether to generate network plots.
#' @param plot.stability A boolean indicating whether to generate additional stability plots.
#' @param calc.final.stability A boolean indicating whether to compute bootstrapped stability measures.
#' @param silently A boolean indicating whether to suppress console output.
#'
#' @return A list of validated parameters ready for use in p_AIGENIE.
p_AIGENIE_checks <- function(item.difficulty, level.description, openai.API, groq.API, custom, user.prompts,
                             item.type.definitions, cleaning.fun, system.role, scale.title, audience,
                             sub.domain, model, item.examples, target.N, temperature, top.p,
                             items.only, adaptive, EGA.model, EGA.algorithm, embedding.model,
                             keep.org, plot, plot.stability, calc.final.stability, silently) {

  # Check that none of the essential parameters are NA
  check_no_na(item.difficulty, level.description, openai.API, groq.API, custom, user.prompts,
              item.type.definitions, cleaning.fun, system.role, scale.title, audience, sub.domain, model,
              item.examples, target.N, temperature, top.p, items.only, adaptive, EGA.model, EGA.algorithm,
              embedding.model, keep.org, plot, plot.stability, calc.final.stability, silently)

  # Validate boolean parameters
  bool_params <- list(custom = custom, items.only = items.only, adaptive = adaptive,
                      keep.org = keep.org, plot = plot, plot.stability = plot.stability,
                      calc.final.stability = calc.final.stability, silently = silently)
  for (param in names(bool_params)) {
    if (!is.logical(bool_params[[param]]) || length(bool_params[[param]]) != 1) {
      stop(paste0("'", param, "' must be a single boolean value."))
    }
  }

  # Validate audience if provided: it must be a non-empty string
  if (!is.null(audience)) {
    if (!is.character(audience) || length(audience) != 1 || trimws(audience) == "") {
      stop("If provided, 'audience' must be a non-empty string describing the intended audience of the scale.")
    }
    audience <- trimws(audience)
  }

  # Validate system.role, scale.title, and sub.domain
  system.role <- validate_system_role(system.role)
  scale.title <- validate_title_or_domain(scale.title, "scale title")
  sub.domain <- validate_title_or_domain(sub.domain, "sub domain")

  # Validate item.difficulty using our custom helper
  validated_difficulty <- validate_item_difficulty(item.difficulty)

  # Determine valid item types from item.difficulty (the names)
  labels <- names(validated_difficulty)

  # Custom mode validations
  if (custom) {
    user.prompts <- validate_user_prompts(user.prompts)
    if (!is.function(cleaning.fun)) {
      stop("When 'custom' is TRUE, 'cleaning.fun' must be a function.")
    }
    labels <- names(user.prompts)
  }

  # Validate item.type.definitions (if provided)
  item.type.definitions <- validate_item_type_definitions(item.type.definitions, labels)

  # Validate model
  model <- validate_model(model, silently)

  # Validate item.examples if provided (for performance items, it must be a data frame)
  if (!is.null(item.examples)) {
    item.examples <- validate_item_examples_p(item.examples, valid.types = labels)
    names(item.examples)[names(item.examples) == "difficulty"] <- "attribute"
    item.examples <- validate_item_examples_df(item.examples, validated_difficulty)
    names(item.examples)[names(item.examples) == "attribute"] <- "difficulty"
  }


  # Validate target.N using the valid labels
  target.N <- validate_target_N(target.N, labels, items.only)

  # Validate level.description
  level.description <- validate_level_description(level.description, item.examples, item.difficulty, silently)

  # Validate API keys
  api_keys <- validate_api_keys(openai.API, groq.API, model)
  openai.API <- api_keys$openai.API
  groq.API <- api_keys$groq.API

  # Validate temperature and top.p
  params <- validate_temperature_top.p(temperature, top.p)
  temperature <- params$temperature
  top.p <- params$top.p

  # Validate custom flag further
  custom <- validate_custom(custom, item.difficulty, user.prompts, cleaning.fun)

  # Validate embedding.model
  embedding.model <- validate_embedding(embedding.model)

  # Return a list of validated parameters
  return(list(
    item.difficulty = validated_difficulty,
    level.description = level.description,
    openai.API = openai.API,
    groq.API = groq.API,
    custom = custom,
    user.prompts = user.prompts,
    item.type.definitions = item.type.definitions,
    cleaning.fun = cleaning.fun,
    system.role = system.role,
    scale.title = scale.title,
    audience = audience,
    sub.domain = sub.domain,
    model = model,
    item.examples = item.examples,
    target.N = target.N,
    temperature = temperature,
    top.p = top.p,
    items.only = items.only,
    adaptive = adaptive,
    EGA.model = EGA.model,
    EGA.algorithm = EGA.algorithm,
    embedding.model = embedding.model,
    keep.org = keep.org,
    plot = plot,
    plot.stability = plot.stability,
    calc.final.stability = calc.final.stability,
    silently = silently
  ))
}
