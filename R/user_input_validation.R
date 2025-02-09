

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
  model <- validate_model(model)

  # Validate item.examples
  item.examples <- validate_item_examples(item.examples, model)

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
#' @param plot Logical; if \code{TRUE}, displays the network plots.
#' @param silently Logical; if \code{TRUE}, suppresses console output.
#' @return A list containing:
#' \describe{
#'   \item{\code{items}}{A cleaned and validated data frame of your item data.}
#'   \item{\code{openai.API}}{Your validated OpenAI API key.}
#' }
GENIE_checks <- function(item.data, openai.API, EGA.model,EGA.algorithm, embedding.model,
                         plot, plot.stability, calc.final.stability, silently) {

  # Ensure there is no missingness
  check_no_na(item.data, openai.API, EGA.model,EGA.algorithm, embedding.model,
              plot, plot.stability, calc.final.stability, silently)

  # quickly validate booleans
  if(!(plot==FALSE || plot==TRUE)){stop("'plot' must be a boolean.")}
  if(!(silently==FALSE || silently==TRUE)){stop("'silently' must be a boolean.")}
  if(!(plot.stability==FALSE || plot.stability==TRUE)){stop("'plot.stability' must be a boolean.")}
  if(!(calc.final.stability==FALSE || calc.final.stability==TRUE)){stop("'calc.final.stability' must be a boolean.")}

  # Validate the EGA model
  EGA.model <- validate_EGA_model(EGA.model)

  # Validate the EGA algorithm
  EGA.algorithm <- validate_EGA_algorithm(EGA.algorithm)

  # Validate the embedding model
  embedding.model <- validate_embedding(embedding.model)

  # validate the API
  openai.API <- validate_openai(openai.API)

  item.attributes <- validate_and_extract_attributes(item.data)

  string <- "your provided data"
  # Validate the different aspects of item.data
  item.labels <- item.data[["type"]]
  item.attribute.labels <- item.data[["attribute"]]
  items <- item.data[["statement"]]
  validate_non_empty_items_labels(items, item.labels, item.attribute.labels, string)
  item.data <- deduplicate_item_data(item.data, string)
  validate_no_duplicate_items(item.data, string)
  validate_items_per_type(item.data)
  validate_total_items(item.data)

  # Validate item.attributes
  item.attributes <- validate_item_attributes(item.attributes, FALSE)

  test <- item.attributes
  names(test) <- NULL
  test <- unlist(test)
  validate_item_attribute_labels <- sapply(test, function(x){x %in% item.attribute.labels})
  validate_item_type_labels <- sapply(names(item.attributes), function(x){x %in% item.labels})

  if(!all(validate_item_attribute_labels)){
    stop("Ensure the labels in your `item.attributes` object align with the labels in your `attribute` column of the provided data frame.")
  }

  if(!all(validate_item_type_labels)){
    stop("Ensure the names of the item types in your `item.attributes` object align with the labels in your `type` column of the provided data frame.")
  }

  # Return the cleaned item data object
  return(list(items = item.data, openai.API=openai.API, item.attributes = item.attributes,
              embedding.model=embedding.model, EGA.model=EGA.model, EGA.algorithm = EGA.algorithm))
}



#AI GENIE Custom----
#' Validate Return Object from Cleaning Function (List Version)
#'
#' Validates the output of the user-provided text cleaning function (\code{cleaning.fun}) to ensure it returns a list of cleaned items. This function checks that the output is a list of character strings, contains no missing or empty values, and correctly formats the item data by associating each item with its corresponding type.
#'
#' @param output The output object returned by the user-provided \code{cleaning.fun} function.
#' @param n_empty The number of times the cleaning function failed to return any valid items
#' @param item_attributes A named list containing item type labels and their corresponding attributes.
#' @return A list containing the character vector of cleaned item statements, the character vector containing the cleaned item attributes, and the number of items the cleaning function failed to return viable output consecutively.
validate_return_object <- function(output, n_empty, item_attributes) {
  string <- "output of the provided text cleaning function"

  # Check if output is a data frame
  if (!is.data.frame(output)) {
    stop(paste("The", string, "must be a data frame of cleaned items."))
  }

  if(ncol(output) != 2 || !("item" %in% colnames(output)) || !("attribute" %in% colnames(output))) {
    stop(paste("The", string, "must be a data frame with two columns: one named `item` and one named `attribute`."))
  }


  # Ensure all elements in the data frame are character strings
  if (!all(sapply(output$item, is.character)) || !all(sapply(output$attribute, is.character))) {
    stop(paste("All cells in the", string, "must be character strings."))
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

  return(list(items=items, item_attributes=item_attributes, n_empty=n_empty))
}



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
  if(!is.logical(silently)){
    stop("Silently must either be set to TRUE or FALSE.")
  }

  # Validate the model
  model <- validate_model(model)

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
