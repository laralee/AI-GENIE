

#' AIGENIE Input Validation
#'
#' Validates and processes the input parameters for the \code{AIGENIE} function. This function ensures that all required parameters are provided, correctly formatted, and meet the necessary conditions for the AI-GENIE pipeline to run successfully. It handles both default and custom modes.
#'
#' @param item.attributes (Required when \code{custom = FALSE}) A named list or data frame containing item type labels and their corresponding attributes.
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
#' @param keep.org Logical; if \code{TRUE}, includes the original items in the returned results.
#' @param plot Logical; if \code{TRUE}, displays the network plots.
#' @param plot.stability Logical; Specifies whether to display the secondary network stability plots.
#' @param silently Logical; if \code{TRUE}, suppresses console output.
#' @param ... Additional arguments (currently not used).
#' @return A list of validated and processed parameters ready for use in the \code{AIGENIE} function.
AIGENIE_checks <- function(item.attributes, openai.API, groq.API, custom,
                           user.prompts, item.type.definitions, cleaning.fun, system.role,
                           scale.title, sub.domain, model, item.examples,
                           target.N, temperature, top.p, items.only, adaptive, EGA.model,
                           keep.org, plot, plot.stability, silently, ...) {

  # Check for missing arguments (no NA values)
  check_no_na(item.attributes, openai.API, groq.API, custom,
              user.prompts, item.type.definitions, cleaning.fun, system.role,
              scale.title, sub.domain, model, item.examples,
              target.N, temperature, top.p, items.only, adaptive, EGA.model, keep.org,
              plot, plot.stability, silently)

  # Validate that 'silently' is a boolean
  if (!is.logical(silently) || length(silently) != 1) {
    stop("'silently' must be a boolean.")
  }

  # Validate that the correct combination of inputs has been supplied
  custom <- validate_custom(custom, item.attributes, user.prompts, cleaning.fun)

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

  # Validate EGA.model
  validate_EGA_model(EGA.model)

  # Validate scale.title and sub.domain
  scale.title <- validate_title_or_domain(scale.title, "scale title")
  sub.domain <- validate_title_or_domain(sub.domain, "sub domain")

  # Validation when custom = FALSE
  if (!custom) {
    attributes_result <- validate_item_attributes(item.attributes, items.only)
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

  # Validate item.examples
  item.examples <- validate_item_examples(item.examples, model)

  # Validate target.N
  target.N <- validate_target_N(target.N, labels, items.only)

  # Validate API keys and model
  api_keys <- validate_api_keys(openai.API, groq.API, model)
  openai.API <- api_keys$openai.API
  groq.API <- api_keys$groq.API

  # Validate model
  model <- validate_model(model)

  # Validate temperature and top.p
  params <- validate_temperature_top.p(temperature, top.p)
  temperature <- params$temperature
  top.p <- params$top.p

  # Return everything
  return(list(
    item.attributes=item.attributes, openai.API=openai.API, groq.API=groq.API, custom=custom,
    user.prompts=user.prompts, item.type.definitions=item.type.definitions, cleaning.fun=cleaning.fun,
    system.role=system.role, scale.title=scale.title, sub.domain=sub.domain, model=model, item.examples=item.examples,
    target.N=target.N, temperature=temperature, top.p=top.p, items.only=items.only, adaptive=adaptive, EGA.model=EGA.model,
    keep.org=keep.org, plot=plot, silently=silently
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
GENIE_checks <- function(item.data, openai.API, EGA.model, plot, silently) {

  check_no_na(item.data, openai.API, plot, silently, EGA.model)

  # quickly validate booleans
  if(!(plot==FALSE || plot==TRUE)){stop("'plot' must be a boolean.")}
  if(!(silently==FALSE || silently==TRUE)){stop("'silently' must be a boolean.")}
  validate_EGA_model(EGA.model)

  # validate the API
  openai.API <- validate_openai(openai.API)

  string <- "your provided data"
  # Validate the different aspects of item.data
  item.data <- validate_item_data_type(item.data, string)
  validate_no_missing_data(item.data)
  validate_columns(item.data, string)
  item.data <- clean_item_data(item.data, string)
  item.data <- find_cols(item.data, string)
  item.labels <- item.data[["type"]]
  items <- item.data[["statement"]]
  validate_non_empty_items_labels(items, item.labels, string)
  item.data <- deduplicate_item_data(item.data, string)
  validate_no_duplicate_items(item.data, string)
  validate_items_per_type(item.data)
  validate_total_items(item.data)

  min_item_types <- 2
  if(length(item.labels) < min_item_types){
    stop("For a meaningful network analysis, include at least two distinct item types.")
  }

  # Return the cleaned item data object
  return(list(items = item.data, openai.API=openai.API))
}



#AI GENIE Custom----
#' Validate Return Object from Cleaning Function (List Version)
#'
#' Validates the output of the user-provided text cleaning function (\code{cleaning.fun}) to ensure it returns a list of cleaned items. This function checks that the output is a list of character strings, contains no missing or empty values, and correctly formats the item data by associating each item with its corresponding type.
#'
#' @param output The output object returned by the user-provided \code{cleaning.fun} function.
#' @param current_label Character; the current item type label being processed.
#' @return A character vector of cleaned item statements.
validate_return_object <- function(output, current_label) {
  string <- "the output of the provided text cleaning function"

  # Check if output is a list
  if (!is.list(output)) {
    stop(paste("The", string, "must be a list of cleaned items."))
  }

  # Ensure all elements in the list are character strings
  if (!all(sapply(output, is.character))) {
    stop(paste("All elements in", string, "must be character strings."))
  }

  # Flatten the list into a character vector
  items <- unlist(output)

  # Remove any NA or empty strings
  items <- items[!is.na(items) & items != ""]

  if (length(items) == 0) {
    stop(paste("No valid items were returned by the cleaning function for item type:", current_label))
  }

  return(items)
}


