

#### USER FACING FUNCTIONS ####

#' AI-GENIE: Automatic Item Generation, Validation, and Reduction
#'
#' This function orchestrates the full AI-GENIE pipeline for developing item pools for psychological inventories.
#' It operates in two modes:
#' \itemize{
#'   \item In the default (non-custom) mode, prompts are automatically constructed from a provided
#'         named list of item attributes (and, optionally, item type definitions and example items).
#'         The language model is then used to generate candidate items.
#'   \item In custom mode (\code{custom = TRUE}), the user provides their own prompts (via \code{user.prompts})
#'         and a custom cleaning function (via \code{cleaning.fun}) to parse the language model's output.
#' }
#' After item generation, if \code{items.only = FALSE}, the function passes the generated items to a
#' reduction pipeline that:
#' \itemize{
#'   \item Embeds the items using the specified embedding model.
#'   \item Performs redundancy removal via Unique Variable Analysis (UVA).
#'   \item Conducts Exploratory Graph Analysis (EGA) and bootstrapped EGA to refine the item pool.
#'   \item Selects the optimal EGA model based on Normalized Mutual Information (NMI).
#' }
#'
#' @param item.attributes A required named list in which each element is a character vector of attributes for an item type.
#'                        The names of the list elements serve as the item type labels.
#' @param openai.API A required character string containing your OpenAI API key.
#' @param groq.API An optional character string for your Groq API key (required for non-GPT models such as "llama3", "mixtral", or "gemma2").
#' @param custom Logical; defaults to \code{FALSE}. If \code{TRUE}, the function uses custom prompts (provided via \code{user.prompts})
#'               and a custom cleaning function (provided via \code{cleaning.fun}).
#' @param user.prompts (Required when \code{custom = TRUE}) A named list of custom prompt strings, one for each item type.
#' @param item.type.definitions An optional named list or data frame providing brief definitions (up to 250 characters)
#'                              for each item type. These definitions are prepended to the generated prompts.
#' @param cleaning.fun (Required when \code{custom = TRUE}) A function to clean and parse the language model's raw output.
#'                     It must accept a single argument (the raw output text) and return a data frame with two columns:
#'                     \code{item} (the item statement) and \code{attribute} (the characteristic targeted).
#' @param system.role An optional character string describing the role the language model should assume
#'                    (e.g., "an expert psychometrician and test developer"). If \code{NULL}, a default is generated.
#' @param scale.title An optional character string specifying the title or name of your inventory.
#' @param sub.domain An optional character string specifying the inventory's sub-domain or specialty.
#' @param model A character string specifying the language model to use. Options include \code{"gpt3.5"}, \code{"gpt4o"},
#'              \code{"llama3"}, \code{"mixtral"}, \code{"deepseek"}, or \code{"gemma2"}.
#'              The parameter also accepts an API alias directly, if a specific model version is desired. Defaults to \code{"gpt3.5"}.
#' @param item.examples An optional character vector of high-quality example item statements.
#' @param target.N An integer or vector of integers specifying the target number of items to generate.
#'                 If a single number is provided, it is approximately divided among the item types;
#'                 if a vector, each element corresponds to the target for the respective item type.
#' @param temperature Numeric; defaults to \code{1}. Controls the randomness of the language model's output (range 0–2).
#' @param top.p Numeric; defaults to \code{1}. Sets the top-p sampling parameter for the language model (range 0–1).
#' @param items.only Logical; defaults to \code{FALSE}. If \code{TRUE}, the function stops after generating items
#'                  and returns a data frame of items without performing further reduction or analysis.
#' @param adaptive Logical; defaults to \code{TRUE}. If \code{TRUE}, previously generated items are incorporated
#'                 into subsequent prompts to reduce redundancy.
#' @param EGA.model An optional character string specifying the Exploratory Graph Analysis model to use
#'                  (e.g., \code{"tmfg"} or \code{"glasso"}). If \code{NULL}, both are evaluated and the one with
#'                  the highest NMI is selected.
#' @param EGA.algorithm A character string specifying the clustering algorithm for EGA (default: \code{"walktrap"}).
#' @param embedding.model A character string specifying the OpenAI embedding model to use
#'                        (e.g., \code{"text-embedding-3-small"}). Defaults to \code{"text-embedding-3-small"}.
#' @param keep.org Logical; defaults to \code{FALSE}. If \code{TRUE}, the original generated item pool and/or embeddings
#'                 are retained in the output.
#' @param plot Logical; defaults to \code{TRUE}. If \code{TRUE}, the function generates network plots comparing
#'             the pre- and post-reduction item pools.
#' @param plot.stability Logical; defaults to \code{FALSE}. If \code{TRUE}, additional network stability plots are produced.
#' @param calc.final.stability Logical; defaults to \code{FALSE}. If \code{TRUE}, the function computes bootstrapped stability
#'                             measures before and after reduction (which may increase computation time).
#' @param silently Logical; defaults to \code{FALSE}. If \code{TRUE}, console output is suppressed.
#' @param ... Additional arguments passed to underlying functions in the pipeline.
#'
#' @return If \code{items.only = TRUE}, returns a data frame of generated items (with columns such as \code{statement} and \code{attribute}).
#'         Otherwise, returns a list with two elements:
#'         \describe{
#'           \item{\code{overall_sample}}{
#'             A list containing the overall sample-level analysis results:
#'             \itemize{
#'               \item \code{main_result}: A data frame of the refined item pool after reduction, including columns such as \code{ID}, \code{type}, \code{statement}, and \code{EGA_communities}.
#'               \item \code{final_ega_obj}: The final EGA object after reduction.
#'               \item \code{final_bootega_obj}: The final bootstrapped EGA (bootEGA) object after reduction (if stability analysis was performed).
#'               \item \code{initial_ega_obj}: The initial EGA object computed on the full generated pool.
#'               \item \code{initial_bootega_obj}: The initial bootEGA object computed on the redundancy-reduced items.
#'               \item \code{selected_model}: The EGA model used throughout the pipeline (either as specified or selected based on NMI).
#'               \item \code{nmi}: The final Normalized Mutual Information (NMI) value after reduction.
#'               \item \code{start_nmi}: The NMI value computed on the original generated item pool.
#'               \item \code{start_N}: The number of items in the initial generated pool.
#'               \item \code{final_N}: The number of items in the final refined pool.
#'               \item \code{network_plot}: A network plot object comparing the pre- and post-reduction item networks.
#'               \item \code{stability_plot}: A stability plot object (if \code{calc.final.stability = TRUE}).
#'               \item \code{embeddings}: A list of embeddings used in the analysis. This list includes:
#'                 \itemize{
#'                   \item \code{full}: The full embeddings matrix for the reduced items.
#'                   \item \code{sparse}: A sparsified version of the embeddings.
#'                   \item \code{embed_type_used}: A string indicating whether "full" or "sparse" embeddings were ultimately used.
#'                 }
#'               \item Optionally, if \code{keep.org = TRUE}, additional elements are included:
#'                 \itemize{
#'                   \item \code{original_sample_items}: The original generated item pool as a data frame.
#'                   \item \code{original_sample_full}: The full embeddings matrix for the original items.
#'                   \item \code{original_sample_sparse}: A sparsified version of the original embeddings.
#'                 }
#'             }
#'           }
#'           \item{\code{item_type_level}}{
#'             A named list containing analysis results for each individual item type.
#'             Each element is a list with the corresponding output (similar in structure to the overall sample output)
#'             for that item type.
#'           }
#'         }
#' @export
#' @examples
#' #' \dontrun{
#'
#' ########################################################
#' #### Example 1: Using AI-GENIE with Default Prompts ####
#' ########################################################
#'
#' # Add an OpenAI API key
#' key <- "INSERT YOUR KEY HERE"
#'
#' # Item type definitions
#' trait.definitions <- list(
#'   neuroticism = "Neuroticism is a personality trait that describes one's tendency to experience negative emotions like anxiety, depression, irritability, anger, and self-consciousness.",
#'   openness = "Openness is a personality trait that describes how open-minded, creative, and imaginative a person is.",
#'   extraversion = "Extraversion is a personality trait that describes people who are more focused on the external world than their internal experience."
#' )
#'
#' # Item attributes
#' aspects.of.personality.traits <- list(
#'   neuroticism = c("anxious", "depressed", "insecure", "emotional"),
#'   openness = c("creative", "perceptual", "curious", "philosophical"),
#'   extraversion = c("friendly", "positive", "assertive", "energetic")
#' )
#'
#' # Example items - these should be plain, unformatted, high-quality examples.
#' # items related to neuroticism
#' personality.items <- c(
#'   "I am someone who would panic should I ever misplace my belongings.",
#'   "I am someone who has a low tolerance for others' incompetence.",
#'
#'   # items related to openness
#'   "I am someone who loves to explore new cultures when I travel.",
#'   "I am someone who enjoys engaging in philosophical debates.",
#'
#'   # items related to extraversion
#'   "I am someone who is almost always the life of the party.",
#'   "I am someone who is rarely intimidated when meeting new people."
#' )
#'
#' # Name the field or specialty
#' sub.domain <- "Personality Measurement"
#'
#' # Name the Inventory being created
#' scale.title <- "Three of 'Big Five:' A Streamlined Personality Inventory"
#'
#' # Run AI-GENIE to generate, validate, and redundancy-check an item pool for your new scale.
#' personality.inventory.results <- AIGENIE(
#'   item.attributes = aspects.of.personality.traits,
#'   openai.API = key,
#'   item.examples = personality.items,
#'   sub.domain = sub.domain,
#'   scale.title = scale.title
#' )
#'
#' # View the final item pool
#' View(personality.inventory.results$overall_result$main_result)
#'
#'
#' #######################################################
#' #### Example 2: Using AI-GENIE with Custom Prompts ####
#' #######################################################
#'
#'
#' # Define a custom system role
#' system.role <- "You are an expert methodologist who specializes in scale development for personality measurement. You are especially equipped to create novel personality items that mimic the style of popular 'Big Five' assessments."
#'
#' # Define custom prompts for each personality trait
#' custom.personality.prompts <- list(
#'
#'   # Prompt for generating neuroticism traits
#'   neuroticism = paste0(
#'     "Generate NINE unique, psychometrically robust single-statement items designed to assess ",
#'     "the Big Five personality trait neuroticism. Put EACH item on its own line. ",
#'     "Neuroticism has the following characteristics: anxious, depressed, insecure, and emotional. ",
#'     "Generate EXACTLY THREE items that target EACH characteristic. This is VERY important. ",
#'     "Format the items EXACTLY like so: \n<characteristic>||<item statement>\n",
#'     "The formatting is EXTREMELY important, so follow it EXACTLY. Be creative! Avoid repetition."
#'  ),
#'
#'   # Prompt for generating openness traits
#'   openness = paste0(
#'     "Generate TWELVE unique, psychometrically robust single-statement items designed to assess ",
#'     "the Big Five personality trait openness. Put EACH item on its own line. ",
#'     "Openness has the following characteristics: worldly, artistic, philosophical, and curious.",
#'     "Generate EXACTLY THREE items that target EACH characteristic. This is VERY important.",
#'     "Format the items EXACTLY like so: \n<characteristic>||<item statement>\n",
#'     "The formatting is EXTREMELY important, so follow it EXACTLY. Be creative! Avoid repetition."
#'   ),
#'
#'   # Prompt for generating extraversion traits
#'   extraversion = paste0(
#'   "Generate EIGHT unique, psychometrically robust single-statement items designed to assess ",
#'   "the Big Five personality trait extraversion. Put EACH item on its own line. ",
#'   "Extraversion has the following characteristics: friendly, positive, assertive, and energetic. ",
#'   "Generate EXACTLY FOUR items that target EACH characteristic. This is VERY important. ",
#'   "Format the items EXACTLY like so: \n<characteristic>||<item statement>\n",
#'   "The formatting is EXTREMELY important, so follow it EXACTLY. Be creative! Avoid repetition."
#'   )
#'
#' )
#'
#' # Define a custom cleaning function that returns a list of cleaned item statements
#' custom_cleaning <- function(content) {
#'
#'   # Split lines and remove empty ones
#'   items <- strsplit(content, "\n")[[1]]
#'   items <- trimws(items)
#'   items <- items[nzchar(items)]
#'   items <- gsub("\\*", "", items)
#'
#'   # Initialize data frame to store cleaned item statements and attributes
#'   item_texts <- c()
#'   item_attributes <- c()
#'
#'   # Iterate over items
#'   for (item in items) {
#'
#'     # Attempt to split by "||"
#'     split_item <- strsplit(item, "||", fixed = TRUE)[[1]]
#'     if (length(split_item) == 2) {
#'
#'       # Assume that the second element is the item statement
#'       item_text <- trimws(split_item[2])
#'
#'       # Assume that the first element is the item characteristic
#'      item_attribute <- trimws(split_item[1])
#'
#'       # Remove unwanted characters
#'       item_text <- gsub(">", "", item_text)
#'       item_text <- gsub("<", "", item_text)
#'       item_attribute <- gsub(">", "", item_attribute)
#'       item_attribute <- gsub("<", "", item_attribute)
#'
#'       # Append to list
#'       item_texts <- c(item_texts, item_text)
#'       item_attributes <- c(item_attribute, item_attributes)
#'
#'     }
#'   }
#'
#'   # Return a data frame of cleaned item statements
#'   return(data.frame("item"=item_texts, "attribute"=item_attributes))
#' }
#'
#' # Run AI-GENIE to generate, validate, and redundancy-check an item pool for your new scale.
#' personality.inventory.results.custom <- AIGENIE(
#'   item.attributes = aspects.of.personality.traits, # created in example 1
#'   user.prompts = custom.personality.prompts,
#'   openai.API = key, # created in example 1
#'   cleaning.fun = custom_cleaning,
#'   item.examples = personality.items, # created in example 1
#'   scale.title = scale.title, # created in example 1,
#'   custom = TRUE
#' )
#'
#' # View the final item pool
#' View(personality.inventory.results.custom$overall_sample$main_result)
#'
#' ################################################################
#' ###### Or, Run AIGENIE with an Open Source Model via Groq ######
#' ################################################################
#'
#' # Add your API Key from Groq
#' groq.key <- "INSERT YOUR GROQ API KEY"
#'
#' # Chose between 'Mixtral', 'Gemma 2', 'Llama 3', or 'DeepSeek'
#' open.source.model <- "Gemma 2"
#'
#' # Use AIGENIE with an open source model via Groq
#' personality.inventory.results.mixtral <- AIGENIE(
#'   item.attributes = aspects.of.personality.traits, # created in example 1
#'   openai.API = key, # Created in example 1
#'   item.examples = personality.items, # Created in example 1
#'   sub.domain = sub.domain, # Created in example 1
#'   scale.title = scale.title, # Created in example 1
#'   model = open.source.model, # Select a model available on Groq's API
#'   groq.API = groq.key
#' )
#'
#' # View the final item pool
#' View(personality.inventory.results.mixtral$overall_sample$main_result)
#'
#' }
#'
AIGENIE <- function(item.attributes, openai.API, groq.API = NULL, custom = FALSE,
                    user.prompts = NULL, item.type.definitions = NULL,
                    cleaning.fun = NULL, system.role = NULL,
                    scale.title = NULL, sub.domain = NULL, model = "gpt3.5", item.examples = NULL,
                    target.N = 100, temperature = 1, top.p = 1, items.only = FALSE, adaptive = TRUE,
                    EGA.model = NULL, EGA.algorithm="walktrap", embedding.model = "text-embedding-3-small", keep.org = FALSE,
                    plot = TRUE, plot.stability = FALSE, calc.final.stability = FALSE, silently = FALSE, ...) {

  # Perform input validation
  validated_params <- AIGENIE_checks(
    item.attributes = item.attributes,
    openai.API = openai.API,
    groq.API = groq.API,
    custom = custom,
    user.prompts = user.prompts,
    item.type.definitions = item.type.definitions,
    cleaning.fun = cleaning.fun,
    system.role = system.role,
    scale.title = scale.title,
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
    silently = silently,
    ...
  )

  # Reassign parameters
  item.attributes <- validated_params$item.attributes
  openai.API <- validated_params$openai.API
  groq.API <- validated_params$groq.API
  custom <- validated_params$custom
  user.prompts <- validated_params$user.prompts
  item.type.definitions <- validated_params$item.type.definitions
  cleaning.fun <- validated_params$cleaning.fun
  system.role <- validated_params$system.role
  scale.title <- validated_params$scale.title
  sub.domain <- validated_params$sub.domain
  model <- validated_params$model
  item.examples <- validated_params$item.examples
  target.N <- validated_params$target.N
  temperature <- validated_params$temperature
  top.p <- validated_params$top.p
  items.only <- validated_params$items.only
  adaptive <- validated_params$adaptive
  EGA.model <- validated_params$EGA.model
  EGA.algorithm <- validated_params$EGA.algorithm
  embedding.model <- validated_params$embedding.model
  keep.org <- validated_params$keep.org
  plot <- validated_params$plot
  plot.stability <- validated_params$plot.stability
  calc.final.stability <- validated_params$calc.final.stability
  silently <- validated_params$silently

    generated_items <- generate.items.internal(
      model = model,
      temperature = temperature,
      top.p = top.p,
      groq.API = groq.API,
      openai.API = openai.API,
      target.N = target.N,
      item.attributes = item.attributes,
      scale.title = scale.title,
      sub.domain = sub.domain,
      item.examples = item.examples,
      system.role = system.role,
      user.prompts = user.prompts,
      item.type.definitions = item.type.definitions,
      cleaner_fun = cleaning.fun,
      custom = custom,
      adaptive = adaptive,
      silently = silently,
      ...
    )

  if (items.only) {
    return(generated_items)
  }

  # Proceed with AI-GENIE reduction and analysis
  results <- run_pipeline(
    items = generated_items,
    openai.key = validated_params$openai.API,
    title = scale.title,
    EGA.model = EGA.model,
    EGA.algorithm = EGA.algorithm,
    embedding.model = embedding.model,
    keep.org = keep.org,
    plot = plot,
    plot.stability = plot.stability,
    calc.final.stability = calc.final.stability,
    silently = silently,
    ...
  )

  return(results)
}





#' GENIE: Item Validation and Reduction (Without Item Generation)
#'
#' This function processes an existing item pool to validate and refine item selection using AI-GENIE.
#' It applies network-based quality assessment, redundancy reduction, and stability evaluation.
#' Unlike \code{AIGENIE}, this function does not generate new items but instead focuses on optimizing
#' an existing dataset.
#'
#' The function performs the following steps:
#' \itemize{
#'   \item Embeds the items using the specified OpenAI embedding model.
#'   \item Runs the reduction pipeline to remove redundant items via Unique Variable Analysis (UVA) and validate items via bootstrapped Exploritory Graph Analysis (bootEGA) for further refinement.
#' }
#'
#' @param items A required data frame containing the item pool. This data frame must include at least three columns:
#'              \code{statement} (the item text), \code{type} (the item type label), and \code{attribute} (the associated attribute/characteristic).
#'              The pool should contain at least 50 unique items overall, with at least 15 items per distinct item type.
#' @param openai.API A required character string containing your OpenAI API key.
#' @param EGA.model An optional character string specifying the EGA model to use (e.g., \code{"tmfg"} or \code{"glasso"}). If set to \code{NULL},
#'                  both models are evaluated, and the one with the highest Normalized Mutual Information (NMI) is selected.
#' @param EGA.algorithm A character string specifying the clustering algorithm for EGA. Options include \code{"walktrap"}, \code{"louvain"}, or \code{"leiden"}. Defaults to \code{"walktrap"}.
#' @param embedding.model A character string specifying the OpenAI embedding model to use (e.g., \code{"text-embedding-3-small"}, \code{"text-embedding-3-large"}, or \code{"text-embedding-ada-002"}).
#'                        Defaults to \code{"text-embedding-3-small"}.
#' @param plot Logical; defaults to \code{TRUE}. If \code{TRUE}, network plots comparing the item pool before and after reduction are generated.
#' @param plot.stability Logical; defaults to \code{FALSE}. If \code{TRUE}, secondary network stability plots are generated.
#' @param calc.final.stability Logical; defaults to \code{FALSE}. If \code{TRUE}, bootstrapped stability analysis is performed (this may significantly increase computation time).
#' @param silently Logical; defaults to \code{FALSE}. If \code{TRUE}, console output is suppressed.
#' @param ... Additional arguments passed to underlying functions in the pipeline.
#'
#' @return Returns a list with two elements:
#'         \describe{
#'           \item{\code{overall_sample}}{
#'             A list containing the overall sample-level analysis results:
#'             \itemize{
#'               \item \code{main_result}: A data frame of the refined item pool after reduction, including columns such as \code{ID}, \code{type}, \code{statement}, and \code{EGA_communities}.
#'               \item \code{final_ega_obj}: The final EGA object after reduction.
#'               \item \code{final_bootega_obj}: The final bootstrapped EGA (bootEGA) object after reduction (if stability analysis was performed).
#'               \item \code{initial_ega_obj}: The initial EGA object computed on the full generated pool.
#'               \item \code{initial_bootega_obj}: The initial bootEGA object computed on the redundancy-reduced items.
#'               \item \code{selected_model}: The EGA model used throughout the pipeline (either as specified or selected based on NMI).
#'               \item \code{nmi}: The final Normalized Mutual Information (NMI) value after reduction.
#'               \item \code{start_nmi}: The NMI value computed on the original generated item pool.
#'               \item \code{start_N}: The number of items in the initial generated pool.
#'               \item \code{final_N}: The number of items in the final refined pool.
#'               \item \code{network_plot}: A network plot object comparing the pre- and post-reduction item networks.
#'               \item \code{stability_plot}: A stability plot object (if \code{calc.final.stability = TRUE}).
#'               \item \code{embeddings}: A list of embeddings used in the analysis. This list includes:
#'                 \itemize{
#'                   \item \code{full}: The full embeddings matrix for the reduced items.
#'                   \item \code{sparse}: A sparsified version of the embeddings.
#'                   \item \code{embed_type_used}: A string indicating whether "full" or "sparse" embeddings were ultimately used.
#'                 }
#'             }
#'           }
#'           \item{\code{item_type_level}}{
#'             A named list containing analysis results for each individual item type.
#'             Each element is a list with the corresponding output (with the same structure as the the overall sample output)
#'             for that item type.
#'           }
#'         }
#'
#' @examples
#' \dontrun{
#' ########################################################################
#' ##### Use AI-GENIE without AI - Validate Existing Items via GENIE ######
#' ########################################################################
#'
#' # Add an OpenAI API key
#' key <- "INSERT YOUR KEY HERE"
#'
#' ## Create a data frame of items to reduce - for this example, we will focus only on Neuroticism and Extraversion
#'
#' ## Neuroticism items (example for the 'anxious' attribute)
#' neuroticism_statements <- c("I often worry about things that may never happen.",
#'                             "The thought of uncertainty makes me feel uneasy.",
#'                             "I often feel on edge and apprehensive about the future.",
#'                             "I tend to overthink and dwell on negative possibilities.",
#'                             "I often find myself anticipating the worst possible outcome.",
#'                             "I frequently feel a sense of dread without any specific reason.",
#'                             "I often feel a sense of impending doom for no reason.",
#'                             "I frequently have irrational fears that something bad will happen.",
#'                             "I frequently worry about things that may never happen.",
#'                             "I often feel a tightness in my chest when faced with uncertainty.",
#'                             "I frequently worry about possible negative outcomes in everyday situations.",
#'                             "I frequently feel a sense of impending doom for no reason.",
#'                             "I frequently feel a knot in my stomach over minor uncertainties.",
#'                             "I am constantly on edge, waiting for something terrible to happen.",
#'                             "I constantly feel a sense of unease and worry, even in everyday situations.",
#'                             "I often feel panicky and overwhelmed by minor stressors.")
#'
#' type <- rep("neuroticism", 16)
#' attribute <- rep("anxious", 16)
#'
#' ## Neuroticism items (example for the 'emotional' attribute)
#' neuroticism_statements <- c(neuroticism_statements,
#'                             "I am easily overwhelmed by intense feelings of sadness.",
#'                             "I find myself getting emotional over small, everyday occurrences.",
#'                             "I am easily moved to tears by even small gestures of kindness.",
#'                             "I experience extreme highs and lows in my emotional state.",
#'                             "My emotions can sometimes feel overwhelming and difficult to control.",
#'                             "My feelings tend to be intense and overwhelming, often difficult to manage.",
#'                             "It is common for me to feel tense and easily startled.",
#'                             "I frequently find myself getting choked up over sentimental moments.",
#'                             "I am easily moved to tears by small acts of kindness.",
#'                             "My feelings are often intense and difficult to control.",
#'                             "I am easily moved to tears by movies or sad stories.",
#'                             "My feelings often overwhelm me, making it hard to cope at times.",
#'                             "My emotions tend to be intense and difficult to regulate.",
#'                             "I am easily moved to tears by touching moments in movies or books.",
#'                             "I find it challenging to control how I respond to my feelings.",
#'                             "It's difficult for me to recognize when I overreact.")
#'
#' type <- c(type, rep("neuroticism", 16))
#' attribute <- c(attribute, rep("emotional", 16))
#'
#' ## Extraversion items (example for the 'assertive' attribute)
#' extraversion_statements <- c("I am not afraid to speak my mind and assert my opinions in group discussions.",
#'                              "I have no problem taking the lead in group projects or activities.",
#'                              "I confidently express my thoughts and ideas in professional settings.",
#'                              "I am not hesitant to take charge and make decisions when needed.",
#'                              "I am not afraid to speak up and advocate for what I believe in.",
#'                              "Others see me as someone who confidently asserts my opinions.",
#'                              "I am not afraid to assert myself and speak up for what I believe in.",
#'                              "Others see me as someone who confidently takes charge in group settings.",
#'                              "I confidently voice my opinions and stand up for my beliefs.",
#'                              "I am not afraid to take charge and lead when necessary.",
#'                              "I confidently speak my mind and stand up for what I believe in.",
#'                              "Taking the lead in group settings is something I excel at without hesitation.",
#'                              "I confidently voice my opinions and assert myself in any situation.",
#'                              "Others perceive me as someone who confidently takes charge and leads.",
#'                              "I confidently express my opinions and assert myself when needed.",
#'                              "Taking the lead in discussions or projects is something I do without hesitation.")
#'
#' type <- c(type, rep("extraversion", 16))
#' attribute <- c(attribute, rep("assertive", 16))
#'
#'
#' ## Extraversion items (example for the 'energetic' attribute)
#' extraversion_statements <- c(extraversion_statements,
#'                              "Approaching each day with enthusiasm and vigor is my standard mode of operation.",
#'                              "My dynamic energy and lively nature bring excitement to every interaction.",
#'                              "Approaching each day with high levels of enthusiasm and energy is simply part of who I am.",
#'                              "My vibrant energy is infectious and lights up any room I enter.",
#'                              "I always am ready to dive into new adventures.",
#'                              "Approaching each day with exuberance is just who I am.",
#'                              "My energetic and lively spirit is infectious.",
#'                              "I have an infectious lively spirit that energizes those around me.",
#'                              "I best thrive in lively environments.",
#'                              "Approaching each day with exuberance is my norm.",
#'                              "I have an abundance of energy that fuels my actions throughout the day.",
#'                              "I enjoy conversations that are upbeat and spry.",
#'                              "I am usually the most energetic person in the room.",
#'                              "I absolutely love it when there is excitement in the air.",
#'                              "My energy is contagious and lights up the room.",
#'                              "My lively nature brings a dynamic energy to any situation.")
#'
#' type <- c(type, rep("extraversion", 16))
#' attribute <- c(attribute, rep("energetic", 16))
#'
#'
#' # Combine into a data frame
#' my.personality.items <- data.frame(statement = c(neuroticism_statements, extraversion_statements),
#'                                    type = type,
#'                                    attribute = attribute,
#'                                    stringsAsFactors = FALSE)
#'
#' # Run GENIE to validate and redundancy-check the item pool
#' my.personality.inventory.results <- GENIE(
#'   items = my.personality.items,
#'   openai.API = key
#' )
#'
#' # View the final refined item pool
#' View(my.personality.inventory.results$overall_sample$main_result)
#'
#' }
#' @export
GENIE <- function(items, openai.API, EGA.model=NULL, EGA.algorithm = "walktrap", embedding.model="text-embedding-3-small", plot=TRUE, plot.stability = FALSE, calc.final.stability=FALSE, silently=FALSE, ...) {

  # check the user-provided items
  checks <- GENIE_checks(item.data=items, openai.API=openai.API, EGA.model=EGA.model,EGA.algorithm = EGA.algorithm, embedding.model=embedding.model,
                         plot=plot, plot.stability= plot.stability, calc.final.stability= calc.final.stability, silently=silently)
  openai.API <- checks[["openai.API"]]
  items <- checks[["items"]]
  item.attributes <- checks[["item.attributes"]]
  EGA.model <- checks[["EGA.model"]]
  EGA.algorithm <- checks[["EGA.algorithm"]]
  embedding.model <- checks[["embedding.model"]]

  # run the pipeline
  run_pipeline <- run_pipeline(items = items, EGA.model = EGA.model, EGA.algorithm= EGA.algorithm,
                               openai.key=openai.API, embedding.model=embedding.model,
                               labels = items$type, keep.org=FALSE, plot = plot, plot.stability = plot.stability,
                               silently= silently, calc.final.stability = calc.final.stability)

  return(run_pipeline)

}


#' Validate and Preview Custom Prompt Output
#'
#' This function is designed to test and preview the output generated by your custom prompts before proceeding with full-scale item generation via \code{AIGENIE}.
#' It validates that all required inputs (API keys, custom prompts, and numerical parameters) are provided and correctly formatted.
#' Then, it calls \code{generate_output} to produce sample output from the language model.
#' This preview helps ensure that the output conforms to the expected format, which is crucial for your cleaning function to operate correctly
#' and for the final generated items to meet your requirements before generating a large number of items via \code{AIGENIE}.
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
#' @return The sample output generated by the language model as produced by your prompts. The structure of the output depends on your custom prompts, the chosen language model, and model settings (i.e., temperature and top-p).
#' @export
#' @examples
#' \dontrun{
#'   # Example: Preview the output from custom prompts before generating items
#'
#'   # Replace with your actual OpenAI API key
#'   key <- "INSERT YOUR KEY HERE"
#'
#'   # Define custom prompts for two traits (e.g., "extraversion" and "openness")
#'   custom_prompts <- list(
#'     extraversion = "Generate three unique, psychometrically robust items to measure extraversion from the Big Five model of personality. Use the format: <extraversion>: <item statement>.",
#'     openness = "Generate three unique, psychometrically robust items to measure openness from the Big Five model of personality. Use the format: <openness>: <item statement>."
#'   )
#'
#'   # Define a system role that instructs the language model on how to behave
#'   system_role <- "You are an expert psychometrician specialized in scale development."
#'
#'   # Run the validation to preview the prompt output. This helps ensure the output is formatted as expected.
#'   test_output <- validate_prompt(
#'     openai.API = key,
#'     user.prompts = custom_prompts,
#'     system.role = system_role
#'   )
#'
#'   # Print the output to inspect its format
#'   print(test_output)
#'
#' ########################################################################
#' ###### Or, Run Validate Prompt with an Open Source Model via Groq ######
#' ########################################################################
#'
#' # Add your API Key from Groq
#' groq.key <- "INSERT YOUR GROQ API KEY"
#'
#' # Chose between 'Mixtral', 'Gemma 2', 'Llama 3', or 'DeepSeek'
#' open.source.model <- "Gemma 2"
#'
#' # Generate output using your custom prompt
#' test_output_open_source <- validate_prompt(
#'     groq.API = groq.key,
#'     user.prompts = custom_prompts,
#'     model = open.source.model,
#'     system.role = system_role
#'   )
#'
#'
#' # Print the output to inspect its format
#' print(test_output_open_source)
#'
#' }
validate_prompt <- function(openai.API=NULL, groq.API = NULL,
                            user.prompts = NULL, N.runs = 3,
                            model="gpt3.5", top.p=1, temperature=1,
                            system.role=NULL, silently=FALSE){

  # Ensure all user inputs are as expected
  validate_promt_inputs <- validate_promt_inputs(openai.API, groq.API,
                                                 user.prompts, N.runs, model,
                                                 top.p, temperature, system.role,
                                                 silently)

  # Reassign variables based on cleaning/validation function
  user.prompts <- validate_promt_inputs$user.prompts
  openai.API <- validate_promt_inputs$openai.API
  groq.API <- validate_promt_inputs$groq.API
  model <- validate_promt_inputs$model
  system.role <- validate_promt_inputs$system.role

  # Call the API and return results of prompts
  return(generate_output(openai.API, groq.API,
                  user.prompts, N.runs, model,
                  top.p, temperature, system.role, silently))
}





