

#### USER FACING FUNCTIONS ####

#' AI-GENIE
#'
#' Generate, validate, and assess your items for quality and redundancy using AI-GENIE (Automatic Item Generation and Validation via Network-Integrated Evaluation). This function generates items using your chosen language model, embeds the items, and performs AI-GENIE item validation and redundancy reduction to refine the item pool. You can either have the function construct prompts for you using provided item attributes and optional high-quality item examples (recommended), or supply your own custom prompts. If providing custom prompts, you must also provide a text cleaning function that can parse and extract item statements from the model output.
#'
#' @param item.attributes (Required when `custom = FALSE`) A named list containing item type labels and their corresponding attributes. The list have names or identifiers representing item types. Each element should be a character vector of attributes for that item type.
#' @param user.prompts (Required when `custom = TRUE`) A named list or data frame of custom prompt strings for each item type. Each prompt must be a single character string associated with an item type label.
#' @param item.type.definitions An optional named list or data frame providing definitions for each item type. Each definition should be a character string not exceeding 250 characters. This helps the language model understand the item types better. Definitions are included at the beginning of the prompts for their corresponding item types.
#' @param cleaning.fun (Required when `custom = TRUE`) A text cleaning function that can clean and parse the model's expected text output given your custom prompt. The function must accept exactly one parameter (the model's output) and return a list of cleaned items.
#' @param openai.API A required character string of your OpenAI API key.
#' @param groq.API (Required when using open-source models) A character string of your Groq API key.
#' @param custom Logical; defaults to `FALSE`. Indicates whether you intend to supply your own custom prompts.
#' @param system.role An optional character string describing the language model's role (e.g., "a professional methodologist and scale developer").
#' @param scale.title An optional character string specifying the name of your inventory.
#' @param sub.domain An optional character string specifying the inventory's sub-domain or specialty (e.g., "abnormal psychology").
#' @param model A character string specifying the model to use for item generation. Defaults to `"gpt3.5"`. Options are `"gpt3.5"`, `"gpt4o"`, `"llama3"`, `"mixtral"`, or `"gemma2"`. A Groq API key is required for `"llama3"`, `"mixtral"`, or `"gemma2"` models.
#' @param item.examples An optional character vector of well-crafted, high-quality example item strings. If premium items are not readily available, it is recommended to leave this parameter as `NULL`.
#' @param target.N An integer or integer vector specifying the target number of items to generate. Defaults to `100`. If an integer is provided, the function will generate approximately equal numbers of items per item type summing to this total. If an unequal distribution is desired, provide an integer vector specifying the target number of items for each item type, in the same order as the names of `item.attributes` or `user.prompts`. Each item type must have a target of at least 15 items, and the total must be at least 50 items. This requirement does not apply if only generating items (see `items.only`).
#' @param temperature Numeric; defaults to `1`. A value between `0` and `2` setting the temperature of the language model.
#' @param top.p Numeric; defaults to `1`. A value between `0` and `1` setting the top-p parameter of the language model.
#' @param items.only Logical; defaults to `FALSE`. Set to `TRUE` if you only want the items generated without further processing. When `TRUE`, item pool reduction through AI-GENIE is skipped, and the function returns a data frame of the generated items.
#' @param adaptive Logical; defaults to `TRUE`. Indicates whether to use an adaptive prompting approach (recommended). When `TRUE`, the language model receives a list of previously generated items to avoid redundancy. Set to `FALSE` to skip this step if context length is a concern.
#' @param EGA_model A character string specifying the model to use with Exploratory Graph Analysis (EGA). Options are `"tmfg"` or `"glasso"`. Defaults to `tmfg`. If set to `NULL`, both models are tested, and the one yielding the best Normalized Mutual Information (NMI) is returned.
#' @param keep.org Logical; defaults to `FALSE`. When `TRUE`, returns a data frame of the original item pool.
#' @param plot Logical; defaults to `TRUE`. Specifies whether to display the main summary network plots.
#' @param plot.stability Logical; defaults to `FALSE`. Specifies whether to display the secondary network stability plots.
#' @param silently Logical; defaults to `FALSE`. When `TRUE`, suppresses console output.
#' @return A list containing:
#' \describe{
  #'   \item{\code{main_result}}{A data frame of the item pool after AI-GENIE reduction. The data frame has the columns `ID`, `type`, `statement`, and `EGA_communities`.}
  #'   \item{\code{final_ega_obj}}{The final EGA object after reduction.}
  #'   \item{\code{final_bootega_obj}}{The final bootEGA object after reduction.}
  #'   \item{\code{initial_ega_obj}}{The initial EGA object with the entire item pool.}
  #'   \item{\code{initial_bootega_obj}}{The initial bootEGA object generated from redundancy-reduced data.}
  #'   \item{\code{embeddings}}{The embeddings generated for the items.}
  #'   \item{\code{embedding_type}}{The type of embeddings used ("sparse" or "full").}
  #'   \item{\code{selected_model}}{The EGA model used throughout the pipeline.}
  #'   \item{\code{nmi}}{The Normalized Mutual Information (NMI) of the final item pool.}
  #'   \item{\code{start_nmi}}{The NMI of the original item pool.}
  #'   \item{\code{start_N}}{The starting sample size (number of items).}
  #'   \item{\code{final_N}}{The final sample size after reduction.}
  #'   \item{\code{original_items (optional)}}{(ONLY returns if `keep.org` is `TRUE`) The original sample generated.}
  #' }
#' @export
#' @examples
#' \dontrun{
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
#'   neuroticism = c("perfectionism", "temperamental", "overthinking"),
#'   openness = c("worldly", "artistic", "philosophical", "curious"),
#'   extraversion = c("gregarious", "talkative")
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
#' View(personality.inventory.results$main_result)
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
#'     "Generate EIGHT unique, psychometrically robust single-statement items designed to assess ",
#'     "the Big Five personality trait neuroticism. Put EACH item on its own line. ",
#'     "Format the items EXACTLY like so: \n<neuroticism>||<item statement>\n",
#'     "The formatting is EXTREMELY important, so follow it EXACTLY. Be creative! Avoid repetition. ",
#'     "Try to capture as many aspects of the trait neuroticism as you can."
#'   ),
#'
#'   # Prompt for generating openness traits
#'   openness = paste0(
#'     "Generate EIGHT unique, psychometrically robust single-statement items designed to assess ",
#'     "the Big Five personality trait openness. Put EACH item on its own line. ",
#'     "Format the items EXACTLY like so: \n<openness>||<item statement>\n",
#'     "The formatting is EXTREMELY important, so follow it EXACTLY. Be creative! Avoid repetition. ",
#'     "Try to capture as many aspects of the trait openness as you can."
#'   ),
#'
#'   # Prompt for generating extraversion traits
#'   extraversion = paste0(
#'     "Generate EIGHT unique, psychometrically robust single-statement items designed to assess ",
#'     "the Big Five personality trait extraversion. Put EACH item on its own line. ",
#'     "Format the items EXACTLY like so: \n<extraversion>||<item statement>\n",
#'     "The formatting is EXTREMELY important, so follow it EXACTLY. Be creative! Avoid repetition. ",
#'     "Try to capture as many aspects of the trait extraversion as you can."
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
#'   # Initialize list to store cleaned item statements
#'   item_texts <- list()
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
#'       # Remove unwanted characters
#'       item_text <- gsub(">", "", item_text)
#'       item_text <- gsub("<", "", item_text)
#'
#'       # Append to list
#'       item_texts <- c(item_texts, item_text)
#'
#'     }
#'   }
#'
#'   # Return a list of cleaned item statements
#'   return(item_texts)
#' }
#'
#' # Run AI-GENIE to generate, validate, and redundancy-check an item pool for your new scale.
#' personality.inventory.results.custom <- AIGENIE(
#'   user.prompts = custom.personality.prompts,
#'   openai.API = key, # created in example 1
#'   cleaning.fun = custom_cleaning,
#'   item.examples = personality.items, # created in example 1
#'   scale.title = scale.title, # created in example 1
#'   custom = TRUE
#' )
#'
#' # View the final item pool
#' View(personality.inventory.results.custom$main_result)
#' }
AIGENIE <- function(item.attributes = NULL, openai.API, groq.API = NULL, custom = FALSE,
                    user.prompts = NULL, item.type.definitions = NULL,
                    cleaning.fun = NULL, system.role = NULL,
                    scale.title = NULL, sub.domain = NULL, model = "gpt3.5", item.examples = NULL,
                    target.N = 100, temperature = 1, top.p = 1, items.only = FALSE, adaptive = TRUE,
                    EGA_model = "tmfg", keep.org = FALSE, plot = TRUE, plot.stability = FALSE,
                    silently = FALSE, ...) {

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
    EGA_model = EGA_model,
    keep.org = keep.org,
    plot = plot,
    plot.stability = plot.stability,
    silently = silently,
    ...
  )

    generated_items <- generate.items.internal(
      model = validated_params$model,
      temperature = validated_params$temperature,
      top.p = validated_params$top.p,
      groq.API = validated_params$groq.API,
      openai.API = validated_params$openai.API,
      target.N = validated_params$target.N,
      item.attributes = validated_params$item.attributes,
      scale.title = validated_params$scale.title,
      sub.domain = validated_params$sub.domain,
      item.examples = validated_params$item.examples,
      system.role = validated_params$system.role,
      user.prompts = validated_params$user.prompts,
      item.type.definitions = validated_params$item.type.definitions,
      cleaner_fun = cleaning.fun,
      custom = validated_params$custom,
      adaptive = validated_params$adaptive,
      silently = validated_params$silently,
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
    EGA_model = EGA_model,
    keep.org = keep.org,
    plot = plot,
    plot.stability = plot.stability,
    silently = silently,
    ...
  )

  return(results)
}





#--- GENIE
#' GENIE (Item Validation and Reduction without Item Generation)
#'
#' Validate and assess your existing items for quality and redundancy using AI-GENIE (Automatic Item Generation and Validation via Network-Integrated Evaluation). This function embeds user-provided items and performs AI-GENIE item validation and redundancy reduction to refine the item pool.
#'
#' @param items A required data frame containing your item statements and item type labels. The data frame should have two columns: one containing the item statements and one containing the item type labels. The column names do not need to be specific; the function will determine which column contains the item statements and which contains the item type labels. There must be at least two distinct item types which each contain at least 15 items. The total number of unique items should be at least 50.
#' @param openai.API A required character string of your OpenAI API key.
#' @param EGA_model A character string specifying the model to use with Exploratory Graph Analysis (EGA). Options are `"tmfg"` or `"glasso"`. Defaults to `NULL`, in which case both models are tested, and the one yielding the best Normalized Mutual Information (NMI) is returned.
#' @param plot Logical; defaults to `TRUE`. Specifies whether to display summary network plots.
#' @param plot.stability Logical; defaults to `FALSE`. Specifies whether to display the secondary network stability plots.
#' @param silently Logical; defaults to `FALSE`. When `TRUE`, suppresses console output.
#' @return A list containing:
#' \describe{
#'   \item{\code{main_result}}{A data frame of the item pool after AI-GENIE reduction. The data frame has the columns `ID`, `type`, `statement`, and `EGA_communities`.}
#'   \item{\code{final_ega_obj}}{The final EGA object after reduction.}
#'   \item{\code{final_bootega_obj}}{The final bootEGA object after reduction.}
#'   \item{\code{initial_ega_obj}}{The initial EGA object with the entire item pool.}
#'   \item{\code{initial_bootega_obj}}{The initial bootEGA object generated from redundancy-reduced data.}
#'   \item{\code{embeddings}}{The embeddings generated for the items.}
#'   \item{\code{embedding_type}}{The type of embeddings used ("sparse" or "full").}
#'   \item{\code{selected_model}}{The EGA model used throughout the pipeline.}
#'   \item{\code{nmi}}{The Normalized Mutual Information (NMI) of the final item pool.}
#'   \item{\code{start_nmi}}{The NMI of the original item pool.}
#'   \item{\code{start_N}}{The starting sample size (number of items).}
#'   \item{\code{final_N}}{The final sample size after reduction.}
#'   \item{\code{original_items (optional)}}{(ONLY returns if `keep.org` is `TRUE`) The original sample generated.}
#' }
#' @export
#' @examples
#' \dontrun{
#' ########################################################################
#' ##### Use AI-GENIE without AI - Validate Existing Items via GENIE ######
#' ########################################################################
#'
#' # Add an OpenAI API key
#' key <- "INSERT YOUR KEY HERE"
#'
#' trait_labels <- c(rep("openness", 20), rep("neuroticism", 20), rep("extraversion", 20))
#'
#' items <- c(
#'   # Openness
#'   "I am someone who loves to experience new cultures when I travel.",
#'   "I am someone who appreciates the value of beautiful artwork.",
#'   "I am someone who enjoys venturing beyond my comfort zone.",
#'   "I am someone who likes an element of spontaneity in my daily routine.",
#'   "I am someone who tries a new dish or cuisine whenever I get the chance.",
#'   "I am someone who has no trouble acknowledging different perspectives.",
#'   "I am someone who likes engaging in philosophical debates.",
#'   "I am someone who feels intense awe when listening to incredible music.",
#'   "I am someone who will often take the scenic route.",
#'   "I am someone who can adapt quickly to unexpected situations.",
#'   "I am someone who enjoys solving complex problems.",
#'   "I am someone who is very curious.",
#'   "I am someone who likes to read books.",
#'   "I am someone who enjoys learning new things.",
#'   "I am someone who likes to explore new ideas.",
#'   "I am someone who enjoys creative activities.",
#'   "I am someone who is open to trying new hobbies.",
#'   "I am someone who likes to think about abstract concepts.",
#'   "I am someone who enjoys philosophical discussions.",
#'   "I am someone who appreciates diverse perspectives.",
#'
#'   # Neuroticism
#'   "I am someone who is rarely late to anything.",
#'   "I am someone who gets upset when my daily routine is disrupted.",
#'   "I am someone who has little patience for others' incompetencies.",
#'   "I am someone who feels anxiousness and doubt when friends don't answer my calls.",
#'   "I am someone who is extremely organized, no matter what the cost.",
#'   "I am someone who often experiences too many emotions when things go wrong.",
#'   "I am someone who needs to be calmed down after an intense fight.",
#'   "I am someone who often feels uneasy when plans change suddenly.",
#'   "I am someone who really dislikes it when other people mess up.",
#'   "I am someone who hates when people insist on changing the plans.",
#'   "I am someone who often worries about the future.",
#'   "I am someone who feels nervous in unfamiliar situations.",
#'   "I am someone who gets stressed easily.",
#'   "I am someone who often feels overwhelmed.",
#'   "I am someone who is very sensitive to criticism.",
#'   "I am someone who tends to over think things.",
#'   "I am someone who often feels insecure.",
#'   "I am someone who has a hard time letting go of negative thoughts.",
#'   "I am someone who frequently feels down or depressed.",
#'   "I am someone who often feels tense or on edge.",
#'
#'    # Extraversion
#'   "I am someone who enjoys being the center of attention.",
#'   "I am someone who feels energized when I am around other people.",
#'   "I am someone who often initiates conversations with strangers.",
#'   "I am someone who thrives in lively and dynamic environments.",
#'   "I am someone who prefers to work in teams rather than alone.",
#'   "I am someone who finds it easy to make new friends.",
#'   "I am someone who is enthusiastic about participating in group activities.",
#'   "I am someone who frequently speaks first in group discussions.",
#'   "I am someone who enjoys sharing my thoughts and ideas with others.",
#'   "I am someone who is comfortable expressing my emotions in social settings.",
#'   "I am someone who likes to take on leadership roles.",
#'   "I am someone who enjoys social gatherings.",
#'   "I am someone who feels comfortable in large groups.",
#'   "I am someone who likes to meet new people.",
#'   "I am someone who enjoys being in the spotlight.",
#'   "I am someone who is very talkative.",
#'   "I am someone who likes to be active in social events.",
#'   "I am someone who enjoys networking.",
#'   "I am someone who feels at ease in social situations.",
#'   "I am someone who likes to engage in group activities."
#' )
#'
#' my.personality.items <- data.frame(items, trait_labels)
#'
#' # Run Genie to validate and redundancy-check an existing item pool
#' my.personality.inventory.results <- GENIE(items = my.personality.items,
#'                                          openai.API = key)
#'
#' # View the final item pool
#' View(my.personality.inventory.results$main_result)
#' }
GENIE <- function(items, openai.API, EGA_model=NULL, plot=TRUE, plot.stability = FALSE, silently=FALSE, ...) {

  # check the user-provided items
  checks <- GENIE_checks(item.data=items, openai.API=openai.API, EGA_model=EGA_model,
                         plot=plot, silently=silently)
  openai.API <- checks[["openai.API"]]
  items <- checks[["items"]]

  # run the pipeline
  run_pipeline <- run_pipeline(items = items, EGA_model = EGA_model,openai.key=openai.API,
                               labels = items$type, keep.org=FALSE, plot = plot, plot.stability = FALSE,
                               silently= silently)

  return(run_pipeline)

}
