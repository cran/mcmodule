#' Create Monte Carlo Nodes from Data and Configuration Table
#'
#' Creates Monte Carlo nodes (mcnodes) based on instructions provided in a configuration
#' table (mctable) and input variables from a dataframe.
#'
#' @param data A data frame containing the input variables for creating Monte Carlo nodes
#' @param mctable A configuration table specifying MC node definitions. Must contain columns:
#'   \itemize{
#'     \item mcnode: Name of the Monte Carlo node
#'     \item mc_func: Distribution function to use (if applicable)
#'     \item transformation: Optional transformation to apply to input data
#'     \item from_variable: Optional source variable name for transformation
#'   }
#' @param envir Environment where MC nodes will be created (default: parent.frame())
#' @return No return value, creates MC nodes in the specified environment
#' @import mc2d
#' @examples
#' create_mcnodes(data = imports_data, mctable = imports_mctable)
#'
#' @export
create_mcnodes <- function(data, mctable = set_mctable(), envir = parent.frame()) {
  # Validate that mctable has required columns
  valid_mctable <- all(c("mcnode", "mc_func") %in% names(mctable))
  if (!valid_mctable) stop("mctable must contain 'mcnode' and 'mc_func' columns")

  # Validate that mctable is not empty
  if (nrow(mctable) < 1) stop("mctable is empty")

  # Check if data contains any columns matching mcnode names
  data_mc_inputs <- grepl(paste(paste0("\\<", mctable$mcnode, ".*"), collapse = "|"), names(data))
  if (!any(data_mc_inputs)) stop("data must contain columns matching mctable 'mcnode' names")

  # Check and clean mctable
  mctable<-check_mctable(mctable)

  # Process each Monte Carlo node defined in mctable
  for (i in 1:length(mctable$mcnode)) {
    # Extract current mcnode configuration
    mcrow <- mctable[i, ]
    mc_name <- mcrow$mcnode

    #### TRANSFORM INPUT DATA IF SPECIFIED ####
    # Check if transformation is needed and source data exists
    transformation <- as.character(mcrow$transformation)
    value_transform_l <- any(c(mcrow$mcnode, mcrow$from_variable) %in% names(data)) & !is.na(transformation)

    if (value_transform_l) {
      # Determine source variable and apply transformation
      value_name <- ifelse(is.na(mcrow$from_variable), as.character(mcrow$mcnode), as.character(mcrow$from_variable))
      assign("value", data[[value_name]], envir = envir)
      data[as.character(mc_name)] <- eval(parse(text = transformation), envir = envir)
      rm("value", envir = envir)
    }

    # Identify columns that correspond to this Monte Carlo node
    mc_inputs_l <- grepl(paste0("\\<", mc_name, "(\\>|_[^_]*\\>)"), names(data))

    ##### PROCESS MONTE CARLO NODE INPUTS ####
    if (any(mc_inputs_l)) {
      # Get all related input columns (e.g., min, max, mode)
      mc_inputs <- names(data)[mc_inputs_l]

      # Process each input parameter
      for (j in 1:length(mc_inputs)) {
        # Validate input data type
        if (!is.numeric(data[[mc_inputs[j]]]) & !is.logical(data[[mc_inputs[j]]])) {
          warning(paste0(mc_inputs[j], " is ", class(data[[mc_inputs[j]]]), " and should be numeric or logical"))
          next
        }

        #### CREATE MONTE CARLO DATA OBJECTS ####
        mcdata_exp <- paste0("mcdata(data = data$", mc_inputs[j], ", type = '0', nvariates = ", nrow(data), ")")

        # Handle NA values based on whether it's a distribution
        if (is.na(mcrow$mc_func)) {
          assign(mc_inputs[j], mcnode_na_rm(eval(parse(text = mcdata_exp))), envir = envir)
        } else {
          assign(mc_inputs[j], eval(parse(text = mcdata_exp)), envir = envir)
        }
      }

      #### CREATE DISTRIBUTION-BASED MONTE CARLO NODES ####
      if (!is.na(mcrow$mc_func)) {
        # Extract distribution function parameters
        mc_func <- mcrow$mc_func
        func_args <- deparse(args(as.character(mc_func)))
        func_args <- unlist(strsplit(func_args, ", "))
        func_args <- func_args[grepl(" .*=.*", func_args)]
        func_args <- gsub(" =.*", "", func_args)

        # Map parameters to input data
        mc_parameters <- paste(mc_name, func_args, sep = "_")
        parameters_available <- func_args[mc_parameters %in% mc_inputs]

        # Validate required parameters exist
        valid_parameters <- length(parameters_available) > 0
        if (!valid_parameters) {
          warning(paste0(mc_name, " ", mc_func, " mcstock node not created because no ", mc_func, " parameter was provided. ", mc_name, " mcdata node created \n"))
          next
        }

        ##### HANDLE MISSING MODE PARAMETER ####
        if ("mode" %in% func_args && !"mode" %in% parameters_available) {
          # Calculate mode as average of min and max for PERT distribution
          proxy_mode <- (get(mc_parameters[func_args == "min"]) + get(mc_parameters[func_args == "max"])) / 2
          proxy_mode_name <- paste(mc_name, "mode", sep = "_")
          assign(proxy_mode_name, proxy_mode, envir = envir)
          mc_inputs <- c(mc_inputs, proxy_mode_name)
          parameters_available <- c(parameters_available, "mode")
          warning("Mode not provided for rpert in ", mc_name, ", mode was assumed to be the mean of min and max \n")
        }

        # Prepare expressions for Monte Carlo node creation
        mc_parameters_exp <- paste(paste0(parameters_available, " = envir$", mc_inputs), collapse = ", ")
        mc_na_rm_parameters_exp <- paste(paste0(parameters_available, " = ", "mcnode_na_rm(envir$", mc_inputs, ")"), collapse = ", ")

        mc_func_exp <- paste0("func = ", mc_func)
        mcstoc_exp <- paste0("mcstoc(", mc_func_exp, ", type = 'V', ", mc_parameters_exp, ", nvariates = ", nrow(data), ")")
        mcstoc_na_rm_exp <- paste0("mcstoc(", mc_func_exp, ", type = 'V', ", mc_na_rm_parameters_exp, ", nvariates = ", nrow(data), ")")

        #### ATTEMPT NODE CREATION WITH ERROR HANDLING ####
        tryCatch(assign(as.character(mc_name), eval(parse(text = mcstoc_exp)), envir = envir),
          error = function(e) {
            message("An error occurred generating ", mc_name, " ", mc_func, " mcstock node:\n", e)
          },
          warning = function(w) {
            #### RETRY WITH NA REMOVAL IF INITIAL ATTEMPT FAILS ####
            tryCatch(
              assign(as.character(mc_name), suppressWarnings(eval(parse(text = mcstoc_na_rm_exp))), envir = envir),
              error = function(e) {
                message("After mcnode_na_rm: An error occurred generating ", mc_name, " ", mc_func, " mcstock node:\n", e)
              },
              warning = function(w) {
                message("After mcnode_na_rm: A warning occurred generating ", mc_name, " ", mc_func, " mcstock node:\n", w, "Check data inputs: is min < mode < max?\n")
              }
            )
          },
          finally = {
            # Cleanup temporary input objects
            remove(list = mc_inputs, envir = envir)
          }
        )
      }
    }
  }
}
