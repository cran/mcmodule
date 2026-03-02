#' Create mcnodes from Data and Configuration Table
#'
#' Creates mcnodes based on mctable specifications and input data.
#' Applies transformations and generates mcnodes in the calling environment.
#'
#' @param data (data frame). Input data containing variables for mcnode creation.
#' @param mctable (data frame). Configuration table with columns:
#'   mcnode, mc_func, transformation, from_variable.
#' @param envir (environment, optional). Environment where nodes are created.
#'   Default: parent.frame().
#'
#' @return NULL (invisibly). mcnodes created in `envir`.
#' @import mc2d
#' @examples
#' create_mcnodes(
#'   data = imports_data,
#'   mctable = imports_mctable
#' )
#'
#' @export
create_mcnodes <- function(
  data,
  mctable = set_mctable(),
  envir = parent.frame()
) {
  # Validate that mctable has required columns
  valid_mctable <- all(c("mcnode", "mc_func") %in% names(mctable))
  if (!valid_mctable) {
    stop("mctable must contain 'mcnode' and 'mc_func' columns")
  }

  # Validate that mctable is not empty
  if (nrow(mctable) < 1) {
    stop("mctable has 0 rows")
  }

  # Validate that data is not empty
  if (nrow(data) < 1) {
    stop("data has 0 rows")
  }

  # Check if data contains any columns matching mcnode names
  data_mc_inputs <- grepl(
    paste(paste0("\\<", mctable$mcnode, ".*"), collapse = "|"),
    names(data)
  )
  if (!any(data_mc_inputs)) {
    stop("data must contain columns matching mctable 'mcnode' names")
  }

  # Check and clean mctable
  mctable <- check_mctable(mctable)

  # Process each Monte Carlo node defined in mctable
  for (i in 1:length(mctable$mcnode)) {
    # Extract current mcnode configuration
    mcrow <- mctable[i, ]
    mc_name <- mcrow$mcnode

    #### TRANSFORM INPUT DATA IF SPECIFIED ####
    # Check if transformation is needed and source data exists
    transformation <- as.character(mcrow$transformation)
    value_transform_l <- any(
      c(mcrow$mcnode, mcrow$from_variable) %in% names(data)
    ) &
      !is.na(transformation)

    if (value_transform_l) {
      # Determine source variable and apply transformation
      value_name <- ifelse(
        is.na(mcrow$from_variable),
        as.character(mcrow$mcnode),
        as.character(mcrow$from_variable)
      )
      assign("value", data[[value_name]], envir = envir)
      data[as.character(mc_name)] <- eval(
        parse(text = transformation),
        envir = envir
      )
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
        if (
          !is.numeric(data[[mc_inputs[j]]]) & !is.logical(data[[mc_inputs[j]]])
        ) {
          warning(paste0(
            mc_inputs[j],
            " is ",
            class(data[[mc_inputs[j]]]),
            " and should be numeric or logical"
          ))
          next
        }

        #### CREATE MONTE CARLO DATA OBJECTS ####
        mcdata_exp <- paste0(
          "mcdata(data = data$",
          mc_inputs[j],
          ", type = '0', nvariates = ",
          nrow(data),
          ")"
        )

        # Handle NA values based on whether it's a distribution
        if (is.na(mcrow$mc_func)) {
          assign(
            mc_inputs[j],
            mcnode_na_rm(eval(parse(text = mcdata_exp))),
            envir = envir
          )
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
          warning(paste0(
            mc_name,
            " ",
            mc_func,
            " mcstoc node not created because no ",
            mc_func,
            " parameter was provided. ",
            mc_name,
            " mcdata node created \n"
          ))
          next
        }

        # Prepare expressions for Monte Carlo node creation
        # Match each parameter to its corresponding column name
        matched_inputs <- sapply(parameters_available, function(param) {
          param_name <- paste(mc_name, param, sep = "_")
          matched <- mc_inputs[mc_inputs == param_name]
          if (length(matched) == 0) {
            stop(paste0(
              "Parameter '",
              param,
              "' for ",
              mc_name,
              " does not have a matching column in data"
            ))
          }
          if (length(matched) > 1) {
            warning(paste0(
              "Multiple columns match parameter '",
              param,
              "' for ",
              mc_name,
              ", using first match"
            ))
            matched <- matched[1]
          }
          matched
        })
        mc_parameters_exp <- paste(
          paste0(parameters_available, " = envir$", matched_inputs),
          collapse = ", "
        )
        mc_na_rm_parameters_exp <- paste(
          paste0(
            parameters_available,
            " = ",
            "mcnode_na_rm(envir$",
            matched_inputs,
            ")"
          ),
          collapse = ", "
        )

        mc_func_exp <- paste0("func = ", mc_func)
        mcstoc_exp <- paste0(
          "mcstoc(",
          mc_func_exp,
          ", type = 'V', ",
          mc_parameters_exp,
          ", nvariates = ",
          nrow(data),
          ")"
        )
        mcstoc_na_rm_exp <- paste0(
          "mcstoc(",
          mc_func_exp,
          ", type = 'V', ",
          mc_na_rm_parameters_exp,
          ", nvariates = ",
          nrow(data),
          ")"
        )

        #### ATTEMPT NODE CREATION WITH ERROR HANDLING ####
        tryCatch(
          assign(
            as.character(mc_name),
            eval(parse(text = mcstoc_exp)),
            envir = envir
          ),
          error = function(e) {
            message(
              "An error occurred generating ",
              mc_name,
              " ",
              mc_func,
              " mcstoc node:\n",
              e
            )
          },
          warning = function(w) {
            #### RETRY WITH NA REMOVAL IF INITIAL ATTEMPT FAILS ####
            tryCatch(
              assign(
                as.character(mc_name),
                suppressWarnings(eval(parse(text = mcstoc_na_rm_exp))),
                envir = envir
              ),
              error = function(e) {
                message(
                  "After mcnode_na_rm: An error occurred generating ",
                  mc_name,
                  " ",
                  mc_func,
                  " mcstoc node:\n",
                  e
                )
              },
              warning = function(w) {
                message(
                  "After mcnode_na_rm: A warning occurred generating ",
                  mc_name,
                  " ",
                  mc_func,
                  " mcstoc node:\n",
                  w,
                  "Check data inputs: is min < mode < max?\n"
                )
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
