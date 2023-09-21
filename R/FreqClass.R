Freq <- R6::R6Class( # nolint: object_name
  "Freq",
  inherit = BaseReactive,
  private = list(
    .desirable = NULL,
    .measure = NULL,
    .random_effects = NULL,
    .formatted_data = NULL,
    .pairwise = NULL,
    .n_connection = NULL,
    .netmeta = NULL,
    .netcomb = NULL,
    .valid = NULL
  ),
  public = list(
    initialize = function() {
      private$.random_effects <- 0
      private$.valid <- reactiveVal(FALSE)
      super$initialize()
    },
    print = function() {
      cat("Desirable: ", private$.data_type, "\n",
        "Measure: ", private$.default, "\n",
        "Random Effects: ", private$.format, "\n",
        "Valid: ", private$.valid(), "\n"
      )
    },
    invalidate = function() {
      private$.measure <- NULL
      private$.random_effects <- NULL
      private$.desirable <- NULL
      private$.formatted_data <- NULL
      private$.pairwise <- NULL
      private$.n_connection <- NULL
      private$.netmeta <- NULL
      private$.netcomb <- NULL
      private$.valid <- reactiveVal(FALSE)
      super$invalidate("Frequentist")
    },
    valid = function(value) {
      if (missing(value)) {
        print("Returning Valid as Reactive")
        private$.valid()
      } else {
        print("Setting Valid")
        if (!value) {
          private$.formatted_data <- NULL
          private$.pairwise <- NULL
          private$.n_connection <- NULL
          private$.netmeta <- NULL
          private$.netcomb <- NULL
        }
        private$.valid <- reactiveVal(value)
        invisible()
      }
    },
    measure = function(value) {
      if (missing(value)) {
        private$.measure
      } else {
        private$.measure <- value
      }
    },
    formatted_data = function(value) {
      if (missing(value)) {
        private$.formatted_data
      } else {
        private$.formatted_data <- value
      }
    },
    outcome_measure = function() {
      if (is.null(private$.measure)) {
        return("Outcome Measure")
      }
      dplyr::case_when(
        private$.measure == "md" ~ "Mean Difference (MD)",
        private$.measure == "smd" ~ "Standardised Mean Difference (SMD)",
        private$.measure == "or" ~ "Odds Ratio (OR)",
        private$.measure == "rr" ~ "Risk Ratio (RR)",
        private$.measure == "rd" ~ "Risk Difference (RD)",
        TRUE ~ "Outcome Measure"
      )
    },
    random_effects = function(value) {
      if (missing(value)) {
        private$.random_effects
      } else {
        private$.random_effects <- value
      }
    },
    random_effects_logical = function() {
      if (private$.random_effects == 1) {
        return(TRUE)
      }
      return(FALSE)
    },
    pairwise = function(value) {
      if (missing(value)) {
        private$.pairwise
      } else {
        private$.pairwise <- value
      }
    },
    n_connection = function(value) {
      if (missing(value)) {
        private$.n_connection
      } else {
        private$.n_connection <- value
      }
    },
    netmeta = function(value) {
      if (missing(value)) {
        private$.netmeta
      } else {
        private$.netmeta <- value
      }
    },
    netcomb = function(value) {
      if (missing(value)) {
        private$.netcomb
      } else {
        private$.netcomb <- value
      }
    },
    studies = function() {
      if (! is.null(private$.formatted_data)) {
        levels(as.factor(private$.formatted_data$study))
      } else {
        NULL
      }
    }
  )
)