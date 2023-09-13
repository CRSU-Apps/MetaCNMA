Data <- R6::R6Class( # nolint: object_name
  "Data",
  inherit = BaseReactive,
  private = list(
    .data_type = NULL,
    .default = NULL,
    .format = NULL,
    .valid = NULL,
    .data = NULL,
    .measure = NULL,
    .desirable = NULL,
    .outcome_name = NULL
  ),
  public = list(
    print = function() {
      cat("Data Type:", private$data_type)
    },
    invalidate = function() {
      private$.valid <- FALSE
      super$invalidate("Data")
    },
    data_type = function(value) {
      if (missing(value)) {
        private$.data_type
      } else {
        private$.data_type <- value
        self$invalidate()
      }
    },
    format = function(value) {
      if (missing(value)) {
        private$.format
      } else {
        private$.format <- value
      }
    },
    default = function(value) {
      if (missing(value)) {
        private$.default
      } else {
        private$.default <- value
      }
    },
    valid = function(value) {
      if (missing(value)) {
        private$.valid
      } else {
        private$.valid <- value
      }
    },
    data = function(value) {
      if (missing(value)) {
        private$.data
      } else {
        private$.data <- value
      }
    },
    measure = function(value) {
      if (missing(value)) {
        private$.measure
      } else {
        private$.measure <- value
      }
    },
    desirable = function(value) {
      if (missing(value)) {
        private$.desirable
      } else {
        private$desirable <- value
      }
    },
    outcome_name = function(value) {
      if (missing(value)) {
        private$.outcome_name
      } else {
        private$.outcome_name <- value
      }
    },
    load_data = function(format,
                         data,
                         default = FALSE,
                         measure = NULL,
                         desirable = NULL,
                         outcome_name = NULL) {
      private$.format <- format
      private$.data <- data
      private$.default <- default
      private$.valid <- TRUE
    }
  )
)