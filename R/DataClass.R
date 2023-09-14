Data <- R6::R6Class( # nolint: object_name
  "Data",
  inherit = BaseReactive,
  private = list(
    .data_type = NULL,
    .default = NULL,
    .format = NULL,
    .valid = NULL,
    .valid_dep = NULL,
    .valid_reactive = NULL,
    .data = NULL,
    .measure = NULL,
    .desirable = NULL,
    .outcome_name = NULL
  ),
  public = list(
    initialize = function() {
      private$.valid_dep <- function(x) NULL
      super$initialize()
    },
    print = function() {
      cat("Data Type: ", private$.data_type, "\n",
        "Default: ", private$.default, "\n",
        "Format: ", private$.format, "\n",
        #"Valid: ", private$.valid(), "\n",
        #"Data: ", private$.data, "\n",
        "Measure: ", private$.measure, "\n",
        "Desirable: ", private$.desirable, "\n",
        "Outcome Name: ", private$.outcome_name, "\n"
      )
    },
    invalidate = function() {
      private$.default <- NULL
      private$.format <- NULL
      private$.data <- NULL
      private$.measure <- NULL
      private$.desirable <- NULL
      private$.outcome_name <- NULL
      private$.valid <- FALSE
      private$.valid_dep(FALSE)
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
        if (is.null(private$.valid_reactive)) {
          private$.valid_reactive <- reactive({
            private$.valid_dep <- reactiveVal(FALSE)
            private$.valid
          })
        }
        print("Returning Valid as Reactive")
        private$.valid_reactive()
      } else {
        print("Setting Valid")
        private$.valid <- value
        private$.valid_dep(value)
        invisible()
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
      private$.measure <- measure
      private$.desirable <- desirable
      private$.outcome_name <- outcome_name
      private$.valid <- TRUE
      private$.valid_dep(TRUE)
    }
  )
)