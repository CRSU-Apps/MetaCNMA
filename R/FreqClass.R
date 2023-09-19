Freq <- R6::R6Class( # nolint: object_name
  "Freq",
  inherit = BaseReactive,
  private = list(
    .desirable = NULL,
    .measure = NULL,
    .random_effects = NULL,
    .formatted_data = NULL,
    .valid = NULL
  ),
  public = list(
    initialize = function() {
      private$.valid <- reactiveVal(FALSE)
      private$.random_effects <- 0
      private$.desirable <- NULL
      private$.measure <- NULL
      private$.formatted_data <- NULL
      super$initialize()
    },
    print = function() {
    },
    invalidate = function() {
      private$.measure <- NULL
      private$.random_effects <- NULL
      private$.valid <- reactiveVal(FALSE)
      super$invalidate("Frequentist")
    },
    valid = function(value) {
      if (missing(value)) {
        print("Returning Valid as Reactive")
        private$.valid()
      } else {
        print("Setting Valid")
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
    random_effects = function(value) {
      if (missing(value)) {
        private$.random_effects
      } else {
        private$.random_effects <- value
      }
    },
    formatted_data = function(value) {
      if (missing(value)) {
        private$.formatted_data
      } else {
        private$.formatted_data <- value
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