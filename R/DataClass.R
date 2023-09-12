Data <- R6::R6Class( # nolint: object_name
  "Data",
  inherit = BaseReactive,
  private = list(
    .data_type = NULL,
    .default = NULL
  ),
  public = list(
    print = function() {
      cat("Data Type:", private$data_type)
    },
    invalidate = function() {
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
    default = function(value) {
      if (missing(value)) {
        private$.default
      } else {
        private$.ddefault <- value
      }
    }
  )
)