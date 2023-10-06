BaseReactive <- R6::R6Class( # nolint: object_name
  "BaseReactive",
  private = list(
    reactive_dep = NULL,
    reactive_expr = NULL,
    .invalidate = function(name = "base") {
      private$count <- private$count + 1
      # Don't invalidate on first invalidate call
      if (private$count > 1) {
        print(paste0("Invalidating ", name))
        private$reactive_dep(private$count)
      }
      invisible()
    },
    count = 0
  ),
  public = list(
    initialize = function() {
      # Until someone calls $reactive(), private$reactive_dep() is a no-op. Need
      # to set it here because if it's set in the definition of private above,
      # it will be locked and can't be changed.
      private$reactive_dep <- function(x) NULL
    },
    reactive = function() {
      # Ensure the reactive stuff is initialized.
      if (is.null(private$reactive_expr)) {
        private$reactive_dep <- reactiveVal(0)
        private$reactive_expr <- reactive({
          private$reactive_dep()
          self
        })
      }
      private$reactive_expr
    },
    invalidate = function(name = "base") {
      private$.invalidate(name)
    }

  )
)