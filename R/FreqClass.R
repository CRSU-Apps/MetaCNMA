Freq <- R6::R6Class( # nolint: object_name
  "Freq",
  inherit = BaseReactive,
  private = list(
  ),
  public = list(
    print = function() {
    },
    invalidate = function() {
      super$invalidate("Frequentist")
    }

  )
)