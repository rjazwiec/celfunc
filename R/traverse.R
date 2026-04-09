#' Enumerate all element paths in a nested list
#'
#' Recursively traverses a nested list and returns the access path to every
#' element. Named elements are represented as `parent$name`; unnamed elements
#' as `parent[[i]]`.
#'
#' @param x A list to traverse.
#' @param parent Character. Prefix prepended to each path. Used internally
#'   during recursion; leave at the default `""` when calling directly.
#'
#' @return A character vector of element paths. Returns `character(0)` if `x`
#'   is not a list.
#'
#' @examples
#' lst <- list(a = 1, b = list(c = 2, d = list(e = 3)))
#' get_list_names(lst)
#' # [1] "a"      "b"      "b$c"    "b$d"    "b$d$e"
#'
#' # Unnamed elements use [[i]] notation
#' get_list_names(list(1, list(2, 3)))
#' # [1] "[[1]]"       "[[2]]"       "[[2]][[1]]"  "[[2]][[2]]"
#'
#' @export
get_list_names <- function(x, parent = "") {
  if (!is.list(x)) return(character(0))

  nms <- names(x)
  if (is.null(nms)) nms <- rep("", length(x))

  # Accumulate into a pre-allocated list to avoid O(n²) vector growing with c()
  parts <- vector("list", length(x))

  for (i in seq_along(x)) {
    current <- if (nchar(nms[i]) > 0) {
      paste0(parent, nms[i])
    } else {
      paste0(parent, "[[", i, "]]")
    }
    # Use "$" after named segments, no separator after index [[i]] segments
    separator   <- if (nchar(nms[i]) > 0) "$" else ""
    parts[[i]]  <- c(current, get_list_names(x[[i]], paste0(current, separator)))
  }

  unlist(parts, use.names = FALSE)
}
