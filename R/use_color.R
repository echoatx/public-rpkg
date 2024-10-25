#' Use a Specific Color from a Predefined List for Data Visualization
#'
#' Returns the hex value of the color name that is supplied as an argument or
#' returns the list of colors that may be used if the supplied argument is
#' `"ls"`.
#'
#' @param specific_color the color you want to use or `"ls"` if you need the
#'   list of colors.
#'
#' @return a hex color value or the list of available colors.
#' @export
use_color <- function(specific_color)
{
    output <- NULL
    
    if(specific_color %in% my_palette$Color)
    {
        output = "hex"
    }
    else if(specific_color == "ls")
    {
        output = "list"
    }
    else
    {
        stop("ERROR: The input for this function must be a valid color label. For a list of valid color labels please use this function with \"ls\" as the argument.")
    }
    
    result <- switch(output,
                     "hex" = my_palette$Hex[which(my_palette$Color == specific_color)],
                     "list" = my_palette$Color)
    
    return(result)
}
