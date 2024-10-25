applyHmisClass <- function(x)
{
  requireNamespace("lubridate", quietly = TRUE)
  
  if ("Interval" %in% class(x))
  {
    return(x)
  }
  else if ("Date" %in% class(x))
  {
    return(x)
  }
  else if ("numeric" %in% class(x) & !"Date" %in% class(x))
  {
    class(x) <- c("HMIS Data Standards Fiscal Year", class(x))
    
    return(x)
  }
  else
  {
    class(x) <- c("HMIS Data File", class(x))
    
    return(x)
  }
}
