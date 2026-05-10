
##### Format norm definition
get_norm <- function(norm,normScheme = GCDKitNormScheme){
  #' Interpret the normalization definition
  #'
  #' @description
    #' If the main argument, norm, is already a named vector, use it without further
    #' action. Otherwise if it is a char string, try to match it with one of the
    #' named normalizations from the normScheme database (normally, imported from
    #' GCDkit's spider.data)
    #'
  #' @param norm Named vector or char. Either normalization values, as a named vector; or
  #' the name of a normalization to look for.
  #' @param normScheme A tibble containing normalization names in the first column, and
  #' named vectors in the second.
  #' @returns A named vector with the normalization factors, in order
  #' @export

  if(is.character(norm)){
    whichNorm <- grep(norm,names(GCDKitNormScheme))

    if(length(whichNorm) == 0){
      cat("No matching normalization scheme!\n")
      return(NULL)
    }

    if(length(whichNorm) > 1){
      cat("Ambiguous normalization scheme!\n")
      print(names(GCDKitNormScheme)[whichNorm])
      return(NULL)
    }

    if(length(whichNorm) == 1){
      print(names(GCDKitNormScheme)[whichNorm])
      return(normScheme[[whichNorm]] )
    }
  }

    if(!is.null(dim(norm))){ # A df or a matrix
      norm <- data.frame(norm)
      ee <- as.numeric(norm[1,])
      names(ee) <- colnames(norm)
      return(ee)
    }

  # else
    return(norm)

}


##### Prepare data
# This function should be able to find out if the data is already formatted !
# The easiest is probably to give it an attr ...
spider_data <- function(df,norm){
  #' Prepare data to plot a spidergram
  #'
  #' @description
    #' The data used in a spiderplot needs to be (i) arranged in the prescribed order
    #' and (ii) normalized to the reference values. In addition, (iii) since we are
    #' not using a proper coorinate systems (X has no meaning), we are using a so-called
    #' "parallel coordinates" plot and as such we must reformat the data to be used in this
    #' way. The resulting data will not be usable for general purposes (if only because we create
    #' one line per element/sample combination!)
    #'
  #' @param df a data.frame that will be formatted for spidergrams
  #' @param norm A normalization scheme, either as a named vector or the name of a scheme existing
  #' in the database. It will be processed by get_norm().
  #'
  #' @returns a tibble in long form, with one line for each sample/element combination. All
  #' the other variables are retained, to allow further aes() mappings.
  #' @export
  #' @import dplyr

  # Check of the data is already in spider-ready form
  if(has_attribute(df,"normScheme")){
    return(df)
  }

  # Coerce df to a tibble
  if(is.null(dim(df))){ # df is a vector
    df <- bind_rows(df)
  }else{ # df is either a matrix or a df (or tbl)
    df <- as_tibble(df)
  }

  # Get norm scheme from database if needed
  norm <- get_norm(norm)

  # Convenience
  elt <- names(norm)

  # Add missing columns to df
  df[elt[!(elt %in% colnames(df))]] = NA

  # Normalize
  df %>%
    mutate(across(elt,
                  ~ .x / norm[cur_column()]
    )
    ) %>%
    # Add row identifier
    rowid_to_column(var = ".row") %>%
    # To long format
    pivot_longer(cols = all_of(elt),
                      names_to = "Element", values_to = "Normalized") %>%
    {.} -> df_out

  attr(df_out,"normScheme") <- "normalized"

  return(df_out)

}



#### Plot data
ggspiderplot <- function(df, norm,
                         .norm =  get_norm(norm), .df = spider_data(df,norm),
                         ... ){
  #' A function to build the skeleton of a spidergram
  #'
  #' @description
    #' A spidergram is a special type of diagram, using "parallel coordinates". This
    #' is frowned upon in data sciences but commonly used in geochemistry, for better or
    #' for worse!
    #' From a ggplot perspective, the core definition of the spidergram includes
    #' (i) mapping of the X-axis to element names; (ii) mapping of the Y-axis to elements
    #' normalized values; (iii) using a log scale for Y (although this can be overridden latter,
    #' if needed). All of the other mappings (colors, symbols etc) are let to the geometries.
    #'
    #' Plotting such a diagram requires specially prepared data, which is normally done using
    #' function spider_data().
    #'
    #' @param df Data frame containing compositional data to plot
    #' @param norm named vector or string containing either the the normalization
    #' values, or the name of a scheme. Normally processed by get_norm
    #' @param .df Data frame already formatted for spidergrams. Normally not used,
    #' only supplied if the user needs to override the call to spider_data()
    #' @param .norm Named vector with normalization info, if the user needs
    #' to bypass the call to get_norm)
    #'
    #' @returns a ggplot object, to which geometries must be added.
    #' @export
    #' @import ggplot2

  ggplot(.df, aes(x=Element, y=Normalized, group = .row))+
    scale_x_discrete(limits=names(.norm))+
    scale_y_log10()
}


GeomLineContinuous <- ggproto("GeomLineContinuous", GeomLine,
## TODO ## Make it accept "normal" data without explicit transf using spider_data()
                     # Specify the required aesthetics
                     required_aes = c("x", "y"),

                     # Transform the data before any drawing takes place
                     setup_data = function(data, params) {
                       data[!is.na(data$y),]
                     }
)

geom_line_continuous <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       ..., na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE) {
  #' Lines that connect accross missing values in spidergrams
  #' @description
    #' This is an exact equivalent to geom_line(), with the exception
    #' that the line is connected across missing values.
    #'
  #' @seealso [geom_line()]
  #' @import ggplot2
  #' @export

  layer(
    data = data,
    mapping = mapping,
    geom = GeomLineContinuous,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

GeomRange <- ggproto("GeomRange", GeomRibbon,

                              # Specify the required aesthetics
                              required_aes = c("x", "y"),

                              # Transform the data before any drawing takes place
                              setup_data = function(data, params) {
                               # browser()

                                .fmax <- function(x){
                                  if(any(!is.na(x))){return(max(x,na.rm=T))}else{return(NA)}
                                }
                                .fmin <- function(x){
                                  if(any(!is.na(x))){return(min(x,na.rm=T))}else{return(NA)}
                                }

                                data %>% group_by(x,PANEL) %>%
                                  summarize(ymax = .fmax(y),
                                            ymin = .fmin(y) ) %>%
                                  filter(!is.na(ymin)&!is.na(ymax)) %>%
                                  mutate(group = 1)

                              }
)

geom_range <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       ..., na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE) {
  #' A field encompassing all the values
  #' @description
  #' This generates a polygon that includes all the values, for each element
  #'
  #' @import ggplot2
  #' @export

  layer(
    data = data,
    mapping = mapping,
    geom = GeomRange,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


