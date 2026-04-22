
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
    whichNorm <- grep(norm,normScheme$name)

    if(length(whichNorm) == 0){
      cat("No matching normalization scheme!\n")
      return(NULL)
    }

    if(length(whichNorm) > 1){
      cat("Ambiguous normalization scheme!\n")
      print(normScheme[whichNorm,]$name)
      return(NULL)
    }

    if(length(whichNorm) == 1){
      print(normScheme[whichNorm,]$name)
      return(normScheme[whichNorm,]$scheme[[1]] )
    }

  }else{
    return(norm)
  }

}


##### Prepare data
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
  #' @norm A normalization scheme, either as a named vector or the name of a scheme existing
  #' in the database. It will be processed by get_norm().
  #'
  #' @returns a tibble in long form, with one line for each sample/element combination. All
  #' the other variables are retained, to allow further aes() mappings.
  #' @export


  # Coerce df to a data frame if matrix or vector ####TODO
  # df <- tibble(df)

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
    )

  # Add row identifier
  df$.row <- rownames(df)

  # To long format
  df %>% pivot_longer(cols = all_of(elt),
                      names_to = "Element", values_to = "Normalized")

  return(df)
}



#### Plot data
ggspiderplot <- function(df, norm,
                         .norm =  get_norm(norm), .df = spider_data(df,norm),
                         interpolate_missing = F,... ){
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
    #' @param interpolate_missing if true, a line is drawn through missing values
    #'
    #' @returns a ggplot object, to which geometries must be added.


  if(interpolate_missing){
    .df <- .df[!is.na(df2$Normalized),]
  }

  ggplot(.df, aes(x=Element, y=Normalized, group = .row))+
    scale_x_discrete(limits=names(.norm))+
    scale_y_log10()
}
