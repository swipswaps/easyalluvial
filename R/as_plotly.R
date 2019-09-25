
trace_hist_all = function(p, data_input){
  
  vars = p$data$x %>% levels()
  
  traces = list()
  
  for (var in vars) {
    
    is_pred = var == 'pred'
    
    if(! is_pred){
      is_num = is.numeric( data_input[[var]] )
    }else{
      var_pred = names(data_input)[! names(data_input) %in% names(p$alluvial_params$dspace) ]
      if(length(var_pred) > 1){
        stop( paste('\n"data_input" should only contain explanatory and response variables, so response variable can be inferred.
                  \nPotential response variables:', paste( var_pred, collapse = ', ')
                    , '\nPlease pass correct response variable as string using the "pred_var" parameter.' ) )
      }
      is_num = is.numeric( data_input[[var_pred]] )
    }
    
    if( is_num ){
      trace = trace_hist_num(p, data_input, var = var)
    }else{
      trace = trace_hist_cat(p, data_input, var = var)
    }
    
    traces = append(traces, trace)
  }
  
  return(traces)
}

trace_rug_all = function(p, data_input){
  vars = p$data$x %>% levels()
  
  traces = list()
  
  for (var in vars) {
    
    trace = trace_rug(p, data_input, var)

    traces = append(traces, trace)
  }
  
  return(traces)
}

trace_rug = function(p, data_input, var){
  
  vars = p$data$x %>% levels
  
  is_num = is.numeric(data_input[[var]])
  
  if(is_num){
    x = data_input[[var]]
    y = rep('rug', length(data_input[[var]]) )
    type = 'scatter'
  }else{
    x = c()
    y = c()
    type = 'bar'
  }
  
  trace = list( type = type
        , mode = 'markers'
        , x = x
        , y = y
        , marker = list(symbol = 'line-ns-open', color = 'black')
        , xaxis = paste0( 'x', which(vars == var) )
        , yaxis = paste0( 'y1', which(vars == var) ) 
        , name = paste0('rug_', var)
        , showlegend = FALSE
  )
  
  trace = list(trace)
  names(trace) <- paste0('rug_', var)
  
  return( trace )
}

trace_hist_mod = function(p, data_input, var){
  
  p_hist = plot_hist(var = var, p = p, data_input = data_input)
  
  df = p_hist$data
  
  vars = p$data$x %>% levels
  
  values = data_input[[var]]

  dens = density(values)
  x_var = dens$x
  y_var = dens$y
    
  trace_var = list( x = x_var
                    , y = y_var
                    , type = 'scatter'
                    , line = list( color = 'grey' )
                    , marker = list( size = 0
                                     , color = 'grey'
                                     , opacity = 0)
                    , showlegend = FALSE
                    , name = paste0( var, '_dens')
                    , xaxis = paste0( 'x', which(vars == var) )
                    , yaxis = paste0( 'y', which(vars == var) ) 

                    )
  
  trace_var = list(trace_var)
  names(trace_var) <- paste0( var, '_dens')
  
  lines_at = p$alluvial_params$dspace[[var]] %>%
    unique() %>%
    sort()
  
  # we have to rejoin with fill labels
  # labels need to be consistent {var}_{fill_label} so that we 
  # can track the traces
  
  fill_labels = p$data %>%
    filter( x == var) %>%
    select( value, fill_value) %>%
    distinct() %>%
    arrange( desc(value) ) %>%
    mutate( line_at = lines_at) %>%
    rename( label = value
            , color = fill_value )
  

  trace_vlines = fill_labels %>%
    mutate( trace = pmap(list(label, color, line_at)
                         , function(l, c, lin) list( x0 = lin
                                       , y0 = 0
                                       , x1 = lin
                                       , y1 = max(y_var)
                                       , type = 'line'
                                       , line = list( color = 'lightgrey')
                                       , showlegend = F
                                       , name = paste0( var,'_' ,l )
                                       , xref = paste0( 'x', which(vars == var) )
                                       , yref = paste0( 'y', which(vars == var) ) ) )
      ) %>%
      .$trace
    

  names(trace_vlines) <- paste0( var,'_', fill_labels$label )
  
  traces = append(trace_var, trace_vlines)
  
}

trace_hist_num = function(p, data_input, var){
  
  
  p_hist = plot_hist(var = var, p = p, data_input = data_input)
  
  df = p_hist$data
  
  vars = p$data$x %>% levels
  
  if(var == 'pred'){
    
    # we have to rejoin with fill labels
    # labels need to be consistent {var}_{fill_label} so that we 
    # can track the traces
    
    fill_labels = p$data %>%
      filter( x == 'pred') %>%
      select(fill, value, fill_value) %>%
      mutate( rwn = as.numeric(fill) ) %>%
      distinct() %>%
      arrange( desc(rwn) ) %>%
      mutate( rwn = row_number() )
    
    
    df = df %>%
      filter(variable == 'pred') %>%
      left_join( fill_labels, by = 'rwn')
  }
  
  if( p$alluvial_type == 'model_response' & var != 'pred'){
    return( trace_hist_mod(p, data_input, var) )
  }
  
  df = df %>%
    group_by(fill) %>%
    nest() %>%
    mutate(trace = map2(data, fill, function(x,y) list( x = x$x
                                     , y = x$y
                                     , fillcolor = x$fill_value[1]
                                     , fill = 'tozeroy'
                                     , type = 'scatter'
                                     , line = list( color = x$fill_value[1] )
                                     , marker = list( size = 0
                                                      , color = x$fill_value[1]
                                                      , opacity = 0)
                                     , showlegend = FALSE
                                     #, text = y
                                     #, hoveron = 'points'
                                     , name = y
                                     , xaxis = paste0( 'x', which(vars == var) )
                                     , yaxis = paste0( 'y', which(vars == var) ) ) ) )
  traces = df$trace
  
  names(traces) <- paste0( var, '_', df$fill)
  
  return(traces)
}

trace_hist_cat = function(p, data_input, var){
  
  p_hist = plot_hist(var = var, p = p, data_input = data_input)
  
  df_label = p$data %>%
    filter( x == var ) %>%
    mutate( value = fct_drop(value) ) %>%
    select( value ) %>%
    distinct() %>%
    arrange( desc(value) ) %>%
    mutate( rwn = row_number() )
  
  if( var %in% names(p_hist$data) ){
    # is the case for model response 
    df = p_hist$data %>%
      mutate( rwn = as.integer( !! as.name(var) ) ) %>%
      left_join( df_label, by = 'rwn') %>%
      rename( var_key = !! as.name(var) )
  }else{
    df = p_hist$data %>%
      mutate( var_key = as.factor(var_key)
              , rwn = as.integer( var_key ) ) %>%
      left_join( df_label, by = 'rwn')
  }
  
  lvl = levels(df[[var]])
  
  vars = p$data$x %>% levels
  
  df = df %>%
    mutate( var_key = fct_relevel(var_key, lvl) ) %>%
    group_by(var_key, fill_value, value) %>%
    count() %>%
    arrange(var_key) %>%
    mutate(trace = list( list( x = list(var_key)
                        , y = list(n)
                        , type = 'bar'
                        , marker = list(color = as.character(fill_value) )
                        , showlegend = FALSE
                        # , width = 0.5
                        , name = paste0(var, '_', value)
                        , xaxis = paste0( 'x', which(vars == var) )
                        , yaxis = paste0( 'y', which(vars == var) ) ) ) )
  traces = df$trace
  
  names(traces) <- paste0( var, '_', df$value)
  
  return(traces)
}


trace_parcats = function(p
                         , domain
                         , hoveron
                         , hoverinfo
                         , labelfont){
  
  if(p$alluvial_type == 'model_response'){
    df = p$data %>%
      arrange( desc(value) ) %>%
      mutate(value_str = as.character(value)
              , value_str = ifelse( x != 'pred', str_split(value_str, '\\\n'), value_str)
              , value_str = map_chr(value_str, 1)
              , value_str = as_factor(value_str)
              , value_str = fct_rev(value_str)
              , value = value_str) %>%
      select( - value_str)
  }else{
    df = p$data
  }
  
  df = df %>%
    select( - fill_value, - fill ) %>%
    spread(x, value) %>%
    arrange(alluvial_id)
  
  if( nrow(df) != ( df$alluvial_id %>% as.numeric %>% max() ) ){
    stop('data assumption violated')
  }
  
  dim_names = df %>%
    select(-n, - alluvial_id, - fill_flow) %>%
    names()
  
  for(name in dim_names){
    
    order_array = df[[name]] %>%
      fct_drop() %>%
      levels()
    
    l = list(label = name
             , values = df[[name]]
             , categoryorder = 'array'
             , categoryarray = order_array )
    
    if(name == dim_names[1]){
      dimensions = list( l )
    }else{
      dimensions[[length(dimensions) + 1]] = l
    }
  }
  
  parcats = list(
    type = 'parcats'
    , dimensions = dimensions
    , counts = df$n
    , line = list( shape = 'hspline'
                   , color = df$fill_flow)
    , hoveron = hoveron
    , hoverinfo = hoverinfo
    , labelfont = labelfont
    # , arrangement = 'freeform'
    , domain = domain )

}

create_layout_rug = function(trace_rugs, layout_hist, offset = 0.01){
  
  
  layout = layout_hist[ which( startsWith(names(layout_hist), 'yaxis' ) )]
  
  for( i in seq(1, length(layout) ) ){
    domain_old = layout[[i]]$domain
    domain_new = c(domain_old[1] - offset, domain_old[1] - 0.001 )
    layout[[i]]$domain <- domain_new
    layout[[i]]$anchor <- paste0('x', i)
    names(layout)
    
  }
  
  names(layout) <- paste0('yaxis1', seq(1, length(layout) ) )
  
  return(layout)
}

create_layout_hist = function(trace_hist, lim_up = 0.9, lim_right = 1, space = 0.025){
  
  yaxis = map(trace_hist, 'yaxis') %>% unique() %>% unlist %>% sort()
  xaxis = map(trace_hist, 'xaxis') %>% unique() %>% unlist %>% sort()
  
  coord_x = seq(0,1, lim_right / length(yaxis) )
  
  spaces = map(coord_x, ~ c(.-space/2, .+space/2) ) %>% unlist() 
  
  coord_x = c(0, spaces[3:(length(spaces)-2) ], 1)
  
  layout = list()
  
  for( i in seq(1, length(xaxis), 1) ){
    
    
    layout_x = list( domain = coord_x[(i*2-1):(i*2)]
                     #, anchor = yaxis[i]
                     , anchor = paste0('y1', i)
                     , showgrid = F
                     , showline = F
                     , zeroline = F )
    
    layout_x = list(layout_x)
    
    num = xaxis[i] %>% str_extract('[0-9]+')
    names(layout_x) <- paste0('xaxis', num)
    
    layout = append(layout, layout_x )
  }
  
  for( i in seq(1, length(yaxis), 1) ){
    layout_y = list( domain = c(lim_up, 1)
                     #, anchor = xaxis[i]
                     #, anchor = paste0('xaxis1', i)
                     , showgrid = F
                     , showline = F
                     , showticklabels = F
                     , zeroline = F )

    layout_y = list(layout_y)
    
    num = yaxis[i] %>% str_extract('[0-9]+')
    names(layout_y) <- paste0('yaxis', num)
    
    layout = append(layout, layout_y )
    
    
  }
  
  return(layout)
}

map_trace = function(p, trace_hist){
  
  df = p$data %>%
    mutate( x_value = map2_chr(x, value, function(x,y) paste0(x,'_' ,y) )
            , trace_number = map(x_value, ~ which(names(trace_hist) == . ) )
            , trace_number = map_int(trace_number, ~ ifelse( is_empty(.), NA, .)  ) ) %>%
    filter( ! is.na(trace_number) ) %>%
    mutate( type = map_chr(trace_number, ~ trace_hist[[.]][['type']] ) 
           , color_marker = map(trace_number, ~ trace_hist[[.]][['marker']][['color']] )
           , color_line = map(trace_number, ~ trace_hist[[.]][['line']][['color']] )
           , color = ifelse( type == 'line', color_line, color_marker)
           , color = map_chr(color, ~ . )
           ) %>%
    select(alluvial_id, trace_number, type, color) %>%
    group_by(alluvial_id) %>%
    nest() %>%
    ungroup() %>%
    mutate(trace_number = map(data, 'trace_number')
           , type = map(data, 'type')
           , color = map(data, 'color')
           , alluvial_id = as.character(alluvial_id) )
  
  map_curve = df$trace_number
  names(map_curve) <- df$alluvial_id
  
  map_type = df$type
  names(map_type) <- df$alluvial_id
  
  map_color = df$color
  names(map_color) <- df$alluvial_id
  
  return( list( map_curve = map_curve
                , map_type = map_type
                , map_color = map_color) )
  
}

get_shapes = function(traces){
  
  types = map_chr(traces, ~ .$type) %>%
    unname()
  
  lines_index = which(types == 'line')
  
  shapes =  traces[lines_index] 
  
  map_trace_2_shape = tibble( trace_index = seq(1, length(traces) ) ) %>%
    mutate( shape_index = map( trace_index, ~ which(lines_index == .) ) 
            , shape_index = map_int( shape_index, ~ ifelse( is_empty(.), as.integer(0), .) )  
            )
  
  ls_shapes = list( map_trace_2_shape = map_trace_2_shape$shape_index
                    , shapes = shapes)
  
  return(ls_shapes)
}

#' <Add Title>
#'
#' <Add Description>
#'
#' @import htmlwidgets
#'
#' @export
alluvial_as_plotly <- function(p, marginal_histograms = T, data_input = NULL
                               , width = NULL, height = NULL, elementId = NULL
                               , hoveron = 'color'
                               , hoverinfo = 'count+probability'
                               , labelfont = list( size = 36 )) {

  if(marginal_histograms & is.null(data_input) ){
    stop('data_input required if marginal_histograms == TRUE')
  }
  
  if(marginal_histograms){
    domain = list(y = c(0, 0.7))
  }else{
    domain = list( y = c(0, 1) )
  }
  
  parcats = trace_parcats(p, domain = domain
                          , hoveron = hoveron
                          , hoverinfo = hoverinfo
                          , labelfont = list( size = 36) )
  
  if(marginal_histograms){
    trace_hist = trace_hist_all(p, data_input)
    trace_rugs = trace_rug_all(p, data_input)
    traces = append( list(parcats = parcats), trace_hist) 
    traces = append( traces, trace_rugs )
    layout_hist = create_layout_hist(trace_hist)
    layout_rug = create_layout_rug(trace_rugs, layout_hist)
    layout = append(layout_hist, layout_rug)
    ls_shapes = get_shapes(trace_hist)
    shapes = ls_shapes$shapes
    map_trace_2_shape = ls_shapes$map_trace_2_shape
    ls_map = map_trace(p, trace_hist)
    map_curve = ls_map$map_curve
    map_type = ls_map$map_type
    map_color = ls_map$map_color
    
  }else{
    trace_hist = list()
    traces = list(parcats = parcats)
    layout = list()
    shapes = list()
    map_trace_2_shape = list()
    map_curve = list()
    map_type = list()
    map_color = list()
    
  }
  
  x = list( traces = traces
            , layout = layout
            , shapes = shapes
            # JS does not make copies of variables thus we pass one to
            # change and one to keep. 
            , shapes_original = shapes
            , map_curve = map_curve %>% unname()
            , map_trace_2_shape = map_trace_2_shape
            , map_type = map_type %>% unname()
            , map_color = map_color %>% unname()
            , parcats_cols = parcats$line$color
            )
  
  # create widget
  htmlwidgets::createWidget(
    name = 'alluvial_as_plotly',
    x,
    width = width,
    height = height,
    package = 'easyalluvial',
    elementId = elementId
  )
}

#' Shiny bindings for alluvial_as_plotly
#'
#' Output and render functions for using alluvial_as_plotly within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a alluvial_as_plotly
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name alluvial_as_plotly-shiny
#'
#' @export
alluvial_as_plotlyOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'alluvial_as_plotly', width, height, package = 'easyalluvial')
}

#' @rdname alluvial_as_plotly-shiny
#' @export
render_alluvial_as_plotly <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, alluvial_as_plotlyOutput, env, quoted = TRUE)
}
