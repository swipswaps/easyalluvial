
context('as_ploty')

test_that('as_plotly_alluvial_wide'
  ,{
    
    p = alluvial_wide(mtcars2, max_variables = 5)
    
    alluvial_as_plotly(p, marginal_histograms = F)
    
    expect_error( alluvial_as_plotly(p, marginal_histograms = T) )
    
    alluvial_as_plotly(p, marginal_histograms = T, data_input = mtcars2)
    
    df = select(mtcars2, mpg, disp, wt, am, qsec)
    
    p = alluvial_wide(df)
    
    alluvial_as_plotly(p, marginal_histograms = T, data_input = df)

})


test_that('as_plotly_alluvial_wide_num_only'
          ,{
            
    df = mtcars2 %>%
      select_if(is.numeric)
            
    p = alluvial_wide(df, max_variables = 5)
    
    alluvial_as_plotly(p, data_input = df)
    
})

test_that('parcats_params',{
  
  p = alluvial_wide(mtcars2, max_variables = 5)
  
  # hoveron
  alluvial_as_plotly(p, hoveron = 'category', marginal_histograms = T, data_input = mtcars2)
  alluvial_as_plotly(p, hoveron = 'color', marginal_histograms = T, data_input = mtcars2)
  alluvial_as_plotly(p, hoveron = 'dimension', marginal_histograms = T, data_input = mtcars2)
  
  #hoverinfo
  alluvial_as_plotly(p, hoverinfo = 'count', marginal_histograms = F)
  alluvial_as_plotly(p, hoverinfo = 'probability', marginal_histograms = F)
  alluvial_as_plotly(p, hoveron = 'count+probability', marginal_histograms = F)
  
  # hovertemplate probably needs some kind of format string, too complicated will drop this
  # alluvial_as_plotly(p, hovertemplate = 'count', marginal_histograms = F)
  # alluvial_as_plotly(p, hovertemplate = 'probability', marginal_histograms = F)
  # alluvial_as_plotly(p, hovertemplate = 'category', marginal_histograms = F)
  # alluvial_as_plotly(p, hovertemplate = 'categorycount', marginal_histograms = F)
  # alluvial_as_plotly(p, hovertemplate = 'colorcount', marginal_histograms = F)
  # alluvial_as_plotly(p, hovertemplate = 'bandcolorcount', marginal_histograms = F)
  
  # arrangement
  alluvial_as_plotly(p, arrangement = 'fixed', marginal_histograms = F)
  alluvial_as_plotly(p, arrangement = 'perpendicular', marginal_histograms = F)
  alluvial_as_plotly(p, arrangement = 'freeform', marginal_histograms = F)
  
  #bundlecolors
  alluvial_as_plotly(p, bundlecolors = T, marginal_histograms = F)
  alluvial_as_plotly(p, bundlecolors = F, marginal_histograms = F)
  
  #sortpaths
  alluvial_as_plotly(p, sortpaths = 'forward', marginal_histograms = F)
  alluvial_as_plotly(p, sortpaths = 'backward', marginal_histograms = F)
  
  #labelfont
  alluvial_as_plotly(p, labelfont = list(size = 18, color = 'blue'), marginal_histograms = F)
  
  #tickfont
  alluvial_as_plotly(p, tickfont = list(size = 18, color = 'blue'), marginal_histograms = F)
  
})

test_that('as_plotly_alluvial_long'
          ,{
            
    p = alluvial_long(quarterly_flights, key = qu, value = mean_arr_delay, id = tailnum)
    
    alluvial_as_plotly(p, marginal_histograms = F)
    
    p = alluvial_long(quarterly_flights, key = qu, value = mean_arr_delay, id = tailnum, fill = carrier)
    
    alluvial_as_plotly(p, marginal_histograms = F)
    
    p = alluvial_long(quarterly_sunspots, key = qu, value = spots, id = year)
    
    alluvial_as_plotly(p, marginal_histograms = F)
    
})

test_that('as_plotly_alluvial_model_response'
          ,{
            
    df = mtcars2[, ! names(mtcars2) %in% 'ids' ]
    m = randomForest::randomForest( disp ~ ., df)
    imp = m$importance
    dspace = get_data_space(df, imp, degree = 3)
    pred = predict(m, newdata = dspace)
    p = alluvial_model_response(pred, dspace, imp, degree = 3)
    
    alluvial_as_plotly(p, marginal_histograms = F, imp = F)
    
    alluvial_as_plotly(p, marginal_histograms = T, imp = F, data_input = df)
    
    alluvial_as_plotly(p, marginal_histograms = T, imp = T, data_input = df)
    
    # grid = add_marginal_histograms(p, df)
    
    # pdb
    pred = get_pdp_predictions(df, imp, m, degree = 3)
    p = alluvial_model_response(pred, dspace, imp, degree = 3, method = 'pdp')
    alluvial_as_plotly(p, marginal_histograms = T, imp = T, data_input = df)
    
    # categorical response ---------------------------
    
    df = titanic %>%
      select_if(is.factor)
    
    set.seed(0)
    m = randomForest::randomForest( Survived ~ ., df)
    imp = m$importance
    
    expect_warning( {dspace = get_data_space(df, imp, degree = 3, max_levels = 5)} )
    
    expect_true( nrow(dspace) == 30 )
    
    pred = predict(m, newdata = dspace,type = 'response')
    p = alluvial_model_response(pred, dspace, imp, degree = 3)
    
    alluvial_as_plotly(p, marginal_histograms = F, imp = F)
    
    suppressWarnings({
      # warning is raised because number of factors is cut to 5
      alluvial_as_plotly(p, marginal_histograms = T, imp = F, data_input = df)
      
      alluvial_as_plotly(p, marginal_histograms = T, imp = T, data_input = df)
    })
    
    # grid = add_marginal_histograms(p, df)
    
})