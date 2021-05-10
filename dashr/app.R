if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(dash)
pacman::p_load(dashCoreComponents)
pacman::p_load(dashHtmlComponents)
pacman::p_load_gh("plotly/dashDaq")
pacman::p_load(dashBio)
pacman::p_load(manhattanly)
pacman::p_load(anytime)
pacman::p_load(dashTable)
pacman::p_load(jsonlite)

data("HapMap")
data("significantSNP")
# volcanoly(HapMap, snp = "SNP", gene = "GENE")
# manhattanly(HapMap, snp = "SNP", gene = "GENE", genomewideline = 6, suggestiveline = 2)
dataset <- HapMap

parse_contents = function(contents, filename, date){
  content_type = strsplit(contents, ",")
  content_string = strsplit(contents, ",")
  decoded = jsonlite::base64_dec(content_string)
  
  if('csv' %in% filename){
    df = read.csv(utf8::as_utf8(decoded))
  } else if('xls' %in% filename){
    df = read.table(decoded, encoding = 'bytes')
  } else{
    return(htmlDiv(list(
      'There was an error processing this file.'
    )))
  }
  
  return(htmlDiv(list(
    htmlH5(filename),
    htmlH6(anytime(date)),
    # dashDataTable(df_to_list('records'),columns = lapply(colnames(df), function(x){list('name' = x, 'id' = x)})),
    htmlHr(),
    htmlDiv('Raw Content'),
    htmlPre(paste(substr(toJSON(contents), 1, 100), "..."), style=list(
      'whiteSpace'= 'pre-wrap',
      'wordBreak'= 'break-all'
    ))
  )))
}

app <- Dash$new()

####################################################################################################

#################################### CREATE LAYOUT #################################################

app$layout(
  htmlDiv(
    list(
      # create and style title
      htmlDiv(
        list(
          htmlH2(
            id = "title",
            'Interactive GWAS Viewer',
            style = list(
              textAlign = 'left'
            )
          )#,
          # htmlH5(
          #   'Statistical & functional analysis of metabolomics data',
          #   style = list(
          #     textAlign = 'left'
          #   )
          # )
        ),
        className = "banner"
      ),
      
      
      
      # create top container and graph child, set style
      htmlDiv(
        list(
          htmlDiv(
            id = "top-graphs",
            style = list(
              display = "flex",
              borderRadius = '5px',
              justifyContent = "space-evenly",
              width = "100%",
              # height = "50px",
              margin = "0 auto"
            ),
            children = list(
              # create tabs
              htmlDiv(
                id = "control-tabs",
                className = "control-tabs",
                children = list(
                  dccTabs(
                    id="tabs",
                    # style = list(colors = list(primary = "white")),
                    value = "what-is",
                    children = list(
                      dccTab(
                        label = "About",
                        value = "what-is",
                        children = htmlDiv(
                          className = "control-tab",
                          children = list(
                            htmlH4(
                              className = "what-is",
                              children = "manhattanly package"
                            ),
                            dccMarkdown(
                              'Manhattan, Q-Q and volcano plots are popular graphical methods for visualizing 
                              results from high-dimensional data analysis such as a (epi)genome wide asssociation 
                              study (GWAS or EWAS), in which p-values, Z-scores, test statistics are plotted 
                              on a scatter plot against their genomic position. Manhattan plots are used for 
                              visualizing potential regions of interest in the genome that are associated with 
                              a phenotype. Q-Q plots tell us about the distributional assumptions of the observed 
                              test statistics. Volcano plots are the negative log10 p-values plotted against their 
                              effect size, odds ratio or log fold-change. They are used to identify clinically 
                              meaningful markers in genomic experiments, i.e., markers that are statistically 
                              significant and have an effect size greater than some threshold.
                              Interactive manhattan, Q-Q and volcano plots allow the inspection of specific value 
                              (e.g. rs number or gene name) by hovering the mouse over a point, as well as zooming 
                              into a region of the genome (e.g. a chromosome) by dragging a rectangle around the relevant area. 
                              This pacakge creates interactive Q-Q, manhattan and volcano plots that are usable from the R console, 
                              the RStudio viewer pane, R Markdown documents, in Dash apps, Dash apps, Shiny apps, embeddable in 
                              websites and can be exported as .png files. By hovering the mouse over a point, you can see
                              annotation information such as the SNP identifier and GENE name. You can also drag a 
                              rectangle to zoom in on a region of interest and then export the image as a .png file.'
                            )#,
                      #       htmlH4(
                      #         className = "what-is",
                      #         children = "What is Volcano Plot?"
                      #       ),
                      #       dccMarkdown(
                      #         'A scatterplot that shows statistical significance (P value) versus magnitude of change (fold change) in this case
                      # of volatile organic compounds (VOCs).'
                      #       )
                          )
                        )
                      ),
                      
                      dccTab(
                        label = "Parameters",
                        value = "parameters",
                        children = htmlDiv(
                          className = "control-tab",
                          children = list(
                            
                            # htmlDiv(
                            #   className = "app-controls-block",
                            #   children = list(
                            #     htmlDiv(
                            #       className = "app-controls-name",
                            #       children = "Select Date"
                            #     ),
                            #     dccDropdown(
                            #       id = "d_date-box",
                            #       className = "dropdowns",
                            #       style = list(marginRight = "10px",
                            #                    width = "100%"),
                            #       # style = list(width = "45%"),
                            #       options = lapply(list("20190723", "20190905"),
                            #                        function(x){
                            #                          list(label = x, value = x)
                            #                        }
                            #       ),
                            #       value = c('20190723','20190905'),
                            #       multi = TRUE
                            #     )
                            #   )
                            # ),
                            
                            # htmlDiv(
                            #   className = "app-controls-block",
                            #   children = list(
                            #     htmlDiv(
                            #       className = "app-controls-name",
                            #       children = "Select Treatment"
                            #     ),
                            #     dccDropdown(
                            #       id = "d_treatment",
                            #       className = "dropdowns",
                            #       style = list(marginRight = "10px", width = "100%"),
                            #       # style = list(width = "45%"),
                            #       options = lapply(list("Control", "NI"),
                            #                        function(x){
                            #                          list(label = x, value = x)
                            #                        }
                            #       ),
                            #       value = c('Control', 'NI'),
                            #       multi = TRUE
                            #     )
                            #   )
                            # ),
                            
                            
                           # Manhattan Inputs ----------------------------------------------------------
                            
                            htmlDiv(
                              className = "app-controls-block",
                              children = list(
                                htmlDiv(
                                  className = "app-settings-name",
                                  children = "Settings for Manhattan Plot:"
                                ))),
                            
                            htmlDiv(
                              className = "app-controls-block",
                              children = list(
                                htmlDiv(
                                  className = "app-controls-name",
                                  children = "Suggestive and Genome-wide line"
                                ),
                                dccRangeSlider(
                                  id = 'manhattanplot-input',
                                  min = 0,
                                  max = 10,
                                  step = 0.1,
                                  marks = setNames(
                                    lapply(0:10,
                                           function(i){
                                             list(label = as.character(i))
                                           }),
                                    0:10
                                  ),
                                  value = c(5, 8)
                                )
                              )
                            ),
                            
                           # Volcano Inputs ----------------------------------------------------------
                           
                           htmlDiv(
                             className = "app-controls-block",
                             children = list(
                               htmlDiv(
                                 className = "app-settings-name",
                                 children = "Settings for Volcano Plot:"
                               ))),
                           
                           htmlDiv(
                             className = "app-controls-block",
                             children = list(
                               htmlDiv(
                                 className = "app-controls-name",
                                 children = "Effect size threshold"
                               ),
                               dccRangeSlider(
                                 id = 'volcanoplot-input',
                                 min = -3,
                                 max = 3,
                                 step = 0.1,
                                 marks = setNames(
                                   lapply(-3:3,
                                          function(i){
                                            list(label = as.character(i))
                                          }),
                                   -3:3
                                 ),
                                 value = c(-0.5, 1)
                               )
                             )
                           ),
                           
                           htmlDiv(
                             className = "app-controls-block",
                             children = list(
                               htmlDiv(
                                 className = "app-controls-name",
                                 children = "-log10(p-value) threshold"
                               ),
                               dccSlider(
                                 id = "vp-genomic-line-val",
                                 value = 2,
                                 marks = setNames(
                                   lapply(0:10,
                                          function(i){
                                            list(label = as.character(i))
                                          }),
                                   0:10
                                 ),
                                 max = 10,
                                 min = 0,
                                 step = 0.1
                               )
                             )
                           ),
                           

                          # File upload -------------------------------------------------------------
                          
                          htmlDiv(
                            list(
                              dccUpload(
                                id='upload-data',
                                children=htmlDiv(list(
                                  'Drag and Drop or ',
                                  htmlA('Select Files')
                                )),
                                style=list(
                                  'width'= '100%',
                                  'height'= '60px',
                                  'lineHeight'= '60px',
                                  'borderWidth'= '1px',
                                  'borderStyle'= 'dashed',
                                  'borderRadius'= '5px',
                                  'textAlign'= 'center',
                                  'margin'= '10px'
                                ),
                                # Allow multiple files to be uploaded
                                multiple=TRUE
                              ),
                              htmlDiv(id='output-data-upload')
                            )
                          )

                           
                            
                            
                            # htmlDiv(
                            #   className = "app-controls-block",
                            #   children = list(
                            #     daqLEDDisplay(
                            #       label = "VOCs down",
                            #       id = "vp-upper-left",
                            #       size = 10,
                            #       color = "#19D3F3"
                            #     )
                            #   )
                            # ),
                            
                            # htmlDiv(
                            #   className = "app-controls-block",
                            #   children = list(
                            #     daqLEDDisplay(
                            #       label = "VOCs Up",
                            #       id = "vp-upper-right",
                            #       size = 10,
                            #       color = "#19D3F3"
                            #     )
                            #   )
                            # )
                          )
                        )
                      )
                    )
                  )
                )
              ),
              
              # htmlDiv(
              #   id = "left-top-graph",
              #   className = "container",
              #   list(
              #     htmlDiv(
              #       style = list(width = "100%"),
              #       list(
              #         dccGraph(
              #           id = "3d-pca",
              #           figure = create3dScatter(pca),
              #           style = list(width = '100%')
              #         )
              #       )
              #     )
              #   ),
              #   # style container top left
              #   style = list(
              #     marginTop = "10px",
              #     marginBottom = "10px",
              #     marginLeft = 0,
              #     marginRight = 0,
              #     paddingTop = "2rem",
              #     paddingBottom = "2rem",
              #     borderRadius = '5px',
              #     width = "38%",
              #     float = "none",
              #     boxSizing = "border-box",
              #     boxShadow = '2px 2px 1px #f2f2f2'
              #   )
              # ),
              
              htmlDiv(
                id = "right-top-graph",
                className = "container",
                list(
                  htmlDiv(
                    style = list(width = "100%"),
                    list(
                      # dccGraph(
                      #   id = "volcano-graph",
                      #   figure = manhattanly::volcanoly(x = dataset, snp = "SNP", gene = "GENE"),
                      #   style = list(width = '100%')
                      # )
                      
                      dccGraph(
                        id = "manhattan-plot",
                        figure = manhattanly(HapMap, snp = "SNP", gene = "GENE",
                                             annotation1 = "DISTANCE", annotation2 = "EFFECTSIZE"),
                        style = list(width = '100%')
                      )
                      
                    )
                  )
                ),
                # style container top right
                style = list(
                  marginTop = "10px",
                  marginBottom = "10px",
                  marginLeft = 0,
                  marginRight = 0,
                  paddingTop = "2rem",
                  paddingBottom = "2rem",
                  borderRadius = '5px',
                  width = "35%",
                  float = "none",
                  boxSizing = "border-box",
                  boxShadow = '2px 2px 1px #f2f2f2'
                )
              )
              
              
            )
          )
        )
      ),
      
      
      # create bottom container and graph child, set style
      htmlDiv(
        list(
          htmlDiv(
            id = "bottom-graphs",
            style = list(
              display = "flex",
              borderRadius = '5px',
              justifyContent = "space-evenly",
              width = "100%",
              margin = "0 auto"
            ),
            children = list(
              htmlDiv(
                id = "left-bottom-graph",
                className = "container",
                list(
                  htmlDiv(
                    style = list(width = "100%"),
                    list(
                      # dccGraph(
                      #   id = "manhattan-plot",
                      #   figure = manhattanly::manhattanly(x = dataset, snp = "SNP", gene = "GENE"),
                      #   style = list(width = '100%')
                      # )
                      
                      dccGraph(
                        id = "volcano-graph",
                        figure = manhattanly::volcanoly(x = dataset, snp = "SNP", gene = "GENE"),
                        style = list(width = '100%')
                      )
                    )
                  )
                ),
                # style container bottom left
                style = list(
                  marginTop = "10px",
                  marginBottom = "10px",
                  marginLeft = 0,
                  marginRight = 0,
                  paddingTop = "3rem",
                  paddingBottom = "2rem",
                  borderRadius = '5px',
                  width = "48%",
                  float = "none",
                  boxSizing = "border-box",
                  boxShadow = '2px 2px 1px #f2f2f2'
                )
              ),
              
              htmlDiv(
                id = "right-bottom-graph",
                className = "container",
                list(
                  htmlDiv(
                    style = list(width = "100%"),
                    list(
                      dccGraph(
                        id = "qq-plot",
                        figure = manhattanly::qqly(x = dataset, snp = "SNP", gene = "GENE"),
                        style = list(width = '100%')
                      )
                    )
                  )
                ),
                # style container top left
                style = list(
                  marginTop = "10px",
                  marginBottom = "10px",
                  marginLeft = 0,
                  marginRight = 0,
                  paddingTop = "3rem",
                  paddingBottom = "2rem",
                  borderRadius = '5px',
                  width = "48%",
                  boxSizing = "border-box",
                  boxShadow = '2px 2px 1px #f2f2f2'
                )
              )
            )
          )
        )
      )
    )
  )
)



####################################################################################################

#################################### CALLBACKS #####################################################

# app$callback(
#   output=list(id="3d-pca", property="figure"),
#   list(input(id='d_date-box', property='value'),
#        input(id='d_treatment', property='value')),
#   function(selected_date, selected_treatment){
#     filtered_variable <- pca %>% dplyr::filter(Date %in% c(selected_date), Treatment %in% c(selected_treatment))
#     create3dScatter(filtered_variable)
#   }
# )


  # Manhattan plot ----------------------------------------------------------

  app$callback(
    output = list(id = "manhattan-plot", property = "figure"),
    params = list(input(id = 'manhattanplot-input', property = 'value')),
    function(genomic_line){
      # filtered_variable <- heat.df %>% dplyr::filter(Date %in% c(selected_date), Treatment %in% c(selected_treatment))
      lower_upper <- unlist(genomic_line)
      manhattanly::manhattanly(dataset, snp = "SNP", gene = "GENE", 
                               annotation1 = "DISTANCE", 
                               annotation2 = "EFFECTSIZE",
                               highlight = significantSNP,
                               genomewideline = lower_upper[2], 
                               suggestiveline = lower_upper[1])
    }
  )




# Volcano Plot ------------------------------------------------------------

app$callback(
  output = list(id = 'volcano-graph', property = 'figure'),
  params = list(input(id = 'volcanoplot-input', property = 'value'),
                input(id = "vp-genomic-line-val", property = "value")),
  function(effects, genomic_line) {
    manhattanly::volcanoly(
      x = dataset,
      snp = "SNP",
      gene = "GENE",
      genomewideline = as.numeric(genomic_line),
      effect_size_line = unlist(effects),
    )
  }
)



# Data upload -------------------------------------------------------------

app$callback(
  output = list(id='output-data-upload', property = 'children'),
  params = list(input(id = 'upload-data', property = 'contents'),
                state(id = 'upload-data', property = 'filename'),
                state(id = 'upload-data', property = 'last_modified')),
  function(list_of_contents, list_of_names, list_of_dates){
    if(is.null(list_of_contents) == FALSE){
      children = lapply(1:length(list_of_contents), function(x){
        parse_contents(list_of_contents[[x]], list_of_names[[x]], list_of_dates[[x]])
      })
      
    }
    return(children)
  })

# app$callback(
#   output("vp-upper-right", "value"),
#   list(
#     input("volcano-graph", "figure"),
#     input("vp-genomic-line-val", "value"),
#     state("volcanoplot-input", "value")
#   ),
#   function(fig, thresh, bounds){
#     u_lim <- bounds[[2]]
#     number = 0
#     if (length(fig[["data"]]) > 1){
#       x <- unlist(fig[["data"]][[1]]["x"])
#       y <- unlist(fig[["data"]][[1]]["y"])
#       number <- sum((x > u_lim) & (y > thresh))
#     }
#     number
#   }
# )
# 
# 
# app$callback(
#   output("vp-upper-left", "value"),
#   list(
#     input("volcano-graph", "figure"),
#     input("vp-genomic-line-val", "value"),
#     state("volcanoplot-input", "value")
#   ),
#   function(fig, thresh, bounds){
#     u_lim <- bounds[[1]]
#     number = 0
#     if (length(fig[["data"]]) > 1){
#       x <- unlist(fig[["data"]][[1]]["x"])
#       y <- unlist(fig[["data"]][[1]]["y"])
#       number <- sum((x < u_lim) & (y > thresh))
#     }
#     number
#   }
# )


app$run_server(debug=F, threaded=T, showcase = T)

