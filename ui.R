#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/

#
#
#
#     UI FOR TOOL2 : BETWEEN WORKER ANALYSIS TOOL
#
#
#
#
#

library(shiny)
library(ggplot2)
library(shinythemes)
library(shinyBS)
library(shinycssloaders)
library(colourpicker)

source("langParams.R")

#### for the data entry zone


inputTextarea <- function(inputId, value="", nrows, ncols) {
  tagList(
    singleton(tags$head(tags$script(src = "textarea.js"))),
    singleton(tags$head(tags$script(src = "ga-id.js"))),
    singleton(tags$head(tags$script(src = "google-analytics.js"))),
    singleton(tags$body(tags$noscript(src = "gatm.js"))),
    tags$textarea(id = inputId,
                  class = "inputTextarea",
                  rows = nrows,
                  cols = ncols,
                  as.character(value),
                  '40.1\tworker-10\n28.5\tworker-2\n48.5\tworker-9\n87\tworker-1\n6.73\tworker-5\n105\tworker-7\n64.6\tworker-1\n87.5\tworker-6\n6.38\tworker-2\n68.6\tworker-6\n41.4\tworker-10\n92.2\tworker-8\n19.1\tworker-2\n67.9\tworker-8\n345\tworker-1\n63.7\tworker-4\n17.6\tworker-3\n89.1\tworker-7\n59.8\tworker-9\n87.4\tworker-1\n89.2\tworker-4\n82.3\tworker-8\n12.6\tworker-5\n198\tworker-7\n25.1\tworker-3')
  )
}

tt <- function(param, txt="[0 &lt; valid &le; 100]") {
  return(bsTooltip(param, txt, "right", options = list(container = "body")))
}

inputWidth <- "110px"

mycss <- "
div.shiny-plot-output { margin: 0 auto }
div.plot-1 { width: 50% !important }
div.plot-2 { width: 100% !important }
div.shiny-plot-output > img { width: 100% }
div.width-100, div.width-100 > * { width: 100% !important }
div.width-140, div.width-140 > * { width: 140px !important }
div.colour-inp { width: 24% !important }
div.colour-inp > div > label { font-size: 12px }
.inline-b { display: inline-block !important }
.ital { font-style: italic !important }
.va-mid { vertical-align: middle !important }
div.float-l { float: left !important }
div.float-r { float: right !important }
#fieldset { border: 1px dotted grey !important; padding: 5px !important }
fieldset > legend { font-size: 14px !important; margin-bottom: 2px !important }
div.fieldset-body { display: none }
legend.toggle-personnalisation > * { cursor: pointer; font-size: 15px !important; font-style: italic }
div[data-shiny-input-type=colour] { margin-bottom: 2px !important }
div[data-shiny-input-type=colour] input { height: 35px }
div.fig-var-desc { text-align: justify; font-size: 14px !important; margin-left: 20px; margin-right: 20px }
div.shiny-input-container { margin-bottom: 2px !important }
"

#### for the riskmeter

shinyUI(
  fluidPage(
    tags$head(tags$style(HTML(mycss))),
    theme = shinytheme("flatly"),
    titlePanel(
      gett("main.title.t2")
    ),
    sidebarLayout(
      sidebarPanel(
        singleton(
          tags$head(tags$script('Shiny.addCustomMessageHandler("handler1", function(message) {
          $fig = $("#fracDepVariantes");
          $fig.removeClass("plot-2 plot-1");
          $fig.addClass("plot-"+ message);
        });'))
        ),
        singleton(
          tags$head(tags$script('$(document).ready(function() {
          $("legend.toggle-personnalisation").click(function() {
            $fieldset = $(this).closest("fieldset");
            $icon = $fieldset.find(".glyphicon");
            $fbody = $fieldset.find(".fieldset-body");
            $fbody.toggleClass("show");
            var show = $fbody.hasClass("show");
            if ( show ) {
              $fbody.show("blind");
            } else {
              $fbody.hide("blind");
            }
            $icon.removeClass("glyphicon-plus glyphicon-minus");
            $icon.addClass("glyphicon-" + (show ? "minus" : "plus"));
          });
        });'))
        ),
        singleton(
          tags$head(tags$script('
            $(document).ready(function() {
              $("#Data").keydown(function(e) {
                if ( e.keyCode == 9 ) {
                  e.preventDefault();
                  var tabChar = "\t";
                  var start = this.selectionStart;
                  var end = this.selectionEnd;
                  var val = this.value;
                  this.value = val.slice(0, start) + tabChar + val.slice(end);
                                
                  // Move the caret
                  this.selectionStart = this.selectionEnd = start + 1;
                  return false;
                }
              });
            })
          '))
        ),
        h4(gett("input.1")),
        br(),
        numericInput("oel", gett("input.2"), 100, width=inputWidth),
        tt("oel", gett("input.2.tooltip") ),
        numericInput("al", gett("input.3"), 1, width=inputWidth),
        tt("al", gett("input.3.tooltip")),
        numericInput("conf", gett("input.4"), 90, width=inputWidth),
        tt("conf",gett("input.4.tooltip")),
        numericInput("psi", gett("input.5"), 30, width=inputWidth),
        tt("psi",gett("input.5.tooltip")),
        numericInput("rappap_cover", gett("input.5.1") , 80, width=inputWidth),
        tt("rappap_cover", gett("input.5.1.tooltip")),
        numericInput("wwct", gett("input.5.2"), 0.2, width=inputWidth),
        tt("wwct", gett("input.5.2.tooltip")),
        h4(gett("input.6")),
        inputTextarea('Data', '',10,17 ),
        #br(),
        #br(),
        #br(),
        #br(),
        #radioButtons("prior.sigma", gett("prior.1"),
        #             choiceNames = list(
        #               gett("prior.a"),
        #               gett("prior.b")
        #             ),
        #             selected=1,
        #             choiceValues = list(
        #               1, 0
        #             )),
        #tt("prior_sigma_1", gett("prior.1.tooltip")),
        #tt("prior_sigma_0", gett("prior.1.tooltip")),
        #actionButton("go",gett("input.7")),
        #bsTooltip("go", gett("input.7.tooltip"), "right", options = list(container = "body") ),
        width = 3
      ),
      mainPanel(
        tags$head(tags$style(type="text/css", "
                           #waitmessage {
                           position: fixed;
                           top: 0px;
                           left: 0px;
                           width: 100%;
                           padding: 5px 0px 5px 0px;
                           text-align: center;
                           font-weight: bold;
                           font-size: 100%;
                           color: #000000;
                           background-color: #CCFF66;
                           z-index: 105;
                           }
              #probSituUnacceptable, [for='probSituUnacceptable'] {display: none;}
              input[type='number'] {
    -moz-appearance:textfield;
                           }
                           
                           input::-webkit-outer-spin-button,
                           input::-webkit-inner-spin-button {
                           -webkit-appearance: none;
                           }
                           ")
        ),
        conditionalPanel(condition="$('html').hasClass('shiny-busy')", tags$div(gett("input.8"),id="waitmessage")),
        tabsetPanel( 
          tabPanel(
            gett("descriptive.tab.name"),
            h3(gett("descriptive.title")),
            tabsetPanel(
              tabPanel(
                gett("descriptive.subtab.name.1"),
                br(),
                tableOutput('res.desc'),
                h4(gett("descriptive.1")),
                p(htmlOutput("descriptive.2")),
                br(),
                h3(gett("descriptive.3")),
                plotOutput('qqplot'),
                br(),
                p(gett("descriptive.4")),
                ##### NEW FOR BOXPLOT
                br(),
                h3(gett("descriptive.5")),
                plotOutput('boxplot'),
                p(gett("descriptive.6")),
                br()
                
                ####END OF NEW FOR BOXPLOT
              ),
                   
                   
              #######################DESCRIPTIVE - INDIVIDUAL
                   
              tabPanel(
                gett("descriptive.subtab.name.2"),
                br(),
                p(gett("descriptive.1.1")),
                br(),
                uiOutput("Box1"),
                br(),
                tableOutput('res.desc.ind'),
                h4(gett("descriptive.1")),
                p(htmlOutput("descriptive.2.1")),
                h3(gett("descriptive.3")),
                plotOutput('qqplot.ind'),
                br(),
                p(gett("descriptive.4")),
                br()
              )
            )
          ),
          #
          #
          #
          ############EXCEEDANCE FRACTION PANEL
          #
          #
          #
          #
          
          tabPanel(
            gett("frac.tab.name"),
            h3(gett("frac.title")),
            numericInput("frac_threshold", gett("frac.1"), 5, width="160px"),
            tt("frac_threshold", gett("frac.1.tooltip")),
            
            
####NEW     
      #      numericInput("frac_threshold_ind", gett("frac.ind.1"), 20, width="160px"),
      #      tt("frac_threshold_ind", gett("frac.ind.1.tooltip")),            
            
            
            tabsetPanel(
              tabPanel(
                #################   group 
                
                ####### frac
                
                
                gett("frac.subtab.name.1"),
                br(),
                #h4(strong(gett("frac.2"))),
                #p(gett("frac.3")),
                
                
                
                div(style="block",class="plot-with-text",
                    div(style="display: inline-block;width:55%",class="text",
                        h4(strong(gett("frac.7"))),
                        p("\u25B9", gett("frac.8")),
                        p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong(gett("frac.9"),"\u2265 ",textOutput("acceptableExpo1", inline=TRUE))),
                        p("\u25B9", gett("frac.10")),
                        p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong(textOutput("probrisk",inline=TRUE))),
                        p("\u25B9", gett("frac.11")),
                        p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong( '<', textOutput("probSituUnacceptable1",inline=TRUE))),
                        p("\u25B9", gett("frac.12")),
                        p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong(textOutput("finalrisk",inline=TRUE)))
                    ),
                    div(style="display: inline-block;width:44%;vertical-align:top", class="plot", plotOutput("risquemetre.group.frac"))
                ),
                
                h4(strong(gett("frac.4"))),
                #distribution           
                p(textOutput("gm1.dist", inline=TRUE), strong(textOutput("gm1",inline=TRUE))),
                  p(textOutput("gsd1.dist", inline=TRUE), strong(textOutput("gsd1",inline=TRUE))),           
                br(),
         
                #parameters           
                h4(strong(gett("frac.5"))),
                           
                p( gett("frac.6"),strong(textOutput("Frac",inline=TRUE)) ),
                p( gett("frac.6.1"), strong(textOutput("Frac.ci",inline=TRUE)) ),

                br(),
                
                h4(strong(gett("frac.graph.1"))),
                           
                #calendar
                           
                #p(strong(gett("frac.graph.2"))),
                #p(gett("frac.graph.3")),
                #plotOutput("calendarplot"),
                #br(),
                
                p(strong(gett("frac.graph.13"))),
                p(gett("frac.graph.13.1")),
                tags$span(class='inline-b ital va-mid', gett("frac.graph.13.4.variante")),
                div(class='inline-b',
                    radioButtons("varianteFracDep", label = NULL, inline = TRUE,
                                 c("1" = "figure1",  "2" = "figure2", "3" = "figure3", "4" = "figure4"))
                ),
                tags$fieldset(
                  tags$legend(class = "toggle-personnalisation", tags$span(gett("frac.graph.13.2")), tags$span(class = "glyphicon glyphicon-plus") ),
                  tags$div(class='fieldset-body',
                           tags$div( class='colour-inp inline-b',
                                     tags$div(tags$label(gett("frac.graph.13.3.1"))), colourInput( "couleurRisque", NULL, "red", returnName = TRUE, palette = "limited")),
                           tags$div( class='colour-inp inline-b',
                                     tags$div(tags$label(gett("frac.graph.13.3.2"))), colourInput("couleurAucunRisque", NULL, "gray50", returnName = TRUE, palette = "limited")),
                           tags$div( class='colour-inp inline-b',
                                     tags$div(tags$label(gett("frac.graph.13.3.3"))), colourInput("couleurFond", NULL, "gray70", returnName = TRUE, palette = "limited")),
                           tags$div( class='colour-inp inline-b',
                                     tags$div(tags$label(gett("frac.graph.13.3.4"))), colourInput("couleurSeuil", NULL, "gray40", returnName = TRUE, palette = "limited"))
                  )
                ),
                br(),
                div(class='fig-var-desc', textOutput('fracDepVarianteDesc')),
                withSpinner(plotOutput('fracDepVariantes')),
                           
                #sequential
                           
                p(strong(gett("frac.graph.4"))),
                p(gett("frac.graph.5")),
                plotOutput("seqplot.frac"),
                br(),
                           
                #density
                           
                p(strong(gett("frac.graph.7"))),
                p(gett("frac.graph.8")),
                plotOutput("distplot.frac"),
                br(),
                           
                #riskband
                           
                p(strong(gett("frac.graph.11"))),
                p( gett("frac.graph.12.1"), textOutput("acceptableExpoDiv10_1", inline=TRUE), gett("frac.graph.12.2"), textOutput("acceptableExpoDiv10_2", inline=TRUE), gett("frac.graph.12.3"), textOutput("acceptableExpo2", inline=TRUE), gett("frac.graph.12.4"), textOutput("acceptableExpo3", inline=TRUE), gett("frac.graph.12.5") ),
                plotOutput("riskband.frac"),
                br()
              ),
                    
                    
                    
              ######################### EXCEEDANCE - BETWEEN WORKER ANALYSIS
              
              tabPanel(
                gett("frac.subtab.name.2"),
                br(),
                
                numericInput("frac_threshold_ind", gett("frac.ind.1"), 20, width="160px"),
                tt("frac_threshold_ind", gett("frac.ind.1.tooltip")), 
                
                h4(strong(gett("frac.13"))),
                br(),
                p( gett("frac.14.1"), strong(textOutput("ProbFracInd",inline=TRUE)) ),
                p( gett("frac.14.2"), textOutput("indivOverexpThresh1", inline=TRUE) ,gett("frac.14.3"),strong(textOutput("ProbFracInd.greater",inline=TRUE)) ),
                br(),
                             
                             
                             ##riskband plot
                             
                p( strong(gett("frac.graph.11")) ),
                p( gett("frac.16.1"), textOutput("indivOverexpThresh1.1", inline=TRUE), gett("frac.16.2"), textOutput("indivOverexpThresh1.2", inline=TRUE), gett("frac.16.3"), textOutput("indivOverexpThresh2.1", inline=TRUE), gett("frac.16.2"), textOutput("indivOverexpThresh2.2", inline=TRUE), "", gett("frac.16.3"), textOutput("indivOverexpThresh3.1", inline=TRUE), gett("frac.16.4"), textOutput("indivOverexpThresh3.2", inline=TRUE), "."),
                plotOutput("riskband.frac.ind"),
                br(),
                             
                ##between worker variability
                             
                h4(strong(gett("frac.17"))),
                br(),
                p(textOutput("gsdw.frac", inline=TRUE),strong(textOutput("GSDW.1",inline=TRUE))),
                p(textOutput("gsdb.frac", inline=TRUE),strong(textOutput("GSDB.1",inline=TRUE))),
                br(),
                p(textOutput("rho.frac", inline=TRUE), strong(textOutput("rho.1",inline=TRUE))),
                
                p(gett("frac.21.1"), textOutput("ww1",inline=TRUE), gett("frac.21.2"),strong(textOutput("rho.greater0.2",inline=TRUE))),
                br(),
                p(gett("frac.22.1"), textOutput("rapRatio1", inline=TRUE), gett("frac.22.2"),strong(textOutput("rap.1",inline=TRUE))),
                p(gett("frac.23"),strong(textOutput("rap.greater2",inline=TRUE))),
                p(gett("frac.24"),strong(textOutput("rap.greater10",inline=TRUE))),
                br(),
                p(htmlOutput("frac.25")),
                br(),
                plotOutput("worker.boxplot.1"),
                br()
              ),
                    
                    
              ################### EXCEEDANCE - INDIVIDUAL WORKER ANALYSIS
              tabPanel(
                gett("frac.subtab.name.3"),
                br(),
                p(gett("frac.26")),
                br(),
                uiOutput("Box2"),
                br(),
                #h4(strong(gett("frac.2"))),
                #p(gett("frac.3")),
                
                div(style="block",class="plot-with-text",
                    div(style="display: inline-block;width:55%",class="text",
                        h4(strong(gett("frac.7"))),
                        p("\u25B9", gett("frac.8")),
                        p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong(gett("frac.9"),"\u2265 ", textOutput("acceptableExpo4", inline=TRUE) ) ),
                        p( "\u25B9", gett("frac.10")),
                        p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong(textOutput("probrisk.worker",inline=TRUE)) ),
                        p("\u25B9", gett("frac.11")),
                        p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong( '<', textOutput("probSituUnacceptable4",inline=TRUE))),
                        p("\u25B9", gett("frac.12")),
                        p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong(textOutput("finalrisk.ind",inline=TRUE)))
                    ),
                    div(style="display: inline-block;width:44%;vertical-align:top", class="plot", plotOutput("risquemetre.ind.frac"))
                ),
                h4(strong(gett("frac.4"))),
                             
                
                             
		p(textOutput("frac.gm.ind", inline=TRUE), strong(textOutput("gm.ind.1",inline=TRUE))),
                p(textOutput("frac.gsd.ind", inline=TRUE),strong(textOutput("gsd.ind.1",inline=TRUE))),
                br(),
                       
                
                       
                h4(strong(gett("frac.5"))),
                p(gett("frac.6"),strong(textOutput("Frac.worker",inline=TRUE))),
                p( gett("frac.6.1"),textOutput("conf.out.ind.9",inline=TRUE), strong(textOutput("Frac.worker.ci",inline=TRUE)) ),
                br(),
                       
                       #####graphs
                       
                h4(strong(gett("frac.graph.1"))),
                br(),
                        
                       #sequential
                       
                p(strong(gett("frac.graph.4"))),
		            p(gett("frac.graph.5")),
                plotOutput("seqplot.frac.worker"),
                br(),
                       
                       #risk band
                       
                p( strong(gett("frac.graph.11"))),
		p( gett("frac.graph.12.1"), textOutput("acceptableExpoDiv10_3", inline=TRUE), gett("frac.graph.12.2"), textOutput("acceptableExpoDiv10_4", inline=TRUE), gett("frac.graph.12.3"), textOutput("acceptableExpo5", inline=TRUE), gett("frac.graph.12.4"), textOutput("acceptableExpo6", inline=TRUE), gett("frac.graph.12.5") ),
                plotOutput("riskband.frac.worker"),
                br()
              )
            )
          ),
                           #
       #
       #
       #
       ############95th percenetile PANEL
       #
       #
       #
       #
       
       
        
          tabPanel(
            textOutput("percentile1", inline=TRUE),
            h3(gett("perc.title"), textOutput("percentile2", inline=TRUE)),
            numericInput("target_perc", gett("perc.1"), 95, width="160px"),
            tt("target_perc",gett("perc.1.tooltip")),
                 
            
####NEW     
           # numericInput("perc_threshold_ind", gett("frac.ind.1"), 20, width="160px"),
           # tt("perc_threshold_ind", gett("perc.ind.1.tooltip")), 
            
                 #################95th PERCENTILE - GROUP ANALYSIS
            tabsetPanel(
              tabPanel(
                gett("frac.subtab.name.1"),
                br(),
                   
                #quick glance
                   
                #h4(strong(gett("frac.2"))),
                #p(gett("frac.3")),
                
                div(style="block",class="plot-with-text",
                    div(style="display: inline-block;width:55%",class="text",
                        h4(strong(gett("frac.7"))),
                        p("\u25B9", gett("frac.8")),
                        p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong(textOutput("percentile5",inline=TRUE), "\u2265 ",  gett("OEL"))),
                        p("\u25B9", gett("frac.10")),
                        p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong(textOutput("probrisk.perc",inline=TRUE))),
                        p("\u25B9", gett("frac.11")),
                        p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong('<', textOutput("probSituUnacceptable2",inline=TRUE))),
                        p("\u25B9", gett("frac.12")),
                        p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong(textOutput("finalrisk.perc",inline=TRUE)))
                    ),
                    div(style="display: inline-block;width:44%;vertical-align:top", class="plot", plotOutput("risquemetre.group.pPerc"))
                ),
                
                #parameters-distrib
                 
                h4(strong(gett("frac.4"))),

		p(textOutput("gm1.perc", inline=TRUE), strong(textOutput("gm2", inline=TRUE))),
                 p(textOutput("gsd1.perc", inline=TRUE), strong(textOutput("gsd2",inline=TRUE))),
								br(),
                #parameter estimates - percentile
                 
                h4(strong(gett("perc.5"), textOutput("percentile3", inline=TRUE))),
                p(gett("frac.6"),strong(textOutput("Perc",inline=TRUE))),
                p(gett("frac.6.1"),strong(textOutput("Perc.ci",inline=TRUE))),
                 
                br(),
                 
                 ##illustrative graphss
                 
                h4(strong(gett("frac.graph.1"))),
                 
                 #sequential
                 
                p(strong(gett("frac.graph.4"))),
                 
                p(gett("perc.graph.5")),
                 
                plotOutput("seqplot.perc"),
                br(),
                 
                 #density
                 
                p(strong(gett("frac.graph.7"))),
                 
                p(gett("perc.graph.8")),
                 
                plotOutput("distplot.perc"),
                 
                 #riskband
                 
                br(),
                p(strong(gett("frac.graph.11"))),
                p(gett("perc.graph.12.1"), textOutput("percentile9", inline=TRUE),  htmlOutput("frac.graph.12.2.3", inline=TRUE)),
                plotOutput("riskband.perc"),
                br()
              ),
                 
                 
              ##################### 95th PERCENTILE - BETWEEN WORKER ANALYSIS
                 
              tabPanel(
                gett("frac.subtab.name.2"),
                br(),
                
                numericInput("perc_threshold_ind", gett("frac.ind.1"), 20, width="160px"),
                tt("perc_threshold_ind", gett("perc.ind.1.tooltip")), 
                
                
                h4(strong(gett("frac.13"))),
                br(),
                
                p(gett("frac.14.1"),strong(textOutput("ProbPpercInd",inline=TRUE))),
                
                p(gett("frac.14.2"), textOutput("indivOverexpThresh4", inline=TRUE), gett("frac.14.3"),strong(textOutput("ProbPpercInd.greater",inline=TRUE))),
                
                br(),
                
                #risk plot
                
                p(strong(gett("frac.graph.11"))),
                
                p( gett("frac.16.1"), textOutput("indivOverexpThresh1.3", inline=TRUE), gett("frac.16.2"), textOutput("indivOverexpThresh1.4", inline=TRUE), gett("frac.16.3"), textOutput("indivOverexpThresh2.3", inline=TRUE), gett("frac.16.2"), textOutput("indivOverexpThresh2.4", inline=TRUE), "", gett("frac.16.3"), textOutput("indivOverexpThresh3.3", inline=TRUE), gett("frac.16.4"), textOutput("indivOverexpThresh3.4", inline=TRUE), "."),
                plotOutput("riskband.pPerc.ind"),
                br(),
                
                ##parameter estimates
                
                h4(strong(gett("frac.17"))),
                
                br(),
                
                p(textOutput("gsdw.frac.2", inline=TRUE),strong(textOutput("GSDW.2",inline=TRUE))),
                p(textOutput("gsdb.frac.2", inline=TRUE),strong(textOutput("GSDB.2",inline=TRUE))),
                br(),
                p(textOutput("rho.frac.2", inline=TRUE), strong(textOutput("rho.2",inline=TRUE))),
                
                p(textOutput("rho.frac.prob", inline=TRUE), gett("bohs_info"), strong(textOutput("rho.greater0.2.2",inline=TRUE))),
                
                br(),
                p( gett("frac.22.1"), textOutput("rapRatio2", inline=TRUE), gett("frac.22.2"), strong(textOutput("rap.2",inline=TRUE)) ),
                p(gett("frac.23"),strong(textOutput("rap.greater2.2",inline=TRUE))),
       
                p(gett("frac.24"),strong(textOutput("rap.greater10.2",inline=TRUE))),
                br(),
                htmlOutput("perc.25"),
                br(),
                plotOutput("worker.boxplot.2"),
                br()
              ),
                 
                 
              ####################  95th PERCENTILE - INDIVIDUAL WORKER ANALYSIS
              tabPanel(
                gett("frac.subtab.name.3"),
                br(),
                p(gett("frac.26")),
                br(),
                uiOutput("Box3"),
                br(),
                h4(strong(gett("frac.2"))),
                p(gett("frac.3")),
                
                div(style="block",class="plot-with-text",
                    div(style="display: inline-block;width:55%",class="text",
                        h4(strong(gett("frac.7"))),
                        p("\u25B9", gett("frac.8")),
                        p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong(textOutput("percentile13", inline=TRUE),"\u2265", gett("OEL"))),
                        p("\u25B9", gett("frac.10")),
                        p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong(textOutput("probrisk.worker.2",inline=TRUE))),
                        p("\u25B9", gett("frac.11")),
                        p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong('<', textOutput("probSituUnacceptable5",inline=TRUE))),
                        p("\u25B9", gett("frac.12")),
                        p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong(textOutput("finalrisk.perc.ind",inline=TRUE)))
                    ),
                    div(style="display: inline-block;width:44%;vertical-align:top", class="plot", plotOutput("risquemetre.ind.pPerc"))
                ),
                
                #param estimates
                
                h4(strong(gett("frac.4"))),
                
		p(textOutput("frac.gm.ind.2", inline=TRUE), strong(textOutput("gm.ind.2",inline=TRUE))),
                p(textOutput("frac.gsd.ind.2", inline=TRUE),strong(textOutput("gsd.ind.2",inline=TRUE))),
                br(),
                
                h4(strong(gett("perc.5"), textOutput("percentile12", inline=TRUE))),
                
                p(gett("frac.6"),strong(textOutput("Pperc.worker",inline=TRUE))),
                
                p(gett("frac.6.1"),strong(textOutput("conf.out.ind.18",inline=TRUE))),
                
                br(),
                
                #graph
                
                h4(strong(gett("frac.graph.1"))),
                br(),
                
                #sequential
                
                p(strong(gett("frac.graph.4"))),
                
                p(gett("perc.graph.5")),
                
                plotOutput("seqplot.pPerc.worker"),
		
                br(),
                
                #riskbland
                
                p(strong(gett("frac.graph.11"))),
                p(gett("perc.graph.12.1"), textOutput("percentile14", inline=TRUE), htmlOutput("perc.graph.12.2", inline=TRUE)),
                plotOutput("riskband.pPerc.worker"),
                
                
                br()
              )
            )
          ),
        
 #
 #
 #
 #
 #
 #
  ###############################   Arithmetic mean
 #
 #
 #
 #
 #
 #
 #
          tabPanel(
            gett("am.tab.name"),
            h3(gett("am.title")),
 
            
            
####NEW     
       #     numericInput("am_threshold_ind", gett("frac.ind.1"), 10, width="160px"),
      #      tt("am_threshold_ind", gett("am.ind.1.tooltip")),           
                          
                 
 #############################ARITHMETIC MEAN - GROUP ANALYSIS
            tabsetPanel(
              tabPanel(
                gett("frac.subtab.name.1"),
                br(),
                #h4(strong(gett("frac.2"))),
                #p(gett("frac.3")),
                
                br(),
                div(style="block",class="plot-with-text",
                    div(style="display: inline-block;width:55%",class="text",
                        h4(strong(gett("frac.7"))),
                        p("\u25B9", gett("frac.8")),
                        p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong(gett("AM"), "\u2265", gett("OEL"))),
                        p("\u25B9", gett("frac.10")),
                        p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong(textOutput("probrisk.AM",inline=TRUE))),
                        p("\u25B9", gett("frac.11")),
                        p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong(textOutput("probSituUnacceptable3",inline=TRUE))),
                        p("\u25B9", gett("frac.12")),
                        p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong(textOutput("finalrisk.AM",inline=TRUE)))
                    ),
                    div(style="display: inline-block;width:44%;vertical-align:top", class="plot", plotOutput("risquemetre.group.am"))
                ),
                p(strong(gett("am.13")),gett("am.13.1")),
                br(),
                
                #par estimates
                 
                h4(strong(gett("frac.4"))),
                 
                 p(textOutput("gm1.AM", inline=TRUE), strong(textOutput("gm3", inline=TRUE))),
                 p(textOutput("gsd1.AM", inline=TRUE), strong(textOutput("gsd3",inline=TRUE))), 
                br(),
                h4(strong(gett("am.5"))),
                 
                p(gett("frac.6"),strong(textOutput("AM",inline=TRUE))),
                 
                p(gett("frac.6.1"),strong(textOutput("AM.ci",inline=TRUE))),
                 

                 
                br(),
                 
                 ###graphs
                 
                h4(strong(gett("frac.graph.1"))),
                 
                 #seq plot
                 
                p(strong(gett("frac.graph.4"))),
                 
                p(paste0(gett("am.graph.5"), ".")),
                 
                plotOutput("seqplot.AM"),
                br(),
                 
                 #density plot
                 
                p(strong(gett("frac.graph.7"))),
                 
                p(gett("am.graph.8")),
                 
                plotOutput("distplot.AM"),
                 
                br(),
                 
                 #risk band plot
                 
                p(strong(gett("frac.graph.11"))),
                 
                p(htmlOutput("am.graph.12", inline=TRUE)),
                plotOutput("riskband.am"),
                br()
              ),
        
        
        
 ################################## ARITHMETIC MEAN - BETWEEN WORKER ANALYSIS
              tabPanel(
                gett("frac.subtab.name.2"),
                br(),
                
                numericInput("am_threshold_ind", gett("frac.ind.1"), 10, width="160px"),
                tt("am_threshold_ind", gett("am.ind.1.tooltip")),           
                
                
                
                h4(strong(gett("frac.13"))),
                 
                br(),
                 
                p(gett("frac.14.1"),strong(textOutput("ProbAMInd",inline=TRUE))),
                p(gett("frac.14.2"), textOutput("indivOverexpThresh7", inline=TRUE), gett("frac.14.3"),strong(textOutput("ProbAMInd.greater",inline=TRUE))),
                br(),
                 
                #riskbland plot
                 
                p(strong(gett("frac.graph.11"))),
                 
                p( gett("frac.16.1"), textOutput("indivOverexpThresh1.5", inline=TRUE), gett("frac.16.2"), textOutput("indivOverexpThresh1.6", inline=TRUE), gett("frac.16.3"), textOutput("indivOverexpThresh2.5", inline=TRUE), gett("frac.16.2"), textOutput("indivOverexpThresh2.6", inline=TRUE), "", gett("frac.16.3"), textOutput("indivOverexpThresh3.5", inline=TRUE), gett("frac.16.4"), textOutput("indivOverexpThresh3.6", inline=TRUE), "."),
                plotOutput("riskband.am.ind"),
                br(),
                 
                 ##par estimates
                 
                 
                h4(strong(gett("frac.17"))),
                 
                br(),
                 
                p(textOutput("gsdw.frac.3", inline=TRUE),strong(textOutput("GSDW.3",inline=TRUE))),
                p(textOutput("gsdb.frac.3", inline=TRUE),strong(textOutput("GSDB.3",inline=TRUE))),
                br(),
                p(textOutput("rho.frac.3", inline=TRUE), strong(textOutput("rho.3",inline=TRUE))),
                 

                p(textOutput("rho.perc.prob", inline=TRUE), gett("bohs_info"), strong(textOutput("rho.greater0.2.3",inline=TRUE))),
                br(),
                 
                 
                p(gett("frac.22.1"), textOutput("rapRatio3", inline=TRUE), gett("frac.22.2"),strong(textOutput("rap.3",inline=TRUE))),
                 
                p(gett("frac.23"),strong(textOutput("rap.greater2.3",inline=TRUE))),
                 
                p(gett("frac.24"),strong(textOutput("rap.greater10.3",inline=TRUE))),
                br(),
                p(htmlOutput("am.25")),
                br(),
                plotOutput("worker.boxplot.3"),
                br()
              ),
 
 
        
 ####################  ARITHMETIC MEAN - INDIVIDUAL WORKER ANALYSIS
              tabPanel(
                gett("frac.subtab.name.3"),
                br(),
                p(gett("frac.26")),
                br(),
                uiOutput("Box4"),
                br(),
                #h4(strong(gett("frac.2"))),
                #p(gett("frac.3")),
                
                div(style="block",class="plot-with-text",
                    div(style="display: inline-block;width:55%",class="text",
                        h4(strong(gett("frac.7"))),
                        p("\u25B9", gett("am.8")),
                        p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong(gett("AM"), "\u2265", gett("OEL"))),
                        p("\u25B9", gett("frac.10")),
                        p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong(textOutput("probrisk.worker.3",inline=TRUE))),
                        p("\u25B9", gett("frac.11")),
                        p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong(textOutput("probSituUnacceptable6",inline=TRUE))),
                        p("\u25B9", gett("frac.12")),
                        p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong(textOutput("finalrisk.am.ind",inline=TRUE)))
                    ),
                    div(style="display: inline-block;width:44%;vertical-align:top", class="plot", plotOutput("risquemetre.ind.am"))
                ),
                 
                 #par estimates distribution
                 
                h4(strong(gett("frac.4"))),
                 
		p(textOutput("frac.gm.ind.3", inline=TRUE), strong(textOutput("gm.ind.3",inline=TRUE))),
                p(textOutput("frac.gsd.ind.3", inline=TRUE),strong(textOutput("gsd.ind.3",inline=TRUE))),
                br(),
                
		            # par estimates AM
                 
                h4(strong(gett("am.5"))),
                 
                p(gett("frac.6"),strong(textOutput("am.worker",inline=TRUE))),
                p(gett("frac.6.1"),strong(textOutput("am.worker.ci",inline=TRUE)) ),
                br(),
                 
                 
                 #### graphes
                 
                h4(strong(gett("frac.graph.1"))),
                br(),
                 
                 #seq
                 
                p(strong(gett("frac.graph.4"))),
                
                p(gett("am.graph.5")),
                 
                 
                plotOutput("seqplot.am.worker"),
                 
                br(),
                 
                 #risk band plot
                 
                p(strong(gett("frac.graph.11"))),
                 
                p(htmlOutput("am.graph.12.2")),
                plotOutput("riskband.am.worker"),
                br()
              )
            )
          ),
        
        ############INSTRUCTIONS
        
        tabPanel(gett("inst.tab.name"),
                 h3(gett("inst.title.t2")),
                 br(),
                 
                 p(gett("inst.1")),
                 
                 p(gett("inst.2")),
                 
                 p(gett("inst.3")),
                 
                 p(gett("inst.5"))
                 
        ),
        
        ############About
        
        tabPanel(gett("about.tab.name"),
                 h3(gett("about.title.t2")),
                 br(),
                 p(htmlOutput("about.1.t2")),
                 
                 p(gett("about.2")),
                 
                 p(htmlOutput("about.3"))),
        
        
        ############BACKGROUND
        
tabPanel(gett("back.tab.name"),
         h3(gett("back.title")),
         br(),
         p(htmlOutput("back.1.intro")),
         p(htmlOutput("back.2", inline=TRUE))
                 
                 
                 
                 
        )
        
      )
      )
      )
    )
 )
