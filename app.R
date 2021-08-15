# By Steve Haroz. This code is shared with an MIT license.
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# https://shiny.rstudio.com/images/shiny-cheatsheet.pdf

library(shiny)
library(dplyr)
library(stringr)
library(metathis)
library(statcheck)

# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
        tags$link(rel="icon", type="image/png", href="favicon.png")
    ),
    
    # Application metadata
    meta() %>%
      meta_social(
        title = "StatCheck simple edition",
        description = "Check for errors in statistical tests",
        image = "https://steveharoz.shinyapps.io/statchecksimple/logo.png",
        twitter_creator = "@sharoz",
        twitter_card_type = "summary"
      ),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        div(
            class = "col-md-4",
            
            # Application title
            titlePanel(
                img(src='logo.png', width='100%', alt='StatCheck simple edition'),
                'StatCheck simple edition'
            ),
            
            # the input
            textAreaInput("statcheckInput", "Enter text that reports statistical tests:", "Here is some text with statistical tests F(12,34)=0.56, p=0.078. Blah blah t(123)=.45, p=0.65. Blah blah T(100)=1.7, P=0.046 and also X2(1, N=56) = 7.8, p < .01. \n\nSome non-standard reporting F2,20=2; p = 0.16 and some abnormal spaces F(2,20)\u202F=\u202F2; p\u00A0=\u00A00.16 and T[25] = 1.8;p=0.08 and T25 = 35;p=0 and X\u00B2(1, N=56) = 7.8, p=0.005", width = "100%", row = "10"),
            
            # formatting fixes
            span(
              "Recognize non-standard reporting: ", 
              checkboxInput("statCheckFix", NULL, TRUE, width = '20px'), 
              title = "(Recommended) Extend statcheck to be much more forgiving to deviations from APA style.",
              id="formatting_input"   
            ),
            
            # Credit
            wellPanel(id = "credit_box",
                p(
                    "StatCheck simple edition by",
                    a('Steve Haroz ', href="http://steveharoz.com")
                ),
                p(
                    a('StatCheck', href="http://statcheck.io"),
                    "by",
                    a('Sacha Epskamp', href="http://sachaepskamp.com/"),
                    "and",
                    a('Michèle B. Nuijten', href="https://mbnuijten.com")
                ),
                p(
                  span("Updates: "),
                  a(icon("twitter"), span(" @sharoz"), href="https://twitter.com/sharoz")
                ),
                p(
                    span("Source code: "),
                    a(icon("github"), span("Github"), href="https://github.com/steveharoz/statcheck-simple-edition")
                ),
                p(
                  span("Funding: "), 
                  a(href='http://aviz.fr', img(src='aviz.png', height='14px', alt='Aviz', style='opacity:0.75'))
                ),
                br(), 
                p("To cite this page: ", br(), span("Haroz, S., Nuijten, M. B., Epskamp, S. (2021). StatCheck simple edition [web application]. Retrieved from http://steveharoz.com/statchecksimple", id="citation"))
            ),
        ),

        # Show the statcheck result table
        div(
           tableOutput("statcheckOutput"),
           class = "col-md-4",
           id = "results"
        )
    ),
    
    div(HTML(
    '<!-- StatCounter -->
	  <script type="text/javascript">
		sc_project=1771316; 
		sc_invisible=1; 
		sc_partition=16; 
		sc_security="caf1424b"; 
	  </script>
    <script type="text/javascript" src="https://www.statcounter.com/counter/counter_xhtml.js"></script><noscript><div class="statcounter"><a href="https://www.statcounter.com/" target="_blank"><img class="statcounter" src="https://c17.statcounter.com/1771316/0/caf1424b/1/" alt="hit counter code" /></a></div></noscript>'
    ))
)


# authors and copy editors seem to think that reporting statistics is the right time to get creative with typography
# this function makes text more statcheck-friendly
preprocess_text = function(text) {
  # the edge of paragraph can cause issues
  text = paste("", text, "")
  
  # weird spaces
  text = text %>% 
    str_replace_all(fixed(" "), " ") %>%  # half spaces
    str_replace_all("\\s", " ")
  
  # non-standard separators
  text = text %>% 
    str_replace_all(fixed(";"), fixed(",")) %>% 
    str_replace_all(fixed("["), fixed("(")) %>% 
    str_replace_all(fixed("]"), fixed(")"))
  
  # squared symbol
  text = text %>% str_replace_all(fixed("\u00B2"), fixed("2"))
  
  # statcheck has trouble with statistics or p values that are exactly 0 or 1
  text = text %>% 
    str_replace_all("=\\s?0(?!(\\d|(\\.\\d)))", fixed("= 0.0")) %>% # seen this one
    str_replace_all("<\\s?0(?!(\\d|(\\.\\d)))", fixed("< 0.0")) %>% # seen this one
    str_replace_all(">\\s?0(?!(\\d|(\\.\\d)))", fixed("> 0.0")) %>% 
    str_replace_all("=\\s?1(?!(\\d|(\\.\\d)))", fixed("= 1.0")) %>% # seen this one 
    str_replace_all("<\\s?1(?!(\\d|(\\.\\d)))", fixed("< 1.0")) %>% # seen this one
    str_replace_all(">\\s?1(?!(\\d|(\\.\\d)))", fixed("> 1.0")) 
  
  # recognize t and f tests without parentheses
  # this issue is common when subscripts are used for the degrees of freedom
  REGEX_NUMBER = "[0-9]*\\.?[0-9]+" # positive number with optional decimal
  REGEX_T = paste0("[^A-Za-z](t|T)", REGEX_NUMBER, "\\s*=")
  REGEX_F = paste0("[^A-Za-z](f|F)", REGEX_NUMBER, ",", REGEX_NUMBER, "\\s*=")
  
  # add parentheses to smooshed t-tests
  text = text %>% 
    str_replace_all(REGEX_T, function(s) {
      degrees_of_freedom = str_extract(s, REGEX_NUMBER)
      paste0(" t(", degrees_of_freedom, ")=")
    })
  
  # add parentheses to smooshed f-tests
  text = text %>% 
    str_replace_all(REGEX_F, function(s) {
      degrees_of_freedom = str_extract_all(s, REGEX_NUMBER)[[1]]
      paste0(" f(", degrees_of_freedom[[1]], ",", degrees_of_freedom[[2]], ")=")
    })
  
  return(text)
}

# Define server logic required to draw the table
server <- function(input, output) {
    output$statcheckOutput <- renderTable({ 
        statcheck_input = input$statcheckInput %>% str_to_lower()
        statcheck_fix = input$statCheckFix
        
        # fix formatting issues if possible
        if (statcheck_fix) {
          statcheck_input = preprocess_text(statcheck_input)
        }
        
        # get raw statcheck results
        resultTable = statcheck(statcheck_input)
        if (is.null(resultTable))
            return(NULL)
        
        # clean up the columns
        resultTable = resultTable %>% 
            rename(Reported_P = Reported.P.Value, Computed_P = Computed)
        
        # digits
        resultTable = resultTable %>% 
            mutate(df1 = ifelse(is.na(df1), "-", df1)) %>% 
            mutate(df2 = ifelse(is.na(df2), "-", df2)) %>% 
            rowwise() %>% 
            mutate(df1 = format(df1, digits=3, drop0trailing=TRUE)) %>% 
            mutate(df2 = format(df2, digits=3, drop0trailing=TRUE)) %>% 
            mutate(Reported_P = format(Reported_P, digits=2, nsmall=3, drop0trailing=TRUE)) %>% 
            mutate(Computed_P = format(Computed_P, digits=2, nsmall=3, drop0trailing=TRUE)) %>% 
            ungroup()
        
        # statistics symbols
        resultTable = resultTable %>% 
          mutate(Statistic = ifelse(Statistic == "Chi2", "&chi;<sup>2<sup>", Statistic))
        
        # comparison type
        resultTable = resultTable %>% 
            mutate(Test.Comparison = ifelse(Test.Comparison == "=", "", Test.Comparison)) %>% 
            mutate(Value = paste0(Test.Comparison, "&nbsp;", Value)) %>% 
            mutate(Reported.Comparison = ifelse(Reported.Comparison == "=", "", Reported.Comparison)) %>% 
            mutate(Reported_P = paste0(Reported.Comparison, "&nbsp;", Reported_P))
        
        # make it fancy
        resultTable = resultTable  %>% 
            mutate(Reported_P = ifelse(
                Error & !OneTail,
                paste("<span class='error'>", Reported_P, "</span>"),
                Reported_P
            )) %>% 
            mutate(Computed_P = ifelse(
                Error & !OneTail,
                paste("<span class='error'>", Computed_P, "</span>"),
                Computed_P
            )) %>% 
            mutate(Correct = ifelse(OneTail, "One-tailed", 
             ifelse(
                  Error,
                  "<span class='error'>INCORRECT</span>",
                  "&#10003;"
            ))) %>% 
            rename(" " = Statistic)
        
        # return the table
        resultTable %>% select(-Source, -APAfactor, -Raw, -DecisionError, -Test.Comparison, -Reported.Comparison, -Error, -OneTail, -OneTailedInTxt)
        
    }, striped = TRUE,
    align = "r",
    sanitize.text.function = function(x) {x})
    # # from https://stackoverflow.com/a/51516534/529799
    # sanitize.text.function = function(x) sapply(x, function(x){
    #     xs <- strsplit(as.character(x), "")[[1]]
    #     paste0(sprintf("&#%d;", sapply(xs, utf8ToInt)), collapse="")
    # }))
}

# Run the application 
shinyApp(ui = ui, server = server)
