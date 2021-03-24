
role <- function(r) {
  span(class = 'label label-primary', r)
}

# create "About" page
tabAbout = tabPanel("About",
  div(class = "panel panel-default",

    ## citation
      div(class = 'panel-heading', 
          h3(class='panel-title', style = 'font-weight: bold', 
             "Citation")
      ),
      div(class = "panel-body", style = 'font-size:110%;',
        HTML("If you use <i>BC-BET</i> in your work, please cite the following publication", 
        "(<a href = 'http://biomedcentral.com/1471-2490/15/59'>link</a>):</br></br>",
        "Dancik, G.M. An online tool for evaluating diagnostic and prognostic gene 
         expression biomarkers in bladder cancer. <i>BMC Urol</i> 2015, 15:59.")
        ),
        
      
    ## contributors  
    div(class = 'panel-heading', 
        h3(class='panel-title', style = 'font-weight: bold', 
           "Contributors")
    ),
    div(class = "panel-body",
        HTML("<ul>"),
        HTML("<li>"),
        p(span(style='font-weight:bold', "Garrett M. Dancik, PhD"),
         "is an Associate Professor of Computer Science / ",
         a(href = "https://gdancik.github.io/bioinformatics/", "Bioinformatics"), "at ",
         a(href = "https://easternct.edu", "Eastern Connecticut State University"), "(Wilimantic, CT).", 
         role("Maintainer"),
         role("Contributor")
         ),
        HTML("</li>"),
        
        HTML("<li>"),
        p(span(style='font-weight:bold', "John Doe"), "is a ...", 
          role("Contributor")
        ),
        
        HTML("</li>"),
        HTML("</ul>")
    )

    
  )
)

