
role <- function(r) {
  span(class = 'label label-primary', r)
}

# create "About" page
tabAbout = tabPanel("About",
  div(class = "panel panel-default",

    ## citation
      div(class = 'panel-heading', 
          h3(class='panel-title', style = 'font-weight: bold', 
             "Publications")
      ),
      div(class = "panel-body",
        HTML("If you use <i>BC-BET</i> in your work, please cite the following publication", 
        "(<a href = 'http://biomedcentral.com/1471-2490/15/59'>link</a>):</br></br>",
        "<ul><li>Dancik, G.M. An online tool for evaluating diagnostic and prognostic gene 
         expression biomarkers in bladder cancer. <i>BMC Urol</i> 2015, 15:59.</li></ul>")
        ),
        
      
    ## contributors  
    div(class = 'panel-heading', 
        h3(class='panel-title', style = 'font-weight: bold', 
           "Primary Contributors")
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
        HTML("</ul>")
    ), # end panel body
    
    ## contributors  
    div(class = 'panel-heading', 
        h3(class='panel-title', style = 'font-weight: bold', 
           "Additional Contributors")
    ),
    div(class = "panel-body",
        HTML("<ul>"),
        HTML("<li>"),
        p(span(style='font-weight:bold', "Christopher Kelly"), "is an undergraduate Computer Science major, ", 
          a(href = "https://easternct.edu", "Eastern Connecticut State University"), "(Wilimantic, CT).", 
          role("Contributor")
        ),
        HTML("</li>"),
        HTML("</ul>")
    ), # end panel body

    div(class = 'panel-heading', 
        h3(class='panel-title', style = 'font-weight: bold', 
           "Acknowledgements")
    ),
    div(class = "panel-body",
        HTML("<i>BC-BET</i> was inspired by postdoctoral work carried out under Dan Theodorescu, MD., PhD., who is currently director of the Comprehensive Cancer Center at the Cedars-Sinai Health System in Los Angeles.")
    )
    
  )
)

