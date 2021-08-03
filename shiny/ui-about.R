
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
        HTML("<p>If you use <i>BC-BET</i> in your work, please <span style = 'color:red'><b>cite</b></span> the following publication", 
        "(<a href = 'http://biomedcentral.com/1471-2490/15/59'>link</a>):",
        "<ul><li><b>Dancik, G.M. An online tool for evaluating diagnostic and prognostic gene 
         expression biomarkers in bladder cancer. <i>BMC Urol</i> 2015, 15:59.</b></li></ul><p>")
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
        p(span(style='font-weight:bold', "Christopher Kelly"), "graduated from ", 
          a(href = "https://easternct.edu", "Eastern Connecticut State University"), "(Wilimantic, CT) with a B.S. in Computer Science.", 
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
        HTML("<p><i>BC-BET</i> was inspired by postdoctoral work carried out under <a href = 'https://bio.cedars-sinai.org/theodorescud/index.html'>Dan Theodorescu</a>, MD, PhD, who is currently director of the <a href = 'https://www.cedars-sinai.org/locations/samuel-oschin-comprehensive-cancer-institute-150.html'>Samuel Oschin Comprehensive Cancer Center</a> at the Cedars-Sinai Health System in Los Angeles.</li>",
             "This work was supported, in part, by grants from the American Association of University Professors and Connecticut State University Board of Regents (DANR14 and YRDA01).</p>")
    )
    
  )
)

