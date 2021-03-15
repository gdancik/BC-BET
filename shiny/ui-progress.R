############################################################################
# Functions for working with progress bars
#   pleaseWait() - generates div containing progress bar, which will
#      cover the screen; toggle this 'on' and 'off' by adding removing
#      the 'hide' class
############################################################################

pleaseWait <- function() {
  HTML(paste0("<div id = 'please-wait' class=\"progress hide\">",
              "<div id = 'please-wait-progress' class=\"progress-bar progress-bar-striped active\" role=\"progressbar\" aria-valuenow=\"100\"
                    aria-valuemin=\"0\" aria-valuemax=\"100\" style=\"opacity: 1; width:100%\" !important>",
                                  "<span style = \"display:inline-block; margin-top: 5px; font: 1.1em\"><b>Please wait ...</b></span>",
              "</div></div>")
       )
}

