## NOTE ALSO THE EXISTENCE OF CACHE PARAMETERS WHICH CAN BE PASSED (see YAML header)!!

# With both params at their default value defined in the YAML header:
    # Render html_document2 :
bookdown::render_book("index.Rmd", 
                      "bookdown::html_document2")

    # Render pdf_document2:
bookdown::render_book("index.Rmd", 
                      "bookdown::pdf_document2")

    # Render all defined formats:
bookdown::render_book("index.Rmd", 
                      "all")

# Render html_document2 with both params at value 1:
bookdown::render_book("index.Rmd", 
                      "bookdown::html_document2", 
                      params = list(refresh_data = 1,
                                    refresh_figures = 1))

# Render html_document2 with both params at value 2:
bookdown::render_book("index.Rmd", 
                      "bookdown::html_document2", 
                      params = list(refresh_data = 2,
                                    refresh_figures = 2))
