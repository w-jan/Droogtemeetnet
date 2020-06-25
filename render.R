# Render html_document2 with both params at their default value defined in the
# YAML header:
bookdown::render_book("index.Rmd", 
                      "bookdown::html_document2")

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
