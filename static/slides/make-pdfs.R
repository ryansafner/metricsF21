# make-pdfs
pagedown::chrome_print(input = "https://metricsf21.classes.ryansafner.com/slides/3.6-slides.html",
                       output = "static/slides/3.7-slides.pdf",
                       timeout = 300)

# create png of first slide
pagedown::chrome_print(input = "https://metricsf21.classes.ryansafner.com/slides/3.6-slides.html",
                       output = "static/slides/3.7-slides.png",
                       selector = "div.remark-slide",
                       format = "png",
                       timeout = 300)

