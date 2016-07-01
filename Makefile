all: Presentation.html

Presentation.html:Presentation.Rmd revealOpts.css
	Rscript -e "rmarkdown::render('$<')"
  
Presentation.Rmd:GandL.rda

GandL.rda:
	Rscript -e "source(GandLfit.R)"

clean:
	rm Presentation.html