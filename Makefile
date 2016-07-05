all: Presentation.html

Presentation.html:Presentation.Rmd revealOpts.css
	Rscript -e "rmarkdown::render('$<')"
  
Presentation.Rmd:GandL.rda bernoulli.rda bibliography.bib
	touch Presentation.Rmd

bernoulli.rda:bernoulli.R bernoulli.stan
	Rscript -e "source('bernoulli.R')"

GandL.rda:GandL_Simulate.rda
	Rscript -e "source('GandLfit.R')"

GandL_Simulate.rda:GandL_Simulate.R
	Rscript "GandL_Simulate.R"

clean:
	rm Presentation.html