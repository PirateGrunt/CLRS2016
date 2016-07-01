all: Presentation.html

Presentation.html:Presentation.Rmd revealOpts.css
	Rscript -e "rmarkdown::render('$<')"
  
Presentation.Rmd:GandL.rda bernoulli.rda

GandL.rda:
	Rscript -e "source('GandLfit.R')"

bernoulli.rda:bernoulli.R bernoulli.stan
	Rscript -e "source('bernoulli.R')"

clean:
	rm Presentation.html