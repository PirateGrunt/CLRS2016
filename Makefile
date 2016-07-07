all: Presentation.html

Presentation.html:Presentation.Rmd revealOpts.css
	Rscript -e "rmarkdown::render('$<')"
  
Presentation.Rmd:GandL_Fit.rda HierarchicalFit.rda bernoulli.rda bibliography.bib Claims.rda
	touch Presentation.Rmd

bernoulli.rda:bernoulli.R stan/bernoulli.stan
	Rscript -e "source('bernoulli.R')"

GandL_Fit.rda:GandL_Simulate.rda GandL_Fit.R
	Rscript -e "source('GandL_Fit.R')"

GandL_Simulate.rda:GandL_Simulate.R
	Rscript "GandL_Simulate.R"
	
HierarchicalFit.rda:HierarchicalFit.R
	Rscript "HierarchicalFit.R"
	
Claims.rda:Claims.R stan/Claims.stan
	Rscript "Claims.R"

clean:
	rm Presentation.html