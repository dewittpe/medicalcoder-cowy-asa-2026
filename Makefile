RVANILLA := R --vanilla --quiet
RSCRIPTVANILLA := Rscript --vanilla --quiet
QUARTO := quarto

.PHONY: all clean

all: slides.html

.deps:
	$(RSCRIPTVANILLA) -e "pkgs <- c('data.table', 'arrow', 'medicalcoder', 'ggh4x', 'ggpubr', 'gt')"\
		-e "missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]"\
		-e "if (length(missing)) { message('Installing missing R packages: ', paste(missing, collapse = ', ')); install.packages(missing, repos = 'https://cloud.r-project.org'); }"
	@touch $@

slides.html: slides.qmd slides.scss rstudio_default-light.theme mimiciv.feather
	$(QUARTO) render $<

mimiciv.feather: mimiciv-data-prep.R .deps
	$(RSCRIPTVANILLA) $<

clean:
	$(RM) .deps
	$(RM) .mimiciv.feather
	$(RM) slides.html
	$(RM) -r slides_cache/
