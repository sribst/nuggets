TARGET = resume

default: pdf

pdf:
	pdflatex $(TARGET).tex


watch: pdf
	evince $(TARGET).pdf

clean:

	@rm -f $(TARGET).{aux,dvi,toc,bbl,blg,log,out}
	@rm -f *~
