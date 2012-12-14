TARGET=foldbanner

SRCDIR=src
MAINSRC=$(SRCDIR)/Main.hs

all:
	ghc --make -i$(SRCDIR) $(MAINSRC) -o $(TARGET)

clean:
	rm $(SRCDIR)/*.hi $(SRCDIR)/*.o $(TARGET)
