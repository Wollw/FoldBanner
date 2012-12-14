TARGET=foldbanner

all:
	ghc $(TARGET)

clean:
	rm *.hi *.o $(TARGET)
