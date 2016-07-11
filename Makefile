.PHONY : all clean

all :
	ghc -O2 --make *.hs -o main -threaded -rtsopts

clean :
	rm main *.hi *.o *~ .pce.hs.swp .pce.hs.swo .main.hs.swp .pce.hs.swo  

