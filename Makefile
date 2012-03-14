FC = gfortran

FFLAGS = -O0 -Wall -fbounds-check -Wno-uninitialized -g3
HFLAGS = -O2 -W -Wall

LBFGSB  = lbfgsb.f
LINPACK = linpack.f
BLAS    = blas.f
TIMER   = timer.f

SRCS = lbfgsb.f linpack.f blas.f timer.f

FOBJS = $(patsubst %.f,%.o,$(SRCS))

test : Test
	./Test

Test : $(FOBJS) Makefile *.hs
	ghc --make $(HFLAGS) $@ $(FOBJS) -lgfortran

all :  $(FOBJS)

%.o : %.f Makefile
	$(FC) -c $(FFLAGS) $< -o $@

clean :
	rm -f $(FOBJS)
	rm -f *.{o,hi}

.PHONY: all clean
