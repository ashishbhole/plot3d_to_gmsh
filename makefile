.SUFFIXES: .o .f95 .f95

.f95.o:
	$(CPP) $(CPPFLAGS) -P $*.f95 > $*.f95
	$(CFT) -c $(FFLAGS) $*.f95

.f95.o:
	$(CFT) -c $(FFLAGS) $<

.f95.f95:
	$(CPP) $(CPPFLAGS) -P $*.f95 > $*.f95

.f95:
	$(CFT) -o $@ $(FFLAGS) $<

.f95:
	$(CFT) -o $@ $(FFLAGS) $<

         RM = /bin/rm
         CFT = gfortran -fcheck=all -O3 -fdefault-real-8
       

RBIN = conv.exe

SRCS =	main.f95  \
        read_input.f95 \
        check.f95      \
        msh_ascii.f95


OBJS=$(SRCS:.f95=.o)

$(RBIN): ${OBJS}
		${CFT} ${FFLAGS} -o $(RBIN) ${OBJS} ${LIBS}

clean:
		$(RM) -f *.o *.mod *.out *.exe
		

