CC=gcc
CFLAGS=-c -g -pedantic -Wall -Wwrite-strings -Wstrict-prototypes
GMPDIR=gmp
LDFLAGS=-g
LIBS=-lgmp
RM=rm -f

EXE=g95

OBJS=module.o matchexp.o format.o io.o scanner.o error.o parse.o expr.o \
     primary.o symbol.o arith.o match.o st.o intrinsic.o array.o interface.o \
     misc.o decl.o select.o simplify.o

g95: $(OBJS)
	@$(CC) -c date.c
	$(CC) $(LDFLAGS) -o $@ date.o $(OBJS) -L$(GMPDIR)/.libs $(LIBS)
	@rm date.o

intrinsic.o: intrinsic.c simplify.h g95.h
	$(CC) $(CFLAGS) -I$(GMPDIR) intrinsic.c

simplify.o: simplify.c simplify.h g95.h
	$(CC) $(CFLAGS) -I$(GMPDIR) simplify.c

%.o: %.c g95.h
	$(CC) $(CFLAGS) -I$(GMPDIR) $<

clean:
	$(RM) $(EXE) *.o

distclean: clean
	$(RM) core *~ *.orig *.rej doc/*~

package:
	tar cvzf g95.tgz `cat MANIFEST`
