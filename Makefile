CC=gcc
CFLAGS=-c -g -pedantic -Wall -pipe
GMPDIR=gmp
LDFLAGS=-g
LIBS=-lgmp
RM=rm -f

EXE=g95

OBJS=module.o matchexp.o format.o io.o scanner.o error.o parse.o expr.o \
     primary.o symbol.o arith.o match.o st.o intrinsic.o array.o interface.o \
     misc.o decl.o select.o

%.o: %.c g95.h
	$(CC) $(CFLAGS) -I$(GMPDIR) $<

g95: $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $(OBJS) -L$(GMPDIR)/.libs $(LIBS)

clean:
	$(RM) $(EXE) *.o

distclean: clean
	$(RM) core *~ *.orig *.rej doc/*~

package:
	tar cvzf g95.tgz `cat MANIFEST`
