
if GCC

bin_PROGRAMS = f951 g95

f951_SOURCES = arith.c array.c bbt.c check.c convert.c decl.c		\
dependency.c error.c expr.c format.c g95.h interface.c intrinsic.c	\
intrinsic.h io.c iresolve.c lang-options.h match.c matchexp.c misc.c	\
module.c options.c parse.c primary.c resolve.c scanner.c scalarize.c	\
select.c simplify.c st.c support.c symbol.c trans-array.c trans-decl.c	\
trans-intrinsic.c trans-stmt.c trans.c trans-const.c trans-expr.c	\
trans-io.c trans-types.c f95-lang.c f95-tree.c

f951_LDADD = @GCC_DIR@/gcc/main.o @GCC_DIR@/gcc/c-dump.o \
             @GCC_DIR@/gcc/c-pretty-print.o

g95_SOURCES = g95spec.c

g95_LDADD = @GCC_DIR@/gcc/gcc.o @GCC_DIR@/gcc/version.o @GCC_DIR@/gcc/intl.o \
            @GCC_DIR@/gcc/prefix.o


else

bin_PROGRAMS = g95

g95_SOURCES = arith.c array.c bbt.c check.c data.c decl.c error.c	\
expr.c format.c g95.h interface.c intrinsic.c intrinsic.h io.c		\
iresolve.c lang-options.h match.c matchexp.c misc.c module.c options.c	\
parse.c primary.c resolve.c scalarize.c scanner.c select.c simplify.c	\
st.c symbol.c

endif

EXTRA_DIST = MANIFEST CONTRIB BUGS autogen.sh
