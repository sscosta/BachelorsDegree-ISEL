#
# Copyright (C) 1988-1998 Swedish Institute of Computer Science.
#
#
# NOTE: This Makefile requires GNU make.
#

SRCDIR=.

all: run_simple1 run_queens

# Include common Makefile parts.
include $(SRCDIR)/../../../Common

ifeq ($(WIN32),no)
SP_ROOTLIBDIR=$(prefix)/lib
else
SP_ROOTLIBDIR=$(SP_BINDIR)
endif

#[PD] 3.8.7 no foreign resources in example
#targets: jqueens.po train.po simple.po simple.$(SHSFX) Queens.class Simple.class
targets: jqueens.po train.po simple.po Queens.class Simple.class

.PHONY: run_queens run_simple1 install clean

.SUFFIXES: .pl .po .class
.pl.po:
	-echo "compile('$<'), save_files(['$^'],'$@'), halt." | $(SICSTUS) -f -m

%.class: %.java
	$(JAVAC) -verbose -deprecation -classpath $(SP_BINAUXDIR)/jasper.jar:. $^

# Building targets
#[PD] 3.8.7 no foreign resources in example
#simple.so: simple.pl
#	$(SPLFR) $(SPLFR_FLAGS) simple.pl

run_queens: jqueens.po Queens.class
	$(JAVAEXEC) $(JAVA_EXECOPTS) \
		-classpath $(SP_BINAUXDIR)/jasper.jar:. \
		-Djava.library.path=$(SP_ROOTLIBDIR) \
		Queens


#[PD] 3.8.7 no foreign resources in example
#run_simple1: Simple.class simple.po simple.so train.po
run_simple1: Simple.class simple.po train.po
	$(JAVAEXEC) $(JAVA_EXECOPTS)  \
		-classpath $(SP_BINAUXDIR)/jasper.jar:. \
		-Djava.library.path=$(SP_ROOTLIBDIR) \
		Simple



clean:
	-rm -f *~ *.o *.$(SHSFX) *.class *.ql *.po *.sav




