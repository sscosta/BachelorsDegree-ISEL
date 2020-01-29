#
# Copyright (C) 1988-1998 Swedish Institute of Computer Science.
#
# CVS: $Id: Makefile,v 1.11 1999/08/25 14:27:00 jojo Exp $
#
# NOTE: This Makefile requires GNU make.
#

SRCDIR=.

# Include common Makefile parts.
include $(SRCDIR)/../../Common

LIB_PATH=$(TOP_SRCDIR)/library/$(PLATFORM)

.SUFFIXES:
.SUFFIXES: .ql .pl

COMMONSRC= compenv.pl arith.pl bb.pl bv.pl dump.pl fourmotz.pl \
	   ineq.pl itf3.pl nf.pl ordering.pl project.pl redund.pl store.pl

ENV=	compenv.ql arith.ql bb.ql bv.ql class.ql dump.ql fourmotz.ql \
	geler.ql ineq.ql itf3.ql nf.ql nfq.ql ordering.ql project.ql redund.ql \
	store.ql arith_q.ql

ARITH= arith_q.pl arith.pl
ATTS= itf3.pl

TARGET_ENV= $(patsubst %,$(QLDIR)/%,$(ENV))

$(TARGET_ENV): $(QLDIR)/%.ql: %.pl compenv.pl hidemod.pl
	-echo "asserta(library_directory('$(LIB_PATH)')), \
		prolog_flag(compiling,_,$(PLFLAGS)), \
		ensure_loaded(compenv), \
	        ['$(filter %hidemod.pl,$^)'], \
		env_fcompile($*,arith_q), halt." | $(SICSTUS) -f
	mv $*.ql $@

$(COMMONSRC):
	$(RM) -f $@; $(LN) ../clpqr/$@ .

all-post: $(COMMONSRC) $(TARGET_ENV)

install:
	$(MKDIR) $(LIBDIR)/clpq
	-@CP@ $(ENV) $(LIBDIR)/clpq

arith_q.ql:	$(ARITH)
bb.ql:		$(ATTS) $(ARITH)
bv.ql:		$(ATTS) $(ARITH)
dump.ql:	$(ATTS)
fourmotz.ql:	$(ATTS) $(ARITH)
ineq.ql:	$(ATTS) $(ARITH)
itf3.ql:	$(ATTS) $(ARITH)
nf.ql:		$(ARITH)
project.ql:	$(ATTS) $(ARITH)
redund.ql:	$(ATTS) $(ARITH)
store.ql:	$(ATTS) $(ARITH)


clean:
	rm -f $(ENV)


