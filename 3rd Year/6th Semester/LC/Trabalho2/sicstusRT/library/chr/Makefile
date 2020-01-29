#
# Copyright (C) 1988-1998 Swedish Institute of Computer Science.
#
# CVS: $Id: Makefile,v 1.3 1999/09/03 14:01:08 jojo Exp $
#
# NOTE: This Makefile requires GNU make.
#

SRCDIR=.

# Include common Makefile parts.
include $(SRCDIR)/../../Common

LIB_PATH1=$(TOP_SRCDIR)/library/chr
LIB_PATH2=$(TOP_SRCDIR)/library/$(PLATFORM)

.SUFFIXES:
.SUFFIXES: .$(OBJEXT) .c .ql .pl

#.c.$(OBJEXT):
#	$(CC) $(CFLAGS) -I$(SP_INCDIR) -c $^

#.pl.ql:
#	-echo "asserta(library_directory('$(LIB_PATH)')), \
#		prolog_flag(compiling,_,$(PLFLAGS)), \
#		ensure_loaded(compenv), \
#	        ['../hidemod'], \
#		env_fcompile($*), halt." | $(SICSTUS) -f

QL= matching.ql getval.ql ordering.ql concat.ql operator.ql sbag_l.ql \
	chrcmp.ql trace.ql

$(QL): %.ql: %.pl hidemod.pl compenv.pl
	-echo "asserta(library_directory('$(LIB_PATH1)')),\
		asserta(library_directory('$(LIB_PATH2)')), \
		prolog_flag(compiling,_,$(PLFLAGS)), \
		ensure_loaded(library(compenv)), \
	        ['$(filter %hidemod.pl,$^)'], \
		env_fcompile(library($*)), halt." | $(SICSTUS) -f
	mv $(patsubst %.pl,%.ql,$<) .

# all-recursive: chr.ql
all-post-recursive: $(QL) sbag.ql

# all: chr.$(OBJEXT) chr.$(SHSFX) chr.$(STSFX) $(QL) chr.ql

#chr.$(SHSFX):	chr.c chr.ql
#	$(SPLFR) $(SPLFR_FLAGS) chr.pl chr.c

#chr.$(STSFX):	chr.c chr.ql
#	$(SPLFR) $(SPLFR_FLAGS) --static chr.pl chr.c

chr.ql $(QL): compenv.pl

sbag.ql: sbag_l.ql
	$(RM) -f $@; $(LN) $^ $@

TAGS: *.pl examples/*.pl
	etags -l prolog *.pl examples/*.pl

install:
	$(MKDIR) $(prefix)/lib/$(SP_DIRNAME)/library/chr
	for file in chr.$(SHSFX) chr.$(STSFX) chr.ql $(QL); do \
		echo "Installing $$file to $(prefix)/lib/$(SP_DIRNAME)/library/$(PLATFORM)/chr..."; \
		$(INSTALL_DATA) $(SRCDIR)/../$(PLATFORM)/$$file \
		   $(prefix)/lib/$(SP_DIRNAME)/library/$(PLATFORM)/chr ;\
	done

#	$(MKDIR) $(LIBDIR)/chr
#	-$(CP) chr.$(SHSFX) chr.$(STSFX) chr.ql $(LIBDIR)
#	-$(CP) $(QL) $(LIBDIR)/chr
#	-$(CP) -r examples $(LIBDIR)/chr

print:
	nenscript -Php4 -fCourier8 chr.pl 
	nenscript -Php4 -fCourier8 chrcmp.pl 
	nenscript -Php4 -fCourier8 trace.pl 

clean:
	-rm -f sbag.pl *.$(OBJEXT) *.$(SHSFX) *.$(STSFX) *.ql TAGS

