#
# Copyright (C) 1988-1998 Swedish Institute of Computer Science.
#
# CVS: $Id: Makefile,v 1.7 1999/08/25 14:27:22 jojo Exp $
#
# NOTE: This Makefile requires GNU make.
#

SRCDIR=.

# Include common Makefile parts.
include $(SRCDIR)/../../Common

.SUFFIXES:

ENV= server.ql client.ql

$(ENV): %.ql: %.pl hidemod.pl
	-echo "prolog_flag(compiling,_,$(PLFLAGS)), \
	       ['$(filter %hidemod.pl,$^)'], \
	       fcompile('$<'), halt."  | $(SICSTUS) -f
	mv $(patsubst %.pl,%.ql,$<) .	


all-post-recursive: $(ENV)

install:
	$(MKDIR) $(LIBDIR)/linda
	-@CP@ $(ENV) $(LIBDIR)/linda

clean:
	rm -f $(PLATFORM) $(ENV)
