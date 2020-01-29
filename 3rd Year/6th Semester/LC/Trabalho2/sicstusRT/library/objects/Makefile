#
# Copyright (C) 1988-1998 Swedish Institute of Computer Science.
#
# CVS: $Id: Makefile,v 1.10 1999/08/25 14:27:24 jojo Exp $
#
# NOTE: This Makefile requires GNU make.
#

SRCDIR=.

# Include common Makefile parts.
include $(SRCDIR)/../../Common

# LIB_PATH=`pwd`/..
LIBDIR=$(LIB_PATH)

.SUFFIXES:

ENV= expand.ql runlib.ql object.ql

$(ENV): %.ql: %.pl hidemod.pl
	-echo "prolog_flag(compiling,_,$(PLFLAGS))," \
		"use_module(library(objects))," \
		"['$(filter %hidemod.pl,$^)']," \
		"fcompile(objects:'$<')." | $(SICSTUS) -f
	mv $(patsubst %.pl,%.ql,$<) .


all-post-recursive: $(ENV)

install:
	$(MKDIR) $(LIBDIR)/objects
	-@CP@ $(ENV) $(LIBDIR)/objects

clean:
	rm -f $(ENV)

expand.ql: expand.pl objects.pl
runlib.ql: runlib.pl objects.pl
object.ql: object.pl objects.pl expand.pl


