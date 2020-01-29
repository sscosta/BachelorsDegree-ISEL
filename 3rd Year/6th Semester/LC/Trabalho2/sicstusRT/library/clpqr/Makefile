#
# Copyright (C) 1988-1998 Swedish Institute of Computer Science.
#
# CVS: $Id: Makefile,v 1.5 1999/08/25 14:27:01 jojo Exp $
#
# NOTE: This Makefile requires GNU make.
#

SRCDIR=.

# Include common Makefile parts.
include $(SRCDIR)/../../Common

# LIB_PATH=`pwd`/..
LIBDIR=$(LIB_PATH)

SRC= expand.pl monash.pl printf.pl examples

all:

install:
	$(MKDIR) $(LIBDIR)/clpqr
	-@CP@ -r $(SRC) $(LIBDIR)/clpqr

clean:
