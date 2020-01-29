#
# Copyright (C) 1988-1999 Swedish Institute of Computer Science.
#
# CVS: $Id: Makefile,v 1.1 1999/09/03 14:00:48 jojo Exp $
#
# NOTE: This Makefile requires GNU make.
#

SRCDIR=.

# Include common Makefile parts.
include $(SRCDIR)/../../Common

.SUFFIXES:
.SUFFIXES: $(EXEEXT) .c .$(OBJEXT)

CFLAGS += -DSICSTUS $(BDBINC) -I$(SRCDIR)/../../Emulator

OBJ=$(patsubst %,%.$(OBJEXT),bdb dbaux hash ixkeys)

.PHONY: clean realclean

OBJ_STAT= $(OBJ:.$(OBJEXT)=_s.$(OBJEXT))
OBJ_DYN= $(OBJ:.$(OBJEXT)=_d.$(OBJEXT))
OBJ_ALL= $(OBJ_STAT) $(OBJ_DYN)

all-recursive: $(OBJ_ALL)

bdb.c: bdb.h dbaux.h
dbaux.c: dbaux.h

$(OBJ_STAT): %_s.$(OBJEXT): %.c
$(OBJ_DYN): %_d.$(OBJEXT): %.c 

$(OBJ_STAT):
	$(CC) $(CFLAGS) $(NOLINK_OPT) $(filter %.c,$^) $(NOLINK_OUTPUT_OPT)$@

$(OBJ_DYN):
	$(CC) $(CFLAGS) $(INCR_CFLAGS) $(NOLINK_OPT) $(filter %.c,$^) $(NOLINK_OUTPUT_OPT)$@

dbtest$(EXEXT): %$(EXEEXT): $*.$(OBJEXT)
	$(CC) $(CFLAGS) $< $(BDBLIB) $(OUTPUT_OPT)$@

bug$(EXEEXT): %$(EXEEXT): $*.c
	$(CC) $(CFLAGS) -DBUG $< $(BDBLIB) $(OUTPUT_OPT)$@

clean:
	$(RM) -f $(OBJ) bdb.$(SHSFX)

