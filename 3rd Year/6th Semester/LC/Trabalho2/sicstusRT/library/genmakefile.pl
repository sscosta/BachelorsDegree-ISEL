# -*- Perl -*-
# CVS: $Id: genmakefile.pl,v 1.13 2001/12/19 22:37:01 perm Exp $
#
# Generate makefile fragments for inclusion in library/Makefile.
#
# Author: Jesper Eskilson <jojo@sics.se>
# Copyright (C) 1999 SICS
#

if (@ARGV < 3) {
  print STDERR "Too few arguments.\n";
  usage();
  exit(1);
}

## [PM] Mar 2000. Use lone LF, even on Win32 There is no good
##      solution. The problem is as follows: On Win32 cygwin we want
##      to mount samba volumes as binary since they are unix
##      filesystems where lines end in LF. Unfortunately the perl we
##      use does not know this so it translates \n to CRLF which when
##      read from cygwin from a binary file system will still be CRLF
##      which will give an error.  By setting it to binary here perl
##      will use LF also on win32. This will do the right thing for
##      volumes binary mounted by cywin but *not* the right thing on
##      text mounted volumes (e.g., local volumes). Hopefully,
##      however, cygwin will just silently refrain from transforming
##      files where LF is the sole line terminator and everything will
##      work out OK also for text mounted systems. Unix systems should
##      be OK since binmode is a no-op there. I do not even want to
##      think about what will happen on the MacOS where \n is CR.
binmode STDOUT;

$resname=$ARGV[0];
shift;
$srcfile=$ARGV[0];
shift;
$objext=$ARGV[0];
shift;
@srcfiles=@ARGV;

sub usage {
  print STDERR "Usage: genmakefile.pl <resname> <sourcefile> <objext> <sourcefiles>\n";
}

if (@ARGV <= 0) {
  print STDERR "*Warning*: No source files specified. Generating empty makefile.\n";
  exit(0);
}

print STDERR "Generating makefile for resource \"$resname\".\n";
# print STDERR "Source files are: @srcfiles\n";

print "# -*- Mode: Makefile; buffer-read-only:t -*-\n";
print "# Automatically generate makefile for foreign resource \"$resname\".\n";
print "# Do not edit!\n\n";
for $srcfile (@srcfiles) {
  push @d_objfiles, "\$(PLATFORM)/$srcfile"."_d.$objext";
  push @s_objfiles, "\$(PLATFORM)/$srcfile"."_s.$objext";
}

#print "DYN_OBJS += @d_objfiles\n";
#print "STAT_OBJS += @s_objfiles\n";

# [PM] 3.9 inherit (correct) definitions from Common
# print "comma=,\n";
# # [PM] 3.9 this was never right (space would be empty).
# print "space= \n";

print "ifeq (\$($resname"."_d_copt),)\n";
print "$resname"."_d_copt=\$($resname"."_copt)\n";
print "endif\n";

print "ifeq (\$($resname"."_s_copt),)\n";
print "$resname"."_s_copt=\$($resname"."_copt)\n";
print "endif\n";

print "ifeq (\$($resname"."_d_splfr_copt),)\n";
print "$resname"."_d_splfr_copt=\$(patsubst %,--cflag=%\$(space),\$($resname"."_d_copt))\n";
print "endif\n";

print "ifeq (\$($resname"."_s_splfr_copt),)\n";
print "$resname"."_s_splfr_copt=\$(patsubst %,--cflag=%\$(space),\$($resname"."_s_copt))\n";
print "endif\n";


print "ifeq (\$($resname"."_d_splfrflags),)\n";
print "$resname"."_d_splfrflags=\$("."$resname"."_splfrflags)\n";
print "endif\n";

print "ifeq (\$($resname"."_s_splfrflags),)\n";
print "$resname"."_s_splfrflags=\$("."$resname"."_splfrflags)\n";
print "endif\n";


# [PM] 3.9b4 bdb and Tcl/Tk does not say __cdecl in their
# headers. Therefore we must use __cdecl for all code that includes
# those headers.
print "# Special hack to ensure that a -Gr (__fastcall) on CFLAGS can be overridden by $resname" . "_cflags_XXX\n";
print "$resname"."_cflags_static=\$(CFLAGS)\n";
print "$resname"."_cflags_dynamic=\$(CFLAGS)\n";
print "$resname"."_incr_cflags=\$(INCR_CFLAGS)\n";
print "\n";
print "ifeq (\$(WIN32),yes)\n";
print "\n";
print "ifneq (\$(strip \$(filter -Gd /Gd, \$($resname"."_d_copt))),) # /Gd or -Gd (use __cdecl calling convention)\n";
print "$resname"."_cflags_dynamic=\$(filter-out -Gr /Gr, \$(CFLAGS)) # remove any \"use __fastcall\" option\n";
print "$resname"."_incr_cflags=\$(filter-out -Gr /Gr, \$(INCR_CFLAGS)) # remove any \"use __fastcall\" option\n";
print "\n";
print "endif				# -Gd specified\n";
print "\n";
print "ifneq (\$(strip \$(filter -Gd /Gd, \$($resname"."_s_copt))),) # /Gd or -Gd (use __cdecl calling convention)\n";
print "$resname"."_cflags_static=\$(filter-out -Gr /Gr, \$(CFLAGS)) # remove any \"use __fastcall\" option\n";
print "\n";
print "endif				# -Gd specified\n";
print "\n";
print "endif				# WIN32\n";
print "\n";
print "\n";



# [PM] 3.9 added -I$(PLATFORM) and dependency on \$(PLATFORM)/<RESNAME>_glue.h
print "# Build dynamic object files\n";
print "@d_objfiles: \$(PLATFORM)/%_d.$objext: %.c \$(PLATFORM)/$resname" . "_glue.h\n";
print "\t\$(CC) -I\$(PLATFORM) \$($resname"."_cflags_dynamic) \$($resname"."_d_copt) -DSPDLL \$($resname"."_incr_cflags) \$< \$(NOLINK_OPT) \$(NOLINK_OUTPUT_OPT)\$@\n\n";

print "# Build static object files\n";
print "@s_objfiles: \$(PLATFORM)/%_s.$objext: %.c \$(PLATFORM)/$resname" . "_glue.h\n";
print "\t\$(CC) -I\$(PLATFORM) \$($resname"."_cflags_static) \$($resname"."_s_copt) \$< \$(NOLINK_OPT) \$(NOLINK_OUTPUT_OPT)\$@\n\n";

print "# Builds a shared resource\n";
print "\$(PLATFORM)/$resname.\$(SHSFX): $srcfile @d_objfiles\n";
print "\t\@echo\n";
print "\t\@echo \"Building \$@...\"\n";
print "\t\@echo =========================================\n";
print "\t\@echo\n";
# [PM] Do not specify --header here. It is built in a separate (earlier) pass by library/Makefile
#      Actually, specify --header with a dummy name, otherwise it will be generated
print "\t\$(SPLFR) \$(SPLFRFLAGS) --header=$resname"."_tmp_header.h --resource=$resname \$($resname"."_d_splfrflags) \$(filter %.$objext %.pl,\$^) \$($resname"."_lib) \$(SPLFR_CFLAGS) \$($resname"."_d_splfr_copt) -o \$@\n";
print "\trm -f $resname"."_tmp_header.h";

print "\n# Builds a static resource\n";
print "\$(PLATFORM)/$resname.\$(STSFX): $srcfile @s_objfiles\n";
print "\t\@echo\n";
print "\t\@echo \"Building \$@...\"\n";
print "\t\@echo =========================================\n";
print "\t\@echo\n";
# [PM] Do not specify --header here. It is built in a separate (earlier) pass by library/Makefile
print "\t\$(SPLFR) \$(SPLFRFLAGS) --header=$resname"."_tmp_header.h --resource=$resname --static \$($resname"."_s_splfrflags) \$(filter %.$objext %.pl,\$^) \$($resname"."_lib) \$(SPLFR_CFLAGS) \$($resname"."_s_splfr_copt) -o \$@\n";
print "\trm -f $resname"."_tmp_header.h";

print "\n# Debug-target\n";
print "$resname"."_debug:\n";
print "\t\@echo $resname"."_d_copt = \\\"\$($resname"."_d_copt)\\\"\n";
print "\t\@echo $resname"."_s_copt = \\\"\$($resname"."_s_copt)\\\"\n";
print "\t\@echo $resname"."_d_splfr_copt = \\\"\$($resname"."_d_splfr_copt)\\\"\n";
print "\t\@echo $resname"."_s_splfr_copt = \\\"\$($resname"."_s_splfr_copt)\\\"\n";
print "\n";

