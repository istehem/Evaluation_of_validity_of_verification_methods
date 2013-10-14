#!/usr/bin/perl
#
#   Copyright Mecel AB 2010
#   Box 14044
#   400 20 GOTHENBURG
#   SWEDEN
#   Phone: +46 (0)31 720 44 00
#   The copyright of the computer program(s) herein is the property of
#   Mecel AB. The programs(s) may be used and/or copied only with the
#   written permission of Mecel AB or in accordance with the terms and
#   conditions stipulated in the agreement/contract under which the
#   program(s) have been supplied.
#
# Creates a MemMap.h with chosen sections and <MSN>> name.
# Just assign $shortName and $longName the name of the module and 
# comment out those sections not to be used.
#

# Name of the module
my $shortName = "CANIF";
my $longName = "CAN Interface";

# The memory sections to be used.
#
# Field 0: Name of the memory section
# Field 1: If alignments (8BIT, 16BIT, etc) shall be used (1) or not (0)
# Field 2: Comment describing the usage of the memory section
#
# Comment out those that not apply to the module.
my $index = 0;
my @memorySections;
$memorySections[$index++] = ["CODE",                     0, "To be used for mapping code to application block, boot block, external flash, etc."];
# $memorySections[$index++] = ["CALLOUT_CODE",             0, "To be used for mapping callouts of the BSW Modules."];
# $memorySections[$index++] = ["VAR_NOINIT",               1, "To be used for all global or static variables that are never initialized."];
# $memorySections[$index++] = ["VAR_POWER_ON_INIT",        1, "To be used for all global static variables that are\n * initialized with values only after power on reset."]; 
# $memorySections[$index++] = ["VAR_POWER_ON_CLEARED",     1, "To be used for all global or static variables that are cleared to zero only after power on reset."];
$memorySections[$index++] = ["VAR",                      1, "To be used for global or static variables that are initialized with values after every reset."];
# $memorySections[$index++] = ["VAR_CLEARED",              1, "Static variables that are cleared to zero after every reset (the normal case)."];
 $memorySections[$index++] = ["VAR_FAST",                 1, "To be used for all global or static variables that have least one of the following properties:\n *   * accessed bitwise\n *   * frequently used\n *   * high number of accesses in source code"];
# $memorySections[$index++] = ["VAR_FAST_CLEARED",         1, "To be used for all global or static variables that have least one of the following properties:\n *   * accessed bitwise\n *   * frequently used\n *   * high number of accesses in source code"];
# $memorySections[$index++] = ["INTERNAL_VAR",             1, "To be used for global or static variables that are accessible from a calibration tool and initialized with values after every reset."];
# $memorySections[$index++] = ["INTERNAL_VAR_CLEARED",     1, "To be used for global or static variables that are accessible from a calibration tool and cleared to zero after every reset."];
# $memorySections[$index++] = ["VAR_SAVED_ZONE",           1, "To be used for RAM buffers of variables saved in non volatile memory."];
# $memorySections[$index++] = ["VAR_SAVED_RECOVERY_ZONE",  0, "To be used for ROM  buffers of variables saved in non volatile memory."];
$memorySections[$index++] = ["CONST",                    1, "To be used for global or static constants."];
# $memorySections[$index++] = ["CALIB",                    1, "To be used for calibration constants."];
# $memorySections[$index++] = ["CARTO",                    1, "To be used for cartography constants."];
$memorySections[$index++] = ["CONFIG_DATA",              1, "Constants with attributes that show that they reside in one segment for module configuration."];
$memorySections[$index++] = ["CONST_POSTBUILD",          1, "To be used for global or static constants that resides in the postbuild area."],
$memorySections[$index++] = ["VAR_POSTBUILD",            1, "To be used for global or static variables that are initialized with values after every reset that resides in the postbuild area."];

# The alignments to the memory sections with 1 above.
# Currently not possible to decrease the number of alignments for a specific section.
my @alignments = ("BOOLEAN", "8BIT", "16BIT", "32BIT", "UNSPECIFIED");



PrintHeader();

# Create the <MSN> sections
foreach $memorySection (@memorySections)
{
   # Print preceding comment about the memory section
   print "/*\n";
   print " * $memorySection->[0] section\n";
   print " * $memorySection->[2]\n";
   print " */\n";

   if ($memorySection->[1])
   {
      # Append alignment
      foreach $alignment (@alignments)
      {
         PrintMsnSection("$memorySection->[0]_$alignment");
      }
   }
   else
   {
      PrintMsnSection($memorySection->[0]);
   }
}

PrintMiddleSection();

# Create the default sections
foreach $memorySection (@memorySections)
{
   # Print preceding comment about the memory section
   print "/*\n";
   print " * $memorySection->[0] section\n";
   print " * $memorySection->[2]\n";
   print " */\n";

   if ($memorySection->[1])
   {
      # Append alignment
      foreach $alignment (@alignments)
      {
         PrintDefaultSection("$memorySection->[0]_$alignment");
      }
   }
   else
   {
      PrintDefaultSection($memorySection->[0]);
   }
}

PrintFooter();


#
# Some support functions
#

# Print the <MSN> section, input is a string with the section name
sub PrintMsnSection
{
   my ($sectionName) = @_;
   
   print "#elif defined (" . $shortName . "_START_SEC_$sectionName)\n";
   print "   #undef      " . $shortName . "_START_SEC_$sectionName\n";
   print "   #define DEFAULT_START_SEC_$sectionName\n";
   print "#elif defined (" . $shortName . "_STOP_SEC_$sectionName)\n";
   print "   #undef      " . $shortName . "_STOP_SEC_$sectionName\n";
   print "   #define DEFAULT_STOP_SEC_$sectionName\n\n";
}

# Print the default section, input is a string with the section name
sub PrintDefaultSection
{
   my ($sectionName) = @_;

   print "#elif defined (DEFAULT_STOP_SEC_$sectionName)\n";
   print "   #ifdef DEFAULT_START_SEC_$sectionName\n";
   print "      #undef DEFAULT_START_SEC_$sectionName\n";
   print "      #undef SECTION_STARTED\n";
   print "      #undef DEFAULT_STOP_SEC_$sectionName\n";
   print "   #else\n";
   print "      #error \"MemMap.h: DEFAULT_STOP_SEC_$sectionName has not been previously started.\"\n";
   print "   #endif\n";
   print "#elif defined (DEFAULT_START_SEC_$sectionName)\n";
   print "   #ifndef SECTION_STARTED\n";
   print "      #define SECTION_STARTED\n";
   print "   #else\n";
   print "      #error \"MemMap.h: DEFAULT_START_SEC_$sectionName has already been started.\"\n";
   print "   #endif\n\n";
}

sub PrintHeader
{
   print "/**\n";
   print " * MISRA violation (rule 87): Exception allowed; construct is required\n";
   print " * per AUTOSAR Memory Mapping.  This header file does not contain the\n";
   print " * typical construct to prevent multiple inclusion.  This is intentional.\n";
   print " * It is necessary to satisfy the requirements for MemMap.h usage.\n";
   print " */\n";
   print "\n";
   print "/** \\file\n";
   print " *\n";
   print " * \\brief Memory Mapping header file for Picea.\n";
   print " *\n";
   print " * This file must be included by all Picea source code files, in order\n";
   print " * to implement memory mapping as required in AUTOSAR_SWS_MemoryMapping.pdf.\n";
   print " *\n";
   print " * This is a template MemMap.h file that is to be copied and used by developers\n";
   print " * during module developement. Using this MemMap.h will find missing sections\n";
   print " * definitions as well as unmatched section pairs.\n";
   print " *\n";
   print " * \\b Application:        na \\n\n";
   print " * \\b Target:             WIN32/iX86 \\n\n";
   print " * \\b Compiler:           na \\n\n";
   print " * \\b Autosar-Vendor-ID:  TBD \\n\n";
   print " *\n";
   print " * \\b Module:             MemMap.h \\n\n";
   print " *\n";
   print " * \\b Time of creation:   2011-03-02 15:33:36\n";
   print " * \\b File-Revision:      \$Revision:/main/14 \$\\n\n";
   print " * \\b Changeable-by-user: Yes \\n\n";
   print " * \\b Delivery-File:      No \\n\n";
   print " *\n";
   print " * \\b Module-Owner:       Mecel Picea Team \\n\n";
   print " * \\b Location:           Mecel \\n\n";
   print " * \\b Phone:              +46 31 720 44 00 \\n\n";
   print " * \\b E-Mail:             picea\@mecel.se \\n\n";
   print " *\n";
   print " * \\b Traceability-Info   na \\n\n";
   print " * \\b Classification:     Not classified \\n\n";
   print " * \\b Deviations:         None\n";
   print " *\n";
   print " */\n";
   print "\n";
   print "/*****************************************************************************/\n";
   print "/* Included standard header files                                            */\n";
   print "/*****************************************************************************/\n";
   print "\n";
   print "/*****************************************************************************/\n";
   print "/* Included other header files                                               */\n";
   print "/*****************************************************************************/\n";
   print "\n";
   print "/*****************************************************************************/\n";
   print "/* Public macros                                                             */\n";
   print "/*****************************************************************************/\n";
   print "#define MEMMAP_VENDOR_ID          (41u)\n";
   print "\n";
   print "#define MEMMAP_SW_MAJOR_VERSION   (1u)\n";
   print "#define MEMMAP_SW_MINOR_VERSION   (0u)\n";
   print "#define MEMMAP_SW_PATCH_VERSION   (0u)\n";
   print "\n";
   print "#define MEMMAP_AR_RELEASE_MAJOR_VERSION      (4u)\n";
   print "#define MEMMAP_AR_RELEASE_MINOR_VERSION      (0u)\n";
   print "#define MEMMAP_AR_RELEASE_REVISION_VERSION   (2u)\n";
   print "\n";
   print "\n";
   print "/*\n";
   print " * The symbol 'START_WITH_IF' is undefined.\n";
   print " *\n";
   print " * Thus, the preprocessor continues searching for defined symbols\n";
   print " * This first #ifdef makes integration of delivered parts of MemMap.h\n";
   print " * easier because every supplier starts with #elif\n";
   print " */\n";
   print "\n";
   print "#if defined (START_WITH_IF)\n";
   print "\n";
   print "/*******************************************************************************\n";
   print "** <MSN> section mappings, some addition might be needed                      **\n";
   print "*******************************************************************************/\n";
   print "\n";
   print "/*\n";
   print " * $shortName ($longName)\n";
   print " */\n";
   print "\n";
}

sub PrintMiddleSection
{
   print "/* -------------------------------------------------------------------------- */\n";
   print "/* End of module section mappings                                             */\n";
   print "/* -------------------------------------------------------------------------- */\n";
   print "#else\n";
   print "  #error \"MemMap.h: No valid <MSN> section define found\"\n";
   print "#endif  /* START_WITH_IF */\n";
   print "\n";
   print "\n";
   print "/*******************************************************************************\n";
   print "** Default section mapping, no changes shall be needed                        **\n";
   print "*******************************************************************************/\n";
   print "\n";
   print "/* \n";
   print " * General start of #elif chain whith #if\n";
   print " */\n";
   print "#if defined (START_WITH_IF)\n";
   print "\n";
}

sub PrintFooter
{
   print "/*\n";
   print " * End of default section mapping, catch any errors\n";
   print " */\n";
   print "#else\n";
   print "   #error \"MemMap.h: No valid default section define found.\"\n";
   print "#endif  /* START_WITH_IF */\n";
   print "\n";
   print "\n";
   print "/*****************************************************************************/\n";
   print "/* Public types                                                              */\n";
   print "/*****************************************************************************/\n";
   print "\n";
   print "/*****************************************************************************/\n";
   print "/* Public constant & variable prototypes                                     */\n";
   print "/*****************************************************************************/\n";
   print "\n";
   print "/*****************************************************************************/\n";
   print "/* Public API function prototypes                                            */\n";
   print "/*****************************************************************************/\n";
   print "\n";
   print "/**\n";
   print " * MISRA violation (rule 87): Exception allowed; construct is required\n";
   print " * per AUTOSAR Memory Mapping.  This header file does not contain the\n";
   print " * typical construct to prevent multiple inclusion.  This is intentional.\n";
   print " * It is necessary to satisfy the requirements for MemMap.h usage.\n";
   print " */\n";
}
