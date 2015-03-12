# Microsoft Developer Studio Generated NMAKE File, Format Version 4.20
# ** DO NOT EDIT **

# TARGTYPE "Java Virtual Machine Java Workspace" 0x0809

!IF "$(CFG)" == ""
CFG=socapplet - Java Virtual Machine Debug
!MESSAGE No configuration specified.  Defaulting to socapplet - Java Virtual\
 Machine Debug.
!ENDIF 

!IF "$(CFG)" != "socapplet - Java Virtual Machine Release" && "$(CFG)" !=\
 "socapplet - Java Virtual Machine Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "socapplet.mak" CFG="socapplet - Java Virtual Machine Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "socapplet - Java Virtual Machine Release" (based on\
 "Java Virtual Machine Java Workspace")
!MESSAGE "socapplet - Java Virtual Machine Debug" (based on\
 "Java Virtual Machine Java Workspace")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 
################################################################################
# Begin Project
# PROP Target_Last_Scanned "socapplet - Java Virtual Machine Debug"
JAVA=jvc.exe

!IF  "$(CFG)" == "socapplet - Java Virtual Machine Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir ""
# PROP BASE Intermediate_Dir ""
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir ""
# PROP Intermediate_Dir ""
# PROP Target_Dir ""
OUTDIR=.
INTDIR=.

ALL : "$(OUTDIR)\socapplet.class" "$(OUTDIR)\socapplet.dep"\
 "$(OUTDIR)\CountButton.class" "$(OUTDIR)\CountButton.dep"

CLEAN : 
	-@erase "$(INTDIR)\CountButton.class"
	-@erase "$(INTDIR)\CountButton.dep"
	-@erase "$(INTDIR)\socapplet.class"
	-@erase "$(INTDIR)\socapplet.dep"

# ADD BASE JAVA /O
# ADD JAVA /O

!ELSEIF  "$(CFG)" == "socapplet - Java Virtual Machine Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir ""
# PROP BASE Intermediate_Dir ""
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir ""
# PROP Intermediate_Dir ""
# PROP Target_Dir ""
OUTDIR=.
INTDIR=.

ALL : "$(OUTDIR)\socapplet.class" "$(OUTDIR)\socapplet.dep"\
 "$(OUTDIR)\CountButton.class" "$(OUTDIR)\CountButton.dep"

CLEAN : 
	-@erase "$(INTDIR)\CountButton.class"
	-@erase "$(INTDIR)\CountButton.dep"
	-@erase "$(INTDIR)\socapplet.class"
	-@erase "$(INTDIR)\socapplet.dep"

# ADD BASE JAVA /g
# ADD JAVA /g

!ENDIF 

################################################################################
# Begin Target

# Name "socapplet - Java Virtual Machine Release"
# Name "socapplet - Java Virtual Machine Debug"

!IF  "$(CFG)" == "socapplet - Java Virtual Machine Release"

!ELSEIF  "$(CFG)" == "socapplet - Java Virtual Machine Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\socapplet.java

!IF  "$(CFG)" == "socapplet - Java Virtual Machine Release"


"$(INTDIR)\socapplet.class" : $(SOURCE) "$(INTDIR)"

"$(INTDIR)\socapplet.dep" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "socapplet - Java Virtual Machine Debug"


"$(INTDIR)\socapplet.class" : $(SOURCE) "$(INTDIR)"

"$(INTDIR)\socapplet.dep" : $(SOURCE) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\socapplet.html

!IF  "$(CFG)" == "socapplet - Java Virtual Machine Release"

!ELSEIF  "$(CFG)" == "socapplet - Java Virtual Machine Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\CountButton.java

!IF  "$(CFG)" == "socapplet - Java Virtual Machine Release"


"$(INTDIR)\CountButton.class" : $(SOURCE) "$(INTDIR)"

"$(INTDIR)\CountButton.dep" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "socapplet - Java Virtual Machine Debug"


"$(INTDIR)\CountButton.class" : $(SOURCE) "$(INTDIR)"

"$(INTDIR)\CountButton.dep" : $(SOURCE) "$(INTDIR)"


!ENDIF 

# End Source File
# End Target
# End Project
################################################################################
