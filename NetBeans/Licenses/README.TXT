To have NetBeans automatically add the license to each new Java Class file.
paste the text from 
license-public_domain_nist_rcs.txt
to replace the text of a new license file with the same name 
created from the Template Manager from the menu Tools->Templates.

Unfortunately this has to be done from within NetBeans for each 
user directory. Then name corresponds with

project.license=public_domain_nist_rcs

set in rcslib/NetBeans/rcsjava/nbproject/project.properties 
