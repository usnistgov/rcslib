
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <ctype.h>

static const char *searchVar = 0;
static bool searchVarFound = false;

struct varlist_item
{
  const char *name;
  long pos;
};



struct struct_dla_list_item
{
  const char *name;
  long fpos;
  int length;
  int cur_element;
  int varlist_length;
  int last_var_pos;
};

static struct struct_dla_list_item *struct_dla_listP=0;
static int struct_dla_list_length=0;
static int struct_dla_list_size=0;
static struct varlist_item *varlistP=0;
static int varlist_length=0;
static int varlist_size=0;

static bool debug=false;
static bool swap_byte_order=false;

static int 
get_var_from_message_file(const char *message_file,
			  const long offset,
			  void *varP,
			  const size_t var_size)
{
  FILE *messageF = fopen(message_file,"rb");
  if(!messageF)
    {
      fprintf(stderr,"Can't open %s -- %s\n",
	      message_file,strerror(errno));
      return -1;
    }
  if(-1 == fseek(messageF,offset,SEEK_SET))
    {
      fprintf(stderr,"fseek for %s to offset %ld failed. -- %s\n",
	      message_file,offset,strerror(errno));
      fclose(messageF);
      return -1;
    }
  int fread_ret = fread(varP,1,var_size,messageF);
  if(((int)var_size) != fread_ret)
    {
      fprintf(stderr,"%s:%d fread for %s returned %d when %d is expected. (offset=%ld(0x%lX)) -- %s\n",
	      __FILE__,__LINE__,
	      message_file,fread_ret,
	      ((int)var_size),
	      offset,offset,
	      strerror(errno));
      fclose(messageF);
      return -1;
    }
  fclose(messageF);
  if(swap_byte_order && var_size > 1 && var_size < 16)
    {
      char tempBuf[16];
      memcpy(tempBuf,varP,var_size);
      for(int i=0; i < ((int)var_size); i++)
	{
	  ((char *)varP)[i] = tempBuf[var_size-i-1];
	}
    }
  return 0;
}

static void 
printVar(FILE *f,
	 const long offset,
	 const char *var_name,
	 const void *varBuf,
	 const char *varTypeBuf )
{
  if(0 != searchVar) {
    if(strcmp(searchVar,var_name) != 0) {
      return;
    }
    else {
      if(!strcmp(varTypeBuf,"double"))
	{
	  double val = *((double *)varBuf);
	  fprintf(f,"%f\n",val);
	}
      else if(!strcmp(varTypeBuf,"float"))
	{
	  float val = *((float *)varBuf);
	  fprintf(f,"%e\n",val);
	}
      else if(!strcmp(varTypeBuf,"short"))
	{
	  short val = *((short *)varBuf);
	  fprintf(f,"%d\n",val);
	}
      else if(!strcmp(varTypeBuf,"ushort"))
	{
	  unsigned short val = *((unsigned short *)varBuf);
	  fprintf(f,"%u\n",val);
	}
      else if(!strcmp(varTypeBuf,"long"))
	{
	  long val = *((long *)varBuf);
	  fprintf(f,"%ld\n",val);
	}
      else if(!strcmp(varTypeBuf,"ulong"))
	{
	  unsigned long val = *((unsigned long *)varBuf);
	  fprintf(f,"%lu\n",val);
	}
      else if(!strcmp(varTypeBuf,"uint"))
	{
	  unsigned int val = *((unsigned int *)varBuf);
	  fprintf(f,"%u\n",val);
	}
      else if(!strcmp(varTypeBuf,"int"))
	{
	  int val = *((int *)varBuf);
	  fprintf(f,"%d\n",val);
	}
      else if(!strcmp(varTypeBuf,"enumeration"))
	{
	  int val = *((int *)varBuf);
	  fprintf(f,"%d\n",val);
	}
      else if(!strcmp(varTypeBuf,"char"))
	{
	  char val  = *((char *)varBuf);
	  fprintf(f,"%c (%d,0x%X)\n",(isprint(val)?val:'.'),val,val);
	}
      else if(!strcmp(varTypeBuf,"bool"))
	{
	  bool val = *((bool *)varBuf);
	  fprintf(f,"%d\n",val);
	}
      else if(!strcmp(varTypeBuf,"char_array"))
	{
	  char *val  = ((char *)varBuf);
	  fprintf(f,"%s\n",val);
	}
      searchVarFound=true;
      return;
    }
  }
  else {
    if(!strcmp(varTypeBuf,"double"))
      {
	double val = *((double *)varBuf);
	fprintf(f,"0x%4.4lX,\t%12s,\t%30s,\t%f\n",offset,varTypeBuf,var_name,val);
      }
    else if(!strcmp(varTypeBuf,"float"))
      {
	float val = *((float *)varBuf);
	fprintf(f,"0x%4.4lX,\t%12s,\t%30s,\t%e\n",offset,varTypeBuf,var_name,val);
      }
    else if(!strcmp(varTypeBuf,"short"))
      {
	short val = *((short *)varBuf);
	fprintf(f,"0x%4.4lX,\t%12s,\t%30s,\t%d\n",offset,varTypeBuf,var_name,val);
      }
    else if(!strcmp(varTypeBuf,"ushort"))
      {
	unsigned short val = *((unsigned short *)varBuf);
	fprintf(f,"0x%4.4lX,\t%12s,\t%30s,\t%u\n",offset,varTypeBuf,var_name,val);
      }
    else if(!strcmp(varTypeBuf,"long"))
      {
	long val = *((long *)varBuf);
	fprintf(f,"0x%4.4lX,\t%12s,\t%30s,\t%ld\n",offset,varTypeBuf,var_name,val);
      }
    else if(!strcmp(varTypeBuf,"ulong"))
      {
	unsigned long val = *((unsigned long *)varBuf);
	fprintf(f,"0x%4.4lX,\t%12s,\t%30s,\t%lu\n",offset,varTypeBuf,var_name,val);
      }
    else if(!strcmp(varTypeBuf,"uint"))
      {
	unsigned int val = *((unsigned int *)varBuf);
	fprintf(f,"0x%4.4lX,\t%12s,\t%30s,\t%u\n",offset,varTypeBuf,var_name,val);
      }
    else if(!strcmp(varTypeBuf,"int"))
      {
	int val = *((int *)varBuf);
	fprintf(f,"0x%4.4lX,\t%12s,\t%30s,\t%d\n",offset,varTypeBuf,var_name,val);
      }
    else if(!strcmp(varTypeBuf,"enumeration"))
      {
	int val = *((int *)varBuf);
	fprintf(f,"0x%4.4lX,\t%12s,\t%30s,\t%d\n",offset,varTypeBuf,var_name,val);
      }
    else if(!strcmp(varTypeBuf,"char"))
      {
	char val  = *((char *)varBuf);
	fprintf(f,"0x%4.4lX,\t%12s,\t%30s,\t%c (%d,0x%X)\n",offset,varTypeBuf,var_name,(isprint(val)?val:'.'),val,val);
      }
    else if(!strcmp(varTypeBuf,"bool"))
      {
	bool val = *((bool *)varBuf);
	fprintf(f,"0x%4.4lX,\t%12s,\t%30s,\t%d\n",offset,varTypeBuf,var_name,val);
      }
    else if(!strcmp(varTypeBuf,"char_array"))
      {
	char *val  = ((char *)varBuf);
	fprintf(f,"0x%4.4lX,\t%12s,\t%30s,\t%s\n",offset,varTypeBuf,var_name,val);
      }
  }
}

static void
printArray(FILE *f,
	   const char *typeString,
	   const char *nameString,
	   const char *message_file,
	   long var_pos,
	   void *varBuf,
	   int var_size,
	   int length)
{
  long pos = var_pos;
  char typeStringCopy[80];
  strcpy(typeStringCopy,typeString);
  char *a = strstr(typeStringCopy,"_array");
  if(a)
    {
      *a=0;
    }
  char nameStringCopy[80];
  for(int i = 0; i < length; i++)
    {
      snprintf(nameStringCopy,sizeof(nameStringCopy),
	       "%s[%d]",nameString,i);
      if(get_var_from_message_file(message_file,
				   pos,
				   varBuf,
				   var_size) == -1)
	{
	  fprintf(stderr,"Failed to read %s of type %s from %s.\n",
		  nameStringCopy,
		  typeStringCopy,
		  message_file);
	  return;
	}
	pos += var_size;
	printVar(f,
		 pos,
		 nameStringCopy,
		 varBuf,
		 typeStringCopy);
    }
}
		     
static int 
packed_message_to_text_file(
			    const char *message_file, 
			    const char *format_file,
			    const char *text_file)
{

  if(struct_dla_listP)
    {
      memset(struct_dla_listP,0,
	     struct_dla_list_size*sizeof(struct struct_dla_list_item));
    }
  struct_dla_list_length=0;
  if(varlistP)
    {
      memset(varlistP,0, varlist_size*sizeof(struct varlist_item));
    }
  varlist_length=0;

  char one[2];
  one[0] =0;
  one[1] =1;
  
  bool running_on_big_endian = (*((short *)one) == 1);
  if(searchVar == 0) {
    printf("running_on_big_endian = %d\n",running_on_big_endian);
  }

  FILE *messageF = fopen(message_file,"rb");
  if(!messageF)
    {
      fprintf(stderr,"Could not open %s. -- %s\n",
	      message_file,strerror(errno));
      return -1;
    }
  char firstByte;
  fread(&firstByte,1,1,messageF);
  fclose(messageF);
  
  bool file_is_big_endian = (firstByte == 'b' || firstByte == 'B');
  swap_byte_order = (file_is_big_endian != running_on_big_endian);
  if(searchVar == 0) {
    printf("file_is_big_endian = %d\n", file_is_big_endian);
    printf("swap_byte_order=%d\n",swap_byte_order);
  }

  static char varBuf[80];
  static char formatLine[512];
  FILE *textF = stdout;
  if(text_file)
    {
      textF = fopen(text_file,"w");
      if(!textF)
	{
	  fprintf(stderr,"Could not open %s for writing. -- %s\n",
		  text_file,strerror(errno));
	  return -1;
	}
    }

  FILE *formatF = fopen(format_file,"r");
  if(!formatF)
    {
      fprintf(stderr,"Could not open %s -- %s\n",
	      format_file,strerror(errno));
      if(text_file)
	{
	  fclose(textF);
	}
      return -1;
    }
  if(feof(formatF) || 0 == fgets(formatLine,sizeof(formatLine),formatF))
    {
      fprintf(stderr,"Format file %s missing header line.\n",format_file);
      if(text_file)
	{
	  fclose(textF);
	}
      fclose(formatF);
      return -1;
    }

  int last_dla_length = -1;
  int last_dla_element_size = -1;
  int last_dla_length_offset=0;
  static char last_offset_from_name[512];
  long offset_from_name_pos=0;
  memset(last_offset_from_name,0,sizeof(last_offset_from_name));
  long last_var_pos=0;
  long last_var_size=0;
  char name_start_to_replace[512];
  char name_start_replace_with[512];
  int name_start_len=0;
  char nameString[512];
  char offsetFromString[512];
  memset(nameString,0,sizeof(nameString));
  memset(offsetFromString,0,sizeof(offsetFromString));
  memset(name_start_to_replace,0,sizeof(name_start_to_replace));
  memset(name_start_replace_with,0,sizeof(name_start_replace_with));
  memset(varBuf,0,sizeof(varBuf));

  long first_offset=0;
  int format_line_count=0;
  searchVarFound=false;
  while(!feof(formatF) && fgets(formatLine,sizeof(formatLine),formatF) &&
	!searchVarFound)
    {
      format_line_count++;
      if(debug) printf("formatLine=%s",formatLine);
      char *typeString=formatLine;
      char *nameStringP=strchr(typeString,',');
      if(!nameStringP)
	{
	  continue;
	}
      *nameStringP=0;
      if(debug) printf("typeString=%s\n",typeString);
      nameStringP++;
      char *sizeString = strchr(nameStringP+1,',');
      if(!sizeString)
	{
	  continue;
	}
      *sizeString=0;
      strcpy(nameString,nameStringP);
      if(name_start_len > 0 &&
	 !strncmp(nameString,name_start_to_replace,name_start_len))
	{
	  strcpy(nameString,name_start_replace_with);
	  strcat(nameString,nameStringP+name_start_len);
	}
      if(debug) printf("nameString=%s\n",nameString);
      sizeString++;
      char *offsetString = strchr(sizeString+1,',');
      if(!offsetString)
	{
	  continue;
	}
      *offsetString=0;
      if(debug) printf("sizeString=%s\n",sizeString);
      offsetString++;      
      char *elsizeString = strchr(offsetString+1,',');
      if(!elsizeString)
	{
	  continue;
	}
      *elsizeString=0;
      if(debug) printf("offsetString=%s\n",offsetString);
      elsizeString++;      
      char *arraylenString = strchr(elsizeString+1,',');
      if(!arraylenString)
	{
	  continue;
	}
      *arraylenString=0;
      if(debug) printf("elsizeString=%s\n",elsizeString);
      arraylenString++;      
      char *offsetFromStringP = strchr(arraylenString+1,',');
      if(!offsetFromStringP)
	{
	  continue;
	}
      *offsetFromStringP=0;
      if(debug) printf("arraylenString=%s\n",arraylenString);
      offsetFromStringP++;      
      char *dlaMaxlenString = strchr(offsetFromStringP,',');
      if(!dlaMaxlenString)
	{
	  continue;
	}
      *dlaMaxlenString=0;
      strcpy(offsetFromString,offsetFromStringP);
      if(name_start_len > 0 &&
	 !strncmp(offsetFromString,name_start_to_replace,name_start_len))
	{
	  strcpy(offsetFromString,name_start_replace_with);
	  strcat(offsetFromString,offsetFromStringP+name_start_len);
	}
      if(debug) printf("offsetFromString=%s\n",offsetFromString);
      dlaMaxlenString++;
      char *commentString = strchr(dlaMaxlenString+1,',');
      if(commentString)
	{
	  *commentString=0;
	  commentString++;
	}
      if(strcmp(offsetFromString, last_offset_from_name))
	{
	  strncpy(last_offset_from_name,offsetFromString,sizeof(last_offset_from_name));
	  if(!strcmp(offsetFromString,nameString))
	    {
	      if(debug) printf("offsetFromString == nameString (%s)",nameString);
	      if(debug) printf("last_var_pos=%ld\n",last_var_pos);
	      if(debug) printf("last_var_size=%ld\n",last_var_size);
	      if(debug) printf("last_dla_length=%d\n",last_dla_length);
	      if(debug) printf("last_dla_element_size=%d\n",last_dla_element_size);
	      if(debug) printf("last_dla_length*last_dla_element_size=%d\n",
		     (last_dla_length*last_dla_element_size));

	      offset_from_name_pos = last_var_pos;
	      if(last_dla_element_size > 0) 
		{
		  offset_from_name_pos += 
		    last_dla_length*last_dla_element_size;
		}
	      else
		{
		  offset_from_name_pos += 
		    last_var_size;
		}
	      if(debug) printf("offset_from_name_pos=%ld\n",
		     offset_from_name_pos);
	    }
	  else 
	    {
	      for(int i =0; i < varlist_length; i++)
		{
		  if( varlistP[i].name &&
		      !strcmp(varlistP[i].name,offsetFromString))
		    {
		      offset_from_name_pos = varlistP[i].pos;
		      if(debug) printf("offset_from_name_pos=%ld\n",offset_from_name_pos);
		      break;
		    }
		}
	    }
	}

      if(debug) printf("dlaMaxlenString=%s\n",dlaMaxlenString);
      if(debug) printf("commentString=%s\n",commentString);

      long var_offset = strtol(offsetString,0,0);
      if(format_line_count==1 && var_offset < 1)
	{
	  fprintf(stderr,"First offset in %s < 1 !!!\n",format_file);
	  first_offset=var_offset-1;
	  fprintf(stderr,"first_offset=%ld\n",first_offset);
	}
      if(debug) printf("var_offset=%ld\n",var_offset);
      if(first_offset != 0 && offset_from_name_pos == 0) {
	var_offset -=first_offset;
	if(debug) printf("var_offset=%ld\n",var_offset);
      }
      if(var_offset < 0)
	{
	  fprintf(stderr,"Offset in %s:%d %s less than zero(%ld)!!!\n",
		  format_file,format_line_count,formatLine,var_offset);
	}
      long var_size = strtol(sizeString,0,0);
      if(debug) printf("var_size=%ld\n",var_size);
      long var_pos=(var_offset+offset_from_name_pos);
      if(debug) printf("offset_from_name_pos=%ld\n",offset_from_name_pos);
      if(debug) printf("var_pos=%ld\n",var_pos);
      last_dla_element_size=0;

      last_var_pos=var_pos;
      last_var_size=var_size;

      if(varlistP == 0 || varlist_size < varlist_length + 2)
	{
	  varlist_size += 1024;
	  varlistP = (struct varlist_item *)
	    realloc(varlistP, varlist_size*sizeof(struct varlist_item));
	}
      varlistP[varlist_length].name = strdup(nameString);
      varlistP[varlist_length].pos = var_pos;
      varlist_length++;

      if(!strcmp(typeString,"char_array") && var_size == 1)
	{
	  if(!strcmp(commentString,"#dynamic_length_array\n"))
	    {
	      var_size = last_dla_length;
	    }
	}
      if(var_size > 0) 
	{
	  if(strstr(typeString,"_array") == 0)
	    {
	      if(get_var_from_message_file(message_file,
					   var_pos,
					   varBuf,
					   (var_size>((int)sizeof(varBuf))?sizeof(varBuf):var_size)
					   ) == -1)
		{
		  fprintf(stderr,"Failed to read %s of type %s from %s.\n",
			  nameString,
			  typeString,
			  message_file);
		  if(text_file)
		    {
		      fclose(textF);
		    }
		  fclose(formatF);
		  return -1;
		}
	      printVar(textF,
		       var_pos,
		       nameString,
		       varBuf,
		       typeString);
	    }
	  else if(!strcmp(typeString,"char_array"))
	    {
	      bool orig_swap_byte_order = swap_byte_order;
	      swap_byte_order=false;
	      if(get_var_from_message_file(message_file,
					   var_pos,
					   varBuf,
					   (var_size>((int)sizeof(varBuf))?sizeof(varBuf):var_size)
					   ) == -1)
		{
		  fprintf(stderr,"Failed to read %s of type %s from %s.\n",
			  nameString,
			  typeString,
			  message_file);
		  swap_byte_order=orig_swap_byte_order;
		  if(text_file)
		    {
		      fclose(textF);
		    }
		  fclose(formatF);
		  return -1;
		}
	      swap_byte_order=orig_swap_byte_order;
	      printVar(textF,
		       var_pos,
		       nameString,
		       varBuf,
		       typeString);
	    }
	  else if(strstr(commentString,"#dynamic_length_array") == 0)
	    {
	      int elsize = atoi(elsizeString);
	      int arraylen = atoi(arraylenString);
	      printArray(
			 textF,
			 typeString,
			 nameString,
			 message_file,
			 var_pos,
			 varBuf,
			 elsize,
			 arraylen);
	    }
	}
      if(commentString)
	{
	  if(!strcmp(commentString,"#dynamic_length\n"))
	    {
	      last_dla_length_offset=var_pos;
	      if(debug) printf("last_dla_length_offset=%d\n",last_dla_length_offset);
	      if(get_var_from_message_file(message_file,
					   var_pos,
					   &last_dla_length,
					   sizeof(last_dla_length)
					   ) == -1)
		{
		  fprintf(stderr,"Failed to read %s of type %s from %s.\n",
			  nameString,
			  typeString,
			  message_file);
		  fclose(formatF);
		  if(text_file)
		    {
		      fclose(textF);
		    }
		  return -1;
		}
	      if(debug) printf("last_dla_length=%d\n",last_dla_length);
	    }
	  else if(!strcmp(commentString,"#dynamic_length_array\n"))
	    {
	      int max_dla_length=atoi(dlaMaxlenString);
	      if(debug) printf("max_dla_length=%d\n",max_dla_length);
	      if(last_dla_length < 0 || last_dla_length > max_dla_length)
		{
		  fprintf(stderr,"invalid dla_length of %d for %s in %s\n",
			  last_dla_length,nameString,message_file);
		  fclose(formatF);
		  return -1;
		}
	      last_dla_element_size=strtol(sizeString,0,0);
	      if(debug) printf("last_dla_element_size=%d\n",last_dla_element_size);
	      if(var_size > 1 && last_dla_length > 0 && strcmp(typeString,"char_array"))
		{
		  printArray(textF,
			     typeString,
			     nameString,
			     message_file,
			     var_pos,
			     varBuf,
			     var_size,
			     last_dla_length);
		}
	    }
	  else if(!strcmp(commentString,"#begin_struct_dla\n"))
	    {
	      if(struct_dla_listP == 0 || struct_dla_list_size < struct_dla_list_length + 2)
		{
		  struct_dla_list_size += 1024;
		  struct_dla_listP = (struct struct_dla_list_item *)
		    realloc(struct_dla_listP, struct_dla_list_size*sizeof(struct struct_dla_list_item));
		  memset(struct_dla_listP + (struct_dla_list_size-1024),0,
			 1024*sizeof(struct struct_dla_list_item));
		}
	      if(last_dla_length < 1)
		{
		  char linecheck[512];
		  snprintf(linecheck,sizeof(linecheck),
			   ",%s,",nameString);
		  int linecheck_len = strlen(linecheck);
		  if(debug) printf("linecheck=%s\n",linecheck);
		  if(debug) printf("linecheck_len=%d\n",linecheck_len);
		  while(!feof(formatF) && fgets(formatLine,sizeof(formatLine),formatF))
		    {
		      if(debug) printf("skipping formatLine=%s",formatLine);
		      if(!strncmp(formatLine,linecheck,linecheck_len) &&
			 strstr(formatLine,",#end_struct_dla\n"))
			{
			  break;
			}
		    }
		  continue;
		}
	      else
		{
		  struct_dla_listP[struct_dla_list_length].name = strdup(nameString);
		  struct_dla_listP[struct_dla_list_length].fpos = ftell(formatF);
		  struct_dla_listP[struct_dla_list_length].varlist_length = varlist_length;
		  struct_dla_listP[struct_dla_list_length].length = last_dla_length;
		  if(debug) printf("struct_dla_listP[%d].length=%d\n",
			 struct_dla_list_length,
			 struct_dla_listP[struct_dla_list_length].length);
		  struct_dla_listP[struct_dla_list_length].cur_element=0;
		  struct_dla_listP[struct_dla_list_length].last_var_pos=last_var_pos;
		  struct_dla_list_length++;
		  if(debug) printf("struct_dla_list_length=%d\n",
			 struct_dla_list_length);
		}
	    }
	  else if(!strcmp(commentString,"#end_struct_dla\n"))
	    {
	      for(int i =0; i < struct_dla_list_length; i++)
		{
		  if( struct_dla_listP[i].name &&
		      !strcmp(struct_dla_listP[i].name,nameString))
		    {
		      varlist_length = struct_dla_listP[i].varlist_length;
		      offset_from_name_pos = last_var_pos;
		      if(last_dla_element_size > 0) 
			{
			  offset_from_name_pos += 
			    last_dla_length*last_dla_element_size;
			}
		      else
			{
			  offset_from_name_pos += 
			    last_var_size;
			}
		      varlistP[varlist_length-1].pos = offset_from_name_pos; 
		      if(debug) printf("struct_dla_listP[%d].length=%d\n",
			     i,struct_dla_listP[i].length);
		      if(struct_dla_listP[i].cur_element < 
			 struct_dla_listP[i].length -1 )
			{
			  fseek(formatF,
				struct_dla_listP[i].fpos,
				SEEK_SET);
			  struct_dla_listP[i].cur_element++;
			  if(debug) printf("struct_dla_listP[%d].cur_element=%d\n",
				 i,struct_dla_listP[i].cur_element);
			  name_start_len = strlen(struct_dla_listP[i].name)+3;
			  snprintf(name_start_to_replace,
				   sizeof(name_start_to_replace),
				   "%s[0]",struct_dla_listP[i].name);
			  snprintf(name_start_replace_with,
				   sizeof(name_start_replace_with),
				   "%s[%d]",struct_dla_listP[i].name,
				   struct_dla_listP[i].cur_element);
			  if(debug) printf("name_start_len=%d\n",
				 name_start_len);
			  if(debug) printf("name_start_to_replace=%s\n",
				 name_start_to_replace);
			  if(debug) printf("name_start_replace_with=%s\n",
				 name_start_replace_with);
			}
		      break;
		    }
		}
	    }
	}
    }
  fclose(formatF);
  if(text_file)
    {
      fclose(textF);
    }
  return 0;
}


int
main(int argc, const char **argv)
{
  if(argc < 3)
    {
      fprintf(stderr,"%s : usage [-d][--var <searchVar>] format_csv_file [packed_message_files] [-o output_file]\n",
	      argv[0]);
      exit(1);
    }
  const char *outfile=0;
  const char *format_file=argv[1];
  int format_file_argnum = 1;

  for(int i = 1; i < argc-1; i++)
    {
      if(!strcmp(argv[i],"-o"))
	{
	  outfile = argv[i+1];
	  break;
	}
      else if (!strcmp(argv[i],"-d"))
	{
	  debug=true;
	  format_file_argnum = i+1;
	  format_file=argv[format_file_argnum];
	  i++;
	  continue;
	}
      else if (!strcmp(argv[i],"--var"))
	{
	  searchVar = argv[i+1];
	  format_file_argnum = i+2;
	  format_file=argv[format_file_argnum];
	  i++;
	  continue;
	}
    }
  
  for(int i=format_file_argnum+1 ; i < argc ; i++)
    {
      if(argv[i][0] == '-')
	{
	  i++;
	  continue;
	}
      packed_message_to_text_file(argv[i],
				  format_file,
				  outfile);
    }
}
