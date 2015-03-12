
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>

struct varlist_item
{
  const char *name;
  long pos;
};

static struct varlist_item *varlistP=0;
static int varlist_length=0;
static int varlist_size=0;


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
      fprintf(stderr,"fseek for %s to offset %d failed. -- %s\n",
	      message_file,offset,strerror(errno));
      fclose(messageF);
      return -1;
    }
  int fread_ret = fread(varP,1,var_size,messageF);
  if(var_size != fread_ret)
    {
      fprintf(stderr,"fread for %s returned %d when %d is expected. -- %s\n",
	      message_file,fread_ret,var_size,strerror(errno));
      fclose(messageF);
      return -1;
    }
  fclose(messageF);
  return 0;
}

static void 
printVar(const char *var_name,
	 const void *varBuf,
	 const char *varTypeBuf )
{
  if(!strcmp(varTypeBuf,"double"))
    {
      double val = *((double *)varBuf);
      printf("%s=%f\n",var_name,val);
    }
  else if(!strcmp(varTypeBuf,"float"))
    {
      float val = *((float *)varBuf);
      printf("%s=%e\n",var_name,val);
    }
  else if(!strcmp(varTypeBuf,"long"))
    {
      long val = *((long *)varBuf);
      printf("%s=%ld\n",var_name,val);
    }
  else if(!strcmp(varTypeBuf,"int"))
    {
      int val = *((int *)varBuf);
      printf("%s=%d\n",var_name,val);
    }
  else if(!strcmp(varTypeBuf,"char"))
    {
      char val  = *((char *)varBuf);
      printf("%s=%c (%d,0x%X)\n",var_name,val,val,val);
    }
  else if(!strcmp(varTypeBuf,"char_array"))
    {
      char *val  = ((char *)varBuf);
      printf("%s=%s\n",var_name,val);
    }
}
		     
static int 
get_var_from_file(const char *varname, 
		  const int array_index, 
		  const char *message_file, 
		  const char *format_file,
		  void *varP,
		  const size_t max_var_size,
		  char *varTypeP,
		  const size_t max_varType_size)
{
  static char formatLine[512];
  FILE *formatF = fopen(format_file,"r");
  if(!formatF)
    {
      fprintf(stderr,"Could not open %s -- %s\n",
	      format_file,strerror(errno));
      return -1;
    }
  if(feof(formatF) || 0 == fgets(formatLine,sizeof(formatLine),formatF))
    {
      return -1;
    }
  long last_dla_end_off = 0;
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

  while(!feof(formatF) && fgets(formatLine,sizeof(formatLine),formatF))
    {
      printf("formatLine=%s",formatLine);
      char *typeString=formatLine;
      char *nameStringP=strchr(typeString,',');
      if(!nameStringP)
	{
	  continue;
	}
      *nameStringP=0;
      printf("typeString=%s\n",typeString);
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
      printf("nameString=%s\n",nameString);
      sizeString++;
      char *offsetString = strchr(sizeString+1,',');
      if(!offsetString)
	{
	  continue;
	}
      *offsetString=0;
      printf("sizeString=%s\n",sizeString);
      offsetString++;      
      char *offsetFromStringP = strchr(offsetString+1,',');
      if(!offsetFromStringP)
	{
	  continue;
	}
      *offsetFromStringP=0;
      printf("offsetString=%s\n",offsetString);
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
      printf("offsetFromString=%s\n",offsetFromString);
      dlaMaxlenString++;
      char *commentString = strchr(dlaMaxlenString+1,',');
      if(commentString)
	{
	  *commentString=0;
	  commentString++;
	}
      if(offsetFromString && *offsetFromString &&
	 strcmp(offsetFromString, last_offset_from_name))
	{
	  if(!strcmp(offsetFromString,nameString))
	    {
	      printf("offsetFromString == nameString (%s)",nameString);
	      printf("last_var_pos=%d\n",last_var_pos);
	      printf("last_var_size=%d\n",last_var_size);
	      printf("last_dla_length=%d\n",last_dla_length);
	      printf("last_dla_element_size=%d\n",last_dla_element_size);
	      printf("last_dla_length*last_dla_element_size=%d\n",
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
	      printf("offset_from_name_pos=%d\n",
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
		      break;
		    }
		}
	    }
	}

      printf("dlaMaxlenString=%s\n",dlaMaxlenString);
      printf("commentString=%s\n",commentString);

      long var_offset = strtol(offsetString,0,0);
      printf("var_offset=%ld\n",var_offset);
      long var_size = strtol(sizeString,0,0);
      printf("var_size=%ld\n",var_size);
      long var_pos=(var_offset+offset_from_name_pos+var_size*array_index);
      printf("var_pos=%ld\n",var_pos);
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
	  var_size=max_var_size;
	  if(!strcmp(commentString,"#dynamic_length_array\n"))
	    {
	      var_size = last_dla_length;
	    }
	}
      if(var_size > 0)
	{
	  if(get_var_from_message_file(message_file,
				       var_pos,
				       varP,
				       (var_size>max_var_size?max_var_size:var_size)
				       ) == -1)
	    {
	      fclose(formatF);
	      return -1;
	    }
	  printVar(nameString,
		   varP,
		   typeString);
	}

      if(!strcmp(nameString,varname))
	{

	  if(get_var_from_message_file(message_file,
				       var_pos,
				       varP,
				       var_size
				       ) == -1)
	    {
	      fclose(formatF);
	      return -1;
	    }
	  fclose(formatF);
	  if(varTypeP)
	    {
	      strncpy(varTypeP,typeString,max_varType_size);
	    }
	  return 0;
	}
      if(commentString)
	{
	  if(!strcmp(commentString,"#dynamic_length\n"))
	    {
	      last_dla_length_offset=var_pos;
	      printf("last_dla_length_offset=%d\n",last_dla_length_offset);
	      if(get_var_from_message_file(message_file,
					   var_pos,
					   &last_dla_length,
					   sizeof(last_dla_length)
					   ) == -1)
		{
		  fclose(formatF);
		  return -1;
		}
	      printf("last_dla_length=%d\n",last_dla_length);
	    }
	  else if(!strcmp(commentString,"#dynamic_length_array\n"))
	    {
	      int max_dla_length=atoi(dlaMaxlenString);
	      printf("max_dla_length=%d\n",max_dla_length);
	      if(last_dla_length < 0 || last_dla_length > max_dla_length)
		{
		  fprintf(stderr,"invalid dla_length of %d for %s in %s\n",
			  last_dla_length,nameString,message_file);
		  fclose(formatF);
		  return -1;
		}
	      last_dla_element_size=strtol(sizeString,0,0);
	      printf("last_dla_element_size=%d\n",last_dla_element_size);
	    }
	  else if(!strcmp(commentString,"#begin_struct_dla\n"))
	    {
	      if(struct_dla_listP == 0 || struct_dla_list_size < struct_dla_list_length + 2)
		{
		  struct_dla_list_size += 1024;
		  struct_dla_listP = (struct struct_dla_list_item *)
		    realloc(struct_dla_listP, struct_dla_list_size*sizeof(struct struct_dla_list_item));
		}
	      if(last_dla_length < 1)
		{
		  char linecheck[512];
		  snprintf(linecheck,sizeof(linecheck),
			   ",%s,",nameString);
		  int linecheck_len = strlen(linecheck);
		  printf("linecheck=%s\n",linecheck);
		  printf("linecheck_len=%d\n",linecheck_len);
		  while(!feof(formatF) && fgets(formatLine,sizeof(formatLine),formatF))
		    {
		      printf("skipping formatLine=%s",formatLine);
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
		  printf("struct_dla_listP[%d].length=%d\n",
			 struct_dla_list_length,
			 struct_dla_listP[struct_dla_list_length].length);
		  struct_dla_listP[struct_dla_list_length].cur_element=0;
		  struct_dla_listP[struct_dla_list_length].last_var_pos=last_var_pos;
		  struct_dla_list_length++;
		  printf("struct_dla_list_length=%d\n",
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
		      varlistP[varlist_length-1].pos = last_var_pos; 
		      printf("struct_dla_listP[%d].length=%d\n",
			     i,struct_dla_listP[i].length);
		      if(struct_dla_listP[i].cur_element < 
			 struct_dla_listP[i].length -1 )
			{
			  fseek(formatF,
				struct_dla_listP[i].fpos,
				SEEK_SET);
			  struct_dla_listP[i].cur_element++;
			  printf("struct_dla_listP[%d].cur_element=%d\n",
				 i,struct_dla_listP[i].cur_element);
			  name_start_len = strlen(struct_dla_listP[i].name)+3;
			  snprintf(name_start_to_replace,
				   sizeof(name_start_to_replace),
				   "%s[0]",struct_dla_listP[i].name);
			  snprintf(name_start_replace_with,
				   sizeof(name_start_replace_with),
				   "%s[%d]",struct_dla_listP[i].name,
				   struct_dla_listP[i].cur_element);
			  printf("name_start_len=%d\n",
				 name_start_len);
			  printf("name_start_to_replace=%s\n",
				 name_start_to_replace);
			  printf("name_start_replace_with=%s\n",
				 name_start_replace_with);
			}
		      break;
		    }
		}
	    }
	}
    }
  fprintf(stderr,"failed to find %s in %s\n", varname, format_file);
  fclose(formatF);
  return -1;
}


int
main(int argc, const char **argv)
{
  char varBuf[80];
  char varTypeBuf[256];

  const char *var_name = argv[1];

  get_var_from_file(
		    var_name, //const char *varname, 
		    0, //const int array_index, 
		    "tst_msg.packed", //const char *message_file, 
		    "tst_msg_encode_packed_format.csv", //const char *format_file,
		    varBuf,//void *varP,
		    sizeof(varBuf),//const size_t max_var_size,
		    varTypeBuf, //char *varTypeP,
		    sizeof(varTypeBuf) // const size_t max_varType_size
		    );
  printVar(var_name,
	   varBuf,
	   varTypeBuf);

}
