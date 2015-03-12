
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
  int last_dla_length_offset=0;
  while(!feof(formatF) && fgets(formatLine,sizeof(formatLine),formatF))
    {
      printf("formatLine=%s",formatLine);
      char *typeString=formatLine;
      char *nameString=strchr(typeString+1,',');
      if(!nameString)
	{
	  continue;
	}
      *nameString=0;
      printf("typeString=%s\n",typeString);
      nameString++;
      char *sizeString = strchr(nameString+1,',');
      if(!sizeString)
	{
	  continue;
	}
      *sizeString=0;
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
      char *offsetFromString = strchr(offsetString+1,',');
      if(!offsetFromString)
	{
	  continue;
	}
      *offsetFromString=0;
      printf("offsetString=%s\n",offsetString);
      offsetFromString++;      
      char *dlaMaxlenString = strchr(offsetFromString,',');
      if(!dlaMaxlenString)
	{
	  continue;
	}
      *dlaMaxlenString=0;
      printf("offsetFromString=%s\n",offsetFromString);
      dlaMaxlenString++;
      char *commentString = strchr(dlaMaxlenString+1,',');
      if(commentString)
	{
	  *commentString=0;
	  commentString++;
	}
      printf("dlaMaxlenString=%s\n",dlaMaxlenString);
      printf("commentString=%s\n",commentString);

      long var_offset = strtol(offsetString,0,0);
      printf("var_offset=%ld\n",var_offset);
      long var_size = strtol(sizeString,0,0);
      printf("var_size=%ld\n",var_size);
      long var_pos=(var_offset+last_dla_end_off+var_size*array_index);
      printf("var_pos=%ld\n",var_pos);

      varlistP[varlist_length].name = strdup(nameString);
      varlistP[varlist_length].pos = var_pos;
      varlist_length++;


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
      if(varlistP == 0 || varlist_size < varlist_length + 2)
	{
	  varlist_size += 1024;
	  varlistP = (struct varlist_item)
	    realloc(varlistP, varlist_size*sizeof(struct varlist_item));
	}
      if(commentString)
	{
	  if(!strcmp(commentString,"#dynamic_length\n"))
	    {
	      last_dla_length_offset=strtol(offsetString,0,0);
	      printf("last_dla_length_offset=%d\n",last_dla_length_offset);
	      printf("last_dla_end_off=%d\n",last_dla_end_off);
	      if(get_var_from_message_file(message_file,
					   (last_dla_length_offset+last_dla_end_off),
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
	      long sz=strtol(sizeString,0,0);
	      last_dla_end_off += sz*last_dla_length + last_dla_length_offset+4;
	      printf("last_dla_end_off=%ld\n",last_dla_end_off);
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
