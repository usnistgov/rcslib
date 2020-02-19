/* 
The NIST RCS (Real-time Control Systems) 
 library is public domain software, however it is preferred
 that the following disclaimers be attached.

Software Copywrite/Warranty Disclaimer

   This software was developed at the National Institute of Standards and
Technology by employees of the Federal Government in the course of their
official duties. Pursuant to title 17 Section 105 of the United States
Code this software is not subject to copyright protection and is in the
public domain. NIST Real-Time Control System software is an experimental
system. NIST assumes no responsibility whatsoever for its use by other
parties, and makes no guarantees, expressed or implied, about its
quality, reliability, or any other characteristic. We would appreciate
acknowledgement if the software is used. This software can be
redistributed and/or modified freely provided that any derivative works
bear some notice that they are derived from it, and any modified
versions bear some notice that they have been modified.

 */


/* This line prevents findsource from adding this file to the Makefile */
#define NOT_A_DEFAULT_RCSLIB_SOURCE 1


#ifdef HAVE_CONFIG_H
#include "rcs_config.h"
#include "rcs_config_include.h"
#else

#include "rcs_defs.hh"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#endif

#include "linklist.hh"  // RCS_LINKED_LIST;
#include "rcs_prnt.hh"  // seperate_words()
#include "cms.hh"  // CMS_BUFFERTYPE, CMS_REMOTE_PORT_TYPE

#ifdef __APPLE__
// Testing a theory why macs are unreliable.
// could be that make check is reusing ports and macs don't properly
// reuse ports. So make them more random.
#include "_timer.h"
#endif

static int default_lines_detected = 0;

#define ARRAY_LEN(X) (sizeof(X)/sizeof(X[0]))
#define ARRAY_LENI(X) ((int) (sizeof(X)/sizeof(X[0])))

struct header_sizes {
    char header[512];
    char msg_type[512];
    long estimated_size;
};

struct header_sizes *header_sizes_table = 0;
size_t header_sizes_table_alloc_size = 0;
size_t header_sizes_table_size = 0;
char *header_dir = (char *) "";

class BUFFER_INFO {
public:
    BUFFER_INFO();
    void initialize();
    int oldstyle;
    char name[CMS_CONFIG_LINELEN];
    char host[CMS_CONFIG_LINELEN];
    char msg_type[CMS_CONFIG_LINELEN];
    CMS_BUFFERTYPE buftype;
    long orig_size;
    long size;
    long size_from_header;
    int buffer_number;
    int shmem_key;
    int bsem_key;
    int max_proc;
    unsigned long vme_addr;
    unsigned long starting_vme_addr;
    CMS_REMOTE_PORT_TYPE remotetype;
    int remote_port;
    CMS_NEUTRAL_ENCODING_METHOD encoding_type;
    int queue_enabled;
    int subdiv;
    char endline[CMS_CONFIG_LINELEN];
    int comment_num;
    int local_processes_connected;
    int force_type;
    int bbd_size;
    double brpi;
    int bufline_line_number;

    RCS_LINKED_LIST proc_list;
    bool use_autocnum;
    bool neut;
    bool enable_diag;
    bool split;
    bool remote_port_set;
    bool max_proc_set_by_user;
    bool new_port;
    bool format_source_found;
    bool format_header_found;
    bool size_found;
    const char *cfg_file;
    char format_source[CMS_CONFIG_LINELEN];
};

enum OPS_TYPE {
    NO_OPS,
    READ_ONLY,
    WRITE_ONLY,
    READ_WRITE
};

char ops_string[4][3] = {
    "NO",
    "R",
    "W",
    "RW"
};

class PROCESS_INFO {
public:
    PROCESS_INFO();
    void initialize();
    char name[CMS_CONFIG_LINELEN];
    int oldstyle;
    char bufname[CMS_CONFIG_LINELEN];
    char host[CMS_CONFIG_LINELEN];
    BUFFER_INFO *buf_info;
    CMS_PROCESSTYPE proctype;
    double timeout;
    double subscription_interval;
    int c_num;
    OPS_TYPE ops;
    char endline[CMS_CONFIG_LINELEN];
    int comment_num;
    double retry_interval;
    int isserver;
    bool master;
    bool written_to_oeconfig;

private:
    PROCESS_INFO(const PROCESS_INFO &);
    PROCESS_INFO &operator=(const PROCESS_INFO &);
};

class VAR_INFO {
public:
    char name[CMS_CONFIG_LINELEN];
    int namelen;
    char val[CMS_CONFIG_LINELEN];
    int vallen;
    VAR_INFO();
};

static const char *NMLCFG_CC_ID = "$Id: nmlcfg.cc 1805 2011-06-24 12:31:15Z shackle $";

int nmlcfg_read_file(char *filename);
bool is_true(char *str);
int parse_buffer_info(char *bufline, BUFFER_INFO *);
int parse_process_info(char *procline, PROCESS_INFO *);
int check_buffer_info(BUFFER_INFO * bi);
int check_process_info(PROCESS_INFO * bi);
int nmlcfg_write_output(char *filename);
int nmlcfg_write_oeconfig(char *filename);
int add_nmlcfg_var(char *line);
int set_format_source_pattern(const char *line);
int set_format_header_pattern(const char *line);
int replace_defined_variables(char *line);
VAR_INFO temp_var_info;
BUFFER_INFO default_buffer;
RCS_LINKED_LIST buffer_list;
PROCESS_INFO default_process;
RCS_LINKED_LIST process_list;
RCS_LINKED_LIST var_list;
RCS_LINKED_LIST file_list;
RCS_LINKED_LIST include_dir_list;
RCS_LINKED_LIST comments;
char temp_dir[256];
char temp_file[256];
char output_file[256];
int nmlcfg_verbose = 0;
char unknown_config_file[] = "UNKNOWN";
char *current_config_file = unknown_config_file;
int linenum = 0;
BUFFER_INFO temp_buffer_info;
PROCESS_INFO temp_process_info;
int inverse_mode = 0;

#ifdef ENABLE_RCS_OE_INTRF
char oeconfig_output_file[256];
bool oemode = false;
#endif

CMS_REMOTE_PORT_TYPE last_remotetype = CMS_TCP_REMOTE_PORT_TYPE;
int last_remote_port = -999;
bool all_ports_different = false;

char remote_port_name[9][9] = {
    "NONE",
    "RPC",
    "TTY",
    "TCP",
    "STCP",
    "UDP",
    "HTTP",
    "OE",
    "GDRS_IM"
};


char buf_type_name[8][8] = {
    "SHMEM",
    "GLOBMEM",
    "PHANTOM",
    "LOCMEM",
    "FILEMEM",
    "BBDMEM",
    "RTLMEM",
    "OEMEM"
};


char proc_type_name[4][8] = {
    "REMOTE",
    "LOCAL",
    "PHANTOM",
    "AUTO"
};

BUFFER_INFO::BUFFER_INFO() :
oldstyle(0),
buftype(CMS_SHMEM_TYPE),
orig_size(0),
size(0),
size_from_header(0),
buffer_number(0),
shmem_key(0),
bsem_key(0),
max_proc(0),
vme_addr(0),
remotetype(CMS_TCP_REMOTE_PORT_TYPE),
remote_port(0),
encoding_type(((sizeof (long) != 8) ? CMS_PACKED_ENCODING : CMS_PACKEDL64_ENCODING)),
queue_enabled(0),
subdiv(0),
comment_num(0),
local_processes_connected(0),
force_type(0),
bbd_size(0),
brpi(0.0),
bufline_line_number(0),
proc_list(),
use_autocnum(false),
neut(false),
enable_diag(false),
split(false),
remote_port_set(false),
max_proc_set_by_user(false),
new_port(false),
format_source_found(false),
format_header_found(false),
size_found(false),
cfg_file(0) {
    initialize();
}

void
BUFFER_INFO::initialize() {
    memset(name, 0, sizeof (name));
    memset(host, 0, sizeof (host));
    memset(msg_type, 0, sizeof (msg_type));
    memset(format_source, 0, sizeof (format_source));
    brpi = -1.0;
    strcpy(host, "localhost");
    buftype = CMS_SHMEM_TYPE;
    size = 960;
    neut = 0;
    oldstyle = 0;
    buffer_number = 1;
    shmem_key = 15001;
    bsem_key = -1;
    max_proc = -1;
    max_proc_set_by_user = false;
    new_port = false;
    format_source_found = false;
    format_header_found = false;
    size_found = false;
    vme_addr = 0;
    remotetype = CMS_TCP_REMOTE_PORT_TYPE;
    remote_port = 30000;
    encoding_type = CMS_PACKED_ENCODING;
    queue_enabled = 0;
    subdiv = 0;
    use_autocnum = 0;
    memset(endline, 0, sizeof (endline));
    comment_num = 0;
    force_type = -1;
    bbd_size = 0;
    enable_diag = 0;
    split = false;
    remote_port_set = false;
}

PROCESS_INFO::PROCESS_INFO() :
oldstyle(0),
buf_info(0),
proctype(CMS_AUTO_TYPE),
timeout(0),
subscription_interval(0),
c_num(0),
ops(NO_OPS),
comment_num(0),
retry_interval(0),
isserver(false),
master(false),
written_to_oeconfig(false) {
    initialize();
}

void
PROCESS_INFO::initialize() {
    memset(name, 0, sizeof (name));
    memset(host, 0, sizeof (host));
    strcpy(host, "localhost");
    memset(bufname, 0, sizeof (bufname));
    oldstyle = 0;
    buf_info = NULL;
    proctype = CMS_AUTO_TYPE;
    isserver = false;
    timeout = -1.0;
    subscription_interval = -1.0;
    retry_interval = -1.0;
    master = 0;
    c_num = -1;
    memset(endline, 0, sizeof (endline));
    comment_num = -1;
    written_to_oeconfig = false;
    ops = READ_WRITE;
}

VAR_INFO::VAR_INFO() :
namelen(0), vallen(0) {
    namelen = 0;
    vallen = 0;
    memset(name, 0, sizeof (name));
    memset(val, 0, sizeof (val));
}

int warn = 1;

char charsum(const char *str) {
    unsigned char uc = 0;
    const unsigned char *cptr = (const unsigned char *) str;
    while (*cptr) {
        uc += *cptr;
        cptr++;
    }
    uc &= 0x7F;
    char c = (char) uc;
    return (char) (c > 0 ? c : -c);
}

struct vme_addr_table_entry {
    unsigned long starting_vme_addr;
    unsigned long next_vme_addr;
    int num_buffers;
};

static RCS_LINKED_LIST *vme_addr_list = 0;

static unsigned long
get_next_vme_addr(unsigned long starting_vme_addr,
        unsigned long size) {
    if (!vme_addr_list) {
        vme_addr_list = new RCS_LINKED_LIST();
    }
    struct vme_addr_table_entry *te
            = (struct vme_addr_table_entry *) vme_addr_list->get_head();
    while (te) {
        if (te->starting_vme_addr == starting_vme_addr) {
            unsigned long ret = te->next_vme_addr;
            te->next_vme_addr += size;
            te->num_buffers++;
            return ret;
        }
        te = (struct vme_addr_table_entry *)
                vme_addr_list->get_next();
    }
    te = new struct vme_addr_table_entry;
    te->starting_vme_addr = starting_vme_addr;
    te->next_vme_addr = starting_vme_addr + size;
    if (te->next_vme_addr % 4 != 0) {
        te->next_vme_addr += (4 - te->next_vme_addr % 4);
    }
    vme_addr_list->store_at_tail(te, sizeof (vme_addr_table_entry), 0);
    return starting_vme_addr;
}

static void
set_buffer_info_size_and_max_proc_and_addr(BUFFER_INFO *bi) {
    if (bi->buftype != CMS_SHMEM_TYPE ||
            strstr(bi->endline, "mutex=") ||
            strstr(bi->endline, "MUTEX=")) {
        int orig_max_proc = bi->max_proc;
        if (!bi->max_proc_set_by_user) {
            if (bi->local_processes_connected > 0) {
                bi->max_proc = bi->local_processes_connected + 1;
            } else if (bi->max_proc <= 0 && process_list.list_size > 1
                    && process_list.list_size < 20) {
                bi->max_proc = process_list.list_size + 1;
            } else if (bi->max_proc <= 0) {
                bi->max_proc = 20;
            }
            if (bi->use_autocnum && default_lines_detected && bi->max_proc < 20) {
                bi->max_proc = 20;
            }
        }
        if (bi->use_autocnum && !bi->oldstyle) {
            bi->size += 40 * bi->max_proc;
        }
        if (!bi->oldstyle && bi->enable_diag && bi->buftype != CMS_GLOBMEM_TYPE) {
            bi->size += 144 * bi->max_proc;
        }
        if (bi->size % 4 != 0) {
            bi->size += (4 - (bi->size % 4));
        }
        if (bi->split) {
            bi->size *= 2;
        }
        if (!bi->oldstyle && bi->enable_diag && bi->buftype == CMS_GLOBMEM_TYPE
                && bi->max_proc > 5 && orig_max_proc < 1) {
            rcs_print_error
                    ("%s:%d Warning the size allocated for this buffer may not be enough to support %d processes and enable the logging of diagnostics data. It is suggested that you either specify max_proc, disable diagnostics, or increase the buffer size by %d.\n",
                    current_config_file, bi->bufline_line_number, bi->max_proc,
                    144 * (bi->max_proc - 5));
        }
    }
    if (bi->buftype == CMS_GLOBMEM_TYPE
            && bi->starting_vme_addr > 0 && bi->vme_addr <= 0) {
        bi->vme_addr = get_next_vme_addr(bi->starting_vme_addr, bi->size);
    }
}

int
main(int argc, char **argv) {
    strcpy(output_file, "stdout");
#ifdef ENABLE_RCS_OE_INTRF
    strcpy(oeconfig_output_file, "config.dat");
#endif

    memset(temp_file, 0, 256);

    if (getenv("ALL_PORTS_DIFFERENT")) {
        all_ports_different = true;
    }

    if (argc < 1) {
        fprintf(stderr,
                "nmlcfg usage: [-D name=val] [-I dir] [-o output_file] [--OEMODE] files . . .\n");
        exit(-1);
    }

    char flag = 0;
    char *nmlcfg_header_dir_env = getenv("NMLCFG_HEADER_DIR");
    if (nmlcfg_header_dir_env) {
        header_dir = strdup(nmlcfg_header_dir_env);
    }

    for (int i = 1; i < argc; i++) {
        if (!strcmp(argv[i], "-inverse") || !strcmp(argv[i], "--inverse")) {
            inverse_mode = 1;
            continue;
        }
        if (!strncmp(argv[i], "--HEADER_DIR=", 13)) {
            header_dir = strdup(argv[i] + 13);
            continue;
        }

#ifdef ENABLE_RCS_OE_INTRF
        if (!strcmp(argv[i], "--OEMODE")) {
            oemode = true;
            continue;
        }
        if (!strncmp(argv[i], "--OECONFIG=", 11)) {
            strcpy(oeconfig_output_file, argv[i] + 11);
            oemode = true;
            continue;
        }
#endif

        if (argv[i][0] == '-') {
            flag = argv[i][1];
            switch (flag) {
                case 'w':
                    warn = 0;
                    flag = 0;
                    break;

                case 'W':
                    flag = 0;
                    warn = 1;
                    break;

                case 'v':
                    nmlcfg_verbose = 1;
                    flag = 0;
                    break;

                default:
                    break;
            }
            continue;
        }
        switch (flag) {
            case 'o':
                strncpy(output_file, argv[i], sizeof (output_file));
                // rcs_print("output_file=%s\n",output_file);
                break;

            case 'D':
            {
                char *eq = strchr(argv[i], '=');
                if (eq != NULL) {
                    *eq = 0;
                    strncpy(temp_var_info.name, argv[i],
                            sizeof (temp_var_info.name));
                    strncpy(temp_var_info.val, eq + 1,
                            sizeof (temp_var_info.val));
                    // rcs_print("(%s)=(%s)\n",temp_var_info.name, temp_var_info.val);
                    temp_var_info.namelen = (int) strlen(temp_var_info.name);
                    temp_var_info.vallen = (int) strlen(temp_var_info.val);
                    char *pppptr = strstr(temp_var_info.val, "%%%");
                    while (pppptr) {
                        memcpy(pppptr, "   ", 3);
                        pppptr = strstr(temp_var_info.val, "%%%");
                    }
                    var_list.store_at_tail(&temp_var_info, sizeof (VAR_INFO), 1);
                } else {
                    fprintf(stderr,
                            "Bad argument after -D, %s has no \'=\' character.\n",
                            argv[i]);
                }
            }
                break;

            case 'I':
                strncpy(temp_dir, argv[i], sizeof (temp_dir));
                include_dir_list.store_at_tail(temp_dir, sizeof (temp_dir), 1);
                break;

            case 0:
                strncpy(temp_file, argv[i], sizeof (temp_file));
                file_list.store_at_tail(temp_file, sizeof (temp_file), 1);
                break;

            default:
                fprintf(stderr, "Bad flag %c (%d,0x%X)\n",
                        flag, flag, flag);
                fprintf(stderr,
                        "nmlcfg usage: [-D name=val] [-I dir] [-o output_file] files . . .\n");
                exit(-1);
                break;
        }
        flag = 0;
    }
    if (file_list.list_size < 1) {
        fprintf(stderr, "%s compiled on %s and %s.\n",
                NMLCFG_CC_ID, __DATE__, __TIME__);
        fprintf(stderr,
                "nmlcfg usage: [-D name=val] [-I dir] [-o output_file] files . . .\n");
        exit(-1);
    }

    char *filename = (char *) file_list.get_head();
    int num_files = file_list.list_size;
    int file_num = 0;
    while (filename != NULL) {
        int filesum = charsum(filename) + 4 * charsum(output_file);
        int rval = filesum * 100;
#ifdef __APPLE__
        // Testing a theory why macs are unreliable.
        // could be that make check is reusing ports and macs don't properly
        // reuse ports. So make them more random.
        rval = ((int) (((long) (etime()*1e3)) & 0x7FFFFFFFL));
        if (rval < 0) {
            rval *= -1;
        }
        printf("rval=%d\n", rval);
#endif
        default_buffer.remote_port = (rval % 10000) + 20000;
        default_buffer.shmem_key += (rval % 100000);
        if (file_num == (num_files - 1)) {
            strncpy(temp_var_info.val, "1",
                    sizeof (temp_var_info.val));
            strncpy(temp_var_info.name, "LAST_INPUT_FILE",
                    sizeof (temp_var_info.name));
            temp_var_info.namelen = (int) strlen(temp_var_info.name);
            temp_var_info.vallen = (int) strlen(temp_var_info.val);
            var_list.store_at_tail(&temp_var_info, sizeof (VAR_INFO), 1);
        }
        nmlcfg_read_file(filename);
        filename = (char *) file_list.get_next();
        file_num++;
    }
    BUFFER_INFO *bi = (BUFFER_INFO *) buffer_list.get_head();
    while (NULL != bi) {
        int id = buffer_list.get_current_id();
        set_buffer_info_size_and_max_proc_and_addr(bi);
        check_buffer_info(bi);
        bi = (BUFFER_INFO *) buffer_list.get_first_newer(id);
    }
    nmlcfg_write_output(output_file);
#ifdef ENABLE_RCS_OE_INTRF
    if (oemode) {
        nmlcfg_write_oeconfig(oeconfig_output_file);
    }
#endif
    return (0);
}

int last_line_blank = 0;

static bool inside_ifed_out_region[256];
static int iflevel = 0;
static int ignored_iflevel = 0;

int
nmlcfg_read_file(char *filename) {
    char line[256];
    int linelen = 0;
    FILE *fp = NULL;
    char *include_dir = 0;
    char tempfile[256];
    int templinenum = 0;

    iflevel = 0;
    for (int i = 0; i < ARRAY_LENI(inside_ifed_out_region); i++) {
        inside_ifed_out_region[i] = false;
    }

    memset(line, 0, sizeof (line));

    if (nmlcfg_verbose) {
        printf("nmlcfg: reading %s . . .\n", filename);
    }

    char *last_slash_p = strrchr(filename, '/');
    if (last_slash_p) {
        int dirlen = (last_slash_p + 1 - filename);
        if (dirlen > 1 && dirlen < ((int) strlen(filename) - 1)
                && dirlen < ((int) sizeof (temp_dir))) {
            memset(temp_dir, 0, sizeof (temp_dir));
            strncpy(temp_dir, filename, dirlen);
            char *include_dir = (char *) include_dir_list.get_head();
            bool already_added = false;
            while (include_dir) {
                if (!strcmp(include_dir, temp_dir)) {
                    already_added = true;
                    break;
                }
                include_dir = (char *) include_dir_list.get_next();
            }
            if (!already_added) {
                include_dir_list.store_at_tail(temp_dir, sizeof (temp_dir), 1);
            }
        }
    }

    fp = fopen(filename, "r");
    if (nmlcfg_verbose && !fp) {
        printf("Can't open %s -- %s\n", filename, strerror(errno));
    }
    if (NULL == fp) {
        include_dir = (char *) include_dir_list.get_head();
        while (fp == NULL && include_dir != NULL) {
            strncpy(tempfile, include_dir, sizeof (tempfile));
            strcat(tempfile, "/");
            strcat(tempfile, filename);
            fp = fopen(tempfile, "r");
            if (nmlcfg_verbose && !fp) {
                printf("Can't open %s -- %s\n", tempfile, strerror(errno));
            }
            include_dir = (char *) include_dir_list.get_next();
        }
    } else {
        strncpy(tempfile, filename, sizeof (tempfile));
    }
    if (NULL == fp) {
        fprintf(stderr, "%s:%d Can't open file %s.\n", current_config_file,
                linenum, filename);

        exit(-1);
    }
    current_config_file = tempfile;
    linenum = templinenum = 0;
    while (!feof(fp)) {
        memset(line, 0, 256);
        if (NULL == fgets(line, (sizeof (line) - 1), fp)) {
            fclose(fp);
            if (iflevel != 0) {
                rcs_print_error("%s:%d: Mismatched if/endif pairs\n",
                        current_config_file, linenum);
                exit(-1);
            }
            return 0;
        }
        templinenum++;
        linenum = templinenum;
        linelen = (int) strlen(line);
        if (linelen < 1) {
            continue;
        }
        while (linelen > 0) {
            if (line[linelen - 1] == '\r' ||
                    line[linelen - 1] == '\n' ||
                    line[linelen - 1] == '\t' || line[linelen - 1] == ' ') {
                line[linelen - 1] = 0;
                linelen--;
                continue;
            }
            if (line[linelen - 1] == '\\') {
                line[linelen - 1] = 0;
                linelen--;
                if (NULL == fgets(line + linelen, 256 - linelen, fp)) {
                    fclose(fp);
                    return 0;
                }
                templinenum++;
                linenum = templinenum;
                linelen = (int) strlen(line);
                continue;
            }
            break;
        }
        if (nmlcfg_verbose) {
            printf("%d %s\n", linenum, line);
        }
        if (linelen < 1) {
            if (!last_line_blank) {
                comments.store_at_tail((void *) " ", 2, 1);
            }
            last_line_blank = 1;
            continue;
        }
        char *var_ptr;
        VAR_INFO *vi;
        bool var_found = false;

        if (inside_ifed_out_region[iflevel]) {
            if (!strcmp(line, "endif") || !strcmp(line, "ENDIF")) {
                if (ignored_iflevel > 0) {
                    ignored_iflevel--;
                    continue;
                }
                if (iflevel < 1) {
                    rcs_print_error("%s:%d: endif without matching if\n",
                            current_config_file, linenum);
                    exit(-1);
                }
                iflevel--;
                continue;
            } else if (!strncmp(line, "if", 1) || !strncmp(line, "IF", 2)) {
                ignored_iflevel++;
            } else if (!strcmp(line, "else") || !strcmp(line, "ELSE")) {
                if (ignored_iflevel > 0) {
                    continue;
                }
                inside_ifed_out_region[iflevel] = false;
                continue;
            } else if (!strncmp(line, "elseifdef", 5) || !strncmp(line, "ELSEIFDEF", 5)) {
                if (ignored_iflevel > 0) {
                    continue;
                }
                var_ptr = line + 5;
                while (*var_ptr == ' ' || *var_ptr == '\t') {
                    var_ptr++;
                }
                vi = (VAR_INFO *) var_list.get_head();
                while (vi != NULL && !var_found) {
                    if (!strcmp(var_ptr, vi->name)) {
                        var_found = true;
                        break;
                    }
                    vi = (VAR_INFO *) var_list.get_next();
                }
                if (!var_found) {
                    char *env_str = getenv(var_ptr);
                    if (nmlcfg_verbose) {
                        printf("getenv(%s) returned %s\n", var_ptr, env_str);
                    }
                    if (env_str) {
                        strncpy(temp_var_info.val, env_str,
                                sizeof (temp_var_info.val));
                        strncpy(temp_var_info.name, var_ptr,
                                sizeof (temp_var_info.name));
                        temp_var_info.namelen = (int) strlen(temp_var_info.name);
                        temp_var_info.vallen = (int) strlen(temp_var_info.val);
                        var_list.store_at_tail(&temp_var_info, sizeof (VAR_INFO), 1);
                        vi = (VAR_INFO *) var_list.get_head();
                        while (vi != NULL && !var_found) {
                            if (!strcmp(var_ptr, vi->name)) {
                                var_found = true;
                                break;
                            }
                            vi = (VAR_INFO *) var_list.get_next();
                        }
                    }
                }
                inside_ifed_out_region[iflevel] = !var_found;
                continue;
            } else if (!strncmp(line, "elseifndef", 6) || !strncmp(line, "ELSEIFNDEF", 6)) {
                if (ignored_iflevel > 0) {
                    continue;
                }
                var_ptr = line + 6;
                while (*var_ptr == ' ' || *var_ptr == '\t') {
                    var_ptr++;
                }
                vi = (VAR_INFO *) var_list.get_head();
                while (vi != NULL && !var_found) {
                    if (!strcmp(var_ptr, vi->name)) {
                        var_found = true;
                        break;
                    }
                    vi = (VAR_INFO *) var_list.get_next();
                }
                if (!var_found) {
                    char *env_str = getenv(var_ptr);
                    if (nmlcfg_verbose) {
                        printf("getenv(%s) returned %s\n", var_ptr, env_str);
                    }
                    if (env_str) {
                        strncpy(temp_var_info.val, env_str,
                                sizeof (temp_var_info.val));
                        strncpy(temp_var_info.name, var_ptr,
                                sizeof (temp_var_info.name));
                        temp_var_info.namelen = (int) strlen(temp_var_info.name);
                        temp_var_info.vallen = (int) strlen(temp_var_info.val);
                        var_list.store_at_tail(&temp_var_info, sizeof (VAR_INFO), 1);
                        vi = (VAR_INFO *) var_list.get_head();
                        while (vi != NULL && !var_found) {
                            if (!strcmp(var_ptr, vi->name)) {
                                var_found = true;
                                break;
                            }
                            vi = (VAR_INFO *) var_list.get_next();
                        }
                    }
                }
                inside_ifed_out_region[iflevel] = var_found;
                continue;
            }
            continue;
        } else if (!strncmp(line, "else", 4) || !strncmp(line, "ELSE", 4)) {
            if (iflevel < 1) {
                rcs_print_error("%s:%d: else without matching if\n",
                        current_config_file, linenum);
                exit(-1);
            }
            inside_ifed_out_region[iflevel] = true;
            continue;
        }

        if (!strcmp(line, "endif") || !strcmp(line, "ENDIF")) {
            if (iflevel < 1) {
                rcs_print_error("%s:%d: endif without matching if\n",
                        current_config_file, linenum);
                exit(-1);
            }
            iflevel--;
            continue;
        }

        if (!strncmp(line, "ifdef", 5) || !strncmp(line, "IFDEF", 5)) {
            if (iflevel >= ARRAY_LENI(inside_ifed_out_region)) {
                rcs_print_error("%s:%d: too many nested if's\n",
                        current_config_file, linenum);
                exit(-1);
            }
            iflevel++;
            var_ptr = line + 5;
            while (*var_ptr == ' ' || *var_ptr == '\t') {
                var_ptr++;
            }
            vi = (VAR_INFO *) var_list.get_head();
            while (vi != NULL && !var_found) {
                if (!strcmp(var_ptr, vi->name)) {
                    var_found = true;
                    break;
                }
                vi = (VAR_INFO *) var_list.get_next();
            }
            if (!var_found) {
                char *env_str = getenv(var_ptr);
                if (nmlcfg_verbose) {
                    printf("getenv(%s) returned %s\n", var_ptr, env_str);
                }
                if (env_str) {
                    strncpy(temp_var_info.val, env_str,
                            sizeof (temp_var_info.val));
                    strncpy(temp_var_info.name, var_ptr,
                            sizeof (temp_var_info.name));
                    temp_var_info.namelen = (int) strlen(temp_var_info.name);
                    temp_var_info.vallen = (int) strlen(temp_var_info.val);
                    var_list.store_at_tail(&temp_var_info, sizeof (VAR_INFO), 1);
                    vi = (VAR_INFO *) var_list.get_head();
                    while (vi != NULL && !var_found) {
                        if (!strcmp(var_ptr, vi->name)) {
                            var_found = true;
                            break;
                        }
                        vi = (VAR_INFO *) var_list.get_next();
                    }
                }
            }
            inside_ifed_out_region[iflevel] = !var_found;
            continue;
        }
        if (!strncmp(line, "ifndef", 6) || !strncmp(line, "IFNDEF", 6)) {
            if (iflevel >= ARRAY_LENI(inside_ifed_out_region)) {
                rcs_print_error("%s:%d: too many nested if's\n",
                        current_config_file, linenum);
                fclose(fp);
                exit(-1);
            }
            iflevel++;
            var_ptr = line + 6;
            while (*var_ptr == ' ' || *var_ptr == '\t') {
                var_ptr++;
            }
            vi = (VAR_INFO *) var_list.get_head();
            while (vi != NULL && !var_found) {
                if (!strcmp(var_ptr, vi->name)) {
                    var_found = true;
                    break;
                }
                vi = (VAR_INFO *) var_list.get_next();
            }
            if (!var_found) {
                char *env_str = getenv(var_ptr);
                if (nmlcfg_verbose) {
                    printf("getenv(%s) returned %s\n", var_ptr, env_str);
                }
                if (env_str) {
                    strncpy(temp_var_info.val, env_str,
                            sizeof (temp_var_info.val));
                    strncpy(temp_var_info.name, var_ptr,
                            sizeof (temp_var_info.name));
                    temp_var_info.namelen = (int) strlen(temp_var_info.name);
                    temp_var_info.vallen = (int) strlen(temp_var_info.val);
                    var_list.store_at_tail(&temp_var_info, sizeof (VAR_INFO), 1);
                    vi = (VAR_INFO *) var_list.get_head();
                    while (vi != NULL && !var_found) {
                        if (!strcmp(var_ptr, vi->name)) {
                            var_found = true;
                            break;
                        }
                        vi = (VAR_INFO *) var_list.get_next();
                    }
                }
            }
            inside_ifed_out_region[iflevel] = var_found;
            continue;
        }
        replace_defined_variables(line);
        if (line[0] == '#') {
            if (line[1] == '#') {
                last_line_blank = 0;
                comments.store_at_tail(line + 1, 255, 1);
            }
            continue;
        }
        if (line[0] == '/' && line[1] == '/') {
            continue;
        }
        if (!strncmp(line, "buffer_default", 14)
                || !strncmp(line, "BUFFER_DEFAULT", 14)) {
            parse_buffer_info(line, &default_buffer);
            if (default_buffer.vme_addr != 0) {
                default_buffer.starting_vme_addr = default_buffer.vme_addr;
                default_buffer.vme_addr = 0;
            }
            continue;
        }
        if (!strncmp(line, "process_default", 14)
                || !strncmp(line, "PROCESS_DEFAULT", 14)) {
            parse_process_info(line, &default_process);
            continue;
        }
        if (!strncmp(line, "include", 7) || !strncmp(line, "INCLUDE", 7)) {
            char *fnptr = line + 7;
            while (*fnptr == ' ' || *fnptr == '\t') {
                fnptr++;
            }
            nmlcfg_read_file(fnptr);
            current_config_file = tempfile;
            continue;
        }
        if (!strncmp(line, "define", 6) || !strncmp(line, "DEFINE", 6)) {
            add_nmlcfg_var(line);
            continue;
        }
        if (!strncmp(line, "format_source_pattern", 21) ||
                !strncmp(line, "FORMAT_SOURCE_PATTERN", 21)) {
            set_format_source_pattern(line);
            continue;
        }
        if (!strncmp(line, "format_header_pattern", 21) ||
                !strncmp(line, "FORMAT_HEADER_PATTERN", 21)) {
            set_format_header_pattern(line);
            continue;
        }
        last_line_blank = 0;
        if (line[0] == 'b' || line[0] == 'B') {
            temp_buffer_info.initialize();
            strcpy(temp_buffer_info.host, default_buffer.host);
            temp_buffer_info.buftype = default_buffer.buftype;
            temp_buffer_info.size = default_buffer.size;
            temp_buffer_info.brpi = default_buffer.brpi;
            temp_buffer_info.neut = default_buffer.neut;
            temp_buffer_info.buffer_number = default_buffer.buffer_number;
            temp_buffer_info.shmem_key = default_buffer.shmem_key;
            temp_buffer_info.bsem_key = default_buffer.bsem_key;
            temp_buffer_info.max_proc = default_buffer.max_proc;
            temp_buffer_info.max_proc_set_by_user
                    = default_buffer.max_proc_set_by_user;
            temp_buffer_info.starting_vme_addr = default_buffer.starting_vme_addr;
            temp_buffer_info.vme_addr = 0;
            temp_buffer_info.remotetype = default_buffer.remotetype;
            temp_buffer_info.remote_port = default_buffer.remote_port;
            temp_buffer_info.remote_port_set = default_buffer.remote_port_set;
            temp_buffer_info.encoding_type = default_buffer.encoding_type;
            temp_buffer_info.queue_enabled = default_buffer.queue_enabled;
            temp_buffer_info.subdiv = default_buffer.subdiv;
            temp_buffer_info.enable_diag = default_buffer.enable_diag;
            temp_buffer_info.use_autocnum = default_buffer.use_autocnum;
            temp_buffer_info.comment_num = comments.list_size;
            parse_buffer_info(line, &temp_buffer_info);
            temp_buffer_info.orig_size = temp_buffer_info.size;
            if (!temp_buffer_info.oldstyle) {
                if (temp_buffer_info.queue_enabled > 1) {
                    temp_buffer_info.size *= temp_buffer_info.queue_enabled;
                    temp_buffer_info.size += temp_buffer_info.queue_enabled * 12;
                }
                if (temp_buffer_info.queue_enabled) {
                    temp_buffer_info.size += 20;
                }
                temp_buffer_info.size += 64;
                if (temp_buffer_info.size % 64) {
                    temp_buffer_info.size +=
                            (64 - (temp_buffer_info.size % 64));
                }
                // if (temp_buffer_info.buftype == CMS_GLOBMEM_TYPE
                // 		  && temp_buffer_info.enable_diag)
                // 		{
                // 		  if (temp_buffer_info.max_proc > 1)
                // 		    {
                // 		      temp_buffer_info.size +=
                // 			144 * temp_buffer_info.max_proc;
                // 		    }
                // 		  else
                // 		    {
                // 		      // This assumes only 5 buffers connect.
                // 		      temp_buffer_info.size += 720;
                // 		    }
                // 		}
            }
            strcat(temp_buffer_info.endline, default_buffer.endline);
            buffer_list.store_at_tail(&temp_buffer_info, sizeof (BUFFER_INFO),
                    1);
            if (!temp_buffer_info.oldstyle) {
                if (temp_buffer_info.buftype == CMS_SHMEM_TYPE) {
                    default_buffer.shmem_key++;
                    if (default_buffer.bsem_key > 0) {
                        default_buffer.bsem_key++;
                    }
                }
                if (
                        (temp_buffer_info.new_port ||
                        default_buffer.new_port) &&
                        (default_buffer.remote_port_set ||
                        temp_buffer_info.remote_port_set) &&
                        temp_buffer_info.remote_port == default_buffer.remote_port) {
                    default_buffer.remote_port++;
                }
                default_buffer.buffer_number++;
            }
            continue;
        }
        if (line[0] == 'p' || line[0] == 'P') {
            temp_process_info.initialize();
            strcpy(temp_process_info.host, default_process.host);
            strcpy(temp_process_info.name, default_process.name);
            strcpy(temp_process_info.bufname, default_process.bufname);
            temp_process_info.proctype = default_process.proctype;
            temp_process_info.subscription_interval =
                    default_process.subscription_interval;
            temp_process_info.retry_interval = default_process.retry_interval;
            temp_process_info.timeout = default_process.timeout;
            temp_process_info.c_num = default_process.c_num;
            temp_process_info.isserver = default_process.isserver;
            temp_process_info.master = default_process.master;
            temp_process_info.ops = default_process.ops;
            temp_process_info.comment_num = comments.list_size;
            parse_process_info(line, &temp_process_info);
            check_process_info(&temp_process_info);
            strcat(temp_process_info.endline, default_process.endline);
            process_list.store_at_tail(&temp_process_info,
                    sizeof (PROCESS_INFO), 1);
            continue;
        }
        rcs_print("%s:%d: Line of config file was not recognized.\n",
                current_config_file, linenum);
        rcs_print("%s:%d: -->%s\n", current_config_file, linenum, line);
    }
    fclose(fp);
    if (iflevel != 0) {
        rcs_print_error("%s:%d: Mismatched if/endif pairs\n",
                current_config_file, linenum);
        exit(-1);
    }
    current_config_file = unknown_config_file;

    return (0);
}

int
replace_defined_variables(char *line) {
    char temp_line[256];
    memset(temp_line, 0, sizeof (temp_line));
    strncpy(temp_line, line, sizeof (temp_line));
    char *cptr = temp_line;
    char *lptr = line;
    while (*cptr) {
        if (*cptr == '$') {
            cptr++;
            int varname_len = 1;
            char *vptr = cptr + 1;
            int used_parens = 0;
            if (*cptr == '(') {
                cptr++;
                used_parens = 1;
                vptr++;
                while (*vptr != ')') {
                    varname_len++;
                    if (*vptr == 0 || *vptr == '\n' || *vptr == '\r') {
                        fprintf(stderr, "Bad variable reference %s:%d.\n",
                                current_config_file, linenum);
                        return (-1);
                    }
                    vptr++;
                }
                if (*vptr != ')') {
                    vptr--;
                }
            } else if (*cptr == '{') {
                cptr++;
                used_parens = 1;
                vptr++;
                while (*vptr != '}') {
                    varname_len++;
                    if (*vptr == 0 || *vptr == '\n' || *vptr == '\r') {
                        fprintf(stderr, "Bad variable reference %s:%d.\n",
                                current_config_file, linenum);
                        return (-1);
                    }
                    vptr++;
                }
                if (*vptr != '}') {
                    vptr--;
                }
            } else {
                while (*vptr != 0 && *vptr != ' ' && *vptr != '\t'
                        && *vptr != '\n' && *vptr != '\r') {
                    varname_len++;
                    vptr++;
                }
            }
            VAR_INFO *vi = (VAR_INFO *) var_list.get_head();
            while (vi != NULL) {
                if (vi->namelen == varname_len) {
                    if (!strncmp(cptr, vi->name, vi->namelen)) {
                        strcpy(lptr, vi->val);
                        lptr += vi->vallen;
                        cptr += vi->namelen;
                        if (used_parens) {
                            cptr++;
                        }
                        break;
                    }
                }
                vi = (VAR_INFO *) var_list.get_next();
            }
            if (NULL == vi) {
                strncpy(temp_var_info.name, cptr,
                        ((int) sizeof (temp_var_info.name)) < varname_len ? sizeof (temp_var_info.name) : varname_len);
                char *env_str = getenv(temp_var_info.name);
                if (env_str) {
                    strncpy(temp_var_info.val, env_str,
                            sizeof (temp_var_info.val));
                    temp_var_info.namelen = (int) strlen(temp_var_info.name);
                    temp_var_info.vallen = (int) strlen(temp_var_info.val);
                    var_list.store_at_tail(&temp_var_info, sizeof (VAR_INFO), 1);
                    vi = &temp_var_info;
                    strcpy(lptr, vi->val);
                    lptr += vi->vallen;
                    cptr += vi->namelen;
                    if (used_parens) {
                        cptr++;
                    }
                }
            }
            if (NULL == vi) {
                char *endname_ptr = cptr;
                while (*endname_ptr != ' ' && *endname_ptr != 0
                        && *endname_ptr != '\t') {
                    endname_ptr++;
                }
                *endname_ptr = 0;
                fprintf(stderr, "Variable %s on line %d of %s is undefined.\n",
                        cptr, linenum, current_config_file);
                vi = (VAR_INFO *) var_list.get_head();
                fprintf(stderr, "List of defined variables : \n");
                while (vi != NULL) {
                    fprintf(stderr, "%s=%s;\n",
                            vi->name, vi->val);
                    vi = (VAR_INFO *) var_list.get_next();
                }
                exit(-1);
            }
        }
        *lptr = *cptr;
        cptr++;
        lptr++;
    }
    *lptr = 0;
    return (0);
}

static char format_source_pattern[CMS_CONFIG_LINELEN];
static bool have_format_source_pattern = false;

int
set_format_source_pattern(const char *line) {
    const char *val_ptr = line + 21;
    while (*val_ptr == ' ' || *val_ptr == '\t') {
        val_ptr++;
    }
    const char *percent_ptr = strchr(val_ptr, '%');
    if (!percent_ptr || (percent_ptr[1] != 's')) {
        rcs_print_error
                ("%s:%d: Format source pattern \"%s\" must have exactly one \"%%s\" in it.\n",
                current_config_file, linenum,
                val_ptr);
        have_format_source_pattern = false;
        return -1;
    }
    strncpy(format_source_pattern,
            val_ptr,
            sizeof (format_source_pattern));
    have_format_source_pattern = (format_source_pattern[0] != 0);
    return (0);
}

static char format_header_pattern[CMS_CONFIG_LINELEN];
static bool have_format_header_pattern = false;

int
set_format_header_pattern(const char *line) {
    const char *val_ptr = line + 21;
    while (*val_ptr == ' ' || *val_ptr == '\t') {
        val_ptr++;
    }
    const char *percent_ptr = strchr(val_ptr, '%');
    if (!percent_ptr || (percent_ptr[1] != 's')) {
        rcs_print_error
                ("%s:%d: Format header pattern \"%s\" must have exactly one \"%%s\" in it.\n",
                current_config_file, linenum,
                val_ptr);
        have_format_header_pattern = false;
        return -1;
    }
    strncpy(format_header_pattern,
            val_ptr,
            sizeof (format_header_pattern));
    have_format_header_pattern = (format_header_pattern[0] != 0);
    return (0);
}

int
add_nmlcfg_var(char *line) {
    char *var_ptr = line + 6;
    while (*var_ptr == ' ' || *var_ptr == '\t') {
        var_ptr++;
    }
    char *val_ptr = var_ptr;
    while (*val_ptr != '=') {
        if (0 == *val_ptr) {
            rcs_print_error
                    ("%s:%d: Invalid define \"%s\" should be of the form define <var>=<value>.\n",
                    current_config_file, linenum,
                    line);
            return -1;
        }
        val_ptr++;
    }
    *val_ptr = 0;
    val_ptr++;
    char *end_var_ptr = val_ptr - 2;
    while (*end_var_ptr == ' ' || *end_var_ptr == '\t') {
        if (var_ptr >= end_var_ptr) {
            rcs_print_error
                    ("%s:%d: Invalid define \"%s\" should be of the form define <var>=<value>.\n",
                    current_config_file, linenum,
                    line);
            return -1;
        }
        *end_var_ptr = 0;
        end_var_ptr--;
    }

    while (*var_ptr == ' ' || *var_ptr == '\t') {
        var_ptr++;
    }
    if (!strcmp("all_ports_different", var_ptr)) {
        all_ports_different = (strcmp(val_ptr, "0") && strcmp(val_ptr, "false") && strcmp(val_ptr, "FALSE"));
    }

    strncpy(temp_var_info.val, val_ptr,
            sizeof (temp_var_info.val));
    strncpy(temp_var_info.name, var_ptr,
            sizeof (temp_var_info.name));
    temp_var_info.namelen = (int) strlen(temp_var_info.name);
    temp_var_info.vallen = (int) strlen(temp_var_info.val);
    var_list.store_at_tail(&temp_var_info, sizeof (VAR_INFO), 1);
    return (0);
}

int
check_process_info(PROCESS_INFO * pi_to_check) {
    BUFFER_INFO *bi = (BUFFER_INFO *) buffer_list.get_head();
    while (NULL != bi) {
        if (!strcmp(bi->name, pi_to_check->bufname)) {
            break;
        }
        bi = (BUFFER_INFO *) buffer_list.get_next();
    }
    if (NULL == bi) {
        if (!strcmp(pi_to_check->bufname, "default")) {
            bi = &default_buffer;
        } else {
            rcs_print_error
                    ("%s:%d: There is no buffer line for %s, but the process %s is configured to connect to it.\n",
                    current_config_file, linenum, pi_to_check->bufname,
                    pi_to_check->name);
            return (-1);
        }
    }
    if (pi_to_check->c_num < 0) {
        pi_to_check->c_num = bi->local_processes_connected;
    }
    PROCESS_INFO *pi = (PROCESS_INFO *) bi->proc_list.get_head();
    while (NULL != pi) {
        if (warn) {
            if (pi->master && pi_to_check->master && bi->name[0] && strcmp(bi->name, "default")) {
                rcs_print
                        ("%s:%d: Warning: Both %s and %s are masters for %s\n",
                        current_config_file, linenum, pi->name, pi_to_check->name,
                        bi->name);
            }
            if (pi->isserver && pi_to_check->isserver && bi->name[0] && strcmp(bi->name, "default")) {
                rcs_print
                        ("%s:%d: Warning: Both %s and %s are servers for %s\n",
                        current_config_file, linenum, pi->name, pi_to_check->name,
                        bi->name);
            }
            if (pi->c_num == pi_to_check->c_num
                    && bi->name[0] && strcmp(bi->name, "default") &&
                    pi->proctype != CMS_REMOTE_TYPE &&
                    pi_to_check->proctype != CMS_REMOTE_TYPE) {
                rcs_print
                        ("%s:%d: Warning: Both %s and %s have the same c_num(%d) for buffer %s.\n",
                        current_config_file, linenum, pi->name, pi_to_check->name,
                        pi->c_num, bi->name);
            }
        }
        pi = (PROCESS_INFO *) bi->proc_list.get_next();
    }
    if (pi_to_check->proctype != CMS_REMOTE_TYPE) {
        bi->local_processes_connected++;
    }
    bi->proc_list.store_at_tail(pi_to_check, sizeof (PROCESS_INFO), 1);
    if (bi->buftype != CMS_SHMEM_TYPE ||
            strstr(bi->endline, "mutex=") ||
            strstr(bi->endline, "MUTEX=")) {
        if (pi_to_check->c_num >= bi->max_proc && bi->max_proc > 0 &&
                pi_to_check->proctype != CMS_REMOTE_TYPE) {
            if (warn) {
                rcs_print
                        ("%s:%d: Warning: Process %s has a c_num of %d which is greater that the maximum processes allowed of %d for buffer %s\n",
                        current_config_file, linenum, pi_to_check->name,
                        pi_to_check->c_num, bi->max_proc, bi->name);
            }
        }
    }
    return (0);
}

int
check_buffer_info(BUFFER_INFO * bi_to_check) {
    BUFFER_INFO *bi = (BUFFER_INFO *) buffer_list.get_head();
    while (bi != NULL) {
        if (bi == bi_to_check) {
            bi = (BUFFER_INFO *) buffer_list.get_next();
            continue;
        }
        if (!strcmp(bi_to_check->name, bi->name)) {
            rcs_print_error("%s:%d: Error: More than one buffer line for %s. \n\tpreviously on ...\n%s:%d \n",
                    bi->cfg_file, bi->bufline_line_number, bi->name,
                    bi_to_check->cfg_file,
                    bi_to_check->bufline_line_number);

            exit(-1);
            break;
        }
        if (bi_to_check->buffer_number == bi->buffer_number) {
            if (warn) {
                rcs_print
                        ("%s:%d: Warning %s and %s both have buffer_number %d\n",
                        bi->cfg_file, bi->bufline_line_number, bi->name, bi_to_check->name,
                        bi->buffer_number);
            }
            return (-1);
        }
        if (bi_to_check->buftype != bi->buftype) {
            bi = (BUFFER_INFO *) buffer_list.get_next();
            continue;
        }
        if (bi->buftype == CMS_SHMEM_TYPE) {
            if (bi->shmem_key == bi_to_check->shmem_key) {
                if (warn) {
                    rcs_print
                            ("%s:%d: Warning: %s and %s both have shared mem key %d\n",
                            bi->cfg_file, bi->bufline_line_number, bi->name,
                            bi_to_check->name, bi->shmem_key);
                }
                return (-1);
            }
            if (bi->bsem_key == bi_to_check->bsem_key && bi->bsem_key > 0) {
                if (warn) {
                    rcs_print
                            ("%s:%d: Warning: %s and %s both have blocking semaphore key %d\n",
                            bi->cfg_file, bi->bufline_line_number, bi->name,
                            bi_to_check->name, bi->bsem_key);
                }
                return (-1);
            }
            if (bi->bsem_key == bi_to_check->shmem_key) {
                if (warn) {
                    rcs_print
                            ("%s:%d: Warning: The blocking semaphore key of %s conflicts with the shared memory key of %s which both equal %d\n",
                            bi->cfg_file, bi->bufline_line_number, bi->name,
                            bi_to_check->name, bi->bsem_key);
                }
                return (-1);
            }
            if (bi->shmem_key == bi_to_check->bsem_key) {
                if (warn) {
                    rcs_print
                            ("%s:%d: Warning: The blocking semaphore key of %s conflicts with the shared memory key of %s which both equal %d\n",
                            bi->cfg_file, bi->bufline_line_number, bi_to_check->name,
                            bi->name, bi->shmem_key);
                }
                return (-1);
            }
        }
        if (bi->buftype == CMS_GLOBMEM_TYPE) {
            if (bi_to_check->vme_addr <= 0 && !bi_to_check->oldstyle) {
                if (warn) {
                    rcs_print
                            ("%s:%d: Warning the VME address of %s is less than or equal to zero.\n",
                            bi->cfg_file, bi->bufline_line_number, bi_to_check->name);
                }
                return (-1);
            }
            if (bi->vme_addr > 0) {
                if (bi->vme_addr >= bi_to_check->vme_addr
                        && bi->vme_addr < bi_to_check->vme_addr + bi_to_check->size) {
                    if (warn) {
                        rcs_print
                                ("%s:%d: Warning: Buffers %s and %s seem to have conflicting VME addresses at 0x%lX and 0x%lX and a size of %ld.\n",
                                bi->cfg_file, bi->bufline_line_number, bi->name,
                                bi_to_check->name, bi->vme_addr,
                                bi_to_check->vme_addr, bi_to_check->size);
                    }
                    return (-1);
                }
                if (bi_to_check->vme_addr >= bi->vme_addr
                        && bi_to_check->vme_addr < bi->vme_addr + bi->size) {
                    if (warn) {
                        rcs_print
                                ("%s:%d: Warning: Buffers %s and %s seem to have conflicting VME addresses at 0x%lX and 0x%lX and a size of %ld.\n",
                                bi->cfg_file, bi->bufline_line_number, bi_to_check->name,
                                bi->name, bi_to_check->vme_addr, bi->vme_addr,
                                bi->size);
                    }
                    return (-1);
                }
            }
        }
        bi = (BUFFER_INFO *) buffer_list.get_next();
    }
    return 0;
}

static const char *
find_file_on_include_list(const char *filename) {
    static char fullfilepath[512];
    memset(fullfilepath, 0, sizeof (fullfilepath));
    strncpy(fullfilepath, filename, sizeof (fullfilepath));
    FILE *f = fopen(filename, "r");
    if (!f) {
        strncpy(fullfilepath, header_dir, sizeof (fullfilepath) - 1);
        fullfilepath[sizeof (fullfilepath) - 1] = 0;
        strncat(fullfilepath, filename, sizeof (fullfilepath) - strlen(fullfilepath) - 1);
        fullfilepath[sizeof (fullfilepath) - 1] = 0;
        f = fopen(fullfilepath, "r");
    }
    char *include_dir = 0;
    if (!f) {
        include_dir = (char *) include_dir_list.get_head();
    }
    static char checked_files[1024];
    int checked_files_space_left = sizeof (checked_files) - 2;
    if (!f) {
        memset(checked_files, 0, sizeof (checked_files));
        strncat(checked_files, filename, checked_files_space_left);
        strncat(checked_files, " : ", checked_files_space_left);
        checked_files_space_left = sizeof (checked_files) - strlen(checked_files) - 2;
        strncat(checked_files, strerror(errno), checked_files_space_left);
        strncat(checked_files, "\n", checked_files_space_left);
    }
    while (!f && include_dir) {
        char *end_include_dir = include_dir + strlen(include_dir) - 1;
        char last_include_dir_char = *end_include_dir;
        //printf("include_dir=%p,%s\nend_include_dir=%p\nlast_include_dir_char=%c\n",
        //include_dir,include_dir,end_include_dir,last_include_dir_char);
        if (last_include_dir_char == '/' ||
                last_include_dir_char == '\\' ||
                last_include_dir_char == ':') {
            SNPRINTF_FUNC(SNPRINTF_ARGS(fullfilepath, sizeof (fullfilepath)),
                    "%s%s", include_dir, filename);
        } else {
            SNPRINTF_FUNC(SNPRINTF_ARGS(fullfilepath, sizeof (fullfilepath)),
                    "%s/%s", include_dir, filename);
        }

        f = fopen(fullfilepath, "r");
        if (!f) {
            checked_files_space_left = sizeof (checked_files) - strlen(checked_files) - 2;
            strncat(checked_files, fullfilepath, checked_files_space_left);
            strncat(checked_files, " : ", checked_files_space_left);
            checked_files_space_left = sizeof (checked_files) - strlen(checked_files) - 2;
            strncat(checked_files, strerror(errno), checked_files_space_left);
            strncat(checked_files, "\n", checked_files_space_left);
        }
        include_dir = (char *) include_dir_list.get_next();
    }
    if (!f) {
        fprintf(stderr, "%s:%d Can't open %s.\n",
                current_config_file, linenum,
                filename);
        fprintf(stderr, "Tried :\n%s\n", checked_files);
        return 0;
    }
    fclose(f);
    return (const char *) fullfilepath;
}

static long
get_size_from_format_source(const char *ccfile_name,
        const char *msg_type = 0) {
    if (header_sizes_table) {
        for (int i = 0; i < ((int) header_sizes_table_size); i++) {
            if (!strcmp(header_sizes_table[i].header, ccfile_name)) {
                if (msg_type == 0 && header_sizes_table[i].msg_type == 0) {
                    return header_sizes_table[i].estimated_size;
                } else if (msg_type != 0 &&
                        !strncmp(header_sizes_table[i].msg_type,
                        msg_type,
                        sizeof (header_sizes_table[i].msg_type))) {
                    return header_sizes_table[i].estimated_size;
                }
            }
        }
    }
    if (!header_sizes_table) {
        header_sizes_table = (header_sizes *) malloc(sizeof (header_sizes)*32);
        header_sizes_table_alloc_size = 32;
        header_sizes_table_size = 0;
    } else if (header_sizes_table_size >= header_sizes_table_alloc_size) {
        header_sizes_table_alloc_size += 32;
        header_sizes_table = (header_sizes *) realloc(header_sizes_table,
                sizeof (header_sizes) * header_sizes_table_alloc_size);
    }
    static char ccfile[512];
    strncpy(ccfile, ccfile_name, sizeof (ccfile));
    FILE *f = fopen(ccfile_name, "r");
    if (!f) {
        strncpy(ccfile, header_dir, sizeof (ccfile) - 1);
        ccfile[sizeof (ccfile) - 1] = 0;
        strncat(ccfile, ccfile_name, sizeof (ccfile) - strlen(ccfile) - 1);
        ccfile[sizeof (ccfile) - 1] = 0;
        f = fopen(ccfile, "r");
    }
    char *include_dir = 0;
    if (!f) {
        include_dir = (char *) include_dir_list.get_head();
    }
    static char checked_files[1024];
    int checked_files_space_left = sizeof (checked_files) - 2;
    if (!f) {
        memset(checked_files, 0, sizeof (checked_files));
        strncat(checked_files, ccfile_name, checked_files_space_left);
        strncat(checked_files, " : ", checked_files_space_left);
        checked_files_space_left = sizeof (checked_files) - strlen(checked_files) - 2;
        strncat(checked_files, strerror(errno), checked_files_space_left);
        strncat(checked_files, "\n", checked_files_space_left);
    }
    while (!f && include_dir) {
        char *end_include_dir = include_dir + strlen(include_dir) - 1;
        char last_include_dir_char = *end_include_dir;
        //printf("include_dir=%p,%s\nend_include_dir=%p\nlast_include_dir_char=%c\n",
        //include_dir,include_dir,end_include_dir,last_include_dir_char);
        if (last_include_dir_char == '/' ||
                last_include_dir_char == '\\' ||
                last_include_dir_char == ':') {
            SNPRINTF_FUNC(SNPRINTF_ARGS(ccfile, sizeof (ccfile)),
                    "%s%s", include_dir, ccfile_name);
        } else {
            SNPRINTF_FUNC(SNPRINTF_ARGS(ccfile, sizeof (ccfile)),
                    "%s/%s", include_dir, ccfile_name);
        }

        f = fopen(ccfile, "r");
        if (!f) {
            checked_files_space_left = sizeof (checked_files) - strlen(checked_files) - 2;
            strncat(checked_files, ccfile, checked_files_space_left);
            strncat(checked_files, " : ", checked_files_space_left);
            checked_files_space_left = sizeof (checked_files) - strlen(checked_files) - 2;
            strncat(checked_files, strerror(errno), checked_files_space_left);
            strncat(checked_files, "\n", checked_files_space_left);
        }
        include_dir = (char *) include_dir_list.get_next();
    }
    if (!f) {
        fprintf(stderr, "%s:%d Can't open %s.\n",
                current_config_file, linenum,
                ccfile_name);
        fprintf(stderr, "Tried :\n%s\n", checked_files);
        exit(-1);
    }
    static char line[512];
    static char est_match_line[512];
    const char *est_match_lineP = "Estimated_size\tMAXIMUM";
    if (msg_type && *msg_type) {
        strncpy(est_match_line, "Estimated_size\t", sizeof (est_match_line) - 1);
        est_match_line[sizeof (est_match_line) - 1] = 0;
        strncat(est_match_line, msg_type,
                sizeof (est_match_line) - strlen(est_match_line) - 1);
        est_match_line[sizeof (est_match_line) - 1] = 0;
        est_match_lineP = est_match_line;
    }
    int est_match_line_len = strlen(est_match_lineP);
    while (!feof(f)) {
        if (!fgets(line, sizeof (line), f)) {
            break;
        }
        if (!strncmp(line, est_match_lineP, est_match_line_len)) {
            long size = strtol(line + est_match_line_len + 1, 0, 0);
            if (nmlcfg_verbose) {
                printf("#: Estimated size line from %s\n", ccfile);
                printf("#: %s\n", line);
            }
            header_sizes_table[header_sizes_table_size].estimated_size = size;
            strncpy(header_sizes_table[header_sizes_table_size].header,
                    ccfile_name,
                    sizeof (header_sizes_table[header_sizes_table_size].header) - 1);
            header_sizes_table[header_sizes_table_size].header[sizeof (header_sizes_table[header_sizes_table_size].header) - 1] = 0;
            if (msg_type && *msg_type) {
                strncpy(header_sizes_table[header_sizes_table_size].msg_type,
                        msg_type,
                        sizeof (header_sizes_table[header_sizes_table_size].msg_type) - 1);
                header_sizes_table[header_sizes_table_size].header[sizeof (header_sizes_table[header_sizes_table_size].header) - 1] = 0;
            } else {
                memset(header_sizes_table[header_sizes_table_size].msg_type,
                        0,
                        sizeof (header_sizes_table[header_sizes_table_size].msg_type));
            }
            header_sizes_table_size++;
            fclose(f);
            return size;
        }
    }
    fclose(f);
    fprintf(stderr, "%s:%d Could not find line starting with %s in %s!\n",
            current_config_file, linenum,
            est_match_lineP, ccfile);
    exit(-1);
    return 0;
}

static long
get_size_from_header(const char *header_name,
        const char *msg_type) {
    if (header_sizes_table) {
        for (int i = 0; i < ((int) header_sizes_table_size); i++) {
            if (!strcmp(header_sizes_table[i].header, header_name)) {
                if (msg_type == 0 && header_sizes_table[i].msg_type == 0) {
                    return header_sizes_table[i].estimated_size;
                } else if (msg_type != 0 &&
                        !strncmp(header_sizes_table[i].msg_type, msg_type,
                        sizeof (header_sizes_table[i].msg_type))) {
                    return header_sizes_table[i].estimated_size;
                }
            }
        }
    }
    if (!header_sizes_table) {
        header_sizes_table = (header_sizes *) malloc(sizeof (header_sizes)*32);
        header_sizes_table_alloc_size = 32;
        header_sizes_table_size = 0;
    } else if (header_sizes_table_size >= header_sizes_table_alloc_size) {
        header_sizes_table_alloc_size += 32;
        header_sizes_table = (header_sizes *) realloc(header_sizes_table,
                sizeof (header_sizes) * header_sizes_table_alloc_size);
    }
    static char headerfilecopy[512];
    static char ccfile[512];
    strncpy(headerfilecopy, header_name, sizeof (headerfilecopy) - 1);
    headerfilecopy[sizeof (headerfilecopy) - 1] = 0;
    char *p = strchr(headerfilecopy, '.');
    if (p) {
        *p = 0;
    }
    SNPRINTF_FUNC(SNPRINTF_ARGS(ccfile, sizeof (ccfile)),
            "%s_n.cc",
            headerfilecopy);

    size_t header_sizes_table_size_start = header_sizes_table_size;
    long size_from_format_source =
            get_size_from_format_source(ccfile,
            msg_type);
    if (size_from_format_source > 0 &&
            header_sizes_table_size > 0 &&
            header_sizes_table_size == (header_sizes_table_size_start + 1)) {
        strncpy(header_sizes_table[header_sizes_table_size - 1].header,
                header_name,
                sizeof (header_sizes_table[header_sizes_table_size - 1].header) - 1);

    }
    header_sizes_table[header_sizes_table_size - 1].header[sizeof (header_sizes_table[header_sizes_table_size - 1].header) - 1] = 0;
    return size_from_format_source;
}

static void
set_format_name(BUFFER_INFO *bi, const char *fname_in_ptr) {
    const char *fname_eq_ptr = strchr(fname_in_ptr, '=');
    if (!fname_eq_ptr) {
        return;
    }
    fname_eq_ptr++;
    static char formatName[CMS_CONFIG_LINELEN];
    strncpy(formatName, fname_eq_ptr, sizeof (formatName) - 1);
    formatName[sizeof (formatName) - 1] = 0;
    char *p = formatName;
    while (p < (formatName + sizeof (formatName)) && *p) {
        if (*p == ' ' || *p == '\t' || *p == '\r' || *p == '\n') {
            *p = 0;
            break;
        }
        p++;
    }
    strncat(bi->endline, " ",
            sizeof (bi->endline) - strlen(bi->endline) - 1);
    bi->endline[sizeof (bi->endline) - 1] = 0;
    strncat(bi->endline, "format_name=",
            sizeof (bi->endline) - strlen(bi->endline) - 1);
    bi->endline[sizeof (bi->endline) - 1] = 0;
    strncat(bi->endline, fname_eq_ptr,
            sizeof (bi->endline) - strlen(bi->endline) - 1);
    bi->endline[sizeof (bi->endline) - 1] = 0;
    if (!bi->format_source_found && have_format_source_pattern) {
        char source[CMS_CONFIG_LINELEN];
        SNPRINTF_FUNC(SNPRINTF_ARGS(source, sizeof (source)),
                format_source_pattern, formatName);
        const char *sourcepath = find_file_on_include_list(source);
        if (sourcepath) {
            bi->size_from_header = get_size_from_format_source(source,
                    bi->msg_type);
            if (bi->size_from_header > 7 && !bi->size_found) {
                bi->size = bi->size_from_header;
            }
            strncpy(bi->format_source, sourcepath, sizeof (bi->format_source) - 1);
            bi->format_source[sizeof (bi->format_source) - 1] = 0;
            bi->format_source_found = true;
            bi->endline[sizeof (bi->endline) - 1] = 0;
            strncat(bi->endline, " ",
                    sizeof (bi->endline) - strlen(bi->endline) - 1);
            strncat(bi->endline, "format_source=",
                    sizeof (bi->endline) - strlen(bi->endline) - 1);
            bi->endline[sizeof (bi->endline) - 1] = 0;
            strncat(bi->endline, sourcepath,
                    sizeof (bi->endline) - strlen(bi->endline) - 1);
            bi->endline[sizeof (bi->endline) - 1] = 0;
        }
    }
    if (!bi->format_header_found && have_format_header_pattern) {
        char header[CMS_CONFIG_LINELEN];
        SNPRINTF_FUNC(SNPRINTF_ARGS(header, sizeof (header)),
                format_header_pattern, formatName);
        if (!bi->format_source_found) {
            bi->size_from_header = get_size_from_header(header,
                    bi->msg_type);
            if (bi->size_from_header > 7 && !bi->size_found) {
                bi->size = bi->size_from_header;
            }
        }
        const char *headerpath = find_file_on_include_list(header);
        if (headerpath) {
            bi->format_header_found = true;
            strncat(bi->endline, " ",
                    sizeof (bi->endline) - strlen(bi->endline) - 1);
            bi->endline[sizeof (bi->endline) - 1] = 0;
            strncat(bi->endline, "header=",
                    sizeof (bi->endline) - strlen(bi->endline) - 1);
            bi->endline[sizeof (bi->endline) - 1] = 0;
            strncat(bi->endline, headerpath,
                    sizeof (bi->endline) - strlen(bi->endline) - 1);
        }
        bi->endline[sizeof (bi->endline) - 1] = 0;
    }

}

int
parse_buffer_info(char *bufline, BUFFER_INFO * bi) {
    static const char * last_current_config_file_dup = 0;
    static const char * last_current_config_file = 0;
    char wordbuf[CMS_CONFIG_LINELEN];
    char *word[32];
    bi->oldstyle = (NULL == strstr(bufline, "name="))
            && (NULL == strstr(bufline, "default"));
    int num_words = separate_words_with_buffer(word, 32, bufline, wordbuf, CMS_CONFIG_LINELEN);
    bi->bufline_line_number = linenum;
    if (last_current_config_file != current_config_file) {
        last_current_config_file_dup = strdup(current_config_file);
        last_current_config_file = current_config_file;
    }
    bi->cfg_file = last_current_config_file_dup;
    if (bi->oldstyle) {
        if (num_words < 9) {
            rcs_print_error("%s:%d Invalid buffer line no \"default\" on \"name=\" (num_words =%d\n)\n",
                    current_config_file,
                    linenum,
                    num_words);
            rcs_print_error("%s", bufline);
            return -1;
        }
        bi->queue_enabled = 0;
        bi->use_autocnum = 0;
        bi->enable_diag = 0;
        bi->bsem_key = -1;
        strcpy(bi->name, word[1]);
        strcpy(bi->host, word[3]);
        char *buffer_type_name = word[2];
        /* Determine the BufferType. */
        if (!strcmp(buffer_type_name, "SHMEM")) {
            bi->buftype = CMS_SHMEM_TYPE;
        } else if (!strcmp(buffer_type_name, "OEMEM")) {
            bi->buftype = CMS_GLOBMEM_TYPE;
        } else if (!strcmp(buffer_type_name, "GLOBMEM")) {
            bi->buftype = CMS_GLOBMEM_TYPE;
        } else if (!strcmp(buffer_type_name, "BBDMEM")) {
            bi->buftype = CMS_BBDMEM_TYPE;
        } else if (!strcmp(buffer_type_name, "PHANTOM")) {
            bi->buftype = CMS_PHANTOM_BUFFER;
        } else if (!strcmp(buffer_type_name, "LOCMEM")) {
            bi->buftype = CMS_LOCMEM_TYPE;
        } else if (!strcmp(buffer_type_name, "FILEMEM")) {
            bi->buftype = CMS_FILEMEM_TYPE;
        } else {
            rcs_print_error("%s:%d: CMS: invalid buffer type (%s)\n",
                    current_config_file, linenum, buffer_type_name);
            exit(-1);
        }
#ifdef ENABLE_RCS_OE_INTRF
        if (oemode) {
            bi->buftype = CMS_OEMEM_TYPE;
        }
#endif
        bi->size = (long) atol(word[4]);
        bi->neut = (bool) (atol(word[5]) != 0);
        bi->buffer_number = atol(word[7]);
        bi->max_proc = 0;
        if (word[8][0] >= '0' && word[8][0] <= '9') {
            bi->max_proc = atol(word[8]);
            bi->max_proc_set_by_user = true;
        }
        memset(bi->endline, 0, sizeof (bi->endline));
        int last_word = 9;
        if (bi->buftype == CMS_SHMEM_TYPE) {
            bi->shmem_key = atol(word[9]);
            last_word = 10;
        }
        for (int i = last_word; i < num_words - 1; i++) {
            if (NULL == word[i]) {
                break;
            }
            strcat(bi->endline, " ");
            strcat(bi->endline, word[i]);
        }
    } else {
        for (int i = 1; i < num_words - 1; i++) {
            if (NULL == word[i]) {
                break;
            }

            if (!strncmp(word[i], "bufname=", 8)
                    || !strncmp(word[i], "BUFNAME=", 8)) {
                strncpy(bi->name, ((char *) word[i]) + 8, sizeof (bi->name) - 1);
                bi->name[sizeof (bi->name) - 1] = 0;
                continue;
            }
            if (!strncmp(word[i], "name=", 5)
                    || !strncmp(word[i], "NAME=", 5)) {
                strncpy(bi->name, ((char *) word[i]) + 5, sizeof (bi->name));
                bi->name[sizeof (bi->name) - 1] = 0;
                continue;
            }
            if (!strncmp(word[i], "bufhost=", 8)
                    || !strncmp(word[i], "BUFHOST=", 8)) {
                strncpy(bi->host, ((char *) word[i]) + 8, sizeof (bi->name));
                bi->host[sizeof (bi->host) - 1] = 0;
                continue;
            }
            if (!strncmp(word[i], "host=", 5)
                    || !strncmp(word[i], "HOST=", 5)) {
                strncpy(bi->host, ((char *) word[i]) + 5, sizeof (bi->host));
                bi->host[sizeof (bi->host) - 1] = 0;
                continue;
            }
            char *buffer_type_name = 0;
            if (!strncmp(word[i], "buftype=", 8)
                    || !strncmp(word[i], "BUFTYPE=", 8)) {
                buffer_type_name = ((char *) word[i]) + 8;
            } else if (!strncmp(word[i], "localtype=", 10)
                    || !strncmp(word[i], "LOCALTYPE=", 10)) {
                buffer_type_name = ((char *) word[i]) + 10;
            }
            if (buffer_type_name != 0) {
                if (!strncmp(buffer_type_name, "SHMEM", 5) ||
                        !strncmp(buffer_type_name, "shmem", 5)) {
                    bi->buftype = CMS_SHMEM_TYPE;
                } else if (!strcmp(buffer_type_name, "GLOBMEM") ||
                        !strcmp(buffer_type_name, "globmem")) {
                    bi->buftype = CMS_GLOBMEM_TYPE;
                } else if (!strcmp(buffer_type_name, "OEMEM") ||
                        !strcmp(buffer_type_name, "oemem")) {
                    bi->buftype = CMS_OEMEM_TYPE;
                } else if (!strcmp(buffer_type_name, "BBDMEM") ||
                        !strcmp(buffer_type_name, "bbdmem") ||
                        !strcmp(buffer_type_name, "BBD") ||
                        !strcmp(buffer_type_name, "bbd")) {
                    bi->buftype = CMS_BBDMEM_TYPE;
                } else if (!strcmp(buffer_type_name, "PHANTOM") ||
                        !strcmp(buffer_type_name, "phantom")) {
                    bi->buftype = CMS_PHANTOM_BUFFER;
                } else if (!strcmp(buffer_type_name, "LOCMEM") ||
                        !strcmp(buffer_type_name, "locmem")) {
                    bi->buftype = CMS_LOCMEM_TYPE;
                } else if (!strcmp(buffer_type_name, "FILEMEM") ||
                        !strcmp(buffer_type_name, "filemem")) {
                    bi->buftype = CMS_FILEMEM_TYPE;
                } else {
                    rcs_print_error
                            ("%s:%d: CMS Invalid buffer type (%s)\n",
                            current_config_file, linenum,
                            buffer_type_name);
                    exit(-1);
                }
                continue;
            }
            if (!strcmp(word[i], "clear_endline")
                    || !strcmp(word[i], "CLEAR_ENDLINE")) {
                memset(bi->endline, 0, sizeof (bi->endline));
                memset(bi->msg_type, 0, sizeof (bi->msg_type));
                memset(bi->format_source, 0, sizeof (bi->format_source));
                bi->format_source_found = false;
                bi->format_header_found = false;
                continue;
            }
            if (!strcmp(word[i], "reset")
                    || !strcmp(word[i], "RESET")) {
                bi->initialize();
                continue;
            }
            if (!strncmp(word[i], "msg_type=", 9)
                    || !strncmp(word[i], "MSG_TYPE=", 7)) {
                strncpy(bi->msg_type, word[i] + 9,
                        sizeof (bi->msg_type) - 1);
                bi->msg_type[sizeof (bi->msg_type) - 1] = 0;
                strncat(bi->endline, " ",
                        sizeof (bi->endline) - strlen(bi->endline) - 1);
                bi->endline[sizeof (bi->endline) - 1] = 0;
                strncat(bi->endline, word[i],
                        sizeof (bi->endline) - strlen(bi->endline) - 1);
                bi->endline[sizeof (bi->endline) - 1] = 0;
                if (bi->format_source_found && !bi->size_found) {
                    bi->size_from_header = get_size_from_format_source(bi->format_source,
                            bi->msg_type);
                    if (bi->size_from_header > 7 && !bi->size_found) {
                        bi->size = bi->size_from_header;
                    }
                }
                continue;
            }
            if (!strncmp(word[i], "header=", 7)
                    || !strncmp(word[i], "HEADER=", 7)) {
                const char *headerpath = find_file_on_include_list(word[i] + 7);
                if (headerpath) {
                    if (!bi->format_source_found) {
                        bi->size_from_header = get_size_from_header(word[i] + 7,
                                bi->msg_type);
                        if (bi->size_from_header > 7 && !bi->size_found) {
                            bi->size = bi->size_from_header;
                        }
                    }
                    bi->format_header_found = true;
                    strncat(bi->endline, " ",
                            (sizeof (bi->endline) - strlen(bi->endline) - 2));
                    strncat(bi->endline, "header=",
                            (sizeof (bi->endline) - strlen(bi->endline) - 2));
                    strncat(bi->endline, headerpath,
                            (sizeof (bi->endline) - strlen(bi->endline) - 2));
                }
                continue;
            }
            if (!strncmp(word[i], "format_source=", 14)
                    || !strncmp(word[i], "FORMAT_SOURCE=", 14)) {
                const char *sourcepath = find_file_on_include_list(word[i] + 14);
                if (sourcepath) {
                    bi->size_from_header = get_size_from_format_source(word[i] + 14,
                            bi->msg_type);
                    if (bi->size_from_header > 7 && !bi->size_found) {
                        bi->size = bi->size_from_header;
                    }
                    strncpy(bi->format_source, word[i] + 14, sizeof (bi->format_source) - 1);
                    bi->format_source[sizeof (bi->format_source) - 1] = 0;
                    bi->format_source_found = true;
                    strncat(bi->endline, " ",
                            (sizeof (bi->endline) - strlen(bi->endline) - 2));
                    strncat(bi->endline, "format_source=",
                            (sizeof (bi->endline) - strlen(bi->endline) - 2));
                    strncat(bi->endline, sourcepath,
                            (sizeof (bi->endline) - strlen(bi->endline) - 2));
                }
                continue;
            }
            if (!strncmp(word[i], "format_name=", 12)
                    || !strncmp(word[i], "FORMAT_NAME=", 12)) {
                set_format_name(bi, (word[i]));
                continue;
            }
            if (!strncmp(word[i], "size=", 5)
                    || !strncmp(word[i], "SIZE=", 5)) {
                bi->size = strtoul(((char *) word[i]) + 5, NULL, 0);
                bi->size_found = true;
                continue;
            }
            if (!strncmp(word[i], "brpi=", 5)
                    || !strncmp(word[i], "BRPI=", 5)) {
                bi->brpi = strtod(((char *) word[i]) + 5, NULL);
                continue;
            }
            if (!strncmp(word[i], "bbd_size=", 9)
                    || !strncmp(word[i], "BBD_SIZE=", 9)) {
                bi->bbd_size = strtol(((char *) word[i]) + 9, NULL, 0);
                continue;
            }
            if (!strncmp(word[i], "force_type=", 11)
                    || !strncmp(word[i], "force_type=", 11)) {
                bi->force_type = strtol(((char *) word[i]) + 11, NULL, 0);
                continue;
            }
            if (!strncmp(word[i], "neut=", 5)
                    || !strncmp(word[i], "NEUT=", 5)) {
                bi->neut = is_true(((char *) word[i]) + 5);
                continue;
            }
            if (!strncmp(word[i], "neutral=", 8)
                    || !strncmp(word[i], "NEUTRAL=", 8)) {
                bi->neut = is_true(((char *) word[i]) + 8);
                continue;
            }
            if (!strcmp(word[i], "neutral")
                    || !strcmp(word[i], "NEUTRAL")) {
                bi->neut = true;
                continue;
            }
            if (!strncmp(word[i], "buffer_number=", 14)
                    || !strncmp(word[i], "BUFFER_NUMBER=", 14)) {
                bi->buffer_number = strtol(((char *) word[i]) + 5, NULL, 0);
                continue;
            }
            if (!strncmp(word[i], "bufnumber=", 10)
                    || !strncmp(word[i], "BUFNUMBER=", 10)) {
                bi->buffer_number = strtol(((char *) word[i]) + 10, NULL, 0);
                continue;
            }
            if (!strncmp(word[i], "bufnumber=", 10)
                    || !strncmp(word[i], "BUFNUMBER=", 10)) {
                bi->buffer_number = strtol(((char *) word[i]) + 10, NULL, 0);
                continue;
            }
            if (!strncmp(word[i], "key=", 4) || !strncmp(word[i], "KEY=", 4)) {
                bi->shmem_key = strtol(((char *) word[i]) + 4, NULL, 0);
                continue;
            }
            if (!strncmp(word[i], "bsem=", 5)
                    || !strncmp(word[i], "BSEM=", 5)) {
                bi->bsem_key = strtol(((char *) word[i]) + 5, NULL, 0);
                continue;
            } else if (!strncmp(word[i], "bsem_needed", 11)
                    || !strncmp(word[i], "BSEM_NEEDED", 11)) {
                if (bi->bsem_key <= 0) {
                    bi->bsem_key = bi->shmem_key + 10000;
                } else {
                    bi->bsem_key++;
                }
                continue;
            }
            if (!strncmp(word[i], "max_proc=", 9)
                    || !strncmp(word[i], "MAX_PROC=", 9)) {
                bi->max_proc = strtol(((char *) word[i]) + 9, NULL, 0);
                bi->max_proc_set_by_user = true;
                continue;
            }
            if (!strncmp(word[i], "vme_addr=", 9)
                    || !strncmp(word[i], "VME_ADDR=", 9)) {
                bi->vme_addr = strtoul(((char *) word[i]) + 9, NULL, 0);
                continue;
            }
            if (!strncmp(word[i], "remotetype=", 11)
                    || !strncmp(word[i], "REMOTETYPE=", 11)) {
                char *remote_type_name = ((char *) word[i]) + 11;
                if (!strncmp(remote_type_name, "tcp", 3) ||
                        !strncmp(remote_type_name, "TCP", 3)) {
                    bi->remotetype = CMS_TCP_REMOTE_PORT_TYPE;
                } else if (!strncmp(remote_type_name, "udp", 3) ||
                        !strncmp(remote_type_name, "UDP", 3)) {
                    bi->remotetype = CMS_UDP_REMOTE_PORT_TYPE;
                } else if (!strncmp(remote_type_name, "gdrs_im", 3) ||
                        !strncmp(remote_type_name, "GDRS_IM", 3)) {
                    bi->remotetype = CMS_GDRS_IM_REMOTE_PORT_TYPE;
                } else if (!strncmp(remote_type_name, "stcp", 4) ||
                        !strncmp(remote_type_name, "STCP", 4)) {
                    bi->remotetype = CMS_STCP_REMOTE_PORT_TYPE;
                } else {
                    rcs_print_error("%s:%d: Remote type not recognized %s\n",
                            current_config_file, linenum,
                            remote_type_name);
                    exit(-1);
                }
                continue;
            }
            if (!strncmp(word[i], "remote_port=", 13)
                    || !strncmp(word[i], "REMOTE_PORT=", 13)) {
                bi->remote_port_set = true;
                bi->remote_port = strtol(((char *) word[i]) + 13, NULL, 0);
                continue;
            }
            if (!strncmp(word[i], "port=", 5)
                    || !strncmp(word[i], "PORT=", 5)) {
                bi->remote_port_set = true;
                bi->remote_port = strtol(((char *) word[i]) + 5, NULL, 0);
                continue;
            }
            if (!strncmp(word[i], "new_port", 8)
                    || !strncmp(word[i], "NEW_PORT", 8)) {
                bi->new_port = true;
                bi->remote_port++;
                continue;
            }
            char *encoding_type_name = NULL;
            if (!strncmp(word[i], "enc=", 4) || !strncmp(word[i], "ENC=", 4)) {
                encoding_type_name = ((char *) word[i]) + 4;
            } else if (!strncmp(word[i], "encoding_type=", 14)
                    || !strncmp(word[i], "ENCODING_TYPE=", 14)) {
                encoding_type_name = ((char *) word[i]) + 14;
            } else if (!strcmp(word[i], "xdr") || !strcmp(word[i], "XDR") ||
                    !strcmp(word[i], "xml") || !strcmp(word[i], "XML") ||
                    !strcmp(word[i], "disp") || !strcmp(word[i], "DISP") ||
                    !strcmp(word[i], "ascii") || !strcmp(word[i], "ASCII") ||
                    !strcmp(word[i], "packed") || !strcmp(word[i], "PACKED") ||
                    !strcmp(word[i], "packedl64") || !strcmp(word[i], "PACKEDL64") ||
                    !strcmp(word[i], "force_raw") || !strcmp(word[i], "FORCE_RAW")) {
                encoding_type_name = word[i];
            }
            if (NULL != encoding_type_name) {
                if (!strncmp(encoding_type_name, "xdr", 3) ||
                        !strncmp(encoding_type_name, "XDR", 3)) {
                    bi->encoding_type = CMS_XDR_ENCODING;
                } else if (!strncmp(encoding_type_name, "xml", 3) ||
                        !strncmp(encoding_type_name, "XML", 3)) {
                    bi->encoding_type = CMS_XML_ENCODING;
                } else if (!strncmp(encoding_type_name, "ascii", 5) ||
                        !strncmp(encoding_type_name, "ASCII", 5)) {
                    fprintf(stderr, "nmlcfg: configuration option \"%s\" no longer supported.\n",
                            encoding_type_name);
                    bi->encoding_type = CMS_DISPLAY_ASCII_ENCODING;
                } else if (!strncmp(encoding_type_name, "disp", 4) ||
                        !strncmp(encoding_type_name, "DISP", 4)) {
                    bi->encoding_type = CMS_DISPLAY_ASCII_ENCODING;
                }
                else if (!strncmp(encoding_type_name, "packedl64", 9) ||
                        !strncmp(encoding_type_name, "PACKEDL64", 9)) {
                    bi->encoding_type = CMS_PACKEDL64_ENCODING;
                } else if (!strncmp(encoding_type_name, "packed", 6) ||
                        !strncmp(encoding_type_name, "PACKED", 6)) {
                    bi->encoding_type = CMS_PACKED_ENCODING;
                } else if (!strncmp(encoding_type_name, "display", 7) ||
                        !strncmp(encoding_type_name, "DISPLAY", 7)) {
                    bi->encoding_type = CMS_DISPLAY_ASCII_ENCODING;
                } else if (!strncmp(encoding_type_name, "force_raw", 7) ||
                        !strncmp(encoding_type_name, "FORCE_RAW", 7)) {
                    bi->encoding_type = CMS_NO_ENCODING;
                } else {
                    rcs_print_error
                            ("%s:%d: Unknown encoding type of %s\n",
                            current_config_file, linenum, (char *) encoding_type_name);
                    exit(-1);
                }
                continue;
            }
            if (!strncmp(word[i], "queue=", 6)
                    || !strncmp(word[i], "QUEUE=", 6)) {
                bi->queue_enabled = strtol(((char *) word[i]) + 6, 0, 0);
                continue;
            } else if (!strncmp(word[i], "queue", 5)
                    || !strncmp(word[i], "QUEUE", 5)) {
                if (warn) {
                    rcs_print("%s:%d: queue without length for buffer %s detected. (queue_length set to 1). use queue=<len> to avoid this warning.\n",
                            current_config_file,
                            linenum,
                            bi->name);
                }
                bi->queue_enabled = 1;
                continue;
            }
            if (!strncmp(word[i], "subdiv=", 7) || !strncmp(word[i], "SUBDIV=", 7)) {
                bi->subdiv = strtol(((char *) word[i]) + 7, 0, 0);
                continue;
            }

            if (!strncmp(word[i], "diag=", 5)
                    || !strncmp(word[i], "DIAG=", 5)) {
                bi->enable_diag = is_true(((char *) word[i]) + 5);
                continue;
            }
            if (!strncmp(word[i], "autocnum=", 9)
                    || !strncmp(word[i], "AUTOCNUM=", 9)) {
                bi->use_autocnum = is_true(((char *) word[i]) + 9);
                continue;
            }
            if (strncmp(word[i], "vme_addr=", 9) &&
                    strncmp(word[i], "VME_ADDR=", 9) &&
                    strncmp(word[i], "CLONED_ON=", 10) &&
                    strncmp(word[i], "cloned_on=", 10) &&
                    strncmp(word[i], "confirm_write", 13) &&
                    strncmp(word[i], "CONFIRM_WRITE", 13) &&
                    strcmp(word[i], "split") &&
                    strcmp(word[i], "SPLIT") &&
                    strcmp(word[i], "no_task_lock") &&
                    strcmp(word[i], "NO_TASK_LOCK") &&
                    strcmp(word[i], "do_not_print_errors") &&
                    strcmp(word[i], "DO_NOT_PRINT_ERRORS") &&
                    strcmp(word[i], "do_not_print_timeout_errors") &&
                    strcmp(word[i], "DO_NOT_PRINT_TIMEOUT_ERRORS") &&
                    strcmp(word[i], "use_local_mutex") &&
                    strcmp(word[i], "USE_LOCAL_MUTEX")
                    ) {
                if (warn) {
                    rcs_print("%s:%d: Warning: %s not recognized on buffer line for %s.\n",
                            current_config_file, linenum, word[i], bi->name);
                }
            }
            strcat(bi->endline, " ");
            strcat(bi->endline, word[i]);
        }
    }

    if (strstr(bi->endline, "split") || strstr(bi->endline, "SPLIT")) {
        if (bi->queue_enabled != 0) {
            if (warn) {
                rcs_print
                        ("%s:%d: Warning %s has both split and queue options set. The options are incompatible, the split will be ignored.\n",
                        bi->cfg_file, bi->bufline_line_number, bi->name);
            }
        } else {
            bi->split = true;
        }
    }

#if 0
    printf("last_remotetype=%d,bi->remotetype=%d,bi->remote_port=%d,default_buffer.remote_port=%d,last_remote_port=%d\n",
            last_remotetype, bi->remotetype, bi->remote_port, default_buffer.remote_port, last_remote_port);
#endif
    if ((!bi->remote_port_set || all_ports_different)
            && bi->remote_port <= last_remote_port) {
        bi->remote_port = last_remote_port + 1;
    }
    last_remote_port = bi->remote_port;
    return (0);
}

int
parse_process_info(char *bufline, PROCESS_INFO * pi) {
    char wordbuf[CMS_CONFIG_LINELEN];
    static int bad_cnums = 0;
    char *word[32];
    pi->oldstyle = (NULL == strstr(bufline, "name="))
            && (NULL == strstr(bufline, "default"));
    int num_words = separate_words_with_buffer(word, 32, bufline, wordbuf, CMS_CONFIG_LINELEN);
    if (pi->oldstyle) {
        if (num_words < 10) {
            rcs_print_error("%s:%d: Invalid line\n",
                    current_config_file, linenum);
            return -1;
        }
        pi->subscription_interval = 0;
        pi->retry_interval = 0;
        strcpy(pi->name, word[1]);
        if (!strncmp(pi->name, "default", 7)) {
            default_lines_detected = 1;
        }
        strcpy(pi->bufname, word[2]);
        if (!strncmp(pi->bufname, "default", 7)) {
            default_lines_detected = 1;
        }
        strcpy(pi->host, word[4]);
        char *process_type_name = word[3];
        /* Determine the ProcessType. */
        if (!strcmp(process_type_name, "REMOTE")) {
            pi->proctype = CMS_REMOTE_TYPE;
        } else if (!strcmp(process_type_name, "LOCAL")) {
            pi->proctype = CMS_LOCAL_TYPE;
        } else if (!strcmp(process_type_name, "PHANTOM")) {
            pi->proctype = CMS_PHANTOM_USER;
        } else if (!strcmp(process_type_name, "AUTO")) {
            pi->proctype = CMS_AUTO_TYPE;
        } else {
            rcs_print_error("%s:%d: CMS: invalid process type (%s)\n",
                    current_config_file, linenum, process_type_name);
            exit(-1);
        }
        pi->ops = NO_OPS;
        if (strstr(word[5], "R") != NULL) {
            pi->ops = READ_ONLY;
        }
        if (strstr(word[5], "W") != NULL) {
            if (pi->ops == READ_ONLY) {
                pi->ops = READ_WRITE;
            } else {
                pi->ops = WRITE_ONLY;
            }
        }
        pi->isserver = atol(word[6]);
        if (!strcmp(word[7], "INF")) {
            pi->timeout = -1.0;
        } else {
            pi->timeout = strtod(word[7], NULL);
        }
        if (NULL != word[8]) {
            pi->master = (bool) (atol(word[8]) != 0);
        }
        if (word[9] != NULL) {
            pi->c_num = atol(word[9]);
        } else {
            pi->c_num = ++bad_cnums;
        }
        memset(pi->endline, 0, sizeof (pi->endline));
        int last_word = 10;
        for (int i = last_word; i < num_words - 1; i++) {
            if (NULL == word[i]) {
                break;
            }
            strcat(pi->endline, " ");
            strcat(pi->endline, word[i]);
        }
    } else {
        for (int i = 1; i < num_words - 1; i++) {
            if (NULL == word[i]) {
                break;
            }

            if (!strncmp(word[i], "bufname=", 8)
                    || !strncmp(word[i], "BUFNAME=", 8)) {
                strncpy(pi->bufname, ((char *) word[i]) + 8,
                        sizeof (pi->bufname));
                if (!strncmp(pi->bufname, "default", 7)) {
                    default_lines_detected = 1;
                }
                continue;
            }
            if (!strncmp(word[i], "name=", 5)
                    || !strncmp(word[i], "NAME=", 5)) {
                strncpy(pi->name, ((char *) word[i]) + 5,
                        sizeof (pi->name));
                if (!strncmp(pi->name, "default", 7)) {
                    default_lines_detected = 1;
                }
                continue;
            }
            if (!strncmp(word[i], "procname=", 9)
                    || !strncmp(word[i], "PROCNAME=", 9)) {
                strncpy(pi->name, ((char *) word[i]) + 9,
                        sizeof (pi->name));
                continue;
            }
            if (!strncmp(word[i], "prochost=", 9)
                    || !strncmp(word[i], "PROCHOST=", 9)) {
                strncpy(pi->host, ((char *) word[i]) + 9,
                        sizeof (pi->host));
                continue;
            }
            if (!strncmp(word[i], "host=", 5)
                    || !strncmp(word[i], "HOST=", 5)) {
                strncpy(pi->host, ((char *) word[i]) + 5,
                        sizeof (pi->host));
                continue;
            }
            if (!strncmp(word[i], "proctype=", 9)
                    || !strncmp(word[i], "PROCTYPE=", 9)) {
                char *process_type_name = ((char *) word[i]) + 9;
                if (!strncmp(process_type_name, "LOCAL", 5) ||
                        !strncmp(process_type_name, "local", 5)) {
                    pi->proctype = CMS_LOCAL_TYPE;
                } else if (!strcmp(process_type_name, "REMOTE") ||
                        !strcmp(process_type_name, "remote")) {
                    pi->proctype = CMS_REMOTE_TYPE;
                } else if (!strcmp(process_type_name, "PHANTOM") ||
                        !strcmp(process_type_name, "phantom")) {
                    pi->proctype = CMS_PHANTOM_USER;
                } else if (!strcmp(process_type_name, "AUTO") ||
                        !strcmp(process_type_name, "auto")) {
                    pi->proctype = CMS_AUTO_TYPE;
                } else {
                    rcs_print_error("%s:%d: CMS Invalid process type (%s)\n",
                            current_config_file, linenum,
                            process_type_name);
                    exit(-1);
                }
                continue;
            }
            if (!strncmp(word[i], "ops=", 4) || !strncmp(word[i], "OPS=", 4)) {
                char *cops_ptr = word[i] + 4;
                pi->ops = NO_OPS;
                if (strstr(cops_ptr, "R") != NULL
                        || strstr(cops_ptr, "r") != NULL) {
                    pi->ops = READ_ONLY;
                }
                if (strstr(cops_ptr, "W") != NULL
                        || strstr(cops_ptr, "w") != NULL) {
                    if (pi->ops == READ_ONLY) {
                        pi->ops = READ_WRITE;
                    } else {
                        pi->ops = WRITE_ONLY;
                    }
                }
                continue;
            }
            if (!strncmp(word[i], "timeout=", 8)
                    || !strncmp(word[i], "TIMEOUT=", 8)) {
                if (!strncmp(((char *) word[i]) + 8, "INF", 3)
                        || !strncmp(((char *) word[i]) + 8, "inf", 3)) {
                    pi->timeout = -1.0;
                } else {
                    pi->timeout = strtod(((char *) word[i]) + 8, NULL);
                }
                continue;
            }
            if (!strncmp(word[i], "master=", 7)
                    || !strncmp(word[i], "MASTER=", 7)) {
                pi->master = is_true(((char *) word[i]) + 7);
                continue;
            }
            if (!strncmp(word[i], "server=", 7)
                    || !strncmp(word[i], "SERVER=", 7)) {
                pi->isserver = strtol(((char *) word[i]) + 7, 0, 0);
                continue;
            }
            if (!strncmp(word[i], "c_num=", 14)
                    || !strncmp(word[i], "C_NUM=", 14)) {
                pi->c_num = strtol(((char *) word[i]) + 5, NULL, 0);
                continue;
            }
            if (!strncmp(word[i], "sub=", 4) || !strncmp(word[i], "SUB=", 4)) {
                pi->subscription_interval =
                        strtod(((char *) word[i]) + 4, NULL);
                continue;
            }
            if (!strncmp(word[i], "retry=", 4)
                    || !strncmp(word[i], "RETRY=", 4)) {
                pi->retry_interval = strtod(((char *) word[i]) + 10, NULL);
                strcat(pi->endline, " ");
                strcat(pi->endline, word[i]);
                continue;
            }
            if (!strncmp(word[i], "read_timeout=", 13)
                    || !strncmp(word[i], "READ_TIMEOUT=", 13)) {
                strcat(pi->endline, " ");
                strcat(pi->endline, word[i]);
                continue;
            }
            if (!strncmp(word[i], "write_timeout=", 14)
                    || !strncmp(word[i], "WRITE_TIMEOUT=", 14)) {
                strcat(pi->endline, " ");
                strcat(pi->endline, word[i]);
                continue;
            }
            if (!strncmp(word[i], "connect_timeout=", 16)
                    || !strncmp(word[i], "CONNECT_TIMEOUT=", 16)) {
                strcat(pi->endline, " ");
                strcat(pi->endline, word[i]);
                continue;
            }
            if (!strncmp(word[i], "local_mutex_master", 18)
                    || !strncmp(word[i], "LOCAL_MUTEX_MASTER", 18)) {
                strcat(pi->endline, " ");
                strcat(pi->endline, word[i]);
                continue;
            }

            if (!strncmp(word[i], "bind_proc_host", 14)
                    || !strncmp(word[i], "BIND_PROC_HOST", 14)) {
                strcat(pi->endline, " ");
                strcat(pi->endline, word[i]);
                continue;
            }

            if (!strcmp(word[i], "waitformaster")
                    || !strcmp(word[i], "WAITFORMASTER")) {
                strcat(pi->endline, " ");
                strcat(pi->endline, word[i]);
                continue;
            }
            if (!strncmp(word[i], "max_timeouts=", 13)
                    || !strncmp(word[i], "MAX_TIMEOUTS=", 13)) {
                strcat(pi->endline, " ");
                strcat(pi->endline, word[i]);
                continue;
            }
            if (!strncmp(word[i], "do_cloning", 10)
                    || !strncmp(word[i], "DO_CLONING", 10)) {
                strcat(pi->endline, " ");
                strcat(pi->endline, word[i]);
                continue;
            }
            if (!strncmp(word[i], "ignore_connect_err", 18)
                    || !strncmp(word[i], "IGNORE_CONNECT_ERR", 18)) {
                strcat(pi->endline, " ");
                strcat(pi->endline, word[i]);
                continue;
            }
            if (!strncmp(word[i], "no_verify_buf", 13)
                    || !strncmp(word[i], "NO_VERIFY_BUF", 13)) {
                strcat(pi->endline, " ");
                strcat(pi->endline, word[i]);
                continue;
            }
            if (!strcmp(word[i], "poll")
                    || !strcmp(word[i], "POLL")) {
                strcat(pi->endline, " ");
                strcat(pi->endline, word[i]);
                continue;
            }

            if (!strcmp(word[i], "no_task_lock")
                    || !strcmp(word[i], "NO_TASK_LOCK")) {
                strcat(pi->endline, " ");
                strcat(pi->endline, word[i]);
                continue;
            }

            if (warn) {
                rcs_print("%s:%d: Warning: %s not recognized on process line for %s %s.\n",
                        current_config_file, linenum, word[i], pi->name, pi->bufname);
            }
            strcat(pi->endline, " ");
            strcat(pi->endline, word[i]);
        }
    }
    return (0);
}

bool
is_true(char *str) {
    if (str[0] == 'y' || str[0] == 'Y' || str[0] == 't' || str[0] == 'T') {
        return true;
    }
    if (str[0] >= '0' && str[0] <= '9') {
        return (atol(str) != 0);
    }
    return false;
}

#ifdef ENABLE_RCS_OE_INTRF

int
nmlcfg_write_oeconfig(char *filename) {
    FILE *fp = NULL;
    fp = fopen(filename, "w");
    if (NULL == fp) {
        rcs_print_error
                ("nmlcfg: error: Can't open %s for output (errno = %d:%s).\n",
                filename, errno, strerror(errno));
        exit(-1);
    }
    fprintf(fp, "OE Configuration File\n");
    fprintf(fp, "Version 2.0\n");
    fprintf(fp, "\n");
    fprintf(fp, "---------------------------------------------------------------\n");
    fprintf(fp, "--                                                           --\n");
    fprintf(fp, "--                  System Configuration                     --\n");
    fprintf(fp, "--                                                           --\n");
    fprintf(fp, "---------------------------------------------------------------\n");
    fprintf(fp, "\n");
    fprintf(fp, "Start System_Partition\n");
    fprintf(fp, "\n");

    PROCESS_INFO *pi = (PROCESS_INFO *) process_list.get_head();
    while (pi) {
        pi->written_to_oeconfig = false;
        pi = (PROCESS_INFO *) process_list.get_next();
    }


    int port = 17032;
    char name_copy[CMS_CONFIG_LINELEN];
    memset(name_copy, 0, sizeof (name_copy));
    pi = (PROCESS_INFO *) process_list.get_head();
    while (pi) {
        if (pi->isserver && !pi->written_to_oeconfig && strcmp(pi->name, name_copy)) {
            fprintf(fp, "\tData_Element OE_HOST\n");
            fprintf(fp, "\t\trecord\n");
            fprintf(fp, "\t\t\tHost_Name         \t=> \"%s\"\n", pi->name);
            fprintf(fp, "\t\t\tVendor            => \"Vendor\"\n");
            fprintf(fp, "\t\t\tPrecision         => 32\n");
            fprintf(fp, "\t\t\tProtocol          => TCP_IP\n");
            fprintf(fp, "\t\t\tIP_Address        => %s\n", pi->host);
            fprintf(fp, "\t\t\tPort              => %d\n", port);
            port++;
            fprintf(fp, "\t\t\tMaintain_Registry => True\n");
            fprintf(fp, "\t\tend_record\n");
            fprintf(fp, "\tEnd_Data_Element\n");
            fprintf(fp, "\n");
            strcpy(name_copy, pi->name);
            pi->written_to_oeconfig = true;
        }
        pi = (PROCESS_INFO *) process_list.get_next();
    }
    pi = (PROCESS_INFO *) process_list.get_head();
    while (pi) {
        if (!pi->isserver && !pi->written_to_oeconfig && strcmp(pi->name, name_copy)) {
            fprintf(fp, "\tData_Element OE_HOST\n");
            fprintf(fp, "\t\trecord\n");
            fprintf(fp, "\t\t\tHost_Name         \t=> \"%s\"\n", pi->name);
            fprintf(fp, "\t\t\tVendor            => \"Vendor\"\n");
            fprintf(fp, "\t\t\tPrecision         => 32\n");
            fprintf(fp, "\t\t\tProtocol          => TCP_IP\n");
            fprintf(fp, "\t\t\tIP_Address        => %s\n", pi->host);
            fprintf(fp, "\t\t\tPort              => %d\n", port);
            port++;
            fprintf(fp, "\t\t\tMaintain_Registry => True\n");
            fprintf(fp, "\t\tend_record\n");
            fprintf(fp, "\tEnd_Data_Element\n");
            fprintf(fp, "\n");
            strcpy(name_copy, pi->name);
            pi->written_to_oeconfig = true;
        }
        pi = (PROCESS_INFO *) process_list.get_next();
    }
    fprintf(fp, "End_Partition\n");
    return (0);
}
#endif

int
nmlcfg_write_output(char *filename) {
    FILE *fp = NULL;
    int using_stdout = 0;
    char *include_dir = 0;
    int first_buffer = 1;
    BUFFER_INFO *bi = 0;
    int comment_num = 0;
    char *comment = (char *) comments.get_head();


    CMS_BUFFERTYPE last_buftype = CMS_SHMEM_TYPE;
    int buffer_default_line_needed = 0;
    long last_vme_addr = 0;
    long last_globmem_vme_size = 0;
    char *bdl = 0;

    static char line[CMS_CONFIG_LINELEN*4];
    static char last_host_name[CMS_CONFIG_LINELEN];
    static char default_buffer_line[CMS_CONFIG_LINELEN];
    memset(line, 0, sizeof (line));
    memset(last_host_name, 0, sizeof (last_host_name));
    strcpy(last_host_name, "localhost");
    memset(default_buffer_line, 0, sizeof (default_buffer_line));
    strcpy(default_buffer_line, "buffer_default ");
    bdl = default_buffer_line;

    if (!strcmp(filename, "stdout")) {
        printf("\n\nOutput:\n");
        fp = stdout;
        using_stdout = 1;
    } else {
        fp = fopen(filename, "w");
    }
    if (NULL == fp) {
        rcs_print_error
                ("nmlcfg: error: Can't open %s for output (errno = %d:%s).\n",
                filename,
                errno,
                strerror(errno));
        exit(-1);
    }
    fprintf(fp, "# %s\n", filename);
    if (header_dir && header_dir[0]) {
        fprintf(fp, "# HEADER_DIR=%s\n", header_dir);
    }

    char *input_filename = (char *) file_list.get_head();
    while (input_filename != NULL) {
        if (strncmp(input_filename, "./", 2)) {
            fprintf(fp, "# INPUT_FILENAME=%s\n", input_filename);
        } else {
            fprintf(fp, "# INPUT_FILENAME=%s\n", input_filename + 2);
        }
        input_filename = (char *) file_list.get_next();
    }

    include_dir = (char *) include_dir_list.get_head();
    while (include_dir != NULL && include_dir[0]) {
        if (strncmp(include_dir, "./", 2)) {
            fprintf(fp, "# INCLUDE_DIR=%s\n", include_dir);
        } else {
            fprintf(fp, "# INCLUDE_DIR=%s\n", include_dir + 2);
        }
        include_dir = (char *) include_dir_list.get_next();
    }

    bi = (BUFFER_INFO *) buffer_list.get_head();
    while (bi != NULL) {
        while (comment != NULL && comment_num < bi->comment_num) {
            fprintf(fp, "%s\n", comment);
            comment = (char *) comments.get_next();
            comment_num++;
        }
        if (first_buffer) {
            fprintf(fp, "\n# Buffers:\n");
            if (!inverse_mode) {
                fprintf(fp,
                        "# %10s \t%7.7s  \t%10s \tsize \tneut \t0 \tbuf# \tmax_proc \t. . .\n",
                        "name", "type", "host");
            }
        }
        first_buffer = 0;
        if (inverse_mode) {
            if (strcmp(last_host_name, bi->host)) {
                buffer_default_line_needed = 1;
                bdl = default_buffer_line + strlen(default_buffer_line);
                size_t bdl_max_size = (size_t)
                        (default_buffer_line + sizeof (default_buffer_line) - bdl);
                SNPRINTF_FUNC(SNPRINTF_ARGS(bdl, bdl_max_size),
                        "host=%s ", bi->host);
                strcpy(last_host_name, bi->host);
            }
            if (last_buftype != bi->buftype) {
                buffer_default_line_needed = 1;
                bdl = default_buffer_line + strlen(default_buffer_line);
                size_t bdl_max_size = (size_t)
                        (default_buffer_line + sizeof (default_buffer_line) - bdl);
                SNPRINTF_FUNC(SNPRINTF_ARGS(bdl, bdl_max_size),
                        "buftype=%s ", buf_type_name[bi->buftype]);
                last_buftype = bi->buftype;
            }
            if (abs((long) bi->vme_addr - (long) last_vme_addr) > 0x10000) {
                buffer_default_line_needed = 1;
                bdl = default_buffer_line + strlen(default_buffer_line);
                size_t bdl_max_size = (size_t)
                        (default_buffer_line + sizeof (default_buffer_line) - bdl);
                SNPRINTF_FUNC(SNPRINTF_ARGS(bdl, bdl_max_size),
                        "vme_addr=0x%lX ", bi->vme_addr);
                last_vme_addr = bi->vme_addr;
            }
            if (buffer_default_line_needed) {
                buffer_default_line_needed = 0;
                fprintf(fp, "%s\n", default_buffer_line);
                memset(default_buffer_line, 0, sizeof (default_buffer_line));
                strcpy(default_buffer_line, "buffer_default ");
            }
            SNPRINTF_FUNC(SNPRINTF_ARGS(line, sizeof (line)),
                    "b name=%s ", bi->name);
            char *l;
            if (bi->size > 1024) {
                l = line + strlen(line);
                SNPRINTF_FUNC(SNPRINTF_ARGS(l, line + sizeof (line) - l),
                        "size=%ld ", bi->size);
            }
            if (bi->neut) {
                l = line + strlen(line);
                SNPRINTF_FUNC(SNPRINTF_ARGS(l, line + sizeof (line) - l),
                        "neutral=1 ");
            }
            if (bi->queue_enabled) {
                l = line + strlen(line);
                SNPRINTF_FUNC(SNPRINTF_ARGS(l, line + sizeof (line) - l),
                        "queue=1 ");
            }
            if (bi->subdiv) {
                l = line + strlen(line);
                SNPRINTF_FUNC(SNPRINTF_ARGS(l, line + sizeof (line) - l),
                        "subdiv=%d ", bi->subdiv);
            }
            if (bi->enable_diag) {
                l = line + strlen(line);
                SNPRINTF_FUNC(SNPRINTF_ARGS(l, line + sizeof (line) - l),
                        "diag=1 ");
            }
            if (bi->use_autocnum) {
                l = line + strlen(line);
                SNPRINTF_FUNC(SNPRINTF_ARGS(l, line + sizeof (line) - l),
                        "autocnum ");
            }
            if (bi->force_type > 0) {
                l = line + strlen(line);
                SNPRINTF_FUNC(SNPRINTF_ARGS(l, line + sizeof (line) - l),
                        "force_type=%d ", bi->force_type);
            }
            fprintf(fp, "%s\n", line);
        } else {
            if (bi->neut && bi->size_from_header * 4 > bi->size) {
                bi->size = bi->size_from_header * 4;
            }
            if (bi->oldstyle) {
                switch (bi->buftype) {
                    case CMS_SHMEM_TYPE:
                        if (strstr(bi->endline, "mutex=") || strstr(bi->endline, "MUTEX=")) {
                            SNPRINTF_FUNC(SNPRINTF_ARGS(line, sizeof (line)),
                                    "B %10s \tSHMEM    \t%10s \t%ld \t%d \t0 \t%d \t%d \t%d \t%s",
                                    bi->name, bi->host, bi->size, bi->neut,
                                    bi->buffer_number,
                                    bi->max_proc,
                                    bi->shmem_key,
                                    bi->endline);
                        } else {
                            if (bi->max_proc > 0) {
                                SNPRINTF_FUNC(SNPRINTF_ARGS(line, sizeof (line)),
                                        "B %10s \tSHMEM    \t%10s \t%ld \t%d \t0 \t%d \t%d \t%d \t%s",
                                        bi->name, bi->host, bi->size, bi->neut,
                                        bi->buffer_number,
                                        bi->max_proc,
                                        bi->shmem_key,
                                        bi->endline);
                            } else {
                                SNPRINTF_FUNC(SNPRINTF_ARGS(line, sizeof (line)),
                                        "B %10s \tSHMEM    \t%10s \t%ld \t%d \t* \t%d \t* \t%d \t%s",
                                        bi->name, bi->host, bi->size, bi->neut,
                                        bi->buffer_number,
                                        // * max_proc
                                        bi->shmem_key,
                                        bi->endline);
                            }
                        }
                        break;

                    default:
                        SNPRINTF_FUNC(SNPRINTF_ARGS(line, sizeof (line)),
                                "B %10s \t%7.7s \t%10s \t%ld \t%d \t* \t%d \t%d \t%s",
                                bi->name, buf_type_name[bi->buftype], bi->host,
                                bi->size, bi->neut, bi->buffer_number,
                                bi->max_proc,
                                bi->endline);
                        break;
                }
            } else {
                switch (bi->buftype) {
                    case CMS_SHMEM_TYPE:
                        if (bi->bsem_key > 0) {
                            if (strstr(bi->endline, "mutex=") || strstr(bi->endline, "MUTEX=")) {
                                SNPRINTF_FUNC(SNPRINTF_ARGS(line, sizeof (line)),
                                        "B %10s \tSHMEM    \t%10s \t%ld \t%d \t* \t%d \t%d \t%d \t%s=%d bsem=%d %10s",
                                        bi->name, bi->host, bi->size, bi->neut,
                                        bi->buffer_number,
                                        bi->max_proc,
                                        bi->shmem_key,
                                        remote_port_name[bi->remotetype],
                                        bi->remote_port, bi->bsem_key, bi->endline);
                            } else {
                                if (bi->max_proc > 0) {
                                    SNPRINTF_FUNC(SNPRINTF_ARGS(line, sizeof (line)),
                                            "B %10s \tSHMEM    \t%10s \t%ld \t%d \t0 \t%d \t%d \t%d \t%s=%d bsem=%d %10s",
                                            bi->name, bi->host, bi->size, bi->neut,
                                            bi->buffer_number,
                                            bi->max_proc,
                                            bi->shmem_key,
                                            remote_port_name[bi->remotetype],
                                            bi->remote_port,
                                            bi->bsem_key, bi->endline);
                                } else {
                                    SNPRINTF_FUNC(SNPRINTF_ARGS(line, sizeof (line)),
                                            "B %10s \tSHMEM    \t%10s \t%ld \t%d \t* \t%d \t* \t%d \t%s=%d bsem=%d %10s",
                                            bi->name, bi->host, bi->size, bi->neut,
                                            bi->buffer_number,
                                            // max_proc=*
                                            bi->shmem_key,
                                            remote_port_name[bi->remotetype],
                                            bi->remote_port,
                                            bi->bsem_key, bi->endline);
                                }
                            }
                        } else {
                            if (strstr(bi->endline, "mutex=") || strstr(bi->endline, "MUTEX=")) {
                                SNPRINTF_FUNC(SNPRINTF_ARGS(line, sizeof (line)),
                                        "B %10s \tSHMEM    \t%10s \t%ld \t%d \t* \t%d \t%d \t%d \t%s=%d %s",
                                        bi->name, bi->host, bi->size, bi->neut,
                                        bi->buffer_number,
                                        bi->max_proc,
                                        bi->shmem_key,
                                        remote_port_name[bi->remotetype],
                                        bi->remote_port, bi->endline);
                            } else {
                                if (bi->max_proc > 0) {
                                    SNPRINTF_FUNC(SNPRINTF_ARGS(line, sizeof (line)),
                                            "B %10s \tSHMEM    \t%10s \t%ld \t%d \t0 \t%d \t%d \t%d \t%s=%d %s",
                                            bi->name, bi->host, bi->size, bi->neut,
                                            bi->buffer_number,
                                            bi->max_proc,
                                            bi->shmem_key,
                                            remote_port_name[bi->remotetype],
                                            bi->remote_port, bi->endline);
                                } else {
                                    SNPRINTF_FUNC(SNPRINTF_ARGS(line, sizeof (line)),
                                            "B %10s \tSHMEM    \t%10s \t%ld \t%d \t* \t%d \t* \t%d \t%s=%d %s",
                                            bi->name, bi->host, bi->size, bi->neut,
                                            bi->buffer_number,
                                            // bi->max_proc = * 
                                            bi->shmem_key,
                                            remote_port_name[bi->remotetype],
                                            bi->remote_port, bi->endline);
                                }
                            }

                        }
                        break;

                    case CMS_GLOBMEM_TYPE:
                        if (bi->vme_addr > 0) {
                            if (bi->vme_addr >= (unsigned long) last_vme_addr &&
                                    bi->vme_addr < (unsigned long) (last_vme_addr + last_globmem_vme_size)) {
                                rcs_print
                                        ("Warning: vme_addr for %s of 0x%lX is inside space for previous globmem buffer.\n",
                                        bi->name,
                                        bi->vme_addr);
                                bi->vme_addr = last_vme_addr + last_globmem_vme_size;
                            }
                            SNPRINTF_FUNC(SNPRINTF_ARGS(line, sizeof (line)),
                                    "B %10s \tGLOBMEM \t%10s \t%ld \t%d \t0 \t%d \t%d  \t%s=%d vme_addr=0x%lX %s",
                                    bi->name, bi->host, bi->size, bi->neut,
                                    bi->buffer_number, bi->max_proc,
                                    remote_port_name[bi->remotetype],
                                    bi->remote_port, bi->vme_addr, bi->endline);
                            last_vme_addr = bi->vme_addr;
                            last_globmem_vme_size = bi->size;
                        } else {
                            SNPRINTF_FUNC(SNPRINTF_ARGS(line, sizeof (line)),
                                    "B %10s \tGLOBMEM \t%10s \t%ld \t%d \t0 \t%d \t%d \t%s=%d %s",
                                    bi->name, bi->host, bi->size, bi->neut,
                                    bi->buffer_number, bi->max_proc,
                                    remote_port_name[bi->remotetype],
                                    bi->remote_port, bi->endline);
                        }
                        break;

                    case CMS_BBDMEM_TYPE:
                        if (bi->bbd_size < 1) {
                            bi->bbd_size = bi->orig_size;
                        }
                        SNPRINTF_FUNC(SNPRINTF_ARGS(line, sizeof (line)),
                                "B %10s \tBBDMEM \t%10s \t%ld \t%d \t0 \t%d \t%d  \t%s=%d bbd_size=%d force_type=%d %s",
                                bi->name, bi->host, bi->size, bi->neut,
                                bi->buffer_number, bi->max_proc,
                                remote_port_name[bi->remotetype], bi->remote_port,
                                bi->bbd_size, bi->force_type, bi->endline);
                        break;


                    default:
                        SNPRINTF_FUNC(SNPRINTF_ARGS(line, sizeof (line)),
                                "B %10s \t%7.7s \t%10s \t%ld \t%d \t0 \t%d \t%d \t%s=%d \t%s",
                                bi->name, buf_type_name[bi->buftype], bi->host,
                                bi->size, bi->neut, bi->buffer_number,
                                bi->max_proc,
                                remote_port_name[bi->remotetype], bi->remote_port,
                                bi->endline);
                        break;
                }
                switch (bi->encoding_type) {
                    case CMS_XDR_ENCODING:
                        if (!strstr(bi->endline, "xdr")) {
                            strcat(line, " xdr ");
                        }
                        break;

                    case CMS_XML_ENCODING:
                        if (!strstr(bi->endline, "xml")) {
                            strcat(line, " xml ");
                        }
                        break;

                    case CMS_ASCII_ENCODING:
                        fprintf(stderr, "CMS ascii encoding no longer supported.\n");
                        //strcat (line, " ascii ");
                        break;

                    case CMS_PACKED_ENCODING:
                        if (!strstr(bi->endline, "packed")) {
                            strcat(line, " packed ");
                        }
                        break;

                    case CMS_PACKEDL64_ENCODING:
                        if (!strstr(bi->endline, "packedl64")) {
                            strcat(line, " packedl64 ");
                        }
                        break;

                    case CMS_DISPLAY_ASCII_ENCODING:
                        if (!strstr(bi->endline, "disp")) {
                            strcat(line, " disp ");
                        }
                        break;

                    case CMS_NO_ENCODING:
                        if (!strstr(bi->endline, "FORCE_RAW")) {
                            strcat(line, " FORCE_RAW ");
                        }
                        break;

                    default:
                        break;
                }
                if (bi->queue_enabled) {
                    strcat(line, " queue");
                    if (bi->queue_enabled > 1) {
                        char *endline = line + (int) strlen(line);
                        size_t endline_left = line + sizeof (line) - endline;
                        SNPRINTF_FUNC(SNPRINTF_ARGS(endline, endline_left),
                                " max_message_size=%ld",
                                bi->orig_size);
                    }
                }
                if (bi->subdiv) {
                    char *endline = line + (int) strlen(line);
                    size_t endline_left = line + sizeof (line) - endline;
                    SNPRINTF_FUNC(SNPRINTF_ARGS(endline, endline_left),
                            " subdiv=%d ", bi->subdiv);
                }
                if (bi->use_autocnum) {
                    strcat(line, " autocnum ");
                }
                if (bi->enable_diag) {
                    strcat(line, " diag ");
                }
                if (bi->brpi > 0.0) {
                    char *endline = line + (int) strlen(line);
                    size_t endline_left = line + sizeof (line) - endline;
                    SNPRINTF_FUNC(SNPRINTF_ARGS(endline, endline_left),
                            " brpi=%f ", bi->brpi);
                }
            }
            fprintf(fp, "%s\n", line);
            memset(line, 0, 256);
        }
        bi = (BUFFER_INFO *) buffer_list.get_next();
    }
    int first_process_line = 1;
    PROCESS_INFO *pi = (PROCESS_INFO *) process_list.get_head();
    char last_pi_procname[CMS_CONFIG_LINELEN];
    int last_was_server = 0;
    CMS_PROCESSTYPE last_pi_proctype = CMS_AUTO_TYPE;
    int last_was_master = 0;
    memset(last_pi_procname, 0, sizeof (last_pi_procname));
    char process_default_string[CMS_CONFIG_LINELEN];
    char *pds = process_default_string;
    memset(process_default_string, 0, sizeof (process_default_string));
    int process_default_string_needed = 0;

    while (pi != NULL) {
        while (comment != NULL && comment_num < pi->comment_num) {
            fprintf(fp, "%s\n", comment);
            comment = (char *) comments.get_next();
            comment_num++;
        }
        if (first_process_line) {
            fprintf(fp, "\n# Processes: \n");
            if (!inverse_mode) {
                fprintf(fp,
                        "# %10s \t%10s \t%10s \t%10s \t%10s \tserver \ttimeout \tmaster \tc_num  \t . . .\n",
                        "Name", "Buffer", "type", "host", "ops");
            }
        }
        first_process_line = 0;
        if (inverse_mode) {
            strcpy(process_default_string, "process_default ");
            if (strcmp(pi->name, last_pi_procname)) {
                pds = process_default_string + strlen(process_default_string);
                process_default_string_needed = 1;
                fprintf(fp, "\n");
                size_t pds_left = (size_t)
                        (process_default_string + sizeof (process_default_string) - pds);
                SNPRINTF_FUNC(SNPRINTF_ARGS(pds, pds_left),
                        " name=%s ", pi->name);
                strcpy(last_pi_procname, pi->name);
            }
            if (last_was_server != pi->isserver) {
                pds = process_default_string + strlen(process_default_string);
                process_default_string_needed = 1;
                size_t pds_left = (size_t)
                        (process_default_string + sizeof (process_default_string) - pds);
                SNPRINTF_FUNC(SNPRINTF_ARGS(pds, pds_left),
                        "server=%d ", pi->isserver);
                last_was_server = pi->isserver;
            }
            if ((last_was_master != 0) != pi->master) {
                pds = process_default_string + strlen(process_default_string);
                process_default_string_needed = 1;
                size_t pds_left = (size_t)
                        (process_default_string + sizeof (process_default_string) - pds);
                SNPRINTF_FUNC(SNPRINTF_ARGS(pds, pds_left),
                        "master=%d ", pi->master);
                last_was_master = pi->master;
            }
            if (last_pi_proctype != pi->proctype) {
                pds = process_default_string + strlen(process_default_string);
                process_default_string_needed = 1;
                size_t pds_left = (size_t)
                        (process_default_string + sizeof (process_default_string) - pds);
                SNPRINTF_FUNC(SNPRINTF_ARGS(pds, pds_left),
                        "proctype=%s ", proc_type_name[pi->proctype]);
                last_pi_proctype = pi->proctype;
            }
            if (process_default_string_needed) {
                process_default_string_needed = 0;
                fprintf(fp, "%s\n", process_default_string);
                memset(process_default_string, 0,
                        sizeof (process_default_string));
            }
            fprintf(fp, "p bufname=%s\n", pi->bufname);
        } else {
            if (!pi->oldstyle && pi->subscription_interval > 0.0) {
                if (pi->timeout > 0.0) {
                    fprintf(fp,
                            "P %10s \t%10s \t%10s \t%10s \t%10s \t%d \t%f \t%d \t%d sub=%f %10s\n",
                            pi->name, pi->bufname,
                            proc_type_name[pi->proctype], pi->host,
                            ops_string[pi->ops], pi->isserver, pi->timeout,
                            pi->master, pi->c_num, pi->subscription_interval,
                            pi->endline);
                } else {
                    fprintf(fp,
                            "P %10s \t%10s \t%10s \t%10s \t%10s \t%d \tINF \t%d \t%d sub=%f %10s\n",
                            pi->name, pi->bufname,
                            proc_type_name[pi->proctype], pi->host,
                            ops_string[pi->ops], pi->isserver, pi->master,
                            pi->c_num, pi->subscription_interval, pi->endline);
                }
            } else {
                int no_cnum = 0;
                if (pi->buf_info != 0) {
                    if (pi->buf_info->use_autocnum) {
                        no_cnum = 1;
                    }
                }
                if (!strcmp(pi->name, "default")) {
                    no_cnum = 1;
                }
                if (!no_cnum) {
                    if (pi->timeout > 0.0) {
                        fprintf(fp,
                                "P %10s \t%10s \t%10s \t%10s \t%10s \t%d \t%f \t%d \t%d \t%s\n",
                                pi->name, pi->bufname,
                                proc_type_name[pi->proctype], pi->host,
                                ops_string[pi->ops], pi->isserver, pi->timeout,
                                pi->master, pi->c_num, pi->endline);
                    } else {
                        fprintf(fp,
                                "P %10s \t%10s \t%10s \t%10s \t%10s \t%d \tINF     \t%d \t%d \t%s\n",
                                pi->name, pi->bufname,
                                proc_type_name[pi->proctype], pi->host,
                                ops_string[pi->ops], pi->isserver, pi->master,
                                pi->c_num, pi->endline);
                    }
                } else {
                    if (pi->timeout > 0.0) {
                        fprintf(fp,
                                "P %10s \t%10s \t%10s \t%10s \t%10s \t%d \t%f \t%d \t%d \t%s\n",
                                pi->name, pi->bufname,
                                proc_type_name[pi->proctype], pi->host,
                                ops_string[pi->ops], pi->isserver, pi->timeout,
                                pi->master, pi->c_num, pi->endline);
                    } else {
                        fprintf(fp,
                                "P %10s \t%10s \t%10s \t%10s \t%10s \t%d \tINF     \t%d \t%d \t%s\n",
                                pi->name, pi->bufname,
                                proc_type_name[pi->proctype], pi->host,
                                ops_string[pi->ops], pi->isserver, pi->master,
                                pi->c_num, pi->endline);
                    }
                }
            }
        }
        pi = (PROCESS_INFO *) process_list.get_next();
    }
    while (comment != NULL) {
        fprintf(fp, "%s\n", comment);
        comment = (char *) comments.get_next();
    }
    fprintf(fp, "\n");
    if (using_stdout) {
        fflush(fp);
    } else {
        fclose(fp);
    }
    bi = (BUFFER_INFO *) buffer_list.get_head();
    while (bi != NULL) {
        bi->proc_list.flush_list();
        bi = (BUFFER_INFO *) buffer_list.get_next();
    }
    buffer_list.flush_list();
    process_list.flush_list();
    return (0);
}
