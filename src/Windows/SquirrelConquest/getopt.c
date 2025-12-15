#include "getopt.h"
#include <string.h>
#include <stdio.h>

char *optarg = NULL;
int optind = 1;
int opterr = 1;
int optopt = 0;

int getopt(int argc, char * const argv[], const char *optstring)
{
    static int optpos = 1;
    const char *optdecl = NULL;
    char *cur = NULL;
    int opt = 0;

    optarg = NULL;

    if (optind >= argc)
    {
        return -1;
    }

    cur = argv[optind];
    if (cur == NULL)
    {
        return -1;
    }

    if (cur[0] != '-' || cur[1] == '\0')
    {
        return -1;
    }

    if (strcmp(cur, "--") == 0)
    {
        optind++;
        return -1;
    }

    opt = (unsigned char)cur[optpos];
    optopt = opt;

    optdecl = strchr(optstring, opt);
    if (optdecl == NULL || opt == ':')
    {
        if (opterr != 0 && optstring[0] != ':')
        {
            fprintf(stderr, "unknown option: -%c\n", opt);
        }

        optpos++;
        if (cur[optpos] == '\0')
        {
            optind++;
            optpos = 1;
        }

        return '?';
    }

    if (optdecl[1] == ':')
    {
        if (cur[optpos + 1] != '\0')
        {
            optarg = &cur[optpos + 1];
            optind++;
            optpos = 1;
            return opt;
        }

        if (optind + 1 < argc)
        {
            optarg = argv[optind + 1];
            optind += 2;
            optpos = 1;
            return opt;
        }

        if (opterr != 0 && optstring[0] != ':')
        {
            fprintf(stderr, "option requires an argument: -%c\n", opt);
        }

        optind++;
        optpos = 1;
        return (optstring[0] == ':') ? ':' : '?';
    }

    optpos++;
    if (cur[optpos] == '\0')
    {
        optind++;
        optpos = 1;
    }

    return opt;
}
