/*
 * Test driver for the 11 examples in Schottstaedt's fux.c, #if EXS.
 * Compile this file, NOT fux.c separately. Keep the original fux.c unmodified.
 * Each invocation executes exactly one case (no cross-case global state).
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* The original file supplies the functions and its own main(). */
#define main fux_original_main
#include "fux.c"
#undef main

struct fux_case {
  const char *name;
  const char *mode_name;
  int mode;
  int length;
  int voice_count;
  int pitches[2];
  int cantus[15];
};

/* Exact inputs from the original #if EXS block, in source order.
 * The comments after AnySpecies() are not interpreted as expected results.
 */
static const struct fux_case cases[] = {
  {"exs01", "Dorian",  Dorian,     11, 1, {57,0}, {50,53,52,50,55,53,57,55,53,52,50}},
  {"exs02", "Dorian",  Dorian,     11, 1, {38,0}, {50,53,52,50,55,53,57,55,53,52,50}},
  {"exs03", "Phrygian",Phrygian,   10, 1, {59,0}, {52,48,50,48,45,57,55,52,53,52}},
  {"exs04", "Phrygian",Phrygian,   10, 1, {40,0}, {52,48,50,48,45,57,55,52,53,52}},
  {"exs05", "Lydian",  Lydian,     12, 1, {65,0}, {53,55,57,53,50,52,53,60,57,53,55,53}},
  {"exs06", "Lydian",  Lydian,     12, 1, {41,0}, {53,55,57,53,50,52,53,60,57,53,55,53}},
  {"exs07", "Mixolydian",Mixolydian,14,1,{55,0}, {43,48,47,43,48,52,50,55,52,48,50,47,45,43}},
  {"exs08", "Mixolydian",Mixolydian,14,1,{43,0}, {43,48,47,43,48,52,50,55,52,48,50,47,45,43}},
  {"exs09", "Aeolian", Aeolian,    12, 1, {57,0}, {45,48,47,50,48,52,53,52,50,48,47,45}},
  {"exs10", "Aeolian", Aeolian,    12, 1, {45,0}, {45,48,47,50,48,52,53,52,50,48,47,45}},
  {"exs11", "Dorian",  Dorian,     11, 2, {57,62},{50,53,52,50,55,53,57,55,53,52,50}}
};

static void list_cases(void)
{
  unsigned int i;
  for (i = 0; i < sizeof(cases)/sizeof(cases[0]); ++i)
    printf("%s\t%s\tspecies=1\tvoices=%d\tlen=%d\tstarts=%d%s\n",
           cases[i].name, cases[i].mode_name, cases[i].voice_count,
           cases[i].length, cases[i].pitches[0],
           cases[i].voice_count == 2 ? ",62" : "");
}

int main(int argc, char **argv)
{
  unsigned int i;
  const struct fux_case *c = NULL;
  if (argc == 2 && strcmp(argv[1], "--list") == 0) {
    list_cases();
    return 0;
  }
  if (argc != 2) {
    fprintf(stderr, "Usage: %s --list | exs01 .. exs11\n", argv[0]);
    return 2;
  }
  for (i = 0; i < sizeof(cases)/sizeof(cases[0]); ++i)
    if (strcmp(argv[1], cases[i].name) == 0) c = &cases[i];
  if (c == NULL) {
    fprintf(stderr, "Unknown case: %s\n", argv[1]);
    return 2;
  }
  FillRhyPat();
  /* Explicitly set all 15 values, including trailing zeroes. */
  fillCantus(c->cantus[0],c->cantus[1],c->cantus[2],c->cantus[3],
             c->cantus[4],c->cantus[5],c->cantus[6],c->cantus[7],
             c->cantus[8],c->cantus[9],c->cantus[10],c->cantus[11],
             c->cantus[12],c->cantus[13],c->cantus[14]);
  vbs[0] = c->pitches[0];
  vbs[1] = c->pitches[1];
  fprintf(stderr, "CASE %s mode=%s species=1 voices=%d length=%d starts=%d",
          c->name,c->mode_name,c->voice_count,c->length,c->pitches[0]);
  if (c->voice_count > 1) fprintf(stderr, ",%d",c->pitches[1]);
  fputc('\n',stderr);
  AnySpecies(c->mode,c->pitches,c->voice_count,c->length,1);
  return 0;
}
