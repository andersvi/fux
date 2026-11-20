#define main fux_original_main
#include "reference-fux.c"
#undef main
int main(int argc, char **argv)
{
  int vbs[6] = {57,0,0,0,0,0};
  FillRhyPat();
  if (argc > 1 && argv[1][0] == 's') {
    fillCantus(50,53,55,52,50,0,0,0,0,0,0,0,0,0,0);
    AnySpecies(Dorian,vbs,1,5,2);
  } else {
    fillCantus(50,53,52,50,55,53,57,55,53,52,50,0,0,0,0);
    AnySpecies(Dorian,vbs,1,11,2);
  }
  return 0;
}
