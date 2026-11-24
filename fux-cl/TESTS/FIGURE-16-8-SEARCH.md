# Søket med utgangspunkt i figur 16.8

Et nytt søk bruker figurens cantus `(50 53 52 50 55 53 57 55 53 52 50)`,
starttoner i genereringsrekkefølge `(38 45 57 62)`, Dorian, hele noter
i bass-stemmen, halve noter i de to indre stemmene, og den floride
stemmerytmen kodet som CMN/C-mønstrene `(9 2 1 7 6 8 1 5 7 7)`.
Dette er eksakt samme tonehøyde- og rytmeutgangspunkt som den trykte
figuren. Som i C bruker de tre tidligere stemmene first-species-regler
selv om to av dem har halvnotevarigheter. `:reference t` velger de
tidligere testede C-kompatible søkebetingelsene.

Kjør fra `fux-cl/` i LispWorks:

```lisp
(load "figure-16-8-search.lisp")
```

Resultatet var én forbedring med skår **5042**. CL og C ga nøyaktig
samme fire tonelister. Lister og poeng ligger i
`figure-16-8-search-results.sexp`. Last
`figure-16-8-search-output.cmn` i CMN for et notebilde i
`figure-16-8-search-output.eps`. Denne notefilen viser søkets noter
uten poengtallene fra Bills figur: de gamle tallene gjelder ikke de
endrede tonene.

Sammenlignet med den historiske figuren:

| Stav ovenfra | Like toner | Endringer (note: Bill → nytt søk) |
| --- | ---: | --- |
| 1, florid | 32/40 | 33: 62→57; 34: 60→62; 35: 62→57; 36: 59→64; 37: 64→67; 38: 59→71; 39: 61→73; 40: 62→74 |
| 2, indre | 20/21 | 18: 57→60 |
| 3, cantus | 11/11 | Ingen |
| 4, indre | 19/21 | 18: 50→48; 19: 52→47 |
| 5, bass | 11/11 | Ingen |

Første forskjeller kommer ved onset 68 (åttendedels-enheter), når
figurens øverste stemme får den første millionstraffen ved omskåring.
CL og C velger samme videre forløp. Det viser at den historiske
figuren ikke gjenoppstår nøyaktig under disse C-reglene, selv når
start, cantus og rytmer er fiksert. Figurens trykte poengsum 4506
kan ikke brukes som søkeskår for denne nye linjen. Figurens noter
gir 2004479 ved lokal omskåring med original C.

`python3 figure-16-8-search-c.py` gjentar C-søket og sammenligner alle
tonehøydene med den lagrede CL-linjen. Det bruker den arkiverte,
SHA-verifiserte `reference/fux.c` med bare tre nødvendige endringer:
allokeringsmarginen for `SaveIndx`, eksplisitte rytmemønstre og
halvnoterytme i de to indre stemmene. Ingen regler endres.
