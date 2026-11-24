# Figur 16.8: uavhengig kontroll mot C

`figure-16-8.lisp` er en separat avskrift av de 104 tonene i Bills
femstemmige figur. Den er basert på rapporten s. 45, bokkapitlet
s. 212–213 og `fux.cmn`. Tonehøydene og varighetene er kontrollert
mot notefilen; `evidence/figure-16-8-fixture.json` er den opprinnelige
maskinlesbare avskriften. Cantus er stemme 0. Genereringsrekkefølgen
for de fire andre stemmene er notestav 5, 4, 2, 1.

Søkekoden bruker species 1-regler på de første tre genererte stemmene,
selv om to av dem har halvnoterytme. Øverste, floride stemme evalueres
som species 5. Første tone i hver stemme er fast, slik at det er 89
faktiske `Check`-kall. De 89 C-kostnadene er hentet fra arkivert
`reference/fux.c` med `CurLim=10000000`, uendrede regler og uten
ficta-etterbehandling. Hele C-utskriften er lagret under `evidence/`.

Kjør etter `(load "load-species.lisp")`:

```lisp
(fux:run-figure-16-8-c-audit)
;; Figure 16.8: CL/C 89/89; C/chapter 80/89.
;; T
```

`figure-16-8-c-audit.lisp` sammenligner de lokale kostnadene note for
note, med samme romlige/rhythmiske tilstand, `:limit 10000000` og
historisk `:crossing-policy :stored`. Testen krever 89/89 mot original
C. En egen opptelling gir 80/89 C mot bokkapitlets trykte tall (med
kapitlets tre leselige scan-varianter). Variasjonen i den trykte figuren
er derfor **ikke** et CL-avvik. C gir blant annet 1000006 og 1000109
for notestav 1, tone 33 og 35, mens figuren trykker 107 og 129.
En annen, eksplisitt SAIL-inspirert variant av ligaturvekten (7 i
stedet for C-kodens 8) ga 84/89 mot trykket i tidligere kildeaudit.
Denne varianten inngår ikke i CL/C-paritetstesten.

Reproduser C-tallene med `python3 benchmark/verify-figure-c.py`; det
kompilerer arkivert original C, skårer alle notene og sammenligner
komplett utskrift med den frosne kontrollfilen. Ingen generering av
musikk skjer i denne kontrollen. Dette beviser paritet for én historisk
figur under disse regelparametrene, ikke identitet med original SAIL
på alle mulige input.
