#!/usr/bin/env python3
"""Recompute the 89 Figure 16.8 note costs from the archived original C.

The fixture was transcribed from the score, independently of the CL code.
This check also catches changes to the frozen C output used by the CL audit.
"""

import hashlib
import json
from pathlib import Path
import subprocess
import tempfile

root = Path(__file__).resolve().parents[1]
source = root / "reference" / "fux.c"
assert hashlib.sha256(source.read_bytes()).hexdigest() == (
    "44a81ddba0882fd5ce05f897eacbf21e3dc0c972637cecd5227a2133fb226947"
)
data = json.loads((root / "evidence" / "figure-16-8-fixture.json").read_text())
staffs = {voice["staff"]: voice for voice in data["voices"]}
base = data["cf"][-1] % 12
code = ['#include <stdio.h>', '#include <stdlib.h>', '#define CM', '#include "fux.c"',
        'int main(void) {', f'BasePitch={base}; Mode=Dorian; TotalTime=80;']
for voice, staff in enumerate((3, 5, 4, 2, 1)):
    row = staffs[staff]
    code.append(f'TotalNotes[{voice}]={len(row["pitches"])};')
    onset = 0
    for note, (pitch, duration) in enumerate(zip(row["pitches"],
                                                 row["durations"]), 1):
        code.extend((f'Ctrpt[{note}][{voice}]={pitch-base};',
                     f'Onset[{note}][{voice}]={onset};',
                     f'Dur[{note}][{voice}]={duration};'))
        onset += duration
code.append('''
for (int v=1; v<=4; v++) {
    int species=(v==4)?5:1;
    printf("voice=%d species=%d staff=%d\\n",v,species,(int[]){3,5,4,2,1}[v]);
    for (int n=2; n<=TotalNotes[v]; n++)
        printf("%d %d %d %d\\n",n,Onset[n][v],Ctrpt[n][v]+BasePitch,
               Check(n,Ctrpt[n][v],v,4,species,10000000));
}
return 0;
}''')
with tempfile.TemporaryDirectory() as tmp:
    driver = Path(tmp) / "figure.c"
    exe = Path(tmp) / "figure"
    driver.write_text("\n".join(code))
    subprocess.run(["gcc", "-std=gnu99", "-O2", "-I", str(source.parent),
                    str(driver), "-o", str(exe)], check=True)
    output = subprocess.check_output([str(exe)], text=True)

frozen = (root / "evidence" / "figure-16-8-original-c.txt").read_text()
assert output == frozen, "Original C no longer reproduces the frozen 89-note output"
rows = [row for row in output.splitlines() if not row.startswith("voice=")]
assert len(rows) == 89
print("Original fux.c: all 89 Figure 16.8 note costs reproduce exactly.")
