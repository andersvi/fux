"""Build the single-file FUX distribution from the editable src/ modules.

Run from anywhere: python3 tools/build.py
Uses only Python's standard library. Do not edit dist/fux.lisp directly.
"""
from pathlib import Path
from zipfile import ZipFile, ZIP_DEFLATED, ZipInfo
import re

ROOT = Path(__file__).resolve().parent.parent
ORDER = [
    "core", "first-species", "search", "poly-first-species", "poly-search",
    "second-species", "second-search", "third-species", "third-search",
    "fourth-species", "fourth-search", "fifth-species", "fifth-search",
    "mixed-second-species", "mixed-search", "mixed-voice-rules",
    "mixed-third-species", "mixed-fourth-species", "mixed-fifth-species",
    "mixed-advanced-search", "many-voice-search", "counterpoint-rhythm",
    "generate-counterpoint", "fux-cmn",
]


def strip_line_comments(source):
    out = []
    quoted = escaped = comment = False
    for ch in source:
        if comment:
            if ch == "\n":
                out.append(ch)
                comment = False
        elif quoted:
            out.append(ch)
            if escaped:
                escaped = False
            elif ch == "\\":
                escaped = True
            elif ch == '"':
                quoted = False
        elif ch == ";":
            comment = True
        else:
            out.append(ch)
            if ch == '"':
                quoted = True
    return "".join(out)


def build():
    sections = [";;;; FUX Common Lisp counterpoint generator.\n"
                ";;;; Generated from src/ by tools/build.py; edit src/ instead.\n"
                ";;;; You can find some examples at the bottom of this file.\n"
                ";;;; Sources: Bill Schottstaedt, Automatic Species Counterpoint,\n"
                ";;;; CCRMA report STAN-M-19 (1984); Automatic Counterpoint,\n"
                ";;;; in Current Directions in Computer Music Research, pp. 199-214\n"
                ";;;; (MIT Press, 1989). See README.md for bibliographic details.\n"]
    for name in ORDER:
        path = ROOT / "src" / f"{name}.lisp"
        if not path.is_file():
            raise FileNotFoundError(path)
        source = path.read_text()
        heading = source.splitlines()[0]
        if not heading.startswith(";;;; "):
            raise ValueError(f"Missing source heading in {path}")
        sections.append(heading + "\n" + strip_line_comments(source))
    example_source = (ROOT / "examples/examples.lisp").read_text()
    if example_source.count("#|") != 1 or example_source.count("|#") != 1:
        raise ValueError("Expected one inactive example block")
    sections.append(example_source[example_source.index("#|"):
                                   example_source.index("|#") + 2])
    content = "\n\n".join(sections).rstrip() + "\n"
    if re.search(r"\(assert\s|\(defun\s+(?:test-|run-.*-tests|compare-)", content):
        raise ValueError("A test or ASSERT reached the single-file build")
    if "(defpackage :fux-history" in content:
        raise ValueError("Historical figure reached the single-file build")
    dist = ROOT / "dist"
    dist.mkdir(exist_ok=True)
    (dist / "fux.lisp").write_text(content)
    zip_path = dist / "fux-public.zip"
    with ZipFile(zip_path, "w", ZIP_DEFLATED) as archive:
        for arcname, path in [
            ("fux.lisp", dist / "fux.lisp"),
            ("README.md", ROOT / "PUBLIC-README.md"),
            ("examples/README.md", ROOT / "examples/README.md"),
            ("examples/examples.lisp", ROOT / "examples/examples.lisp"),
        ]:
            info = ZipInfo(arcname, (2026, 11, 24, 0, 0, 0))
            info.compress_type = ZIP_DEFLATED
            archive.writestr(info, path.read_bytes())
    print(f"Built {dist / 'fux.lisp'} ({len(content)} characters)")
    print(f"Built {zip_path}")


if __name__ == "__main__":
    build()
