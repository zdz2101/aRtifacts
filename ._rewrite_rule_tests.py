from pathlib import Path
import re

root = Path(r"c:/Users/zhuze/OneDrive - ATorus/Documents/aRtifacts/tests/testthat")
files = sorted(root.glob("test-rule_*.R"))

cand = {
    "ADSL": ["USUBJID"],
    "DM": ["USUBJID"],
    "AE": ["USUBJID", "AESEQ"],
    "EX": ["USUBJID", "EXSEQ"],
    "LB": ["USUBJID", "LBSEQ"],
    "VS": ["USUBJID", "VISIT", "VSTESTCD"],
    "EG": ["USUBJID", "EGSEQ"],
    "CM": ["USUBJID", "CMSEQ"],
}

rowid_keep_files = {
    "test-rule_id_002_duplicate_keys.R",
    "test-rule_id_003_high_duplicate_rate_by_site.R",
    "test-rule_id_004_high_duplicate_rate_by_subject.R",
}


def key_expr(domain, var):
    c = cand.get(domain, ["USUBJID"])
    joined = ", ".join([f'\"{x}\"' for x in c])
    return f"intersect(c({joined}), names({var}))"

for path in files:
    text = path.read_text(encoding="utf-8")

    # Fix malformed test_that header and remove injected helper-body fragment before expected_id
    text2 = re.sub(
        r'^test_that\("([^"]+)"\s*,\s*\{\}\s*\n(?:.|\n)*?\n\s*expected_id\s*<-',
        lambda m: f'test_that("{m.group(1)}", {{\n  expected_id <-',
        text,
        count=1,
        flags=re.M
    )
    text = text2

    # Replace transform(... .ROWID ...) snapshots with natural keys, except selected files
    if path.name not in rowid_keep_files:
        pattern = re.compile(
            r'new_domain_snapshot\("([A-Z0-9]+)",\s*transform\(([_A-Za-z][_A-Za-z0-9]*)\s*,\s*\.ROWID\s*=\s*seq_len\(nrow\(\2\)\)\)\s*,\s*key_vars\s*=\s*"\\.ROWID"\)'
        )

        def repl(m):
            domain = m.group(1)
            var = m.group(2)
            return f'new_domain_snapshot("{domain}", {var}, key_vars = {key_expr(domain, var)})'

        text = pattern.sub(repl, text)

    path.write_text(text, encoding="utf-8")

print(f"rewrote {len(files)} files")
