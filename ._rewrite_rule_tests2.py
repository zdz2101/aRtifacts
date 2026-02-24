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

for path in files:
    if path.name in rowid_keep_files:
        continue

    text = path.read_text(encoding="utf-8")

    def repl(m):
        domain = m.group(1)
        var = m.group(2)
        keys = cand.get(domain, ["USUBJID"])
        key_list = ", ".join([f'"{x}"' for x in keys])
        return f'new_domain_snapshot("{domain}", {var}, key_vars = intersect(c({key_list}), names({var})))'

    text = re.sub(
        r'new_domain_snapshot\("([A-Z0-9]+)",\s*transform\(([_A-Za-z][_A-Za-z0-9]*)\s*,\s*\.ROWID\s*=\s*seq_len\(nrow\([^\)]*\)\)\)\s*,\s*key_vars\s*=\s*"\.ROWID"\)',
        repl,
        text
    )

    path.write_text(text, encoding="utf-8")

print("done")
