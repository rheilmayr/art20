"""
REPORT — administrative data (Excel ADM)
through 2022 inclusive.

Reports:
    - Hectares acquired
    - Beneficiary communities (unique by REGION + PJ)
    - Amount in 2025 dollars

Amount conversion:
    nominal CLP from year Y -> Dec-2025 pesos via UF (Dec-31 of each year / Dec-31-2025)
    Dec-2025 pesos -> USD using the 2025 AVERAGE observed exchange rate
"""

from pathlib import Path
import re, urllib.request
import pandas as pd

# admin region codes
REGION_CODES = {
    "TARAPACÁ": 1, 
    "ANTOFAGASTA": 2, 
    "ATACAMA": 3, 
    "VALPARAÍSO": 5,
    "BÍO BÍO": 8, 
    "LA ARAUCANÍA": 9, 
    "LOS RÍOS": 14, 
    "LOS LAGOS": 10, 
    "MAGALLANES": 12
}

# UF on Dec-31 of each year (SII) + UF on Dec-31-2025 (fetched from SII)
UF = {
    1994: 11533.17, 1995: 12482.81, 1996: 13280.43, 1997: 14096.93, 1998: 14685.39,
    1999: 15066.96, 2000: 15769.92, 2001: 16262.66, 2002: 16744.12, 2003: 16920.00,
    2004: 17317.05, 2005: 17974.81, 2006: 18336.38, 2007: 19622.66, 2008: 21452.57,
    2009: 20942.88, 2010: 21455.55, 2011: 22294.03, 2012: 22840.75, 2013: 23309.56,
    2014: 24627.10, 2015: 25629.09, 2016: 26347.98, 2017: 26798.14, 2018: 27565.79,
    2019: 28309.94, 2020: 29070.33, 2021: 30991.74, 2022: 35110.98,
}
UF_2025 = 39727.96

# --- Average observed exchange rate for 2025 (all daily quotes in the year) ---
html = urllib.request.urlopen(
    urllib.request.Request("https://www.sii.cl/valores_y_fechas/dolar/dolar2025.htm",
                           headers={"User-Agent": "Mozilla/5.0"}), timeout=30).read().decode("latin-1")
dvals = [float(n.replace(".", "").replace(",", "."))
         for n in re.findall(r'>\s*([0-9]{2,3}(?:\.[0-9]{3})?,[0-9]{2})\s*<', html)]
dvals = [v for v in dvals if 700 < v < 1300]
DOLAR_PROM = sum(dvals) / len(dvals)

# --- Excel ADM ---
df_path = Path("data") / "26941_Archivo_histórico_Tierras_20b (25_07_2023).xlsx"
df = pd.read_excel(df_path, sheet_name="20 B", header=8) 

# Check if Ñuble is in the sample, which would affect the definition of region Biobio
if "ÑUBLE" in df["PROVINCIA DE LA COMUNIDAD"].value_counts().index.to_list():
    print("Restitutions in Ñuble, definition of region Biobio matters")
else:
    print("No restitutions in Ñuble, ok with either definition of region Biobio")

code = df["REGIÓN DE LA COMUNIDAD"].astype(str).str.strip().str.upper().map(REGION_CODES)
anio = pd.to_numeric(df["AÑO COMPRA INSCRIPCIÓN"], errors="coerce")
m = (anio <= 2022)
sel = df[m].copy()
sel["_code"] = code[m].astype(int)
sel["_anio"] = anio[m].astype(int)

# Hectares
total_ha = pd.to_numeric(sel["HECTÁREAS ADQUIRIDAS"], errors="coerce").sum()

# Unique communities by (REGION, PJ); compound cells like '103 / 108' -> multiple PJs.
# Non-numeric PJ (INDIVIDUAL/COPROPIEDAD/SIN PJ) = individual beneficiary, no community.
claves = set()
for rc, pjcell in zip(sel["_code"], sel["PJ COMUNIDAD"]):
    for tok in re.findall(r"\d+", str(pjcell)):
        claves.add((rc, tok))
n_com = len(claves)

# Amount: nominal by year -> Dec-2025 CLP -> USD (average rate)
nominal = sel.groupby("_anio")["MONTO DEVENGADO"].apply(
    lambda s: pd.to_numeric(s, errors="coerce").sum())
clp_2025 = sum(v * (UF_2025 / UF[y]) for y, v in nominal.items())
usd = clp_2025 / DOLAR_PROM

# Collect the report lines so we can both print them and save to file.
report_lines = []
def report(line=""):
    report_lines.append(line)
    print(line)

# Total hectares in study area & core study area
SA_ha = sel.loc[sel["_code"].isin([8,9,14,10]),"HECTÁREAS ADQUIRIDAS"].sum()

CSA_ha = sel.loc[sel["_code"].isin([9,14]) |
       sel["PROVINCIA DE LA COMUNIDAD"].isin(["ARAUCO","OSORNO","BÍO-BÍO","MALLECO"]),
       "HECTÁREAS ADQUIRIDAS"].sum()

report("=" * 64)
report("FINAL REPORT — ADM through 2022")
report("=" * 64)
report(f"Hectares acquired               : {total_ha:>18,.1f} ha")
report(f"Communities (unique REGION+PJ)   : {n_com:>18,}")
report(f"Nominal amount (current CLP)     : {nominal.sum():>18,.0f}")
report(f"Amount in Dec-2025 pesos         : {clp_2025:>18,.0f}")
report(f"Amount in 2025 dollars           : {usd:>18,.0f}  (US$ {usd/1e6:,.1f} M)")
report("-" * 64)
report(f"Average observed USD rate 2025   : {DOLAR_PROM:>10,.2f} CLP/USD  (n={len(dvals)} days)")
report(f"   range {min(dvals):,.2f} – {max(dvals):,.2f}")
report(f"UF on Dec-31-2025                : {UF_2025:,.2f}")
report("=" * 64)

report("")
report("Core Study Area:")
report(f"  Hectares in study area : {SA_ha:>18,.1f} ha")
report(f"  Share of total hectares     : {SA_ha / total_ha:>18.2%}")
report(f"  Hectares in core study area : {CSA_ha:>18,.1f} ha")
report(f"  Share of total hectares     : {CSA_ha / total_ha:>18.2%}")

# Write the report to results/admin_description.txt
out_path = Path("results") / "admin_description.txt"
out_path.write_text("\n".join(report_lines) + "\n", encoding="utf-8")
print(f"\nReport written to {out_path}")
