import pandas as pd
import numpy as np
import re

# Read data
df = pd.read_csv('00-data/raw/plantas_revisadas.csv', sep=';')

# Rename columns as in script
df = df.rename(columns={
    'Ambiente': 'Environment',
    'Arbusto': 'Individual', 
    'Especie': 'quercus_sp',
    'Individuo': 'quercus_tag_2025',
    'Individuo 2026': 'quercus_tag_2026',
    'Posición': 'quercus_position',
    'Observaciones': 'observations'
})

# Create quercus_tag_corrected
def create_tag_corrected(row):
    tag2026 = str(row['quercus_tag_2026']).strip() if pd.notna(row['quercus_tag_2026']) else ''
    tag2025 = str(row['quercus_tag_2025']).strip() if pd.notna(row['quercus_tag_2025']) else ''
    obs = str(row['observations']).strip() if pd.notna(row['observations']) else ''
    
    # If observations not empty, handle as per script logic? 
    # Script says: when observations=="" use 2026, else depends - but looking at line 25-30
    if tag2026 != '' and tag2026 != 'nan' and tag2026 != 'Rota' and obs == '':
        return tag2026
    if tag2026 == '' or tag2026 == 'nan' and tag2025 != '' and tag2025 != 'nan':
        return tag2025
    if tag2026 != 'nan' and tag2026 != '' and tag2026 != 'Rota' and obs != '':
        # Script's logic is a bit complex - let us look up actual cases
        return tag2026  # or handle differently
    return np.nan

df['quercus_tag_corrected'] = df.apply(create_tag_corrected, axis=1)

# Translate observations
def translate_obs(obs):
    if pd.isna(obs) or str(obs).strip() == '':
        return np.nan
    obs_str = str(obs).strip()
    if obs_str == 'doble numero enves':
        return 'ambiguous_front_back_mismatch'
    if obs_str in ['duda 7', 'Dudas 7']:
        return 'ambiguous_7'
    if obs_str == 'etiqueta perdida':
        return 'unverifiable_lost_tag'
    if obs_str == 'planta desaparecida':
        return 'unverifiable_lost_plant'
    return np.nan

df['observations_clean'] = df['observations'].apply(translate_obs)

# Trim
for col in ['quercus_tag_corrected', 'quercus_sp', 'quercus_position']:
    if col in df.columns:
        df[col] = df[col].astype(str).str.strip()
        df[col] = df[col].replace('nan', np.nan)

# Apply corrections from script
df['quercus_sp'] = df.apply(lambda row: 'Quercus ilex' if row['quercus_tag_corrected'] == 'EX' 
                            else ('Quercus faginea' if row['quercus_tag_corrected'] == 'QJ' 
                                  else row['quercus_sp']), axis=1)

# Build id_quercus
df['id_quercus'] = df.apply(lambda row: 
    'E' + str(row['quercus_tag_corrected']) if row['quercus_sp'] == 'Quercus ilex' and pd.notna(row['quercus_tag_corrected'])
    else ('F' + str(row['quercus_tag_corrected']) if row['quercus_sp'] == 'Quercus faginea' and pd.notna(row['quercus_tag_corrected'])
          else np.nan), axis=1)

# Find duplicates
print("=== DUPLICATES IN (quercus_sp, quercus_tag_corrected) ===")
dups = df[df['quercus_tag_corrected'].notna()].groupby(['quercus_sp', 'quercus_tag_corrected']).size().reset_index(name='n')
dups = dups[dups['n'] > 1].sort_values('n', ascending=False)
print(dups.to_string())
print("\n")

# Show all rows for duplicates
if len(dups) > 0:
    dup_keys = set(zip(dups['quercus_sp'], dups['quercus_tag_corrected']))
    dup_rows = df[df.apply(lambda row: (row['quercus_sp'], row['quercus_tag_corrected']) in dup_keys and pd.notna(row['quercus_tag_corrected']), axis=1)]
    dup_rows = dup_rows.sort_values(['quercus_sp', 'quercus_tag_corrected', 'Individual'])
    print(dup_rows[['Individual', 'quercus_position', 'quercus_sp', 'Environment', 
                    'quercus_tag_2025', 'quercus_tag_2026', 'observations', 
                    'quercus_tag_corrected', 'id_quercus']].to_string())
    print("\n")

# Find missing numbers per species
print("=== MISSING NUMBERS IN SEQUENCE ===")
for sp in ['Quercus ilex', 'Quercus faginea']:
    sp_df = df[(df['quercus_sp'] == sp) & df['quercus_tag_corrected'].notna()]
    # Try to convert to numeric
    tags_num = []
    for t in sp_df['quercus_tag_corrected']:
        try:
            # Extract number
            m = re.findall(r'-?\d+', str(t))
            if m:
                tags_num.append(int(m[0]))
        except:
            pass
    if tags_num:
        tags_num = sorted(set(tags_num))
        min_t = min(tags_num)
        max_t = max(tags_num)
        all_nums = set(range(min_t, max_t + 1))
        missing = sorted(list(all_nums - set(tags_num)))
        print(f"{sp}: min={min_t}, max={max_t}, total_unique={len(tags_num)}, missing_count={len(missing)}")
        if missing:
            print(f"  Missing: {missing[:50]}{'...' if len(missing) > 50 else ''}")
    print()
