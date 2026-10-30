path = r'C:\Users\Usuario\Documents\GitHub\Que_facil\01-scripts\01.1a.4-compute_derived_variables.R'
with open(path, encoding='utf-8') as f:
    c = f.read()
old = '# Eliminar objetos no necesarios'
new = '# Guardar microclima corregido y final\nlibrary(readr)\nreadr::write_csv2(final, "00-data/processed/microclimate.csv")\ncat("Saved: 00-data/processed/microclimate.csv\n")\n\n' + old
if old in c:
    c2 = c.replace(old, new)
    with open(path, 'w', encoding='utf-8') as f:
        f.write(c2)
    print('ok')
else:
    print('nf')
