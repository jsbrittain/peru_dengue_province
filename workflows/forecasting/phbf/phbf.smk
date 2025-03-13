import os
from glob import glob

dirstem = 'data/INLA/Output'
filestem = "zi_pois_season_sq_rsi_ns_mod_historical_tmin_roll_2_prec_roll_2_spi_icen_historical_dir.pred"


rule all:
    input:
        ['.'.join(file.split('.')[:-1]) for file in glob(f'data/INLA/Output/{filestem}*.RDS.trigger')]

rule phbf:
    input:
        trigger = ancient(f'{dirstem}/{filestem}{{index}}.RDS.trigger'),
        workspace = ancient('data/output/province_02.RData'),
        script = ancient('workflows/forecasting/phbf/phbf.R'),
    output:
        outfile = f"{dirstem}/{filestem}{{index}}.RDS",
    shell:
        """
        Rscript {input.script} \
            --workspace {input.workspace} \
            --index {wildcards.index}
        """
