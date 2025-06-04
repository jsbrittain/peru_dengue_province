configfile: "config.yaml"

import os
from glob import glob

dirstem = config['dirstem']
filestem = config['filestem']

rule all:
    input:
        ['.'.join(file.split('.')[:-1]) for file in glob(f'{dirstem}/{filestem}*.RDS.trigger')]

rule phbf:
    input:
        trigger = ancient(f'{dirstem}/{filestem}{{index}}.RDS.trigger'),
        script = ancient('workflows/forecasting/pbf/pbf.R'),
    output:
        outfile = f"{dirstem}/{filestem}{{index}}.RDS",
    params:
        outdir = dirstem,
    shell:
        """
        mkdir -p {params.outdir}
        Rscript {input.script} \
            --index {wildcards.index}
        """
