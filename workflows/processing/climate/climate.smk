import os
from glob import glob

rule all:
    input:
        [os.path.splitext(file)[0] + '.RDS' for file in glob('data/climate/*.tif')]

rule tif2rds:
    input:
        infile = ancient('data/climate/{filestem}.tif'),
        script = ancient('scripts/processing/climate.R'),
    output:
        outfile = 'data/climate/{filestem}.RDS',
    shell:
        """
        Rscript {input.script} \
            --input {input.infile} \
            --output {output.outfile}
        """
