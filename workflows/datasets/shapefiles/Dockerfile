FROM snakemake/snakemake:v9.3.4

COPY . /app
WORKDIR /app

# Pre-build conda environment
RUN snakemake --conda-create-envs-only --snakefile workflow/Snakefile

CMD ["snakemake", "--use-conda", "--cores", "4", "--snakefile", "workflow/Snakefile"]
