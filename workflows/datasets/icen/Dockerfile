FROM snakemake/snakemake:v9.3.4

COPY . /app
WORKDIR /app

CMD ["snakemake", "--cores", "4", "--snakefile", "workflow/Snakefile"]
