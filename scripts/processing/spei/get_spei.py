import os
import requests
from pathlib import Path
from bs4 import BeautifulSoup

url = 'https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/ENSEMBLES-BC/HELIX/VER2016-07-04/'
outdir = '/app/data/spei/'
ext = 'nc'


def download_files(url, ext, outdir):
    Path(outdir).mkdir(parents=True, exist_ok=True)

    r = requests.get(url)
    parsed_html = BeautifulSoup(r.text)
    nc_files = parsed_html.findAll('a', href=True)
    nc_files = [f['href'] for f in nc_files if f['href'].endswith(f'.{ext}')]
    url_files = [url + f for f in nc_files]
    contents = os.listdir(outdir)

    for url_file, nc_file in zip(url_files, nc_files):
        if nc_file in contents:
            print(f'{nc_file} already exists')
            continue
        r = requests.get(url_file)
        with open(outdir + nc_file, 'wb') as f:
            f.write(r.content)
            print(f'{nc_file} downloaded')


download_files(url, ext, outdir)
