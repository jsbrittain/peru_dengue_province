FROM python:3.13.3-slim-bookworm

COPY requirements.txt /app/requirements.txt
RUN pip install --upgrade pip && pip install -r /app/requirements.txt

RUN python -c "\
from sentence_transformers import SentenceTransformer; \
SentenceTransformer('all-MiniLM-L6-v2')"

RUN python -c "\
from transformers import AutoTokenizer; \
AutoTokenizer.from_pretrained('sentence-transformers/all-MiniLM-L6-v2')"

ENV TRANSFORMERS_OFFLINE=true

WORKDIR /app
CMD ["streamlit", "run", "Home.py"]
