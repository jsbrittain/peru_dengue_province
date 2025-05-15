FROM python:3.13.3-slim-bookworm

COPY requirements.txt /app/requirements.txt
RUN pip install --upgrade pip && pip install -r /app/requirements.txt

WORKDIR /app
CMD ["streamlit", "run", "Home.py"]
