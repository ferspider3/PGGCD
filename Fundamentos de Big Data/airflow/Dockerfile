FROM apache/airflow:2.9.2

USER root
RUN apt-get update && apt-get install -y \
    build-essential \
    python3-dev \
    libpq-dev \
    && apt-get clean

USER airflow
RUN pip install --no-cache-dir matplotlib reportlab pandas fpdf seaborn

COPY dags /opt/airflow/dags
COPY requirements.txt /requirements.txt

RUN pip install -r /requirements.txt

