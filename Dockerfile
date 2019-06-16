FROM ubuntu:latest

RUN apt-get update -y
RUN apt-get install -y python-pip python-dev build-essential
COPY ./dose-calc-gke.py /dose-calc-gke.py
COPY ./requirements.txt /requirements.txt
WORKDIR .
RUN pip install -r requirements.txt
EXPOSE 5000
ENTRYPOINT ["python"]
CMD ["dose-calc-gke.py"]