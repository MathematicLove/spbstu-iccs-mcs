FROM python:3.9
WORKDIR /app
COPY sender.py .
RUN pip install pika
CMD ["python", "sender.py"]
