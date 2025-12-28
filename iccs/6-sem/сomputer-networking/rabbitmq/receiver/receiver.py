import pika
import os
import json
import time

RABBITMQ_HOST = os.environ.get('RABBITMQ_HOST', 'rabbitmq')
QUEUE_NAME = os.environ.get('QUEUE_NAME', 'queue1')

MAX_RETRIES = 10
SLEEP_BETWEEN = 10

connection = None
for attempt in range(MAX_RETRIES):
    try:
        params = pika.ConnectionParameters(host=RABBITMQ_HOST)
        connection = pika.BlockingConnection(params)
        print("Connected to RabbitMQ!")
        break
    except pika.exceptions.AMQPConnectionError:
        print(f"Failed to connect to RabbitMQ. Retrying in {SLEEP_BETWEEN} seconds... (Attempt {attempt+1}/{MAX_RETRIES})")
        time.sleep(SLEEP_BETWEEN)

if not connection:
    raise Exception(f"Could not connect to RabbitMQ after {MAX_RETRIES} attempts")

channel = connection.channel()

exchange_name = 'headers_exchange'
channel.exchange_declare(exchange=exchange_name, exchange_type='headers')
channel.queue_declare(queue=QUEUE_NAME)

binding_criteria = os.environ.get('BINDING_CRITERIA', 'x-match')
try:
    binding_args = json.loads(binding_criteria)
except json.JSONDecodeError:
    binding_args = {}
    
channel.queue_bind(queue=QUEUE_NAME, exchange=exchange_name, arguments=binding_args)
print(f"Receiver listening on queue '{QUEUE_NAME}' with binding criteria: {binding_args}")

def callback(ch, method, properties, body):
    print(f"Received in '{QUEUE_NAME}': {body.decode()} with headers: {properties.headers}")


channel.basic_consume(queue=QUEUE_NAME, on_message_callback=callback, auto_ack=False)

#print("Starting consuming...")
#channel.start_consuming()
