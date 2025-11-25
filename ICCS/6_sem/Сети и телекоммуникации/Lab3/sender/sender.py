import pika
import os
import time

RABBITMQ_HOST = os.environ.get('RABBITMQ_HOST', 'rabbitmq')
params = pika.ConnectionParameters(host=RABBITMQ_HOST)

MAX_RETRIES = 10
SLEEP_BETWEEN = 3

connection = None
for i in range(MAX_RETRIES):
    try:
        connection = pika.BlockingConnection(params)
        print("Connected to RabbitMQ!")
        break
    except pika.exceptions.AMQPConnectionError as e:
        print(f"Failed to connect to RabbitMQ. Retrying in {SLEEP_BETWEEN} sec...")
        time.sleep(SLEEP_BETWEEN)

if not connection:
    raise Exception("Could not connect to RabbitMQ after several attempts.")

channel = connection.channel()
exchange_name = 'headers_exchange'
channel.exchange_declare(exchange=exchange_name, exchange_type='headers')

sender_id = os.environ.get('SENDER_ID', 'sender1')

messages = [
    {
        'body': f'Message from {sender_id} with header MayI=Pass, This=Lab)',
        'headers': {'MayI': 'Pass', 'This': 'Lab)', 'sender': sender_id}
    },
    {
        'body': f'Message from {sender_id} with header SalamFrom=Baku, priority=low',
        'headers': {'SalamFrom': 'Paris', 'priority': 'low', 'sender': sender_id}
    },
    {
        'body': f'Message from {sender_id} with header What=WAZAAAAP',
        'headers': {'What': 'WAZAAAAP', 'sender': sender_id}
    },
]

x = 400
while x <= 400:
    for msg in messages:
        properties = pika.BasicProperties(headers=msg['headers'])
        channel.basic_publish(
            exchange=exchange_name,
            routing_key='',
            body=msg['body'],
            properties=properties
        )
        print(f"Sent: {msg['body']} with headers: {msg['headers']}")
        time.sleep(2)

connection.close()
