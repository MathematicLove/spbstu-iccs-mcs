import cv2
import numpy as np
import base64
#import tensorflow as tf

def image_to_ascii(image_path):
    image = cv2.imread(image_path, cv2.IMREAD_GRAYSCALE)
    height, width = image.shape
    aspect_ratio = height / float(width)
    new_width = 100
    new_height = int(aspect_ratio * new_width * 0.55) 
    image = cv2.resize(image, (new_width, new_height))
    
    ascii_chars = "@%#/';abcdef$*+=-:. "
    ascii_str = ""
    
    for row in image:
        for pixel in row:
            ascii_str += ascii_chars[pixel // 32]
        ascii_str += '\n'
    
    return ascii_str

def ascii_to_base64(ascii_str):
    ascii_bytes = ascii_str.encode('utf-8')
    base64_bytes = base64.b64encode(ascii_bytes)
    base64_str = base64_bytes.decode('utf-8')
    return base64_str

def process_image(image_path):
    ascii_art = image_to_ascii(image_path)
    print("ASCII Art:")
    print(ascii_art)
    
    base64_ascii = ascii_to_base64(ascii_art)
    print("\nBase64 Encoded ASCII:")
    print(base64_ascii)
    
    image = cv2.imread(image_path, cv2.IMREAD_GRAYSCALE)
    bw_image_path = 'black_white_image.png'
    cv2.imwrite(bw_image_path, image)
    
    inverted_image = cv2.bitwise_not(image)
    inverted_image_path = 'inverted_image.png'
    cv2.imwrite(inverted_image_path, inverted_image)
    
    print(f"\nBlack and white image saved as {bw_image_path}")
    print(f"Inverted image saved as {inverted_image_path}")

image_path = 'image.png' 
process_image(image_path)
exit()