from time import sleep
import numpy as np
import requests
import pandas as pd

def make_api_call(url, headers):
    response = requests.get(url, headers=headers)
    if response.status_code == 200:
        return response.json()
    else:
        print(f"Request failed with status {response.status_code}. Response: {response.text}")
        return None

def to_csv(data, filename):
    df = pd.DataFrame(data)
    df.to_csv(filename, index=False)
    print(f"Data exported to {filename}")

def print_data(data):
    df = pd.DataFrame(data)
    print(df)

def AccessAPI(endpoint='info', itemid='513717'):
    if (endpoint == 'info'):
        endpoint = ''
    else:
        endpoint = '/' + endpoint

    url = f'https://api.llbean.com/v1/product/info{endpoint}?itemid={itemid}'
    api_key = 'No'
    headers = {'key': api_key}
    print(url)

    raw_data = make_api_call(url, headers)

    if raw_data is not None:
        print(raw_data)
        return raw_data
    else:
        print("Failed to retrieve data")


# def process_data(data):
#     # Extract necessary information
#     item_id = data.get('info', {}).get('properties', {}).get('itemid')
#     description = data.get('info', {}).get('properties', {}).get('desc')
#     links = data.get('info', {}).get('properties', {}).get('links', [])
#     item_link = next((link['link'] for link in links if link.get('title') == 'weburl'), None)
#     image_link = next((link['link'] for link in links if link.get('title') == 'defaultImgUrl'), None)
#     price = data.get('attributes', {}).get('properties', {}).get('fullRetailPrice')
#     inventory = data.get('inventory', {}).get('properties', {}).get('glance', {}).get('retailOnHand')
#     swatches = data.get('attributes', {}).get('properties', {}).get('swatches', [])
#     colors = [swatch['color'] for swatch in swatches]
#     colors_available = len(colors)
#
#     # Create dataframe
#     df = pd.DataFrame({
#         'Item ID': [item_id],
#         'Description': [description],
#         'Item Link': [item_link],
#         'Image Link': [image_link],
#         'Price': [price],
#         'Inventory': [inventory],
#         'Colors Available': [colors_available],
#         'Colors': [', '.join(colors) if colors else None],
#     })
#
#     print(df)
#     # Write dataframe to Excel
#     # df.to_excel('BeanJune20.xlsx', index=False, engine='openpyxl')

import pandas as pd
from collections.abc import MutableMapping

def flatten_dict(d, parent_key='', sep='_'):
    items = []
    for k, v in d.items():
        new_key = f"{parent_key}{sep}{k}" if parent_key else k
        if isinstance(v, MutableMapping):
            items.extend(flatten_dict(v, new_key, sep=sep).items())
        elif isinstance(v, list):
            if k == 'copy':  # specific handling for 'copy' field
                for i, item in enumerate(v):
                    category = item['category']
                    if 'items' in item:
                        for item_sub in item['items']:
                            for j, text in enumerate(item_sub['text']):
                                items.append((f"{category}_{j}", text))
                    elif 'text' in item:
                        for j, text in enumerate(item['text']):
                            items.append((f"{category}_{j}", text))
            else:
                for i, item in enumerate(v):
                    if isinstance(item, MutableMapping):
                        items.extend(flatten_dict(item, f"{new_key}_{i}", sep=sep).items())
                    else:
                        items.append((f"{new_key}_{i}", item))
        else:
            items.append((new_key, v))
    return dict(items)



def process_data(data):
    flat_data = {k: flatten_dict(v) for k, v in data.items()}
    df = pd.json_normalize(flat_data)
    return df







def main(itemid='507646'):
    endpoints = ['info', 'attributes', 'inventory', 'personalization', 'swatches']
    AllData = {}
    for endpoint in endpoints:
        AllData[endpoint] = AccessAPI(endpoint=endpoint, itemid=itemid)
        sleep(1)
    AllData = process_data(AllData)
    return AllData

if __name__ == '__main__':
    data = main(itemid=518196)
    data.to_excel('BeanJune20.xlsx', index=False, engine='openpyxl')
    print(data)
