from time import sleep
import numpy as np
import requests
import pandas as pd
from collections.abc import MutableMapping

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
    pd.set_option('display.max_rows', None)
    pd.set_option('display.max_columns', None)
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


def flatten_dict(d, parent_key='', sep='_'):
    items = []
    for k, v in d.items():
        new_key = f"{parent_key}{sep}{k}" if parent_key else k
        if isinstance(v, MutableMapping):
            items.extend(flatten_dict(v, new_key, sep=sep).items())
        elif isinstance(v, list):
            if k == 'copy':
                for i, item in enumerate(v):
                    category = item['category']
                    if 'items' in item:
                        for item_sub in item['items']:
                            for j, text in enumerate(item_sub['text']):
                                items.append((f"{category}_{j}", text))
                    elif 'text' in item:
                        for j, text in enumerate(item['text']):
                            items.append((f"{category}_{j}", text))
            elif k == 'swatches':
                for i, item in enumerate(v):
                    color_key = f"color_{item['colorIndex']}_{item['color']}"
                    items.append((color_key, item['swatchUrl']))
            elif k == 'skuMap':
                color_info = []
                for i, item in enumerate(v):
                    if isinstance(item, MutableMapping):
                        color_info.append(', '.join([f"{key}: {value}" for key, value in flatten_dict(item, f"{new_key}_{i}", sep=sep).items()]))
                items.append((k, color_info))
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
    AllData = []
    for endpoint in endpoints:
        endpoint_data = AccessAPI(endpoint=endpoint, itemid=itemid)
        sleep(1)
        if endpoint == 'personalization' and not endpoint_data['properties']:
            continue  # Skip over the personalization endpoint if it is empty
        flattened_data = flatten_dict(endpoint_data)
        if endpoint == 'swatchesFIX':
            color_swatches = {key: [value] for key, value in flattened_data.items() if key.startswith('color_')}
            df = pd.DataFrame(color_swatches)
        else:
            df = pd.json_normalize(flattened_data)
        AllData.append(df)
    AllData = pd.concat(AllData, axis=1)
    return AllData





if __name__ == '__main__':
    data = main(itemid=518196)
    data.to_excel('BeanJune20.xlsx', index=False, engine='openpyxl')
    print(data)
