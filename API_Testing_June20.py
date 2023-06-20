from time import sleep

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

def extract_data(raw_data):
    itemid = raw_data['properties']['itemid']
    desc = raw_data['properties']['desc']
    link = [link['link'] for link in raw_data['links'] if 'link' in link][0]

    return {
        'itemid': itemid,
        'desc': desc,
        'link': link
    }

def main(endpoint='info', itemid='513717'):
    url = f'https://api.llbean.com/v1/product/info/{endpoint}?itemid={itemid}'
    # url = 'https://api.llbean.com/v1/product/info?itemid=513717'
    api_key = 'FILL'
    headers = {'key': api_key}

    raw_data = make_api_call(url, headers)

    if raw_data is not None:
        data = extract_data(raw_data)
        pd.set_option('display.max_columns', None)  # or a large number like 1000
        pd.set_option('display.max_rows', None)  # or a large number like 1000
        pd.set_option('display.max_colwidth', None)  # or a large number like -1
        pd.set_option('display.width', None)  # or a large number like 2000

        print_data([data])  # Wrap data with a list to make it compatible with DataFrame
        # to_csv([data], 'Bean20Data.csv')
    else:
        print("Failed to retrieve data")


if __name__ == '__main__':
    # main(endpoint='info', itemid='507646')
    main(endpoint='attributes', itemid='507646')
    # sleep(1)
    # main(endpoint='inventory', itemid='507646')
    # main(endpoint='personalization', itemid='507646')
    # sleep(1)
    # main(endpoint='swatches', itemid='507646')
