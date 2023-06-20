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

def AccessAPI(endpoint='info', itemid='513717'):
    if (endpoint == 'info'):
        endpoint = ''
    else:
        endpoint = '/' + endpoint

    url = f'https://api.llbean.com/v1/product/info{endpoint}?itemid={itemid}'
    api_key =
    headers = {'key': api_key}
    print(url)

    raw_data = make_api_call(url, headers)

    if raw_data is not None:
        print(raw_data)
    else:
        print("Failed to retrieve data")


def main(itemid='507646'):
    endpoints = ['info', 'attributes', 'inventory', 'personalization', 'swatches']
    for endpoint in endpoints:
        AccessAPI(endpoint=endpoint, itemid=itemid)
        sleep(1)

if __name__ == '__main__':
    main()
