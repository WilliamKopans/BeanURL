Key = ""
import openai
import json


openai.api_key = Key
# models = openai.Model.list()
# print(models)

MODEL = "gpt-3.5-turbo"
response = openai.ChatCompletion.create(
    model=MODEL,
    messages=[
        {"role": "system", "content": "As a customer service agent, your task is to provide valuable information about a product to a customer in a clear and concise manner. Your goal is to present the most important details about the product in a bullet point format that is easy to understand and digest. Your friendly and helpful demeanor will ensure that the customer leaves satisfied with the information they have received."},
        {"role": "user", "content": """Product Details
The everyday, everywhere pack. Designed with thoughtful details and the all-day comfort of our hiking packs, this sleek pack will keep up with daily adventures, at school and beyond.

Specs
Designed For: Ages 13 and up.
Capacity: Approx. 1,708 cu. in., 28 L.
Dimensions: 18"H x 11.75"W x 7.5"D.
Why We Love It
We designed this versatile mid-sized pack to comfortably carry everyday loads, using the same ergonomic features as our outdoor hiking packs – like a rigid back panel with extra foam padding and comfortable, breathable mesh straps to create optimum air flow. Built with rugged materials and a smart, purposeful design, it offers all-day organization and unparalleled functionality, whether you're on campus or on the road.

Fabric & Care
Place pack inside mesh laundry bag, then machine wash and line dry.
Construction
Rugged 300-denier ripstop polyester and 420-denier ripstop nylon body.
Adjustable sternum strap and tuck-away waist belt help stabilize heavy loads.
Structured, padded back panel and comfortable straps with mesh for improved air flow.
Extra-durable 1000-denier nylon bottom.
Additional Features
Reflective trim for 360-degree visibility.
Top pocket gives you quick access to your phone and other essential items.
Built-in organizer panel with multiple zippered pockets.
Stretch water bottle pockets accommodate bottles up to 32 oz.
Main compartment with padded laptop sleeve; accommodates most laptops up to 15".
Imported."""},
    ],
    temperature=0,
)

content = response['choices'][0]['message']['content']
print(content)



# print(response)

# As a customer service agent, your task is to provide valuable information about a product to a customer in a clear and concise manner. Your goal is to present the most important details about the product in a bullet point format that is easy to understand and digest. Your friendly and helpful demeanor will ensure that the customer leaves satisfied with the information they have received.




