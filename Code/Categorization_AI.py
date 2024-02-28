# setup OPENAI GPT4 turbo
# pip install openai
# pip install openai==0.10.2

import openai

openai.api_key = 'sk-1z

response = openai.Completion.create(
    engine="text-davinci-003",
    prompt="This is a test",
    max_tokens=5
)



# two ways to look at this problem
# 1. we can use the GPT-4 to match the categories
# 2. we can use the GPT-4 to generate the categories 
# 3. Use GPT-4 to match the Amazon name with A name in the website product names and then choose that category.