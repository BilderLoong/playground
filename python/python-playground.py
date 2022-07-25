import json
import string
import requests
import nltk
from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize
from nltk.stem.wordnet import WordNetLemmatizer
from nltk.tag import pos_tag



s = 'Our next upswing task is to tokenize the words in the text. Word tokenization is the process of splitting the sentences in a given text into individual words. This is a requirement given that our goal is to determine the frequency of the words in the text. For this, we will use the word_tokenize() function.'

tokens = word_tokenize(s)

lem = WordNetLemmatizer()

def clean_tokens(tokens: list):
    cleaned_tokens = []

    stop_words = stopwords.words('english')
    lemmatizer = WordNetLemmatizer()

    for token, tag in pos_tag(tokens):
      token = token.lower()
      if tag.startswith('NN'):
        pos='n'
      elif tag.startswith('VB'):
        pos = 'v'
      else:
        pos = 'a'

      # Remove noise words.
      if len(token) > 0 and token not in stop_words and token not in string.punctuation:
        cleaned_tokens.append(lemmatizer.lemmatize(token, pos)) 

    return cleaned_tokens

cleaned_tokens = clean_tokens(tokens)
print(nltk.FreqDist(clean_tokens).most_common())






proxies = {
    'http':'http://localhost:8080'
}

def get_request_dict(action_name:str,**params):
    res = {'action':action_name,'version':6,'params':params}
    return res
    

def invoke(action_name: str,**params):
    r = requests.post('http://127.0.0.1:8765',
        data=json.dumps(get_request_dict(action_name, **params )), proxies=proxies)
    res = r.json()
    
    # If response contain error, raise exception.
    if(res['error'] is not None):
      raise Exception(res['error'])

    return res['result']

def get_word_query(word: str):
    return f'expression:{word} note:odh_默认模板 -is:suspended' 

res = invoke('findCards', query=get_word_query('upswing'))
