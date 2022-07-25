import nltk
from nltk import FreqDist
from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize
from nltk.stem.wordnet import WordNetLemmatizer
from nltk.tag import pos_tag

from is_know_word import is_know_word

s = "Our next upswing task is to tokenize the words in the text. Word tokenization is the process of splitting the sentences in a given text into individual words. This is a requirement given that our goal is to determine the frequency of the words in the text. For this, we will use the word_tokenize() function."

tokens = word_tokenize(s)

lem = WordNetLemmatizer()


def remove_noise(tokens: list):
    cleaned_tokens = []

    stop_words = stopwords.words("english")
    lemmatizer = WordNetLemmatizer()

    for token, tag in pos_tag(tokens):
        token = token.lower()
        if tag.startswith("NN"):
            pos = "n"
        elif tag.startswith("VB"):
            pos = "v"
        else:
            pos = "a"

        # Remove noise words.
        if (
            len(token) > 0
            and token not in stop_words
            and token not in string.punctuation
        ):
            cleaned_tokens.append(lemmatizer.lemmatize(token, pos))

    return cleaned_tokens


cleaned_tokens = remove_noise(tokens)
print(nltk.FreqDist(remove_noise).most_common())


# def get_freq_dist(text_file):
#     # open a text file in reading mode
#     with open(text_file) as f:
#         text = f.read()
#     words = text.lower().split()

# return FreqDist(words)


# dist = get_freq_dist("./The Obesity Code - Jason Fung.txt")
# print(dist.most_common(100))
