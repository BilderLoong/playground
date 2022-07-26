from counter import get_freq_dist
from is_known_word import is_known_word

s = "Our next upswing task is to tokenize the words in the text. Word tokenization is the process of splitting the sentences in a given text into individual words. This is a requirement given that our goal is to determine the frequency of the words in the text. For this, we will use the word_tokenize() function."


# TODO: Create a csv file with three columns
# - The word name.
# - The word frequency in current book.
# - Whether current word is in Anki - True: is in, False: isn't.

# def create_csv():

res = []
for word, freq in get_freq_dist(s).most_common():
    res.append((word, freq, is_known_word(word)))

print(res)
