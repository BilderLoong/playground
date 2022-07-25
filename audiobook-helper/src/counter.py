from nltk import FreqDist


def get_freq_dist(text_file):
    # open a text file in reading mode
    with open(text_file) as f:
        text = f.read()
    words = text.lower().split()

    return FreqDist(words)


dist = get_freq_dist("./The Obesity Code - Jason Fung.txt")
print(dist.most_common(100))
