# Set Up
import os
import re
import numpy as np
import pandas as pd
from konlpy.tag import Twitter
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.cluster import KMeans

PATH = os.getcwd

class TwitterTokenizer:
    # Tokenization
    def __init__(self):
        self.twitter = Twitter()
        self.stopwords = stopwords
    def nominalize(tweets, start, end):
        nouns = []
        for tweet in tweets[start:end]:
            nouns.append(' '.join([noun for noun in twitter.nouns(str(tweet)) if not noun in stopwords])

        print("tokenizing done")
        return nouns

def load_data(file_path):
    # Data load & simple data pre-processing
    df = pd.read_csv(os.path.join(PATH, file_path), encoding='utf-8', index_col=0)

    # Drop & rename
    df.drop(['from', 'Date'], axis=1, inplace=True)
    df.rename(columns={'x':'contents'}, inplace=True)
    print("loading done")

    return df

def make_stopwords(df):
    # Stopwords
    lines = []
    f = open(os.path.join(path, df), 'r')
    while True:
        line = f.readline()
        if not line:
            break
        lines.append(line)
    f.close()

    stopwords = set(re.sub('\n', '', word) for word in lines)
    print(list(stopwords)[0:10])
    print("making stopwords done")

    return stopwords


def remove_id(df):
    # Remove twitter id
    pattern = re.compile('.@+[A-Za-z0-9\_]*:*')
    twits = [re.sub(pattern, ' ', sentence) for sentence in list(df['contents'])]
    print("removing id done")

    return twits

def embedding_clustering(df):
    # Bag of words
    vect = CountVectorizer(min_df=0.001, encoding='utf-8', max_features=50, ngram_range=(1, 1))
    bow = vect.fit_transform(nouns)
    print("the length of vect : ", len(vect.vocabulary_))

    # Converted to dense matrix for train
    X = bow.toarray()
    print("X shape: ", X.shape)
    vect.get_feature_names()
    d = {'문재인':0, '남북정상회담':1, '지방선거':2, '자유한국당':3, '안철수':4, '더불어민주당':5, '미투':6, '바른미래당':7, '보수':8, '서울시장':9, '진보':10, '박원순':11, '김문수':12}

    # Y : keywords
    Y = np.array(df['Keyword'].map(d)).astype(int).reshape(-1, 1)

    # K-means
    kmeans = KMeans(n_clusters=13)
    kmeans.fit(X)

    # Prediction
    pred = kmeans.predict(X).reshape(-1, 1)

    # Result : prediction + results
    result = np.concatenate([pred, Y], axis=1)
    print(pd.Series(pred.reshape(-1, )).value_counts())
    print(pd.Series(Y.reshape(-1, )).value_counts())
    
    return result

# Main
if __name__ == '__main__':
    df = load_data('tw.csv')
    stopwords = make_stopwords('korean_stopwords.txt')
    twitter = Twitter()
    twits = remove_id(df)
    nouns = TwitterTokenizer.nominalize(twits, 0, 118570)
    result = embedding_clustering(df)
