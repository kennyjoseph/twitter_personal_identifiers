import ujson as json
import re
import pandas as pd
from collections import Counter,defaultdict
import string
from tqdm.notebook import tqdm as tqdm_notebook
from tqdm import tqdm
import re
from functools import cmp_to_key
from stringdist import rdlevenshtein
from os.path import abspath, join
import numpy as np
import emoji
from glob import glob
from joblib import Parallel, delayed

MAX_TOKENS_IN_IDENTITY_EXPRESSION = 5
MIN_CHAR_LEN_IDENTITIES = 2


def my_cmp(x, y):
    return x[1] - y[1] if x[1] - y[1] != 0 else len(y[0]) - len(x[0])

def read_simple_user_info(filename):
    import pandas as pd
    df = pd.read_csv(filename,sep="\t",quoting=3,names=["uid",
                                'name',
                                "screen_name",
                                'url',
                                'protected',
                                'location',
                                'description',
                                "followers_count",
                                "friends_count",
                                "created_at",
                                "utc_offset",
                                'time_zone',
                                "statuses_count",
                                "lang",
                                "status_created_at",
                                'status_coordinates',
                                "status_lang",
                                "profile_image_url_https","verified"], 
                     dtype={"uid":"str"},
                    error_bad_lines=False,
                    lineterminator='\n')
    return df.drop_duplicates("uid")

def distinct_emoji_list(string):
    """Resturns distinct list of emojis from the string"""
    return {x['emoji'] for x in emoji.emoji_lis(string)}


def clean_personal_marker(x, replace_with_underscores=True):
    """ Clean a clause extracted from a description"""
    if not x:
        return None

    # drop weird special characters
    x = x.encode('ascii',errors='ignore').decode().strip()
    x_prev  = x
    while True:

        # remove excess whitespace
        x = re.sub(r"\s+"," ",x).strip()

        # address common cases tha
        x = re.sub(r"^i (love|like|enjoy) ","",x)
        x = re.sub(r"^(i am|i'm|i'm) (a |an )?","",x)
        x = re.sub(r"^(i |a[n]?)\b","",x)
        x = re.sub(r"^(and|the|from|to)\b","",x)
        x = re.sub(r" of$","",x)
        x = re.sub(r'(on )?(snapchat|snap|ig|insta|instagram|email|phone): +[A-Za-z0-9_@.-]+'," ",x)
        x = re.sub(r'\u200d',"",x)
        # drop #
        x = x.replace("#","")
        #if not x.isdigit() and not re.search("(years?|yr[s]?) old",x):
        #    x = re.sub(r"\b[0-9]+\b","NUMBER",x)
        
        x = x.strip().strip(".,/!-]+[#@:)(-?'$%&_").strip()
        x = re.sub(r"[!\(\)?.\{\}]"," ",x).strip()
        if x == x_prev:
            return x
        x_prev = x


def generate_split_profile_description(description, replace_with_underscores=True):
    """Splits up a profile description into  a set of clauses. Returns the clauses and 
    all emojis in the description (which are being treated as identity markers)
    """
    
    #remove URLs and email addresses 
    d = re.sub(r'\w+@\w+\.\w+', '', description.lower()).strip()
    d = re.sub(r'http\S+', '', d).strip()
    d = d.replace("&emsp;","").replace("&nbsp;","")
    
    # get all emoji and remember them, then treat them as split characters
    emojis = distinct_emoji_list(d)
    d = emoji.get_emoji_regexp().sub("|",d)#.encode("ascii","namereplace").decode()
    
    # split on sensible split characters
    # | and 
    spl = [x for x in re.split(r"[\(\)|•*;~°,\n\t]|[!…]+|[-–\/.]+ | [&+:]+ | [+] |([\/])(?=[A-Za-z ])|([.!-]{2,})| and |([#@][A-Za-z0-9_]+)",
                               d.lower()) if (
        x and x.strip() !="" and not x.strip() in "|•&*#;~°.!…-/–")]

    # clean all clauses
    spl = [clean_personal_marker(x, replace_with_underscores=replace_with_underscores) for x in spl ]
    # remove weird things and things that become empty
    spl = [x for x in spl if x.strip() != "" and x.encode() != b'\xef\xb8\x8f' and x != "NUMBER"]
    return spl, emojis


def find_identifiers_simple(description):
    
    """Given a description and a vocabulary, 
    returns all elements of the vocabulary that are in that description.
    Code and approach taken from another paper I have under submission that had to
    match free text to a fixed vocabulary.  Can send along if desired.

    """
    spl, emojis = generate_split_profile_description(description, replace_with_underscores=False)
    return (spl, emojis)

def count_phrases(descriptions,is_notebook,n_cores=1):
    """ This function returns a  count of personal identifiers from a given list of descriptions,
    when a vocabulary does not yet exist"""
    tqdm_fun = tqdm_notebook if is_notebook else tqdm
    phrase_counter = Counter()
    emoji_counter = Counter()
    phrase_order_counter = Counter()
    emoji_order_counter = Counter()
    phrase_total_counter = Counter()
    emoji_total_counter = Counter()

    if n_cores == 1:
        for d in tqdm_fun(descriptions):
            spl, emojis = generate_split_profile_description(d)
            phrase_counter.update(spl)
            emoji_counter.update(emojis)
            for i,p in enumerate(spl):
                phrase_order_counter[p] += i
                phrase_total_counter[p] += len(spl)
            for i,e in enumerate(spl):
                emoji_order_counter[p] += i
                emoji_total_counter[p] += len(spl)

    else:
        results = Parallel(n_jobs=n_cores)(
            delayed(generate_split_profile_description)(d) 
                for d in tqdm(descriptions)
                )
        for spl, emojis in results:
            phrase_counter.update(spl)
            emoji_counter.update(emojis)
            for i,p in enumerate(spl):
                phrase_order_counter[p] += i
                phrase_total_counter[p] += len(spl)
            for i,e in enumerate(spl):
                emoji_order_counter[p] += i
                emoji_total_counter[p] += len(spl)

    
    # Counting emojis separatly
    emoji_idf = pd.DataFrame(emoji_counter.most_common())
    idf = pd.DataFrame(phrase_counter.most_common())
    idf.columns = ['phrase','n']
    emoji_idf.columns = ['phrase','n']

    phrase_order_df = pd.DataFrame(phrase_order_counter.most_common())
    phrase_order_df.columns = ['phrase','order_total']
    emoji_order_df = pd.DataFrame(emoji_order_counter.most_common())
    emoji_order_df.columns = ['phrase','order_total']

    phrase_total_df = pd.DataFrame(phrase_total_counter.most_common())
    phrase_total_df.columns = ['phrase','n_total']
    emoji_total_df = pd.DataFrame(emoji_total_counter.most_common())
    emoji_total_df.columns = ['phrase','n_total']
    

    phrase_df = idf.merge(phrase_order_df, on="phrase").merge(phrase_total_df,on="phrase")
    emoji_df = idf.merge(emoji_order_df, on="phrase").merge(emoji_total_df,on="phrase")
    return phrase_df, emoji_df

